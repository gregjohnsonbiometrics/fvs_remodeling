# =============================================================================
# refit_constrained.R
#
# Companion to fvsRemodeled_CONUS.R. Refits Greg Johnson's CONUS dg and hg
# equations under two improvements that address the issues he flagged in his
# README:
#
#   (1) sign constrained nlsLM fits, so coefficients land in biologically
#       expected directions (no negative crown ratio, no positive elevation
#       penalty in the wrong direction, etc.);
#   (2) optional pooled fits within Conifer / Hardwood functional groups,
#       which stabilises species with marginal sample size and lets us
#       borrow strength across closely related species.
#
# Inputs expected:
#   - tree_dg_subset.RDS (FIA remeasurement training data for diameter growth)
#   - tree_hg_subset.RDS (FIA remeasurement training data for height growth)
#   - species.RDS        (species reference table with SPCD, CommonName, n)
#
# These live in Greg's repo under fvs_remodeling/rds/. The script is designed
# to drop into Greg's own workflow without rerunning his data assembly.
#
# Author   Aaron Weiskittel
# License  GPL 2.0 (matches upstream fvs_remodeling)
# =============================================================================

suppressPackageStartupMessages({
    library(minpack.lm)
})

# Light-weight base R helpers used in place of dplyr / tibble so the script
# runs on a vanilla R install with only minpack.lm. If you have tidyverse
# loaded the code still works; these helpers just avoid the dependency.
filter_eq <- function(df, col, val) df[!is.na(df[[col]]) & df[[col]] == val, , drop = FALSE]


# -----------------------------------------------------------------------------
# 1  Sign and magnitude bounds
# -----------------------------------------------------------------------------
# These bounds are deliberately loose. They block obviously wrong signs (e.g.
# CR coefficient negative, BAL coefficient positive) but are wide enough to
# let nlsLM find species variation. Tighten any bound you have a strong prior
# for.

dg_lower <- c(B0 = -10, B1 = -2,    B2 = -10,  B3 = 0.5,  B4 = 0.1, B5 = -0.01,  B6 = -0.1)
dg_upper <- c(B0 =  5,  B1 =  0,    B2 =  0,   B3 = 5,    B4 = 2,   B5 =  0.01,  B6 =  0.5)
# Rationale:
#   B1 < 0   (size penalty in log term)
#   B2 < 0   (BAL competition reduces growth)
#   B4 > 0   (BAL effect is monotone increasing)
#   B6 sign for EMT free; some species respond positively to warmer minima.

hg_lower <- c(B1 = 0.001,  B2 = 0.5, B3 = 0,    B4 = 0,      B5 = -0.001, B6 = -1, B7 = -0.5, B8 = 0)
hg_upper <- c(B1 = 0.5,    B2 = 5,   B3 = 5,    B4 = 0.05,   B5 =  0.01,  B6 =  3, B7 =  1,   B8 = 5)
# Rationale:
#   B1 > 0   (Chapman Richards rate)
#   B3 >= 0  (positive crown ratio effect)
#   B4 >= 0  (CCFL competition reduces growth)
#   B8 >= 0  (CCH at tip reduces growth)


# -----------------------------------------------------------------------------
# 2  Equation forms (annual increment, summed to period via integrated fit)
# -----------------------------------------------------------------------------
# These mirror Greg's est_dg and est_hg verbatim so that any species whose
# unconstrained fit converged in his original .qmd will also converge here
# given the same data. The only change is that the wrapper passes lower and
# upper bounds to nlsLM via control = nls.lm.control().

est_dg <- function(n, dbh0, bal0, bal1, cr0, cr1, ht0, ht1, elev, emt,
                   B0, B1, B2, B3, B4, B5, B6) {
    dht  <- (ht1  - ht0)  / n
    dbal <- (bal1 - bal0) / n
    dcr  <- (cr1  - cr0)  / n
    cht  <- ht0; cdbh <- dbh0; cbal <- bal0; ccr <- cr0
    for (i in seq_len(max(n))) {
        dg_hat <- exp(B0 +
                      B1 * log((cdbh + 1)^2 / (ccr * cht + 1)^B3) +
                      B2 * cbal^B4 / log(cdbh + 2.7) +
                      B5 * elev + B6 * emt)
        cdbh <- cdbh + ifelse(i <= n, dg_hat, 0)
        cbal <- cbal + ifelse(i <= n, dbal,   0)
        cht  <- cht  + ifelse(i <= n, dht,    0)
        ccr  <- ccr  + ifelse(i <= n, dcr,    0)
    }
    cdbh
}

est_hg <- function(periods, ht, cr, cr2, ccfl, ccfl2, cch, cch2,
                   max_height, elev, td, emt,
                   B1, B2, B3, B4, B5, B6, B7, B8) {
    htc <- ht; crc <- cr; cchc <- cch; ccflc <- ccfl
    crg   <- (cr2   - cr)   / periods
    ccflg <- (ccfl2 - ccfl) / periods
    cchg  <- (cch2  - cch)  / periods
    for (i in seq_len(max(periods))) {
        inc <- max_height * B1 * B2 * (crc)^B3 *
               exp(-B1 * htc - B4 * ccflc - B8 * cchc^0.5 -
                   B5 * elev + B6 * td^0.5 + B7 * emt) *
               (1 - exp(-B1 * htc))^(B2 - 1)
        htc   <- htc   + ifelse(i <= periods, inc,   0)
        crc   <- crc   + ifelse(i <= periods, crg,   0)
        ccflc <- ccflc + ifelse(i <= periods, ccflg, 0)
        cchc  <- cchc  + ifelse(i <= periods, cchg,  0)
    }
    htc
}


# -----------------------------------------------------------------------------
# 3  Constrained per species fits
# -----------------------------------------------------------------------------

fit_dg_species <- function(d, start = NULL,
                           lower = dg_lower, upper = dg_upper,
                           trace = FALSE) {
    if (is.null(start)) {
        start <- c(B0 = -1.29, B1 = -0.5836578, B2 = -0.0953406,
                   B3 = 1.83,  B4 = 0.62814,    B5 = -0.001023,
                   B6 = 0.03985)
    }
    minpack.lm::nlsLM(
        d$endDIA ~ est_dg(d$endGROWYR - d$startGROWYR,
                          d$startDIA, d$startBAL, d$endBAL,
                          d$startCR,  d$endCR,
                          d$startHT,  d$endHT,
                          d$ELEV,     d$EMT,
                          B0, B1, B2, B3, B4, B5, B6),
        data    = d,
        start   = start,
        lower   = lower,
        upper   = upper,
        control = minpack.lm::nls.lm.control(maxiter = 200),
        trace   = trace
    )
}

fit_hg_species <- function(d, max_height, start = NULL,
                           lower = hg_lower, upper = hg_upper,
                           trace = FALSE) {
    if (is.null(start)) {
        start <- c(B1 = 0.0108871, B2 = 1.6261262, B3 = 0.4573120,
                   B4 = 0.0014822, B5 = 0.0001514, B6 = 0.2736003,
                   B7 = 0.0326502, B8 = 0.866)
    }
    minpack.lm::nlsLM(
        d$endACTUALHT ~ est_hg(d$endGROWYR - d$startGROWYR,
                               d$startACTUALHT, d$startCR,   d$endCR,
                               d$startCCFL,     d$endCCFL,
                               d$startCCH,      d$endCCH,
                               max_height, d$ELEV, d$TD, d$EMT,
                               B1, B2, B3, B4, B5, B6, B7, B8),
        data    = d,
        start   = start,
        lower   = lower,
        upper   = upper,
        control = minpack.lm::nls.lm.control(maxiter = 200),
        trace   = trace
    )
}


# -----------------------------------------------------------------------------
# 4  Pooled functional group fits
# -----------------------------------------------------------------------------
# Pool conifer or hardwood species and fit one shared coefficient vector. The
# resulting parameters become the fallback that fvsRemodeled_CONUS.R returns
# when an unfitted SPCD lands in resolve_species() tier "group". By fitting
# rather than averaging, we let the data speak instead of imputing arithmetic
# means of unconstrained per species fits.

fit_dg_pooled <- function(trees, group = c("Conifer", "Hardwood"), ...) {
    group <- match.arg(group)
    d <- if (group == "Conifer") trees[trees$SPCD <  300, , drop = FALSE]
         else                    trees[trees$SPCD >= 300, , drop = FALSE]
    if (nrow(d) < 5000) {
        warning("Pooled fit has only ", nrow(d), " rows; results may be unstable.")
    }
    fit_dg_species(d, ...)
}

fit_hg_pooled <- function(trees, group = c("Conifer", "Hardwood"), ...) {
    group <- match.arg(group)
    d <- if (group == "Conifer") trees[trees$SPCD <  300, , drop = FALSE]
         else                    trees[trees$SPCD >= 300, , drop = FALSE]
    if (nrow(d) < 5000) {
        warning("Pooled fit has only ", nrow(d), " rows; results may be unstable.")
    }
    max_height <- max(d$endACTUALHT, na.rm = TRUE)
    fit_hg_species(d, max_height = max_height, ...)
}


# -----------------------------------------------------------------------------
# 5  Driver: refit all species with sign constraints + pooled fallbacks
# -----------------------------------------------------------------------------

refit_all_constrained <- function(rds_path, min_obs = 5000,
                                  out_path = NULL, trace = FALSE) {

    species   <- readRDS(file.path(rds_path, "species.RDS"))
    trees_dg  <- readRDS(file.path(rds_path, "tree_dg_subset.RDS"))
    trees_hg  <- readRDS(file.path(rds_path, "tree_hg_subset.RDS"))

    # Per species DG fits
    dg_fits <- list(); dg_pars <- NULL
    for (i in seq_len(nrow(species))) {
        if (species$n[i] < min_obs) next
        d <- filter_eq(trees_dg, "SPCD", species$SPCD[i])
        if (nrow(d) < min_obs) next
        f <- try(fit_dg_species(d, trace = trace), silent = TRUE)
        if (inherits(f, "try-error")) {
            warning("DG species ", species$SPCD[i], " failed to converge")
            next
        }
        dg_fits[[as.character(species$SPCD[i])]] <- f
        p <- f$m$getPars()
        dg_pars <- rbind(dg_pars, data.frame(
            spcd = species$SPCD[i], n = nrow(d),
            Common_Name = species$CommonName[i],
            B0 = p[1], B1 = p[2], B2 = p[3], B3 = p[4],
            B4 = p[5], B5 = p[6], B6 = p[7],
            AIC = AIC(f), isConv = f$convInfo$isConv,
            RSS = f$m$deviance()
        ))
        cat(sprintf("DG species %5d (%-22s) n=%6d   converged=%s\n",
                    species$SPCD[i], species$CommonName[i],
                    nrow(d), f$convInfo$isConv))
    }

    # Per species HG fits
    hg_fits <- list(); hg_pars <- NULL
    for (i in seq_len(nrow(species))) {
        if (species$n[i] < min_obs) next
        d <- filter_eq(trees_hg, "SPCD", species$SPCD[i])
        if (nrow(d) < min_obs) next
        max_h <- max(d$endACTUALHT, na.rm = TRUE)
        f <- try(fit_hg_species(d, max_height = max_h, trace = trace),
                 silent = TRUE)
        if (inherits(f, "try-error")) {
            warning("HG species ", species$SPCD[i], " failed to converge")
            next
        }
        hg_fits[[as.character(species$SPCD[i])]] <- f
        p <- f$m$getPars()
        hg_pars <- rbind(hg_pars, data.frame(
            spcd = species$SPCD[i], n = nrow(d),
            Common_Name = species$CommonName[i],
            B0 = max_h, B1 = p[1], B2 = p[2], B3 = p[3],
            B4 = p[4], B5 = p[5], B6 = p[6], B7 = p[7], B8 = p[8],
            AIC = AIC(f), isConv = f$convInfo$isConv,
            RSS = f$m$deviance()
        ))
        cat(sprintf("HG species %5d (%-22s) n=%6d   converged=%s\n",
                    species$SPCD[i], species$CommonName[i],
                    nrow(d), f$convInfo$isConv))
    }

    # Pooled functional group fits used as the resolve_species() group tier
    pooled <- list(
        dg_conifer  = try(fit_dg_pooled(trees_dg, "Conifer"),  silent = TRUE),
        dg_hardwood = try(fit_dg_pooled(trees_dg, "Hardwood"), silent = TRUE),
        hg_conifer  = try(fit_hg_pooled(trees_hg, "Conifer"),  silent = TRUE),
        hg_hardwood = try(fit_hg_pooled(trees_hg, "Hardwood"), silent = TRUE)
    )

    out <- list(
        dg_pars   = dg_pars,
        hg_pars   = hg_pars,
        dg_fits   = dg_fits,
        hg_fits   = hg_fits,
        pooled    = pooled,
        bounds    = list(dg_lower = dg_lower, dg_upper = dg_upper,
                         hg_lower = hg_lower, hg_upper = hg_upper),
        timestamp = Sys.time()
    )

    if (!is.null(out_path)) {
        saveRDS(out, out_path)
        cat("Wrote refit bundle to:", out_path, "\n")
    }

    invisible(out)
}


# -----------------------------------------------------------------------------
# 6  Diagnostic: did sign constraints actually bind?
# -----------------------------------------------------------------------------
# After a refit, check which species hit a bound. A coefficient that lands
# exactly on a bound is a flag that the unconstrained fit wanted to go the
# wrong way, which is exactly the issue Greg's README calls out.

bound_diagnostic <- function(refit, eps = 1e-6) {
    check <- function(pars, lower, upper) {
        coefs <- pars[, intersect(names(pars), names(lower)), drop = FALSE]
        flagged <- list()
        for (b in names(coefs)) {
            at_lower <- which(abs(coefs[[b]] - lower[b]) < eps)
            at_upper <- which(abs(coefs[[b]] - upper[b]) < eps)
            if (length(at_lower)) flagged[[paste0(b, "_at_lower")]] <-
                pars$Common_Name[at_lower]
            if (length(at_upper)) flagged[[paste0(b, "_at_upper")]] <-
                pars$Common_Name[at_upper]
        }
        flagged
    }
    list(
        dg = check(refit$dg_pars, refit$bounds$dg_lower, refit$bounds$dg_upper),
        hg = check(refit$hg_pars, refit$bounds$hg_lower, refit$bounds$hg_upper)
    )
}


# -----------------------------------------------------------------------------
# 7  Entry point
# -----------------------------------------------------------------------------
# Edit rds_path to point at your fvs_remodeling clone, then source this file
# and call refit_all_constrained(). Example:
#
#   refit <- refit_all_constrained(
#       rds_path = "F:/Projects/FVS Remodel/fvs_remodeling/rds",
#       out_path = "F:/Projects/FVS Remodel/fvs_remodeling/rds/refit_constrained.RDS")
#   diag <- bound_diagnostic(refit)
#   str(diag)
#
# The resulting RDS is a drop in replacement for dg_parms.RDS / hg_parms.RDS
# in fvsRemodeled_CONUS.R; just point load_gj_params(local_path = ...) at the
# directory containing the new file (after splitting out dg_pars and hg_pars
# under the original filenames).

# -----------------------------------------------------------------------------
# 8  Synthetic training data generator (smoke test path)
# -----------------------------------------------------------------------------
# Greg's tree_dg_subset.RDS and tree_hg_subset.RDS are NOT in the public
# fvs_remodeling repo (they are local to him). For anyone who wants to
# exercise this script before pulling those files, the function below
# generates a small synthetic training set with known parameters. The
# refit should converge close to the parameters used to simulate, with
# constrained signs always satisfied. This is useful for confirming the
# bounds and the integrated fitting loop work end to end.

simulate_training <- function(n_per_species = 6000,
                              spcds = c(202, 131, 316),
                              years_min = 5, years_max = 12,
                              seed = 42) {
    set.seed(seed)

    sim_one <- function(spcd) {
        # "True" parameters loosely modelled on Greg's published priors.
        true_dg <- c(B0 = -1.4, B1 = -0.55, B2 = -0.10, B3 = 1.85,
                     B4 = 0.62, B5 = -0.001, B6 = 0.04)
        true_hg <- c(B1 = 0.011, B2 = 1.6, B3 = 0.45, B4 = 0.0015,
                     B5 = 0.00015, B6 = 0.27, B7 = 0.033, B8 = 0.86)

        # Draw covariates that span realistic CONUS conditions.
        n <- n_per_species
        startDIA <- exp(runif(n, log(2),  log(30)))
        startHT  <- 4.5 + 1.2 * startDIA + rnorm(n, sd = 5)
        startHT  <- pmax(startHT, 8)
        startCR  <- pmin(pmax(rnorm(n, 0.55, 0.15), 0.1), 0.95)
        startBAL <- runif(n, 0, 200)
        endBAL   <- pmax(0, startBAL + rnorm(n, 0, 5))
        endCR    <- pmin(pmax(startCR + rnorm(n, 0, 0.05), 0.05), 0.95)
        endHT    <- startHT  # filled below
        startCCFL <- runif(n, 0, 250)
        endCCFL   <- pmax(0, startCCFL + rnorm(n, 0, 5))
        startCCH  <- pmin(pmax(rnorm(n, 0.5, 0.2), 0.05), 1)
        endCCH    <- pmin(pmax(startCCH + rnorm(n, 0, 0.02), 0.05), 1)
        ELEV <- runif(n, 200, 6000)
        TD   <- runif(n, 12, 28)
        EMT  <- runif(n, -25, 0)

        years <- sample(years_min:years_max, n, replace = TRUE)
        startGROWYR <- rep(2005, n)
        endGROWYR   <- startGROWYR + years

        # Forward simulate end DIA via the integrated DG equation.
        endDIA <- numeric(n)
        for (i in seq_len(n)) {
            d  <- startDIA[i]; b <- startBAL[i]; c_ <- startCR[i]
            h  <- startHT[i]
            db <- (endBAL[i] - startBAL[i]) / years[i]
            dc <- (endCR[i]  - startCR[i])  / years[i]
            for (k in seq_len(years[i])) {
                inc <- exp(true_dg["B0"] +
                           true_dg["B1"] * log((d + 1)^2 / (c_ * h + 1)^true_dg["B3"]) +
                           true_dg["B2"] * b^true_dg["B4"] / log(d + 2.7) +
                           true_dg["B5"] * ELEV[i] +
                           true_dg["B6"] * EMT[i])
                d <- d + inc
                b <- b + db
                c_ <- c_ + dc
            }
            endDIA[i] <- d + rnorm(1, sd = 0.1)
        }

        # Forward simulate end ACTUALHT via the integrated HG equation.
        max_height <- 280
        endACTUALHT <- numeric(n)
        for (i in seq_len(n)) {
            ht <- startHT[i]; cr_ <- startCR[i]
            ccfl <- startCCFL[i]; cch <- startCCH[i]
            dcr <- (endCR[i] - startCR[i]) / years[i]
            dccfl <- (endCCFL[i] - startCCFL[i]) / years[i]
            dcch <- (endCCH[i] - startCCH[i]) / years[i]
            for (k in seq_len(years[i])) {
                inc <- max_height * true_hg["B1"] * true_hg["B2"] *
                       cr_^true_hg["B3"] *
                       exp(-true_hg["B1"] * ht - true_hg["B4"] * ccfl -
                           true_hg["B8"] * sqrt(max(cch, 0)) -
                           true_hg["B5"] * ELEV[i] +
                           true_hg["B6"] * sqrt(max(TD[i], 0)) +
                           true_hg["B7"] * EMT[i]) *
                       (1 - exp(-true_hg["B1"] * ht))^(true_hg["B2"] - 1)
                ht <- ht + inc
                cr_ <- cr_ + dcr
                ccfl <- ccfl + dccfl
                cch  <- cch + dcch
            }
            endACTUALHT[i] <- ht + rnorm(1, sd = 0.5)
        }

        data.frame(
            SPCD = spcd,
            startDIA = startDIA, endDIA = endDIA,
            startHT = startHT, endHT = endHT,
            startACTUALHT = startHT, endACTUALHT = endACTUALHT,
            startCR  = startCR,  endCR  = endCR,
            startBAL = startBAL, endBAL = endBAL,
            startCCFL = startCCFL, endCCFL = endCCFL,
            startCCH  = startCCH,  endCCH  = endCCH,
            ELEV = ELEV, TD = TD, EMT = EMT,
            startGROWYR = startGROWYR, endGROWYR = endGROWYR
        )
    }

    do.call(rbind, lapply(spcds, sim_one))
}

# Run a smoke test: simulate, refit constrained, compare recovered
# coefficients to truth. This is what gets executed when you
# Rscript refit_constrained.R from the command line.

run_smoke_test <- function(n_per_species = 6000) {
    cat("Generating synthetic training data (n =", n_per_species, "per species)...\n")
    sim_dg <- simulate_training(n_per_species)
    sim_hg <- sim_dg  # same rows used for hg fitting

    cat("Fitting DG (SPCD 202, constrained)...\n")
    d <- filter_eq(sim_dg, "SPCD", 202)
    f_dg <- fit_dg_species(d, trace = FALSE)
    cat("DG fitted coefs:\n"); print(round(f_dg$m$getPars(), 4))
    cat("DG converged    :", f_dg$convInfo$isConv, "\n")
    cat("\n[Truth was]      B0 = -1.40  B1 = -0.55  B2 = -0.10  B3 = 1.85",
        " B4 = 0.62  B5 = -0.001  B6 = 0.04\n\n")

    cat("Fitting HG (SPCD 202, constrained)...\n")
    f_hg <- fit_hg_species(d, max_height = max(d$endACTUALHT), trace = FALSE)
    cat("HG fitted coefs:\n"); print(round(f_hg$m$getPars(), 4))
    cat("HG converged    :", f_hg$convInfo$isConv, "\n")
    cat("\n[Truth was]      B1 = 0.0110  B2 = 1.60  B3 = 0.45  B4 = 0.0015",
        " B5 = 0.00015  B6 = 0.27  B7 = 0.033  B8 = 0.86\n")

    invisible(list(dg = f_dg, hg = f_hg))
}


# -----------------------------------------------------------------------------
# 9  Entry points
# -----------------------------------------------------------------------------
# When sourced interactively, just announce. When run as a script, fire the
# smoke test against synthetic data so the script demonstrates it works end
# to end without needing Greg's private RDS files.

if (interactive()) {
    cat("refit_constrained.R loaded. Options:\n")
    cat("  refit_all_constrained(rds_path = '...')   # against real training data\n")
    cat("  run_smoke_test()                            # against synthetic data\n")
} else if (!is.null(sys.frames()) || identical(commandArgs()[1], "RStudio")) {
    # noop in IDE
} else if (length(commandArgs(trailingOnly = TRUE)) == 0) {
    run_smoke_test()
}

# =============================================================================
# End of refit_constrained.R
# =============================================================================
