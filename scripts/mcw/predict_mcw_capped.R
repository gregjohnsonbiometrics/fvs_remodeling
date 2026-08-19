## ---------------------------------------------------------------------------
## predict_mcw_capped.R
## CONUS crown-width coefficient library: CAPPED MCW predictor (SOURCEABLE)
##
## Written by pipeline/06b_export_clade.R alongside the library it reads.
## Base R plus data.table only: no brms, no posterior, no fit object.
##
## WHY A CAP
##   The MCW equation is a power curve, CW = a1 * DBH^a2. It has no asymptote,
##   so it keeps rising for as long as you keep feeding it diameter. It is a
##   FIT ENVELOPE, valid across the diameter range the species was fit over,
##   not a growth law. Pushed past that range it returns crown widths no North
##   American tree carries: of the 221 species in the library, 90 predict a
##   crown wider than 75 ft at DBH 60 in and 22 exceed 100 ft at that same
##   diameter; by DBH 100 in, 95 exceed 100 ft (measured by
##   pipeline/17_library_stress_test.R, 2026-07-29). Every equation therefore
##   ships with its fitted domain, and this helper enforces it.
##
## THE RULE, PART 1: THE DOMAIN CAP
##   DBH at or below dbh_max_fit : CW = a1 * DBH^a2
##   DBH above dbh_max_fit       : CW = cw_at_dbh_max = a1 * dbh_max_fit^a2
##   The capped prediction is continuous at dbh_max_fit and flat above it, so
##   it never decreases and never runs away.
##
## THE RULE, PART 2: THE OBSERVED CROWN CEILING
##   The domain cap bounds the DIAMETER a curve is asked about. It cannot bound
##   the CROWN WIDTH a curve returns inside its own fitted range, and for some
##   species that is where the implausibility lives. Eastern cottonwood, SPCD
##   742, is the clearest case: fit over 5.2 to 57.9 in DBH on n = 56 trees, its
##   curve returns 105.5 ft of crown at 57.9 in, a diameter its own data
##   contains, while the widest crown ever measured on any of the 66,723
##   hardwoods in the fitting sample is 75.0 ft. A cap keyed only on diameter
##   passes that through untouched.
##
##   So a second, per-clade cap is applied on the crown width itself:
##       CW returned = min(domain-capped CW, cw_ceiling for the clade)
##   cw_ceiling is a column of the library and is the maximum crown width
##   OBSERVED in the fitting sample for that clade. Both caps are applied at
##   PREDICTION time. Neither touches a1 or a2, so the published coefficients
##   stay exactly what the tau 0.95 quantile fit produced and remain traceable
##   to it.
##
## PROVENANCE OF THE CEILING
##   Measured 2026-08-08 over the 127,710 tree fitting sample,
##   data/processed/cw_eco_aug.rds, on the FHM crown-width scale
##   (cw_src = geometric_mean_two_perp, the geometric mean of two perpendicular
##   crown diameters):
##       hardwood, n = 66,723 : max 75.00 ft, p99.9 57.47, p99.5 46.71
##       softwood, n = 60,987 : max 65.18 ft, p99.9 43.48, p99.5 35.47
##   The hardwood ceiling is not one isolated tree. Nine hardwood observations
##   lie between 68.2 and 75.0 ft in a smooth continuum, so the top of the
##   distribution is populated rather than a lone spike, and 75.0 ft is a
##   measured value rather than a recording limit. The ceilings are therefore
##   set at those clade maxima, hardwood 75.0 ft and softwood 65.2 ft (65.18
##   rounded up to 0.1 ft).
##
##   The ceiling is a statement on the FHM measurement scale and inside the
##   FITTED diameter domain. It is NOT a claim about the widest crown a North
##   American tree can carry. The American Forests champion register documents
##   far wider crowns, up to 161 ft of spread on a live oak, Quercus virginiana
##   Mill., at 140 in DBH, and 148 ft on an eastern cottonwood at 144 in DBH.
##   Those numbers cannot license a higher ceiling here for two reasons: the
##   register measures crown spread as the ARITHMETIC mean of the widest and
##   perpendicular spreads, which for the same crown is always at or above the
##   geometric mean these coefficients were fit to, and every one of those
##   champions sits at two to three times the largest diameter in the
##   corresponding fitted range. Within the fitted range there is no external
##   evidence for a wider crown than the sample maximum.
##
## UNITS
##   This helper reads the IMPERIAL library (DBH inches, crown width feet),
##   which is the FVS-facing table. For the metric library
##   (conus_mcw_lcw_library_metric.csv) the identical logic applies with DBH in
##   cm and crown width in m; pass that file instead and read the result as m.
##
## WORKED EXAMPLE (Douglas-fir, SPCD 202, a real species row in the library)
##   Douglas-fir is fit over roughly 5 to 69 in DBH, so its dbh_max_fit is near
##   69 in and the last diameter below is outside the fitted range:
##
##     source("output/predict_mcw_capped.R")
##     lib <- load_mcw_library("output/conus_mcw_lcw_library.csv")
##     cw  <- predict_mcw_capped(202, c(10, 30, 60, 90), lib)
##     ## Warning: predict_mcw_capped: 1 of 4 requested diameters exceeded
##     ##          dbh_max_fit; the prediction was held at cw_at_dbh_max.
##
##   cw[4] equals lib[spcd == 202, cw_at_dbh_max], and so does
##   predict_mcw_capped(202, 10000, lib): the curve is flat above the cap, by
##   construction. To see what the cap is worth, ask for the raw curve too:
##
##     predict_mcw_capped(202, c(60, 90), lib, cap = FALSE)
##
##   Filtering the library on extrap_flag shows which species most need this:
##     lib[extrap_flag == "implausible", .(spcd, common_name, dbh_max_fit,
##                                         cw_at_dbh_max, dbh_at_100ft)]
##
## WORKED EXAMPLE OF THE CEILING (eastern cottonwood, SPCD 742)
##   Its ceiling engages at 36.9 in DBH, which is 63.7 percent of the way up its
##   fitted range, so the ceiling and not the domain cap is what binds:
##
##     predict_mcw_capped(742, c(20, 36.9, 50, 57.9), lib)
##     ## 34.6  75.0  75.0  75.0     feet
##     predict_mcw_capped(742, c(20, 36.9, 50, 57.9), lib, cap = FALSE)
##     ## 34.6  75.0  95.1 105.5     feet, the uncapped curve
##
##   The 13 species whose ceiling engages inside their own fitted range are
##   exactly those with ceiling_binds_in_fit set:
##     lib[ceiling_binds_in_fit == TRUE & coef == "a1",
##         .(spcd, common_name, clade, dbh_max_fit, cw_at_dbh_max, cw_ceiling,
##           dbh_at_ceiling)]
## ---------------------------------------------------------------------------

suppressPackageStartupMessages(library(data.table))

## Observed crown ceiling per clade, FEET. Provenance in the header above: the
## maximum crown width observed in the 127,710 tree fitting sample for the
## clade, measured 2026-08-08 from data/processed/cw_eco_aug.rds (hardwood
## 75.00 ft over n = 66,723; softwood 65.18 ft over n = 60,987, rounded up to
## 65.2). These are the FALLBACK values, used only when the library being read
## carries no cw_ceiling column; the shipped library carries one, written by
## pipeline/06b_export_clade.R from the same two constants, and the column wins.
CW_CEILING_FT <- c(H = 75.0, S = 65.2)

## load_mcw_library(path)
##   path : the Imperial coefficient library CSV written by 06b_export_clade.R,
##          output/conus_mcw_lcw_library.csv.
## Returns ONE row per species carrying a1, a2 and the applicability columns
## dbh_min_fit, dbh_max_fit, cw_at_dbh_max, extrap_flag, and the ceiling columns
## cw_ceiling, dbh_at_ceiling and ceiling_binds_in_fit, keyed by spcd.
load_mcw_library <- function(path = "output/conus_mcw_lcw_library.csv") {
  if (!file.exists(path))
    stop("load_mcw_library: library CSV not found: ", path)
  lib <- data.table::fread(path)
  need <- c("target", "spcd", "clade", "coef_source", "common_name", "coef",
            "estimate", "dbh_min_fit", "dbh_max_fit", "cw_at_dbh_max",
            "dbh_at_75ft", "dbh_at_100ft", "extrap_flag",
            "cw_ceiling", "dbh_at_ceiling", "ceiling_binds_in_fit")
  miss <- setdiff(need, names(lib))
  if (length(miss))
    stop("load_mcw_library: the library is missing required columns: ",
         paste(miss, collapse = ", "),
         ". Re-export it with pipeline/06b_export_clade.R. Note that the three ",
         "ceiling columns were added 2026-08-08; a library exported before that ",
         "date predicts crowns wider than anything in the fitting sample for 13 ",
         "species and must not be used.")
  mcw <- lib[target == "MCW" & !is.na(spcd) & coef %in% c("a1", "a2")]
  if (!nrow(mcw))
    stop("load_mcw_library: no MCW species rows found in ", path)
  w <- mcw[, .(a1 = estimate[coef == "a1"][1],
               a2 = estimate[coef == "a2"][1]),
           by = .(spcd, clade, coef_source, common_name, dbh_min_fit,
                  dbh_max_fit, cw_at_dbh_max, dbh_at_75ft, dbh_at_100ft,
                  extrap_flag, cw_ceiling, dbh_at_ceiling,
                  ceiling_binds_in_fit)]
  w[, spcd := suppressWarnings(as.integer(spcd))]
  n_bad <- w[!is.finite(a1) | !is.finite(a2), .N]
  if (n_bad > 0L)
    warning(sprintf(paste0("load_mcw_library: %d of %d species lack a usable ",
                           "a1 / a2 pair and will return NA."), n_bad, nrow(w)))
  data.table::setkeyv(w, "spcd")
  w[]
}

## predict_mcw_capped(spcd, dbh_in, lib, cap = TRUE)
##   spcd   : FIA species code, length 1 (recycled) or length(dbh_in).
##   dbh_in : diameter at breast height, INCHES.
##   lib    : the table from load_mcw_library(), or a path to the library CSV.
##   cap    : TRUE (default) applies BOTH the domain cap and the per-clade
##            observed crown ceiling. FALSE returns the raw uncapped curve,
##            which is exactly what the caps exist to prevent; it is provided
##            only for diagnostics and warns loudly.
## Returns a numeric vector of maximum crown width in FEET, aligned to dbh_in.
## Species absent from the library return NA. Warnings are issued ONCE, not per
## row: one for unknown species, one for diameters beyond dbh_max_fit and one
## for predictions held at the clade crown ceiling, each reporting how many rows
## were affected.
predict_mcw_capped <- function(spcd, dbh_in,
                               lib = "output/conus_mcw_lcw_library.csv",
                               cap = TRUE) {
  if (is.character(lib)) lib <- load_mcw_library(lib)
  if (!data.table::is.data.table(lib)) lib <- data.table::as.data.table(lib)
  need <- c("spcd", "a1", "a2", "dbh_max_fit", "cw_at_dbh_max")
  if (!all(need %in% names(lib)))
    stop("predict_mcw_capped: lib is not a table from load_mcw_library(); ",
         "it must carry ", paste(need, collapse = ", "), ".")
  # The crown ceiling comes from the library column when it is there. When it is
  # not, it is rebuilt from clade and CW_CEILING_FT rather than left open, so a
  # stale library degrades to the same numbers instead of to no cap at all.
  if (!"cw_ceiling" %in% names(lib)) {
    if (!"clade" %in% names(lib))
      stop("predict_mcw_capped: lib carries neither cw_ceiling nor clade, so ",
           "the observed crown ceiling cannot be applied. Re-export the ",
           "library with pipeline/06b_export_clade.R.")
    lib <- data.table::copy(lib)
    lib[, cw_ceiling := unname(CW_CEILING_FT[as.character(clade)])]
    warning("predict_mcw_capped: the library carries no cw_ceiling column; the ",
            "per-clade ceiling was rebuilt from CW_CEILING_FT (H 75.0 ft, ",
            "S 65.2 ft). Re-export the library to remove this warning.")
  }
  n <- length(dbh_in)
  if (n == 0L) return(numeric(0))
  if (length(spcd) == 1L) spcd <- rep(spcd, n)
  if (length(spcd) != n)
    stop("predict_mcw_capped: spcd must be length 1 or length(dbh_in) = ", n)

  q <- data.table::data.table(row_id = seq_len(n),
                              spcd   = suppressWarnings(as.integer(spcd)),
                              dbh_in = as.numeric(dbh_in))
  k <- lib[, .(spcd = suppressWarnings(as.integer(spcd)), a1, a2,
               dbh_max_fit, cw_at_dbh_max, cw_ceiling)]
  k <- unique(k, by = "spcd")
  q <- merge(q, k, by = "spcd", all.x = TRUE, sort = FALSE)
  data.table::setorder(q, row_id)

  n_unknown <- q[!is.finite(a1) | !is.finite(a2), .N]
  if (n_unknown > 0L)
    warning(sprintf(paste0("predict_mcw_capped: %d of %d rows carry an SPCD ",
                           "that is not in the library; crown width is NA for ",
                           "those rows."), n_unknown, n))

  over  <- is.finite(q$dbh_in) & is.finite(q$dbh_max_fit) &
           q$dbh_in > q$dbh_max_fit
  d_use <- q$dbh_in
  if (isTRUE(cap)) d_use[over] <- q$dbh_max_fit[over]
  out <- q$a1 * d_use ^ q$a2
  if (isTRUE(cap)) {
    hold <- over & is.finite(q$cw_at_dbh_max)
    out[hold] <- q$cw_at_dbh_max[hold]
  }

  # Cap 2, the observed crown ceiling. Applied AFTER the domain cap, because
  # cw_at_dbh_max can itself sit above the ceiling: eastern cottonwood holds at
  # 105.5 ft under the domain cap alone, and only this line brings it to 75.0.
  n_ceil <- 0L
  if (isTRUE(cap)) {
    bind <- is.finite(out) & is.finite(q$cw_ceiling) & out > q$cw_ceiling
    n_ceil <- sum(bind)
    out[bind] <- q$cw_ceiling[bind]
  }

  n_over <- sum(over)
  if (n_over > 0L) {
    if (isTRUE(cap))
      warning(sprintf(paste0("predict_mcw_capped: %d of %d requested diameters ",
                             "exceeded dbh_max_fit; the prediction was held at ",
                             "cw_at_dbh_max for those rows."), n_over, n))
    else
      warning(sprintf(paste0("predict_mcw_capped: %d of %d requested diameters ",
                             "exceeded dbh_max_fit and cap = FALSE was set; the ",
                             "returned crown widths are EXTRAPOLATIONS outside ",
                             "the fitted range and are not defensible."),
                      n_over, n))
  }
  if (n_ceil > 0L)
    warning(sprintf(paste0("predict_mcw_capped: %d of %d predictions exceeded ",
                           "the observed crown ceiling for their clade and were ",
                           "held there (hardwood 75.0 ft, softwood 65.2 ft, the ",
                           "widest crown measured in the fitting sample). This ",
                           "can happen INSIDE the fitted diameter range; see ",
                           "ceiling_binds_in_fit in the library."), n_ceil, n))
  as.numeric(out)
}
