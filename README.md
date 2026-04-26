# fvs_remodeling
FVS Remodeling Project Repository

Maintainers: 

  - Greg Johnson
  - David Marshall
  - Aaron Weiskittel
  
This repo holds scripts and supporting files to re-model Forest Vegetation Simulator (`FVS`) component growth and imputation models for CONUS.

Some early priorities and project status are:

1. Establish a uniform site productivity measure for CONUS (perhaps similar to `Climate SI`, Embedded Climate data[^1], or Asymptotic Above Ground Biomass).
2. Develop a robust system for establishing the age of a plot (or perhaps a tree).
3. Develop growth equations capable of unbiased estimate across all species.
   * A draft diameter growth equation (not using site index) has been developed for all species across CONUS with sufficient data ($\ge$ 5000 observations). There is good reduction in spatial residual variation. Draft results are [here](https://github.com/gregjohnsonbiometrics/fvs_remodeling/blob/main/pdfs/diameter_growth_equations_for_conus.pdf).
      * We are still working on refining the environmental effects (embedded climate variables).
      * There is still some spatial residual variation yet to address.
   * A draft height growth equation has been developed (not using site index) has been developed for all species across CONUS with sufficient data ($\ge$ 5000 observations). There is good reduction in spatial residual variation. Draft results are [here](https://github.com/gregjohnsonbiometrics/fvs_remodeling/blob/main/pdfs/Height_Growth_Equations_for_CONUS.pdf).
      * Some parameters have biologically unreasonable signs (e.g., negative when positive is expected).
      * The crown ratio parameter is not significantly different from 0.0 for most species. We need to explore the ramifications of this.
      * We are still working on refining the environmental effects (embedded climate variables).
      * There is still some spatial residual variation yet to address.
      

Contacts: 

* greg@nosnhoj.org
* davidkathymarshall@comcast.net
* aaron.weiskittel@maine.edu

[^1]: [Introduction to the Satellite Embedding Dataset.](developers.google.com/earth-engine/tutorials/community/satellite-embedding-01-introduction)


## Constrained refit (April 2026)

`rds/dg_parms.RDS` and `rds/hg_parms.RDS` were refit with biological sign
constraints on April 25, 2026. The previous unconstrained `nlsLM` fits had
biologically wrong-signed coefficients in 70% of species across the 99-species
union of the DG and HG fits. The dominant issue was `HG_B4` (CCFL competition
penalty), which was negative in 56% of HG fits — implying that crown
competition increases height growth, opposite to physiology.

### Method

The fits use the same `est_dg` and `est_hg` integrated equations as before but
with `nlsLM` lower / upper bounds on every coefficient. Bounds are tuned to
allow biological flexibility while blocking obviously wrong signs:

```
DG bounds  B0 [-10, 5]    B1 [-2, 0]    B2 [-10, 0]   B3 [0.5, 5]
           B4 [0.1, 2]    B5 [-0.01, 0.01]   B6 [-0.1, 0.5]

HG bounds  B1 [0.001, 0.5]  B2 [0.5, 5]   B3 [0, 5]    B4 [0, 0.05]
           B5 [-0.001, 0.01]  B6 [-1, 3]   B7 [-0.5, 1]  B8 [0, 5]
```

### Training data

The refit used a CONUS-wide remeasurement panel (8.22 M tree-level
remeasurement pairs from FIA), filtered to live trees with REMPER 5–50 yr
and complete predictors. After filtering: 5.41 M DG rows and 4.50 M HG rows.
EMT and TD covariates were extracted from the ClimateNA Normal_1991_2020
rasters at the 138,796 unique plot lat/lons. The reproducible pipeline
(`scripts/refit_constrained.R` plus a Cardinal SLURM submission script) ran
in 47 minutes on a single OSC Cardinal node.

### Coverage

| Equation | Species fit | Convergence | New species vs prior |
| --- | ---: | --- | ---: |
| DG | 113 | 113 of 113 | +29 |
| HG | 109 | 109 of 111 | +13 |

### Bound bindings

91 individual bound bindings across 73 species. The most common are
`HG_B4` pinned at 0 (54 species), `HG_B3` pinned at 0 (21 species), and
`HG_B8` pinned at 0 (9 species). All correspond to coefficients that were
biologically wrong-signed in the unconstrained fit. A full per-species
table is in `pdfs/bound_violations_constrained.csv` (added in this PR).

### Validation

On a 3,000-row sample of `df_dg_res.RDS` / `df_hg_res.RDS` (Douglas-fir
residuals, SPCD 202), the constrained predictions track the previous
unconstrained predictions:

| Equation | RMSE | Bias | Correlation |
| --- | ---: | ---: | ---: |
| DG | 0.245 in | +0.004 | 0.9998 |
| HG | 3.64 ft | -0.535 | 0.9965 |

The HG bias of -0.5 ft reflects the expected downward shift where `HG_B4`
was previously negative; the previous fit was inflating predicted growth
through a wrong-signed competition term.

### Reproducibility

`scripts/refit_constrained.R` (added in this PR) is a self-contained R
script with `simulate_training()` and `run_smoke_test()` so it can be
exercised on synthetic data without the private training panel. The
SLURM job script and per-species log from the production run live in the
sibling `fvs-modern/calibration/refit_constrained/` directory on Cardinal.
