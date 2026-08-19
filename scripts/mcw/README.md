# CONUS MCW/LCW delivery for Greg Johnson (2026-08-14)

One dataset, two quantiles, packaged two ways. `conus_crownwidth_tree_data.csv`
is the tree-level FIA Phase 3 / Forest Health Monitoring crown data (127,710
trees, 221 species) that both canonical crown-width fits were trained on.
`conus_mcw_lcw_library.csv` is what came out of fitting it: one coefficient
row per species per clade per target, giving MCW (tau = 0.95, near open-grown
envelope) and LCW (tau = 0.50, median stand-grown crown, LCW <= MCW by
construction) as `CW = a1 * DBH^a2`. Metric counterparts of the library
(`_metric` suffix, DBH cm and crown width m) are the same fit converted
analytically, not refit. Use the tree-level file to refit or cross-check
independently; use the coefficient library to apply the equations directly or
compare against your own fitted forms.

Files in this delivery:

- `conus_crownwidth_tree_data.csv` / its dictionary `conus_crownwidth_tree_data_datadict.md`
- `conus_mcw_lcw_library.csv`, `conus_mcw_lcw_library_metric.csv` / their dictionary `mcw_lcw_library_datadict.md`
- `library_species_fallback.csv`: species with no direct fit (population-level fallback), flagged in the library's `coef_source` column. Currently header-only: 0 species, for the same reason LCW now covers all 221 (see below).
- `predict_mcw_capped.R`: reference R implementation that applies the extrapolation cap described below; source it once, then call `predict_mcw_capped(spcd, dbh_in, lib)`

Two things to read before testing against these. First, the equations are fit
envelopes, not growth laws: past each species' fitted DBH range
(`dbh_min_fit`/`dbh_max_fit` in the library) the power form keeps climbing
with no asymptote, and a stress test found 90 of 221 species exceed 75 ft at
60 in DBH. Apply the cap in `predict_mcw_capped.R` rather than evaluating the
raw equation outside the fitted range. Second, the coordinates in the
tree-level file are county-representative (aggregated to the county, from the
median of nearby FIADB Phase 3 plots), not the true FHM/FIA plot locations;
that is a hard restriction on this project's data, and the ecoregion label
carried alongside them is about 82 percent accurate as a result. Both files'
own dictionaries go into more depth on known limitations, including that
neither `tree_id` nor `plot_id` is a clean unique row key on its own.

MCW and LCW agree on row scope. LCW is fit without an ecoregion term
(`pipeline/04c_lcw_noeco.R`, canonicalized 2026-08-08), the same specification
as both canonical MCW fits, so it trains on 127,708 of the 127,710 trees in
the tree-level file (2 rows drop for missing crown ratio) rather than the
smaller ecoregion-filtered set an earlier build used. Every one of the 221
fitted species carries both a species-level MCW and a species-level LCW
estimate; `library_species_fallback.csv` is empty (0 population-fallback
species) for exactly that reason. An earlier internal draft of this note
described an open gap here; it was stale, sourced from a project doc that
had not been updated after the 8/8 fix, and is corrected in this version.

Reach Aaron with questions on either file.
