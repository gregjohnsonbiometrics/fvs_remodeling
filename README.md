# fvs_remodeling
FVS Remodeling Project Repository

Maintainers: 

  - Greg Johnson
  - David Marshall
  - Aaron Weiskittel
  
This repo holds scripts and supporting files to re-model Forest Vegetation Simulator (`FVS`) component growth and imputation models for CONUS.

Our objective is to grow any tree list from plots in CONUS in a single, variant-free model.

## Our principles:
- Use parsimonious, biologically consistent model forms for all species,
- Reduce or eliminate site index as a productivity measure, and
- Require no calibration when verified against FIA data.

A [presentation](./pdfs/GJohnsonWMens2026-v2.pdf) delivered to the Western Mensurationists on June 23, 2026 summarizes our work to date.

## Progress

### We rebuilt the fitting data set using the 2026 FIA database.

### We have preliminary equations developed for:

- [Diameter Growth](./scripts/diameter_growth/)
- [Height Growth](./scripts/height_growth/)
- [Mortality](./scripts/mortality/)
- [Change in Crown Length](./scripts/crown_change/)
- [Maximum Crown Width](./scripts/mcw/)

### and we have in development equations for:

- [Height imputation](./scripts/height-diameter/)


### Work has not begun or is continuing on:

- Species mapping for species with low sample sizes
- Improvement in site (location) effects
- Refine height growth equation for consistency across species.
- Explore parameter clustering techniques (perhaps at the genus level) for species mapping.
- Build (or rebuild) imputation equations for: 
   - ℎ𝑡
   - ℎ𝑡𝑙𝑐
   - 𝑑𝑏ℎ???
   - Build a testing simulation framework and a growth verification system to evaluate bias and precision
      

## Documentation

Documentation of equation fitting and model testing can be found [here](./pdfs)

## Related Projects

- [fvs-conus](https://github.com/advanced-forestry-systems/fvs-conus-components)

- [fvs-modern](https://github.com/advanced-forestry-systems/fvs-modern)

## ontacts: 

* greg@nosnhoj.org
* davidkathymarshall@comcast.net
* aaron.weiskittel@maine.edu

