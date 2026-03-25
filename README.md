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

