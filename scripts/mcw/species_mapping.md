
# CONUS Species List and Crown database

`CONUS_Species_list_CWmapped.csv` is a prototype CONUS species list with mapped `MCW`, `LCW`, `HLCW` and `CWA` for all species. As a prototype it is populated with ORGANON SWO parameters for `MCW`, `LCW`, `HLCW` and `CWA` (the version with the most species). Aaron is working on updated `MCW` and `LCW` that would replace what is here. The `HLCW` and `CWA` are probably what we have, although I might add the RAP version RA equations. 

  
The objective is to (1) create a CONUS wide FIA species list and (2) to use it to map `MCW`, `LCW`, `maxCW`, `HLCW` and `CWA` parameters. 
  
## Part 1: Build the CONUS tree-list 
  
### Step 1: 
Get the FIA species group (`SPGRP`) references (`REF_SPECIES_GROUP.csv`) from REF_SPECIES_GROUP.CSV which will be used for species mapping later. 
  
### Step 2a: 
Create the initial species list using the TREE modeling data (18322886 tree records without AK) and select all trees with unique `SPCD` and `SPGRPCD` codes. 
  
### Step 2b: 
A species (`SPCD`) can be assigned to 2 different species groups (`SPGRPCD`) depending on if the tree is in the east or west (suspect this is for reporting). For example, we think of Douglas-fir as a western species with a large range and has a `SPGRP` = 10 (western Douglas-fir). However, there are some Douglas-fir trees in eastern states that are coded with `SPGRP` = 9 (eastern Other eastern softwoods). Another example is white spruce found in the NE which is `SPGRP`=6 (eastern Spruce and balsam fir) and found in the Rockies as is assigned `SPGRP` = 18 (western Engelmann and other spruces). Species with multiple (`SPGRPCD`) were assigned the `SPGRPCD` that represented the species primary ranges (assigned to the variable `SPGRPmapSP`). 
  
This step was done because it covers the primary species, adds a few species that have `SPCD` greater than 999 (tropical/subtropical) that are found in the continental US (usually along the gulf coast) and the tree data includes both a SPCD and `SPGRPCD` making it easier to assign a species group code. 
  
446 species 
  
392 species with `SPCD` 999 or less 
  
54 species with `SPCD` greater than 999 
  
68 species with multiple `SPGRPCD` values 
  
### Step 3: 
Compare the species-list from Step 2 to the FIA reference species list (`REF_SPECIES.csv`) and add any trees with `SPCD` that are 999 or less that were not picked up in step 2 (76 species) and assign a SPGRPmapSP to these as done is Step 2b. This is the CONUS species list which includes all `SPCD` up to 999 and a few species greater than 999. 
  
126 species not in Step 2 (`SPCD` 999 or less) 
  
76 species with multiple `SPGRPCD` values 
  
522 Total CONUS species list 
  
## Part 2: Map the crown width parameters for MCW, LCW, HLCW and CWA to all species in the CONUS species list. 
  
### Step 1: 
A data set was created for each crown variable that included the `SPCD` and parameters. Each species was also assigned to its GENUS and a species group (`SPmap`). In cases where there were parameters for species with the same Genus or `SPmap` one was chosen and the other were assigned NA values. This will be the mapping data. 
  
### Step 2: 
Assign the crown variables to the species the variable was built for (merged the mapping data by `SPCD`) (`SPmap`=1). 
  
### Step 3: 
Using the species with crown parameters (mapping data), map the crown variables to species without crown parameters with the same GENUS (merge the mapping data by GENUS) (`SPmap`=2). 
  
### Step 4: 
Map any remaining species without crown variables using `SPmap` for species in the mapping data (merge by `SPGRPmapSP`) (SPmap=3). 
  
