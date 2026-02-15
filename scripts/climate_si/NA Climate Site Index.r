#FIA SITREE
setwd('/home/aweiskittel/Documents/MAINE/DATA/FIA/')
SITREE=read.csv('ENTIRE_SITETREE.csv')
GEOM=read.csv('ENTIRE_PLOTGEOM.csv')
SPCD=read.csv('ENTIRE_REF_SPECIES.csv')

SITREE$CN=NULL
SITREE$CN=as.factor(SITREE$PLT_CN)

GEOM$CN=as.factor(GEOM$CN)

SPCD$COMMON_NAME=tolower(SPCD$COMMON_NAME)

SITREE=merge(SITREE,GEOM,by='CN')

SITREE$HT.DBH=SITREE$HT/(SITREE$DIA/12)
SITREE$HT.MAI=SITREE$HT/SITREE$AGEDIA

SITREE=SITREE[!is.na(SITREE$AGEDIA) & !is.na(SITREE$HT) & !is.na(SITREE$SPCD) & SITREE$AGEDIA<999 & SITREE$HT.MAI<10 & 
                !is.na(SITREE$SPGRPCD) & !is.na(SITREE$LAT) & !is.na(SITREE$LON),]

SITREE$SOURCE='FIA'
SITREE$ID=paste(SITREE$CN,SITREE$INVYR.x,sep='-')

library(dplyr)
SITREE = SITREE %>%
  rename(PLOT=PLOT.x) %>%
  mutate(DIA=DIA*2.54, HT=HT*0.3048) %>%
  select(c(SOURCE, ID, PLOT, TREE, SPCD, DIA, HT, AGEDIA, LAT, LON))

#Eastern North America
EAST=read.csv('/home/aweiskittel/Documents/MAINE/DATA/Analysis/EastSiteIndex/EastSI2.csv')

EAST = EAST %>%
  subset(SOURCE!='FIA') %>%
  select(c(SOURCE, ID, PLOT,TREE,SPCD, DIA, HT, AGEDIA, LAT, LON))

EAST_sf = st_as_sf(EAST, coords = c("LON", "LAT"), crs = 4326)

# 2. Plot the map
ggplot(data = na_states) +
  geom_sf(fill = "antiquewhite", color = "gray40", size = 0.1) +
  # Focus the view on North America
  geom_sf(data = EAST_sf, aes(color = HT), size = 3) +
  coord_sf(xlim = c(-170, -50), ylim = c(10, 80)) +  
  theme_minimal() +
  labs(title = "North America: States and Provinces")

#NB/NS
NBNS.SITREE=read.csv('/home/aweiskittel/Documents/MAINE/DATA/Analysis/SiteProductivity/NB.NS_AW.csv')
NBNS.SITREE=merge(NBNS.SITREE,SPCD,by.x='Species',by.y='SPECIES_SYMBOL')
NBNS.PLT=read.csv('/home/aweiskittel/Documents/MAINE/DATA/Analysis/SiteProductivity/PlotCoordinates.csv')
NBNS.SITREE=merge(NBNS.SITREE,NBNS.PLT,by.x='ObjectID',by.y='OBJECTID')


NBNS.SITREE = NBNS.SITREE %>%
  rename(ID=ObjectID,PLOT=Plot,TREE=Block,AGEDIA=Age,DIA=DBH,LAT=Lat,LON=Long) %>%
  select(ID,PLOT,TREE,AGEDIA,DIA,HT,LAT,LON,SPCD)
NBNS.SITREE$SOURCE='CAN'

#Ontario
source('/home/aweiskittel/Documents/MAINE/CODE/R/FIA to FVS SPP.r')
ONT.SI=read.csv('/home/aweiskittel/Documents/MAINE/DATA/Ontario/ONT_HT-AGE.csv')
ONT.SI$SPCD=mapply(FVStoFIA.SPP,ONT.SI$FVS_SP)

ONT.SI = ONT.SI %>%
  mutate(ID=paste(PLOT_NUM,'-',TREE_ID)) %>%
  rename(PLOT=PLOT_NUM,TREE=TREE_ID,DIA=DBH,AGEDIA=AGE_DBH,HT=HT_TOTAL,LAT=LATITUDE,LON=LONGITUDE) %>%
  select(ID,PLOT,TREE,DIA,AGEDIA,HT,LAT,LON,SPCD)
ONT.SI$SOURCE='CAN'

#BC data
BC.SITREE=read.delim('/home/aweiskittel/Documents/MAINE/DATA/BC_PSP/BCSiteTrees.csv')
BC.SITREE$SPP=BC.SITREE$SPECIES
#BC.SPP=read.delim('/home/aweiskittel/Documents/MAINE/DATA/BC_PSP/BC.SpeciesCodes_AW.csv')
#BC.SPP$COMMON_NAME=tolower(BC.SPP$COMMON_NAME)
#BC.SPP=merge(BC.SPP,SPCD,by=c('COMMON_NAME'),all=T)
#BC.SPP=BC.SPP[!is.na(BC.SPP$SPP),]
#write.csv(BC.SPP,'/home/aweiskittel/Documents/MAINE/DATA/BC_PSP/BC_SPP.csv',row.names=F)
BC.SPP=read.csv('/home/aweiskittel/Documents/MAINE/DATA/BC_PSP/BC_SPP.csv')
BC.SPP=merge(BC.SPP,SPCD,by='SPCD')
BC.SPP=subset(BC.SPP,select=c('SPCD','SPP'))
BC.SPP$SPP=toupper(BC.SPP$SPP)
BC.SITREE=merge(BC.SITREE,BC.SPP,by='SPP')

BC.SITREE = BC.SITREE %>%
  select(-c(PLOT))  %>%
  rename(DIA=DBH,LON=LONG,TREE=tree,ID=CLSTR_ID,PLOT=PROJ_ID) %>%
  select(ID, PLOT, TREE, SPCD, DIA, HT, AGEDIA, LAT, LON)
BC.SITREE$SOURCE='CAN'  

NA.SITREE=rbind(SITREE,EAST,BC.SITREE,ONT.SI,NBNS.SITREE)
NA.SITREE=merge(NA.SITREE,SPCD,by='SPCD')

NA.SITREE = NA.SITREE %>%
  select(ID, PLOT, TREE, SPCD, DIA, HT, AGEDIA, LAT, LON, GENUS, SPECIES, SCIENTIFIC_NAME)

write.csv(NA.SITREE,'/home/aweiskittel/Documents/MAINE/DATA/Analysis/SiteProductivity/NA_SITREE.csv',row.names=F)

#Fit Site Index Equation
NA.SITREE=read.csv('/home/aweiskittel/Documents/MAINE/DATA/Analysis/SiteProductivity/NA_SITREE.csv')
NA.SITREE$HT.DBH=NA.SITREE$HT/(NA.SITREE$DIA/100)
NA.SITREE$HT.MAI=NA.SITREE$HT/NA.SITREE$AGEDIA

summary(NA.SITREE)

library(nlme)
SI.m1 <- nlme(HT ~ a * (1 - exp(-b * AGEDIA))^c,
                 data = NA.SITREE,
                 fixed = a + b + c ~ 1,
                 random = a ~ 1 | SPCD, 
                 start = c(a = 22, b = 0.028, c = 0.7),
                 weights=varPower(0.2,form=~AGEDIA),verbose=T,
              control=nlmeControl(returnObject=T,maxIter = 500))
summary(SI.m1)

SI.m2 <- nlme(HT ~ a * (1 - exp(-b * AGEDIA))^c,
              data = NA.SITREE,
              fixed = a + b + c ~ 1,
              random = a + b ~ 1 | SPCD, 
              start = c(a = 73, b = 0.028, c = 0.7),
              weights=varPower(0.2,form=~AGEDIA),verbose=T,
              control=nlmeControl(returnObject=T,maxIter = 500))
summary(SI.m2)
AIC(SI.m1,SI.m2)

SI.m3 <- nlme(HT ~ a * (1 - exp(-b * AGEDIA))^c,
              data = NA.SITREE,
              fixed = a + b + c ~ 1,
              random = a + b  + c ~ 1 | SPCD, 
              start = c(a = 73, b = 0.028, c = 0.7),
              weights=varPower(0.2,form=~AGEDIA),verbose=T,
              control=nlmeControl(returnObject=T,maxIter = 500, minScale = 1e-6))
summary(SI.m3)
AIC(SI.m1,SI.m2,SI.m3)

SI.m4 <- nlme(HT ~ a * (1 - exp(-b * AGEDIA))^c,
              data = NA.SITREE,
              fixed = a + b + c ~ 1,
              random = a + c ~ 1 | GENUS/SPCD, 
              start = c(a = 73, b = 0.028, c = 0.7),
              weights=varPower(0.2,form=~AGEDIA),verbose=T, na.action=na.omit,
              control=nlmeControl(returnObject=T,maxIter = 500, minScale = 1e-6))
summary(SI.m4)
AIC(SI.m1,SI.m2,SI.m3,SI.m4)

SI.m5 <- nlme(HT ~ a * (1 - exp(-b * AGEDIA))^c,
              data = NA.SITREE,
              fixed = a + b + c ~ 1,
              random = a + b + c ~ 1 | GENUS/SPCD, 
              start = c(a = 73, b = 0.028, c = 0.7),
              weights=varPower(0.2,form=~AGEDIA),verbose=T, na.action=na.omit,
              control=nlmeControl(returnObject=T,maxIter = 500, minScale = 1e-6))
summary(SI.m5)
AIC(SI.m1,SI.m2,SI.m3,SI.m4, SI.m5)


library(nlme)

# 1. Prepare your data (must have PlotID, Age, and Height)
# No Site Index column is required; the model will solve for it.
base_age <- 50 

# 2. Define the Constrained Formula
# We define the model so that 'si' is the parameter to be estimated.
# This ensures that at Age = base_age, H = si.
constrained_formula <- Height ~ si * ((1 - exp(-k * Age)) / (1 - exp(-k * base_age)))^m

# 3. Fit using nlme
# 'si' is estimated as a random effect per plot to capture site quality.
# 'k' and 'm' are global fixed effects.
fit_si <- nlme(HT ~ si * ((1 - exp(-k * AGEDIA)) / (1 - exp(-k * 50)))^m,
               data = NA.SITREE,
               fixed = si + k + m ~ 1,
               random = si + k + m~ 1 | GENUS/SPCD, # SI is the unique identifier for each site
               start = c(si = 58, k = 0.025, m = 0.657),
               weights=varPower(0.2,form=~AGEDIA),verbose=T, na.action=na.omit,
               control=nlmeControl(returnObject=T,maxIter = 500, minScale = 1e-6))
summary(fit_si)
plot(fit_si)

NA.SITREE$SI=predict(fit_si,newdata=NA.SITREE)
hist(NA.SITREE$SI)
summary(NA.SITREE$SI)


library(tidyverse)
library(sf)
library(maps)
library(rnaturalearth)
library(ggplot2)
library(RColorBrewer)

# 1. Get state/province data for US, Canada, and Mexico
na_states <- ne_states(country = c("united states of america", "canada"), returnclass = "sf")

# Convert data frame to sf object (CRS 4326 is standard GPS lat/long)
NA.SITREE_sf <- st_as_sf(NA.SITREE, coords = c("LON", "LAT"), crs = 4326)

# 2. Plot the map
ggplot() + 
  geom_sf(data = na_states, color = "gray40", size = 0.1) + 
  geom_sf(data = NA.SITREE_sf, aes(color = SI), size = .5) + # 'SI' is mapped to 'color'
  coord_sf(xlim = c(-170, -50), ylim = c(25, 65)) + 
  scale_color_distiller(palette = "RdYlGn") + # This scale controls the 'color' aesthetic for 'SI'
  theme_minimal() + 
  labs(color = "Observed Site Index (m)") +
  labs(title = "North America: States and Provinces")

write.csv(NA.SITREE,'/home/aweiskittel/Documents/MAINE/DATA/Analysis/SiteProductivity/NA_SITREE.csv',row.names=F)

#apply
WEST=load('/home/aweiskittel/Documents/MAINE/DATA/Analysis/WestSiteIndex/WestSI_ALL.RData')
EAST=read.csv('/home/aweiskittel/Documents/MAINE/DATA/Analysis/EastSiteIndex/EastSI2.csv')

library(dplyr)
WestSI= WestSI %>%
  rename(SOURCE=Source,SI=MON_SI,LON=LONG) %>%
  select(SOURCE,ID,SPCD,LAT,LON,ELEV,mat,map,gsp,mtcm,mmin,mtwm,mmax,sday,fday,ffp,dd5,gsdd5,d100,dd0,mmindd0,SI)

EAST = EAST %>%
  select(-c(PLOT,TREE,DIA,HT,AGEDIA,MODIS_GPP,X3PG_GPP,common_name,scientific_name,fvs_ne_var))

AllSI=rbind(WestSI,EAST)

setwd('/home/aweiskittel/Documents/MAINE/DATA/Analysis/SiteProductivity/')
setwd('/users/PUOM0008/crsfaaron/SiteIndex')
AllSI=read.csv('ALL_SI_m.csv')
AllSI$tdiff=AllSI$mtwm-AllSI$mtcm
AllSI$adi=AllSI$dd5**0.5/AllSI$map
AllSI$adimindd0=((AllSI$dd5**0.5)/AllSI$map)*AllSI$mmindd0
AllSI$dd5mtcm =(AllSI$dd5*AllSI$mtcm)/1000
AllSI$gspdd5 =(AllSI$gsp*AllSI$dd5)/1000
AllSI$gspmtcm =(AllSI$gsp*AllSI$mtcm)/1000
AllSI$gsptd  =(AllSI$gsp*AllSI$tdiff)/100
AllSI$mapdd5 =(AllSI$map*AllSI$dd5)/1000
AllSI$mapmtcm =(AllSI$map*AllSI$mtcm)/1000
AllSI$maptd =(AllSI$map*AllSI$tdiff)/100
AllSI$mtcmgsp =AllSI$mtcm/AllSI$gsp
AllSI$mtcmmap =AllSI$mtcm/AllSI$map
AllSI$pratio =AllSI$gsp/AllSI$map
AllSI$prdd5  =AllSI$pratio*AllSI$dd5
AllSI$prmtcm =(AllSI$pratio*AllSI$mtcm)
AllSI$sdi=AllSI$gsdd5**0.5/AllSI$gsp
AllSI$sdimindd0=((AllSI$gsdd5**0.5)/AllSI$gsp)*AllSI$mmindd0
AllSI$tdgsp =AllSI$tdiff/AllSI$gsp
AllSI$tdmap =AllSI$tdiff/AllSI$map
write.csv(AllSI,'/home/aweiskittel/Documents/MAINE/DATA/Analysis/SiteProductivity/ALL_SI_m.csv')


#
library(spatialRF)
library(sf)

# 1. Prepare Main Data Frame
# Convert sf object to data.frame and extract coordinates
AllSI_sf <- st_as_sf(AllSI, 
                         coords = c("LAT", "LON"), 
                         crs = 4326) # Use 4326 for WGS84/GPS data
xy <- st_coordinates(AllSI_sf)
colnames(xy) <- c("x", "y")

df <- st_drop_geometry(AllSI_sf)
df$x <- xy[, 1]
df$y <- xy[, 2]

# 2. Generate Distance Matrix
# Use the coordinates to build a matrix of Euclidean distances
distance_matrix <- as.matrix(dist(xy))

# 3. Handle Multicollinearity (Optional but Recommended)
# spatialRF provides tools to filter highly correlated predictors
selected_predictors <- auto_cor(
  x = df[, c("pred1", "pred2", "pred3")], 
  cor.threshold = 0.7
)


#fit spatial model from scratch
m_spatial <- rf_spatial(
  data = ALLSI,
  dependent.variable.name = SI,
  predictor.variable.names = c(),
  distance.matrix = plants_distance,
  distance.thresholds = c(100, 1000, 2000),
  method = "mem.moran.sequential",
  ranger.arguments = list(num.trees = 30),
  n.cores = 1
)

#spatialRD GLM
#module load gdal/3.73 gcc/12.3.0 geos/3.12.0 proj/9.2.1 R/4.4.0
setwd('/users/PUOM0008/crsfaaron/SiteIndex')
#setwd('/home/aweiskittel/Documents/MAINE/DATA/Analysis/SiteProductivity/')
ALLSI=read.csv('ALL_SI_m.csv')

# 1. Load required libraries
# library(sf)
# library(RandomForestsGLS)
# 
# # --- YOUR PROVIDED CODE ---
# AllSI_sf <- st_as_sf(ALLSI, 
#                      coords = c("LAT", "LON"), 
#                      crs = 4326) 
# xy <- st_coordinates(AllSI_sf)
# colnames(xy) <- c("x", "y")
# 
# df <- st_drop_geometry(AllSI_sf)
# df$x <- xy[, 1]
# df$y <- xy[, 2]
# # --------------------------
# 
# setwd('/home/aweiskittel/Documents/MAINE/DATA/Analysis/SiteProductivity/')
# 
# 
# # 2. Prepare inputs for RandomForestsGLS
# # The package requires coords as a matrix and X as a matrix
# coords_mat <- as.matrix(xy)
# 
# # Define your response (y) and predictors (X)
# # Replace "target_variable" with your actual column name
# y_vec <- df$SI 
# 
# # Select predictor columns (excluding the response and coordinates)
# predictor_cols <- setdiff(names(df), c("ELEV", "mat","map","gsp","mtcm","mmin","mtwm","mmax","sday","fday","ffp","dd5","gsdd5","d100","dd0","mmindd0","tdiff","adi","adimindd0", "dd5mtcm",   "gspdd5", "gspmtcm","gsptd",
#                                        "mapdd5","mapmtcm","maptd","mtcmgsp","mtcmmap","pratio","prdd5","prmtcm","sdi","sdimindd0", "tdgsp","tdmap", "x", "y"))
# X_mat <- as.matrix(df[, predictor_cols])
# 
# # 3. Fit the Spatial Random Forest GLS model
# # ntree: number of trees; param_estimate: whether to estimate spatial parameters
# rf_gls_model <- RFGLS_estimate_spatial(
#   coords = coords_mat, 
#   y = y_vec, 
#   X = X_mat, 
#   ntree = 50,           # Recommended: 50-100 for spatial GLS
#   nthsize = 20,         # Minimum node size
#   cov.model = "exponential", 
#   param_estimate = TRUE # Automatically estimate sigma^2 and phi
# )
# 
# # 4. View Model Summary
# # Note: Traditional 'summary()' may not provide deep details; 
# # check estimated spatial parameters:
# print(rf_gls_model$spatial_param)

setwd('/users/PUOM0008/crsfaaron/SiteIndex')
#setwd('/home/aweiskittel/Documents/MAINE/DATA/Analysis/SiteProductivity/')
ALLSI=read.csv('ALL_SI_m.csv')


# 1. Extract coordinates as a matrix
coords <- as.matrix(ALLSI[, c("LAT", "LON")])

# 2. Extract the response variable as a vector
y <- ALLSI$SI

# 3. Extract covariates as a matrix (excluding coordinates and target)
X <- as.matrix(ALLSI[, c("mat","map","gsp","mtcm","mmin","mtwm","mmax","sday","fday","ffp","dd5","gsdd5","d100","dd0","mmindd0","tdiff","adi","adimindd0", "dd5mtcm",   "gspdd5", "gspmtcm","gsptd",
                           "mapdd5","mapmtcm","maptd","mtcmgsp","mtcmmap","pratio","prdd5","prmtcm","sdi","sdimindd0", "tdgsp","tdmap", "LAT", "LON")])

# 4. Run the estimate
library(parallel)

# Detect logical cores (threads) - Default for most tasks
n_logical <- detectCores() 
mtry = dim(X)[2]/3

library(RandomForestsGLS)
set.seed(2026) 
rf_gls_model <- RFGLS_estimate_spatial(coords = coords, y = y, X = X, 
                                ntree = 100, 
                                #cov.model = "exponential", 
                                mtry = mtry,
                                param_estimate = TRUE, # Automatically estimate sigma^2 and phi
                                h = n_logical/2,      # Parallelize across bootstrap trees
                                n_omp = 2, # Parallelize operations within each tree
                                verbose = TRUE,             # SHOW PROGRESS
                                nthsize = 150,              # CRITICAL: Larger nodes = faster training
                                pinv_choice = 1,            # Use faster decomposition
                                cov.model = "exponential"    # Standard spatial covariance
                                #na.action = na.omit,
                                )
print(rf_gls_model)
print(rf_gls_model$spatial_param)

saveRDS(rf_gls_model, file = "clim_spatial_rf_model.rds")


# 1. Prepare Data
# We include LAT and LON inside the X matrix so ranger can use them for spatial partitioning
y <- ALLSI$SI
X <- ALLSI[, c("mat","map","gsp","mtcm","mmin","mtwm","mmax","sday","fday","ffp",
               "dd5","gsdd5","d100","dd0","mmindd0","tdiff","adi","adimindd0", 
               "dd5mtcm", "gspdd5", "gspmtcm","gsptd", "mapdd5","mapmtcm",
               "maptd","mtcmgsp","mtcmmap","pratio","prdd5","prmtcm","sdi",
               "sdimindd0", "tdgsp","tdmap", "LAT", "LON")]

# 2. Set Hyperparameters
set.seed(2026)
n_cores <- detectCores() # This will pick up your 88 cores
mtry_val <- floor(ncol(X) / 3)

# 3. Run Ranger Model
# Using the 'x' and 'y' arguments is faster and more memory-efficient than formulas
rf_ranger_model <- ranger(
  x = X, 
  y = y, 
  num.trees = 250,           # Sufficient for 326k rows
  mtry = mtry_val,
  importance = 'impurity',   # Allows you to see if LAT/LON are driving the model
  num.threads = n_cores,     # Uses all 88 cores
  verbose = TRUE,            # SHOWS PROGRESS BAR (Growing trees... % )
  min.node.size = 10,        # Standard for regression; increase to 50 for even more speed
  keep.inbag = TRUE          # Helpful if you need standard errors later
)

# 4. Results
print(rf_ranger_model)

# View Variable Importance (Check how high LAT and LON rank)
print(sort(rf_ranger_model$variable.importance, decreasing = TRUE))

# Save
saveRDS(rf_ranger_model, file = "clim_ranger_spatial_model.rds")


write.csv(NA.SI,'/users/PUOM0008/crsfaaron/SiteIndex/NA.SI.MSFL.csv',row.names=F)


#Fit Ranger to Moscow Data
library(ranger)
library(data.table)
setwd('/users/PUOM0008/crsfaaron/SiteIndex')
#setwd('/home/aweiskittel/Documents/MAINE/DATA/Analysis/SiteProductivity/')
ALLSI=fread('NA.SI.MSFL.csv')

# 1. Generate Oblique Coordinates
# We create 45-degree and 135-degree rotations
ALLSI$LAT_LON_P <- ALLSI$LAT + ALLSI$LON
ALLSI$LAT_LON_M <- ALLSI$LAT - ALLSI$LON

# 2. Update Predictor Matrix (X)
# Include the raw coordinates and the new rotated features
X_oblique <- ALLSI[, c("mat","map","gsp","mtcm","mmin","mtwm","mmax","sday","fday","ffp",
                       "dd5","gsdd5","d100","dd0","mmindd0","tdiff","adi","adimindd0", 
                       "dd5mtcm", "gspdd5", "gspmtcm","gsptd", "mapdd5","mapmtcm",
                       "maptd","mtcmgsp","mtcmmap","pratio","prdd5","prmtcm","sdi",
                       "sdimindd0", "tdgsp","tdmap", 
                       "LAT", "LON", "LAT_LON_P", "LAT_LON_M")]
Y= ALLSI$SI

library(parallel)
n_cores <- detectCores() # This will pick up your 88 cores

# 3. Fit Ranger Model
set.seed(2026)
rf_final <- ranger(
  x = X_oblique,
  y = Y,
  num.trees = 500,
  mtry = floor(ncol(X_oblique) / 3),
  num.threads = n_cores,
  verbose = TRUE,
  importance = 'impurity',
  min.node.size = 10
)

# 4. Results
print(rf_final)

# Check if the rotated coordinates (LAT_LON_P/M) are absorbing some 
# of the 'importance' previously held by raw LAT/LON.
print(sort(rf_final$variable.importance, decreasing = TRUE))

saveRDS(rf_final, file = "/users/PUOM0008/crsfaaron/SiteIndex/rf_MSFL_model.rds")

#
#setwd('/users/PUOM0008/crsfaaron/SiteIndex')
library(terra)
setwd('/home/aweiskittel/Documents/MAINE/DATA/Analysis/SiteProductivity/')
CSI=rast('NA_CSI_m.tif')

plot(CSI)


# Load latest or specific versions
module load gcc/12.3.0
module load gdal/3.7.3
module load geos/3.12.0
module load proj/9.2.1
module load R/4.4.0

tdiff=mtwm-mtcm
adi=dd5**0.5/map
adimindd0=((dd5**0.5)/map)*mmindd0
dd5mtcm =(dd5*mtcm)/1000
gspdd5 =(gsp*dd5)/1000
gspmtcm =(gsp*mtcm)/1000
gsptd  =(gsp*tdiff)/100
mapdd5 =(map*dd5)/1000
mapmtcm =(map*mtcm)/1000
maptd =(map*tdiff)/100
mtcmgsp =mtcm/gsp
mtcmmap =mtcm/map
pratio =gsp/map
prdd5  =pratio*dd5
prmtcm =(pratio*mtcm)
sdi=gsdd5**0.5/gsp
sdimindd0=((gsdd5**0.5)/gsp)*mmindd0
tdgsp =tdiff/gsp
tdmap =tdiff/map

##############################################################################################################################
##############################################################################################################################
##############################################################################################################################

library(terra)
library(data.table)
library(dplyr)
library(sf)
library(ranger)
library(parallel)

# 1. Define the directory where you unzipped the files
unzip_dir <- "F:/Projects/FVS Remodel/gis/Normal_1991_2020_bioclim"

# 2. List all raster files (matching .tif or .asc extensions)
# full.names = TRUE is required so terra knows the exact file path
raster_files <- list.files(
  path = unzip_dir, 
  pattern = "\\.(tif|asc)$", 
  full.names = TRUE, 
  ignore.case = TRUE
)

# 1. Load the rasters
climate_stack <- rast(raster_files)

# 2. Get the clean base names (e.g., "Normal_1991_2020_MAT")
raw_names <- gsub("\\.(tif|asc)$", "", basename(raster_files), ignore.case = TRUE)

# 3. Remove the specific prefix "Normal_1991_2020_"
# The ^ ensures it only replaces the string if it's at the beginning
clean_names <- gsub("^Normal_1991_2020_", "", raw_names)

# 4. Assign back to the stack
names(climate_stack) <- clean_names

# Verify
print(names(climate_stack))


# get productivity data

NA.SI=fread('F:/Projects/FVS Remodel/data/std_mean_res.csv') %>% rename( SI = std_mean_res )

NA.SI.pts <- vect(NA.SI, geom=c("LON", "LAT"), crs="EPSG:4326")

NA.SI.pts_transformed <- project(NA.SI.pts, crs(climate_stack))

NA.SI.pts.ClimateNA <- extract(climate_stack, NA.SI.pts_transformed)

NA.SI = cbind(NA.SI, NA.SI.pts.ClimateNA)

write.csv(NA.SI,'F:/Projects/FVS Remodel/data/NA.SI.ClimateNA.csv',row.names=F)


#read and fit ranger
NA.SI.ClimateNA=fread('F:/Projects/FVS Remodel/data/NA.SI.ClimateNA.csv')

# 2. Remove the duplicate "ID" column safely for a data.table
# This removes the 16th column specifically
#NA.SI.ClimateNA <- NA.SI.ClimateNA[, -16, with = FALSE]

# 3. Confirm it's now a data frame for mutate
NA.SI.ClimateNA <- as.data.frame(NA.SI.ClimateNA)

ecoregions <- st_read("F:/Projects/FVS Remodel/gis/ecoregions.gpkg")

NA.SI.ClimateNA  <- st_as_sf(NA.SI.ClimateNA, coords = c("LON", "LAT"), crs = 4326) 
#ecoregions <- st_transform(ecoregions, st_crs(NA.SI.ClimateNA))
NA.SI.ClimateNA <- st_transform(NA.SI.ClimateNA, st_crs(ecoregions))

NA.SI.ClimateNA <- st_join( NA.SI.ClimateNA, ecoregions, join = st_within) 

library( ggplot2 )
NA.SI.ClimateNA %>% ggplot( aes( NA_L2NAME, SI )) + 
  geom_boxplot( notch=T, varwidth = T) + ylim(c(-1,1)) + 
  geom_abline( intercept = 0, slope= 0 ) +
  theme( axis.text.x = element_text(angle = 45, hjust = 1) ) +
  xlab( "" ) + ylab( "Productivity Residual" ) + ggtitle( "Negative Indicates Higher Productivty")

x <- as.data.frame(NA.SI.ClimateNA %>% group_by( NA_L2NAME ) %>% summarize( mSI=median(SI) )) %>% select( NA_L2NAME, mSI)

x <- x %>% inner_join( NA.SI.ClimateNA, by="NA_L2NAME" )

ggplot( data=st_as_sf(x) ) + 
  geom_sf( aes(color=Hmisc::cut2(mSI,g=7) ) ) + 
  viridis::scale_color_viridis(discrete = T, option="magma") +
  labs( color="DG Residual") +
  ggtitle( "L2 Ecoregion Mean DG Residual", subtitle = "All Species" )


x <- as.data.frame(NA.SI.ClimateNA %>% group_by( NA_L3NAME ) %>% summarize( mSI=median(SI) )) %>% select( NA_L3NAME, mSI)

x <- x %>% inner_join( NA.SI.ClimateNA, by="NA_L3NAME" )

ggplot( data=st_as_sf(x) ) + 
  geom_sf( aes(color=Hmisc::cut2(mSI,g=15) ) ) + 
  viridis::scale_color_viridis(discrete = T, option="magma") +
  labs( color="DG Residual") +
  ggtitle( "L3 Ecoregion Mean DG Residual", subtitle = "All Species" )

na.si %>% ggplot( aes( eFFP, SI )) + geom_point() + geom_smooth() + ylim(c(-5,5))

na.si %>% ggplot( aes( PPT_sp, SI )) + geom_point() + geom_smooth() + ylim(c(-5,5))
na.si %>% ggplot( aes( EMT, SI )) + geom_point() + geom_smooth() + ylim(c(-5,5))
na.si %>% ggplot( aes( FFP, SI )) + geom_point() + geom_smooth() + ylim(c(-5,5))


#NA.SI.ClimateNA = x <- merge(NA.SI.ClimateNA,NA.SI.ECO,by=c("ID"))
#x <- NA.SI.ClimateNA %>% inner_join( NA.SI.ECO, by=c("ID") )

NA.SI.ClimateNA <- NA.SI.ClimateNA %>%
  mutate(
    # --- Basic Derived Metrics ---
    tdiff    = MWMT - MCMT,
    pratio   = MSP / MAP,
    
    # --- Aridity & SDI Indices ---
    # Note: ClimateNA DD5 is the standard for gsdd5
    adi      = (DD5^0.5) / MAP,
    sdi      = (DD5^0.5) / MSP,
    
    # --- Interaction with Cold Stress (DD_0) ---
    adimindd0 = ((DD5^0.5) / MAP) * DD_0,
    sdimindd0 = ((DD5^0.5) / MSP) * DD_0,
    
    # --- Climate Interactions (Scaled by 100 or 1000) ---
    dd5mtcm  = (DD5 * MCMT) / 1000,
    gspdd5   = (MSP * DD5) / 1000,
    gspmtcm  = (MSP * MCMT) / 1000,
    gsptd    = (MSP * tdiff) / 100,
    mapdd5   = (MAP * DD5) / 1000,
    mapmtcm  = (MAP * MCMT) / 1000,
    maptd    = (MAP * tdiff) / 100,
    mmindd0  = (EMT & DD_0) / 1000,
    
    # --- Ratios & Complex Interactions ---
    mtcmgsp  = MCMT / MSP,
    mtcmmap  = MCMT / MAP,
    tdgsp    = tdiff / MSP,
    tdmap    = tdiff / MAP,
    prdd5    = pratio * DD5,
    prmtcm   = pratio * MCMT
  )

# 1. Map ClimateNA names to your Model names

# 1. Map your specific model names
# We use 'mmin' for EMT and 'mmax' for EXT as per your previous code
NA.SI.ClimateNA <- NA.SI.ClimateNA %>%
  mutate(
    #mmin = EMT, 
    #mmax = EXT, 
    sday = bFFP, 
    fday = eFFP, 
    ffp  = FFP,
    dd0  = DD_0,
    #mmindd0 = DD_0,
    mtwm = MWMT,
    mtcm = MCMT,
    mat = MAT,
    map = MAP,
    gsp = MSP,
    d100 = DD1040,
    # Geographic Rotations
    LAT_LON_P = LAT + LON,
    LAT_LON_M = LAT - LON
  )

# 1. Define columns to exclude
exclude_cols <- c("ID", "PLOT", "TREE", "SPCD", "DIA", "HT", "AGEDIA", 
                  "GENUS", "SPECIES", "SCIENTIFIC_NAME", "HT.DBH", "HT.MAI", "SI",
                  "LAT_LON_P", "LAT_LON_M", "LAT", "LON")

NA.SI.ClimateNA <- as.data.frame(NA.SI.ClimateNA)

# 2. Get predictor names
predictor_names <- names(NA.SI.ClimateNA)[c(3:34,53:71)]

# 3. Subset correctly for a data.frame (No 'with = FALSE')
X_training <- NA.SI.ClimateNA[, predictor_names]
y_target <- NA.SI.ClimateNA$SI

# 4. Run the 88-core model
set.seed(2026)
rf_final <- ranger(
  x = na.omit(X_training),
  y = y_target,
  num.trees = 500,
  mtry = floor(length(predictor_names) / 3),
  num.threads = detectCores(),
  importance = 'impurity',
  min.node.size = 10,
  verbose = TRUE,
  save.memory = TRUE 
)

print(rf_final)

print(sort(rf_final$variable.importance, decreasing = TRUE))

#saveRDS(rf_final, file = "/users/PUOM0008/crsfaaron/SiteIndex/rf_NASI_ClimateNA_NoGEO_model.rds")
saveRDS(rf_final, file = "/users/PUOM0008/crsfaaron/SiteIndex/rf_NASI_ClimateNA_ECO_model.rds")


#Predict
library(terra)

setwd('/users/PUOM0008/crsfaaron/SiteIndex')
rf_final <- readRDS("rf_NASI_ClimateNA_model.rds")


# 1. Define the directory where you unzipped the files
unzip_dir <- "/users/PUOM0008/crsfaaron/SiteIndex/rasters/ClimateNA/Normal_1991_2020_bioclim"

# 2. List all raster files (matching .tif or .asc extensions)
# full.names = TRUE is required so terra knows the exact file path
raster_files <- list.files(
  path = unzip_dir, 
  pattern = "\\.(tif|asc)$", 
  full.names = TRUE, 
  ignore.case = TRUE
)

# 1. Load the rasters
climate_stack <- rast(raster_files)

# 2. Get the clean base names (e.g., "Normal_1991_2020_MAT")
raw_names <- gsub("\\.(tif|asc)$", "", basename(raster_files), ignore.case = TRUE)

# 3. Remove the specific prefix "Normal_1991_2020_"
# The ^ ensures it only replaces the string if it's at the beginning
clean_names <- gsub("^Normal_1991_2020_", "", raw_names)

# 4. Assign back to the stack
names(climate_stack) <- clean_names

# Verify
print(names(climate_stack))

library(terra)
climate_stack$sday <- climate_stack$bFFP
climate_stack$fday <- climate_stack$eFFP
climate_stack$ffp  <- climate_stack$FFP
climate_stack$mmin <- climate_stack$EMT
climate_stack$mmax <- climate_stack$EXT
climate_stack$d100 <- climate_stack$DD1040
climate_stack$map = climate_stack$MAP  
climate_stack$mat = climate_stack$MAT  
climate_stack$mtcm = climate_stack$MCMT
climate_stack$gsp = climate_stack$MSP
climate_stack$mtwm = climate_stack$MWMT

# 1. Basic Derived Metrics
climate_stack$tdiff  <- climate_stack$MWMT - climate_stack$MCMT
climate_stack$pratio <- climate_stack$MSP / climate_stack$MAP

# 2. Aridity & SDI Indices
climate_stack$adi    <- (climate_stack$DD5^0.5) / climate_stack$MAP
climate_stack$sdi    <- (climate_stack$DD5^0.5) / climate_stack$MSP

# 3. Interaction with Cold Stress (DD_0)
climate_stack$adimindd0 <- climate_stack$adi * climate_stack$DD_0
climate_stack$sdimindd0 <- climate_stack$sdi * climate_stack$DD_0

# 4. Climate Interactions (Scaled by 100 or 1000)
climate_stack$dd5mtcm  <- (climate_stack$DD5 * climate_stack$MCMT) / 1000
climate_stack$gspdd5   <- (climate_stack$MSP * climate_stack$DD5) / 1000
climate_stack$gspmtcm  <- (climate_stack$MSP * climate_stack$MCMT) / 1000
climate_stack$gsptd    <- (climate_stack$MSP * climate_stack$tdiff) / 100
climate_stack$mapdd5   <- (climate_stack$MAP * climate_stack$DD5) / 1000
climate_stack$mapmtcm  <- (climate_stack$MAP * climate_stack$MCMT) / 1000
climate_stack$maptd    <- (climate_stack$MAP * climate_stack$tdiff) / 100

# 5. Ratios & Complex Interactions
climate_stack$mtcmgsp  <- climate_stack$MCMT / climate_stack$MSP
climate_stack$mtcmmap  <- climate_stack$MCMT / climate_stack$MAP
climate_stack$tdgsp    <- climate_stack$tdiff / climate_stack$MSP
climate_stack$tdmap    <- climate_stack$tdiff / climate_stack$MAP
climate_stack$prdd5    <- climate_stack$pratio * climate_stack$DD5
climate_stack$prmtcm   <- climate_stack$pratio * climate_stack$MCMT

# Create Lat/Lon layers matching the extent and resolution of the climate data
lon_layer <- init(climate_stack, "x")
lat_layer <- init(climate_stack, "y")

# Name them to match your model
names(lon_layer) <- "LON"
names(lat_layer) <- "LAT"

# Calculate Oblique Rotations
lat_lon_p <- lat_layer + lon_layer
lat_lon_m <- lat_layer - lon_layer
names(lat_lon_p) <- "LAT_LON_P"
names(lat_lon_m) <- "LAT_LON_M"

# Add them to the stack
climate_stack <- c(climate_stack, lat_layer, lon_layer, lat_lon_p, lat_lon_m)

current_names <- names(climate_stack)

# Replace specific names
names(climate_stack)[current_names == "EMT"]    <- "mmin"
names(climate_stack)[current_names == "EXT"]    <- "mmax"
names(climate_stack)[current_names == "bFFP"]   <- "sday"
names(climate_stack)[current_names == "eFFP"]   <- "fday"
names(climate_stack)[current_names == "FFP"]    <- "ffp"
names(climate_stack)[current_names == "DD_0"]   <- "dd0"
names(climate_stack)[current_names == "MWMT"]   <- "mtwm"
names(climate_stack)[current_names == "MCMT"]   <- "mtcm"
names(climate_stack)[current_names == "MAT"]    <- "mat"
names(climate_stack)[current_names == "MAP"]    <- "map"
names(climate_stack)[current_names == "MSP"]    <- "gsp"
names(climate_stack)[current_names == "DD1040"] <- "d100"

# Crucial: You used 'mmindd0' in the model, which was a copy of DD_0
# We add it as a new layer in the stack
climate_stack$mmindd0 <- climate_stack$DD_0

# Double check the names now
# print(names(climate_stack))

# 1. Variables the model wants
model_vars <- rf_final$forest$independent.variable.names

# 2. Variables the raster has
raster_vars <- names(climate_stack)

# 3. Find what's missing in the raster
missing <- setdiff(model_vars, raster_vars)

# 4. Find what's extra in the raster (won't cause errors, but good to know)
extra <- setdiff(raster_vars, model_vars)

print("Variables missing from Raster (MUST FIX):")
print(missing)


# Wrapper to extract only the $predictions numeric vector from ranger
library(ranger)

ranger_pred_fun <- function(model, data, ...) {
  predict(model, data, ...)$predictions
}


# Generate the prediction raster
# na.rm = TRUE ensures that areas outside your climate grids (e.g., ocean) are ignored
si_map <- predict(
  object = climate_stack,
  model = rf_final,
  fun = ranger_pred_fun,
  na.rm = TRUE,
  cores = 20,            # Utilize all your cores
  filename = "ClimateNA_SI_m.tif", # Save directly to disk
  overwrite = TRUE,
  wopt = list(progress = 3) # Value of 3 shows a progress bar in the console
)


# Initial visual check
#plot(si_map, main = "Predicted Site Index (2026)")

library(terra)
setwd('/home/aweiskittel/Documents/MAINE/DATA/Analysis/SiteProductivity/')

#ClimateNA_SI=rast('ClimateNA_SI_m.tif')
ClimateNA_SI=rast('ClimateNA_ECO_SI_m .tif')
plot(ClimateNA_SI)
