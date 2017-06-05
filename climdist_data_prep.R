library(dplyr)
library(purrr)
library(aws.s3)

# setup
source("climdist/aws_key.R")
bkt <- "leonawicz"

period <- c(1860, 2099)
variables <- c("Precipitation"="pr", "Mean temperature"="tas", "Min Temperature"="tasmin", "Max temperature"="tasmax")
seasons <- c("Annual"="annual", "Winter"="winter", "Spring"="spring", "Summer"="summer", "Autumn"="autumn")
stats <- c("Mean")
mapsets <- c(
  "Alaska/western Canada"="AK-CAN",
  "State/Province"="Political Boundaries",
  "Alaska level 1 ecoregions"="Alaska L1 Ecoregions",
  "Alaska level 2 ecoregions"="Alaska L2 Ecoregions",
  "Alaska level 3 ecoregions"="Alaska L3 Ecoregions",
  "Alaska LCC regions"="LCC Regions",
  "Alaska/Canada LCC regions"="AK-CAN LCC",
  "CAVM regions"="CAVM Regions",
  "Fire management zones"="FMZ Regions",
  "Terrestrial protected areas"="TPA Regions")
rcp <- c("Historical", "RCP 4.5", "RCP 6.0", "RCP 8.5")
rcps <- c("4.5"=rcp[2], "6.0"=rcp[3], "8.5"=rcp[4])
gcms <- c("GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3", "NCAR-CCSM4")
cru <- "CRU 4.0"

dataDir <- "climdist/appData/clim_2km_seasonal"
grpDirs <- list.files(dataDir)
grpDirs <- grpDirs[match(mapsets, grpDirs)]
locs <- map(grpDirs, ~list.files(file.path(dataDir, .x)))
names(locs) <- mapsets

names(locs$`Political Boundaries`) <- locs$`Political Boundaries`
names(locs$`Alaska L1 Ecoregions`) <- locs$`Alaska L1 Ecoregions`
names(locs$`Alaska L2 Ecoregions`) <- locs$`Alaska L2 Ecoregions`
names(locs$`Alaska L3 Ecoregions`) <- locs$`Alaska L3 Ecoregions`
names(locs$`CAVM Regions`) <- locs$`CAVM Regions`
locs$`AK-CAN` <- c("Alaska/western Canada"="AK-CAN")
locs$`LCC Regions` <- c("Arctic"="Arctic", "North Pacific"="N Pacific", "NW interior boreal (north)"="NW Interior Forest N",
                        "NW interior boreal (south)"="NW Interior Forest S", "Western Alaska"="W Alaska")
locs$`AK-CAN LCC` <- c("Northwest boreal"="AK-CAN NW Boreal LCC")
locs$`FMZ Regions` <- c("Chugach N.F."="CGF", "Copper River"="CRS", "Delta"="DAS", "Fairbanks"="FAS",
  "Galena"="GAD", "Haines"="HNS", "Kenai/Kodiak"="KKS", "Military"="MID", "Anchorage/Mat-Su"="MSS",
  "Southwest"="SWS", "Tanana"="TAD", "Tok"="TAS", "Tongass N.F."="TNF", "Upper Yukon"="UYD")
locs$`TPA Regions` <- c(
  "Alaska DNR"="State Department of Natural Resources",
  "BLM"="Bureau of Land Management (BLM)",
  "DOD/DOE"="Department of Defense (DOD) and Department of Energy",
  "FWS"="Fish and Wildlife Service (FWS)",
  "NPS"="National Park Service (NPS)",
  "Govt. of BC"="Government of British Columbia",
  "Parks Canada Agency"="Government of Canada, Parks Canada Agency",
  "Yukon Dept. of Environment"="Government of Yukon, Department of Environment"
)

# Shapefiles
library(rgdal)
library(maptools)
shpDir <- "LowResFlatShapefiles" #"C:/github/DataExtraction/data/shapefiles"
proj4 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Full domain (Alaska / western Canada)
akcan1_shp <- readOGR(file.path(shpDir, "Political/Alaska.shp"), verbose=FALSE) %>% spTransform(proj4) # temporary
# State/Province
akcan2_shp <- readOGR(file.path(shpDir, "Political/AK_CAN.shp"), verbose=FALSE) %>% spTransform(proj4)
akcan2_IDs <- c("Alaska", "Alberta", "Saskatchewan", "Manitoba", "Yukon Territory", "British Columbia")
akcan2_shp <- subset(akcan2_shp, NAME %in% akcan2_IDs)
# Alaska ecoregions
eco3_shp <- readOGR(file.path(shpDir, "AK_Ecoregions/AK_Ecoregions_COMMONER.shp"), verbose=FALSE) %>% spTransform(proj4)
eco2_shp <- readOGR(file.path(shpDir, "AK_Ecoregions/AK_Ecoregions_LEVEL2.shp"), verbose=FALSE) %>% spTransform(proj4)
eco1_shp <- readOGR(file.path(shpDir, "AK_Ecoregions/AK_Ecoregions_LEVEL1.shp"), verbose=FALSE) %>% spTransform(proj4)
# Alaska LCC regions
LCC_shp <- readOGR(file.path(shpDir, "LCC/LCC_regions.shp"), verbose=FALSE) %>% spTransform(proj4)
# Alaska/Canada LCC regions
LCC2_shp <- readOGR(file.path(shpDir, "AKCAN_LCC/AKCAN_LCC_regions.shp"), verbose=FALSE) %>% spTransform(proj4)
LCC2_shp <- subset(LCC2_shp, NAME=="Northwest Boreal LCC")
# CAVM regions
CAVM_shp <- readOGR(file.path(shpDir, "CAVM/CAVM_complete.shp"), verbose=FALSE) %>% spTransform(proj4)
# Alaska fire management zones
FMZ_shp <- readOGR(file.path(shpDir, "FireMgmtZones/FireManagementZones_simplified.shp"), verbose=FALSE) %>% spTransform(proj4)
# Terrestrail Protected Areas
TPA_shp <- readOGR(file.path(shpDir, "NA_TPA/NA_TPA_simplified.shp"), verbose=FALSE) %>% spTransform(proj4)

shp.list <- list(akcan1_shp, akcan2_shp, eco1_shp, eco2_shp, eco3_shp, LCC_shp, LCC2_shp, CAVM_shp, FMZ_shp, TPA_shp)
locs_areas <- map(seq_along(locs), ~shp.list[[.x]]@data$Shape_Area)
names(locs_areas) <- names(shp.list) <- names(locs)

#walk2(mapsets, list(akcan1_shp, akcan2_shp, eco1_shp, eco2_shp, eco3_shp, LCC_shp, LCC2_shp, CAVM_shp, FMZ_shp, TPA_shp),
#      ~saveRDS(.y, file=paste0("climdist/appData/shp/", .x, ".rds")))

locs2 <- locs
locs2$`LCC Regions` <- levels(LCC_shp$NAME)[c(1,2,4,3,5)]
locs2$`TPA Regions` <- levels(TPA_shp$NAME)[c(8,1,2,3,7,4,5,6)]

mapset_colIDs <- rep("NAME", 10)
objs <- c('shp.list', 'locs_areas', 'locs', 'locs2', 'mapsets', 'rcps', 'gcms', 'cru', 'period', 'variables', 'seasons', 'stats', 'mapset_colIDs')
save(list=objs, file="climdist/appData/appData.RData") # general data
