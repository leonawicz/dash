library(dplyr)
library(purrr)
library(aws.s3)

# setup
source("aws_key.R")
bkt <- "leonawicz"

period <- c(1860, 2099)
variables <- c("Precipitation"="pr", "Mean temperature"="tas", "Min Temperature"="tasmin", "Max temperature"="tasmax")
seasons <- c("Annual"="annual", "Winter"="winter", "Spring"="spring", "Summer"="summer", "Autumn"="autumn")
stats <- c("Mean")
mapsets <- c("State/Province"="Political Boundaries",
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

dataDir <- "appData/clim_2km_seasonal"
grpDirs <- list.files(dataDir)
grpDirs <- grpDirs[match(mapsets, grpDirs)]
locs <- map(grpDirs, ~list.files(file.path(dataDir, .x)))
names(locs) <- mapsets

names(locs$`Political Boundaries`) <- c("Alaska and western Canada", locs$`Political Boundaries`[2:7])
names(locs$`Alaska L1 Ecoregions`) <- locs$`Alaska L1 Ecoregions`
names(locs$`Alaska L2 Ecoregions`) <- locs$`Alaska L2 Ecoregions`
names(locs$`Alaska L3 Ecoregions`) <- locs$`Alaska L3 Ecoregions`
names(locs$`CAVM Regions`) <- locs$`CAVM Regions`
locs$`LCC Regions` <- c("Arctic"="Arctic", "North Pacific"="N Pacific", "NW interior boreal (north)"="NW Interior Forest N",
                        "NW interior boreal (south)"="NW Interior Forest S", "Western Alaska"="W Alaska")
locs$`AK-CAN LCC` <- c("Northwest boreal"="AK-CAN NW Boreal LCC")
locs$`FMZ Regions` <- c("Chugach N.F."="CGF", "Copper River"="CRS", "Delta"="DAS", "Fairbanks"="FAS",
  "Galena"="GAD", "Haines"="HNS", "Kenai/Kodiak"="KKS", "Military"="MID", "Anchorage/Mat-Su"="MSS",
  "Southwest"="SWS", "Tanana"="TAD", "Tok"="TAS", "Tongass N.F."="TNF", "Upper Yukon"="UYD")
locs$`TPA Regions` <- c(
  "Alaska DNR"="State Department of Natural Resources",
  "BLM"="Bureau of Land Management (BLM)",
  "DOD/DOE"="DOD and DOE",
  "FWS"="Fish and Wildlife Service (FWS)",
  "NPS"="National Park Service (NPS)",
  "Govt. of BC"="Government of British Columbia",
  "Parks Canada Agency"="Government of Canada, Parks Canada Agency",
  "Yukon Dept. of Environment"="Government of Yukon, Department of Environment"
)

objs <- c('locs', 'mapsets', 'rcps', 'gcms', 'cru', 'period', 'variables', 'seasons', 'stats')
save(list=objs, file="appData/appData.RData") # general data

# Shapefiles
library(rgdal)
library(maptools)
shpDir <- "C:/github/DataExtraction/data/shapefiles"
proj4 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# State/Province
# Alaska
Alaska_shp <- readOGR(file.path(shpDir, "Political/Alaska.shp"), verbose=FALSE) %>% spTransform(proj4)
# Canada
Canada_shp <- readOGR(file.path(shpDir, "Political/CanadianProvinces_NAD83AlaskaAlbers.shp"), verbose=FALSE) %>% spTransform(proj4)
Canada_IDs <- c("Alberta", "Saskatchewan", "Manitoba", "Yukon Territory", "British Columbia")
Canada_shp <- subset(Canada_shp, NAME %in% Canada_IDs)
# Alaska ecoregions
eco32_shp <- readOGR(file.path(shpDir, "AK_ecoregions/akecoregions.shp"), verbose=FALSE) %>% spTransform(proj4)
eco32_shp <- spTransform(eco32_shp, CRS(proj4string(Alaska_shp)))
eco9_shp <- unionSpatialPolygons(eco32_shp, eco32_shp@data$LEVEL_2)
eco3_shp <- unionSpatialPolygons(eco32_shp, eco32_shp@data$LEVEL_1)
# Alaska LCC regions
LCC_shp <- readOGR(file.path(shpDir, "LCC/LCC_summarization_units_singlepartPolys.shp"), verbose=FALSE) %>% spTransform(proj4)
# Alaska/Canada LCC regions
LCC2_shp <- readOGR(file.path(shpDir, "LCC_AKCAN_boreal/AK_LCC_boundaries_AKAlbersNAD83.shp"), verbose=FALSE) %>% spTransform(proj4)
LCC2_shp <- subset(LCC2_shp, LCC_Name=="Northwest Boreal LCC")
# CAVM regions
CAVM_shp <- readOGR(file.path(shpDir, "CAVM/CAVM_complete.shp"), verbose=FALSE) %>% spTransform(proj4)
# Alaska fire management zones
FMZ_shp <- readOGR(file.path(shpDir, "FireMgmtZones/FireManagementZones_simplified.shp"), verbose=FALSE) %>% spTransform(proj4)
# Terrestrail Protected Areas
TPA_shp <- readOGR(file.path(shpDir, "NA_TPA/NA_TPA_simplified.shp"), verbose=FALSE) %>% spTransform(proj4)

walk2(mapsets, list(Canada_shp, eco3_shp, eco9_shp, eco32_shp, LCC_shp, LCC2_shp, CAVM_shp, FMZ_shp, TPA_shp),
      ~saveRDS(.y, file=paste0("appData/shp/", .x, ".rds")))
