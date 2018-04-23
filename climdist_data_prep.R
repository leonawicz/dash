library(dplyr)
library(purrr)

period <- c(1860, 2099)
variables <- c("Precipitation" = "pr", "Mean temperature" = "tas", "Min Temperature" = "tasmin", "Max temperature" = "tasmax")
molist <- as.list(month.abb)
names(molist) <- month.name
seasons <- list("Annual" = "annual", 
                Seasons = list("Winter" = "winter", "Spring" = "spring", "Summer" = "summer", "Autumn" = "autumn"),
                Months = molist)
stats <- c("Mean")
mapsets <- c(
  "Alaska/western Canada" = "AK-CAN",
  "State/Province" = "Political Boundaries",
  "Alaska level 1 ecoregions" = "Alaska L1 Ecoregions",
  "Alaska level 2 ecoregions" = "Alaska L2 Ecoregions",
  "Alaska level 3 ecoregions" = "Alaska L3 Ecoregions",
  "Alaska LCC regions" = "AK LCC regions",
  "Alaska/Canada LCC regions" = "LCC regions",
  "CAVM regions" = "CAVM regions",
  "Fire management zones" = "FMZ regions",
  "Terrestrial protected areas" = "TPA regions")
rcp <- c("Historical", "RCP 4.5", "RCP 6.0", "RCP 8.5")
rcps <- c("4.5" = rcp[2], "6.0" = rcp[3], "8.5" = rcp[4])
gcms <- c("GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3", "NCAR-CCSM4")
cru <- "CRU 4.0"
default_clim <- c(1980, 2009)

dataDir <- "seasonal"
grpDirs <- list.files(dataDir)
grpDirs <- grpDirs[match(mapsets, grpDirs)]
locs <- map(grpDirs, ~list.files(file.path(dataDir, .x)))
names(locs) <- mapsets

names(locs$`Political Boundaries`) <- locs$`Political Boundaries`
names(locs$`Alaska L1 Ecoregions`) <- locs$`Alaska L1 Ecoregions`
names(locs$`Alaska L2 Ecoregions`) <- locs$`Alaska L2 Ecoregions`
names(locs$`Alaska L3 Ecoregions`) <- locs$`Alaska L3 Ecoregions`
names(locs$`CAVM regions`) <- locs$`CAVM regions`
locs$`AK-CAN` <- c("Alaska/western Canada" = "AK-CAN")
locs$`AK LCC regions` <- c("Arctic" = "Arctic LCC", "North Pacific" = "North Pacific LCC", 
                        "NW interior boreal (north)" = "Northwestern Interior Forest North LCC",
                        "NW interior boreal (south)" = "Northwestern Interior Forest  South LCC", 
                        "Western Alaska" = "Western Alaska LCC")
locs$`LCC regions` <- c("Aleutians/Bering sea" = "Aleutian-Bering Sea Islands LCC",
                       "Arctic" = "Arctic LCC", "North Pacific" = "North Pacific LCC",
                       "Northwest boreal" = "Northwest Boreal LCC",
                       "Western Alaska" = "Western Alaska LCC")
locs$`FMZ regions` <- c("Chugach N.F." = "CGF", "Copper River" = "CRS", "Delta" = "DAS", "Fairbanks" = "FAS",
  "Galena" = "GAD", "Haines" = "HNS", "Kenai/Kodiak" = "KKS", "Military" = "MID", "Anchorage/Mat-Su" = "MSS",
  "Southwest" = "SWS", "Tanana" = "TAD", "Tok" = "TAS", "Tongass N.F." = "TNF", "Upper Yukon" = "UYD")
locs$`TPA regions` <- c(
  "Alaska DNR" = "State Department of Natural Resources",
  "BLM" = "Bureau of Land Management (BLM)",
  "DOD/DOE" = "Departments of Defense (DOD) Energy (DOE)",
  "FWS" = "Fish and Wildlife Service (FWS)",
  "NPS" = "National Park Service (NPS)",
  "Govt. of BC" = "Government of British Columbia",
  "Parks Canada Agency" = "Government of Canada, Parks Canada Agency",
  "Yukon Dept. of Environment" = "Government of Yukon, Department of Environment"
)

# Shapefiles
library(maputils)
shp.list <- list(akcan, akcan2, ecoreg1, ecoreg2, ecoreg3, aklcc, lcc, cavm, fmz, tpa)
locs_areas <- map(seq_along(locs), ~shp.list[[.x]]@data$Shape_Area)
names(locs_areas) <- names(shp.list) <- names(locs)

locs2 <- locs
locs2$`AK LCC regions` <- levels(aklcc$NAME)[c(1, 2, 4, 3, 5)]
locs2$`LCC regions`[1] <- gsub("-", "/", levels(lcc$NAME)[1])
locs2$`TPA regions` <- levels(tpa$NAME)[c(8, 1, 2, 3, 7, 4, 5, 6)]

# Additional constants used by app
mapset_colIDs <- rep("NAME", 10)
default_mapset <- "AK-CAN"
regions_list_default <- locs[[default_mapset]]
regions_selected_default <- regions_list_default[1]
cru.max.yr <- 2015 # max observational year
rcp.min.yr <- 2006 # min projected GCM run year
limit.sample <- TRUE # shrink final sampling by a factor of number of RCPs tmes number of GCMs
action_btn_style <- "color: black; margin: 10px 15px 10px 15px; width: 200px;" # action button styling
axis_scales <- c("Fixed" = "fixed", "Free" = "free", "Free X" = "free_x", "Free Y" = "free_y") # facet scale options
req_inputs <- c("regions", "rcps", "seasons", "gcms") # inputs that must be non-null to proceed in app
clropts <- c("Color by..." = "", "RCP", "Model", "Season", "Region") # available variables for coloring/faceting plots
fctopts <- c("Facet by..." = "", "RCP", "Model", "Season", "Region")
mergeopts <- c("Merge distributions..." = "", "RCPs" = "RCP", "GCMs" = "Model") # available variables for marginalizing over
mergeopts_tooltip <- "Compute marginal distributions by marginalizing over levels of selected categorical variables."
faqs <- c("climdist_variables", "distributions", "fmz", "gcm", "rcp", "factsheet_about", "apps") # FAQs to load from snaputils
intro_bg <- "https://s-media-cache-ak0.pinimg.com/originals/4a/95/21/4a9521be7331ee5a44073edbd8492fca.jpg"
intro_css_args <- list(
  container = list(width = '70%', height = '700px'), 
  toast = list(top = '100px', 
             #background = paste0('url(', intro_bg, ') no-repeat center 0px'), #### inactive background override
             'background-size' = '70% 700px'), 
  rgba = c(60, 141, 188, 1), hover.rgba = c(60, 141, 188, 1)) #, radius = '0px') #### inactive radius override
   
objs <- c('shp.list', 'locs_areas', 'locs', 'locs2', 'mapsets', 'rcps', 'gcms', 'cru', 'default_clim',
          'period', 'variables', 'seasons', 'stats', 'mapset_colIDs',
          'default_mapset', 'regions_list_default', 'regions_selected_default', 'cru.max.yr', 'rcp.min.yr',
          'limit.sample', 'action_btn_style', 'axis_scales', 'req_inputs', 'clropts', 'fctopts', 
          'mergeopts', 'mergeopts_tooltip', 'faqs', 'intro_css_args')
save(list = objs, file = "climdist/appData/appData.RData") # general data
