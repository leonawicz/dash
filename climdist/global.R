ptm <- proc.time()
library(rintrojs)
library(leaflet)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinytoastr)

library(apputils)
library(MASS) # rlm
library(sp)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggpmisc)
library(aws.s3)
library(knitr)
library(rmarkdown)

cat("Total library load time:\n")
print(proc.time() - ptm)

ptm <- proc.time()
source("apptext.R")
load("appData/appData.RData") # load any default local data sets
data_source <- "aws" # specify location of data sets as local or aws
dataloc <- if(data_source=="local") "appData" else "s3://leonawicz/apps/ar5_climdist"
source("aws_key.R") # authentication to AWS
cat("Total data load time:\n")
print(proc.time() - ptm)

ptm <- proc.time()
action_btn_style <- "color: black; margin: 10px 15px 10px 15px; width: 200px;"
axis_scales <- c("Fixed"="fixed", "Free"="free", "Free X"="free_x", "Free Y"="free_y")
valid_input_selection <- paste(
  paste(paste0("input.", c("regions", "rcps", "seasons")), "!= null", collapse=" & "), 
  " & (input.gcms != null || input.cru ==true)")
clrfctopts <- c("", "RCP", "Model", "Season", "Region")
faqs <- c("climdist_variables", "distributions", "fmz", "gcm", "rcp", "apps")

#enableBookmarking(store="server") # not yet available on shinyapps.io
cat("Remainder global.R time:\n")
print(proc.time() - ptm)
