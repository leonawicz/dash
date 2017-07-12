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

source("apptext.R")
load("appData/appData.RData") # load any default local data sets
data_source <- "aws" # specify location of data sets as local or aws
dataloc <- if(data_source=="local") "appData" else "s3://leonawicz/apps/ar5_climdist"
source("aws_key.R") # authentication to AWS
#enableBookmarking(store="server") # not yet available on shinyapps.io
