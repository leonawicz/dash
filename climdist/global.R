app_intro_message <- "
<p style='text-align:justify;'>Welcome to SNAP's Climate Analytics app. To get started, a default data set is loaded when you visit this app.
Press 'Build distributions' in the center of the page to reload after updating data selections.
You can see an overview of app features and capabilities below. 
If you are new to the app, taking the tour located in the sidebar is also recommended. Click to dismiss this message at any time.</p>
<ul style='padding: 15px; text-align: justify;'>
<li><h4>Full climate distributions, not just pre-aggregated stats.</h4>
<p>Drill down into SNAP's high-resolution downscaled climate data.
Access past and projected climate across Alaska and western Canada.</p></li>
<li><h4>Advanced analytics for informed decisions</h4>
<p>Integrate climate probability distributions across space, time and other factors. 
Fit models, analyze projections and bound future uncertainty.</p></li>
<li><h4>The data you specify. The output you need.</h4>
<p>Get customized output including dynamic reports generated on the fly in reponse to your unique specifications.</p></li>
<li><h4>Fully downloadable content at the click of a button</h4>
<p>Download data sets available in several file formats as well as pdf graphics and reports based on live content.</p></li>
<li><h4>Bookmarking</h4>
<p>Save the precise state of your application for later or to share specific data and results with colleagues.</p></li>
<li><h4>Get help</h4>
<p>The Information tab provides an overview and links to other resources.
Tooltips are available throughout the app. Go into deeper detail with the interactive tour.</p></li>
<li><h4>Sign in for more</h4>
<p>Parallel processing, even bigger data needs, and other features available to authenticated users.</p></li>
</ul>"
app_intro <- list(
  title="Explore and analyze spatial climate distributions",
  message=app_intro_message,
  logo="https://toolkit.climate.gov/sites/default/files/styles/large/public/snap_cmyk.png?itok=RYgSo91h",
  toast.args=list(extendedTimeOut=30000, progressBar=TRUE)
)

ptm <- proc.time()
library(rintrojs)
library(leaflet)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinytoastr)

library(apputils)
library(sp)
library(dplyr)
library(purrr)
library(ggplot2)
library(aws.s3)

cat("Total library load time:\n")
print(proc.time() - ptm)

ptm <- proc.time()
load("appData/appData.RData") # load any default local data sets
data_source <- "aws"  # specify location of data sets as local or aws
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
