# calback convenience functions
stepEquals <- function(i) paste0("this._currentStep==", i-1, collapse=" || ")

dv <- function(x, quote=TRUE){
  x <- paste0("a[data-value=\"", x, "\"]")
  if(quote) x <- paste0("'", x, "'")
  x
}

rmClass <- function(x) paste0(paste0(
  "$(", dv(x), ").removeClass('active');", collapse="\n"), "\n")

goClass <- function(x){
  if(length(x) > 1) stop("Only add and trigger one class at a time.")
  paste0("$(", dv(x), ").addClass('active');\n$(", dv(x), ").trigger('click');\n")
}

stepcb <- function(condition, action){
  paste0("if (", condition, ") {", paste0(action, collapse="\n"), "}")
}

# tour steps
tour.text <- c(
  "01"="Welcome to the SNAP climate data exploration tool for downscaled preciptiation and temperature.
  This R Shiny app offers 3.7 million spatial probability distributions among 82 unique geographic subdomains
  across more than 45,000 high resolution, spatially explicit climate maps containing a total of approximately 200 billion pixels,
  all without the need to reduce spatial climate distributions to select, precomputed statistics.
  The app exposes hundreds of gigabytes of data to the user and any slice of the data can be rapidly accessed.
  After the tour, for additional information, see the Information tab in the sidebar.",
  "02"="The first thing to note is that the map layer can be changed here.
  The default map layer is the full SNAP Alaska/western Canada domain. Several other map layers are available,
  each offering multiple subdomains.",
  "03"="An interactive map allows you to select one or more subdomains available in the currently loaded map layer.
  The default full domain layer does not contain any selectable subdomains.",
  "04"="When subdomains are available for the selected map layer,
  clicking on polygons in the map pane is convenient for selecting or deselecting subdomains, but this can also be done
  using the region selection menu, which populates with the same subdomains based on the selected map layer.
  The dropdown menu selections and interactive map selections are synchronized.",
  "05"="Four climate variables are available: seasonal total precipitation and seasonal 
  means of minimum, mean, and maximum daily temperatures.",
  "06"="Representative Concentration Pathways (RCPs) refer to different levels of
  projected future greenhouse gas emmissions based on different assumptions about population growth,
  economic development and energy usage. Three RCPs are available for each climate model.
  See the Information tab for more details about RCPs.",
  "07"="There are five General Circulation Models (GCMs) available. Using multiple RCPs and GCMs provides
  a more robust representation of future uncertainty. See the Information tab for more details about GCMs.",
  "08"="Annual and seasonal distributions are available. The seasons winter, spring, summer and autumn
  correspond to three-month precipitation totals and temperature averages beginning in December of the previous year
  and ending in November of the current year.",
  "09"="Years available are from 1860 - 2099. Historical GCM data runs from 1860 - 2005. 
  Projected outputs run from 2006 - 2099. Optional accompanying CRU 4.0 historical data runs from 1900 - 2015.",
  "10"= "Marginal distributions can be computed by integrating out levels of factors specified here.
  Note that integration will be more time-intensive for greater numbers of variables and their selected levels, 
  including the range of years selected. A progress bar will display in the bottom right corner.",
  "11"="When multiple levels of a factor variable are included in the data selection, plots can be colored by levels.",
  "12"="Faceting by a variable's levels works the same as with coloring.",
  "13"="When user selections are complete, click here to load the relevant data slices, compile probability distributions,
  and draw summary plots.",
  "14"="Additional settings pertaining to plot formatting can be found here.",
  "15"="When the only changes made pertain to plot formatting, regenerate plots and statistics without reloading redundant data.
  This saves time, especially with large data sets or when computing marginal distributions as part of your data specifications.
  Rebuilding distributions is only necessary when data selections have changed."
)

tour.pos <- c("left", "right", "bottom", rep("left", 12))

tour.element <- c(
  "#shiny-tab-climate", "#mapset + .selectize-control", "#Map", "#regions + .selectize-control",
  "#variable + .selectize-control",
  "#rcps + .selectize-control", "#gcms + .selectize-control", "#seasons + .selectize-control", ".js-irs-3", 
  "#marginalize + .selectize-control", "#clrby + .selectize-control", "#fctby + .selectize-control",
  "#go_btn", "#settings_btn", "#plot_btn"
)

steps <- reactive({
  data.frame(element=tour.element, intro=tour.text, position=tour.pos)
})



# begin tour on button click
observeEvent(input$help, {
  not.db.climate <- c("info")
  tour.options <- list(steps=steps(), "showProgress"="true", "showStepNumbers"="false")
  tour.events <- list(
    "onchange"=I(paste0(
      stepcb(stepEquals(c(1:15)), c(rmClass(not.db.climate), goClass("climate"))),
      collapse="\n"))
  )
  introjs(session, options=tour.options, events=tour.events)
})
