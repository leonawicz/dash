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
  The first thing to note is that the map layer can be changed here.
  The default map layer is the full SNAP Alaska/western Canada domain. Several other map layers are available,
  each offering multiple subdomains.",
  "02"="An interactive map allows you to select one or more subdomains available in the currently loaded map layer.
  The default full domain layer does not contain any selectable subdomains.",
  "03"="When subdomains are available for the selected map layer,
  clicking on polygons in the map pane is convenient for selecting or deselecting subdomains, but this can also be done
  using the region selection menu, which populates with the same subdomains based on the selected map layer.
  The dropdown menu selections and interactive map selections are synchronized.",
  "04"="Four climate variables are available: seasonal total precipitation totals and seasonal 
  means of minimum, mean, and maximum daily temperatures.",
  "05"="Representative Concentration Pathways (RCPs) refer to different levels of
  projected future greenhouse gas emmissions based on different assumptions about population growth,
  economic development and energy usage. Three RCPs are available for each climate model.
  See the Information tab for more details about RCPs.",
  "06"="There are five General Circulation Models (GCMs) available. Using multiple RCPs and GCMs provides
  a more robust representation of future uncertainty. See the Information tab for more details about GCMs.",
  "07"="Annual and seasonal distributions are available. The seasons winter, spring, summer and autumn
  correspond to three-month precipitation totals and temperature averages beginning in December of the previous year
  and ending in November of the current year.",
  "08"="Years available are from 1860 - 2099. Historical GCM data runs from 1860 - 2005. 
  Projected outputs run from 2006 - 2099. Optional accompanying CRU 4.0 historical data runs from 1900 - 2015.",
  "09"= "Marginal distributions can be computed by integrating out levels of factors specified here.
  Note that integration will be more time-intensive for greater numbers of variables and their selected levels, 
  including the range of years selected. A progress bar will display in the bottom right corner.",
  "10"="When multiple levels of a factor variable are included in the data selection, plots can be colored by levels.",
  "11"="Faceting by a variable's levels works the same as with coloring.",
  "12"="When user selections are complete, click here to load the relevant data slices, compile probability distributions,
  and draw summary plots.",
  "13"="Additional settings pertaining to plot formatting can be found here.",
  "14"="When the only changes made pertain to plot formatting, regenerate plots and statistics without reloading redundant data.
  This saves time, especially with large data sets or when computing marginal distributions as part of your data specifications.
  Rebuilding distributions is only necessary when data selections have changed."
)

tour.pos <- c("right", "bottom", rep("left", 12))

tour.element <- c(
  "#mapset + .selectize-control", "#Map", "#regions + .selectize-control",
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
      stepcb(stepEquals(c(1:14)), c(rmClass(not.db.climate), goClass("climate"))),
      collapse="\n"))
  )
  introjs(session, options=tour.options, events=tour.events)
})
