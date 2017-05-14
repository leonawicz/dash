# Modal-based inputs
observeEvent(input$settings_btn, {
  showModal(modalDialog(
    fluidRow(
      column(4,
        selectInput("metric", "Units", c("US", "Metric"), width="100%"),
        selectInput("facet_scales", "Axis scales", choices=axis_scales, width="100%")
      ),
      column(4,
        sliderInput("alpha_den", "Density transparency", 0.1, 1, 1, 0.1, sep="", width="100%"),
        sliderInput("alpha_ts", "Series transparency", 0.1, 1, 0.1, 0.1, sep="", width="100%")
      ),
      column(4, checkboxInput("include_cru", "Include CRU 4.0 data", FALSE, width="100%"))
    ),
    size="l", easyClose=TRUE
  ))
})

# Map-related observers
# observe region selectInput and update map polygons
observeEvent(input$regions, {
  x <- input$regions
  if(is.null(x) || x[1]!=default_mapset){
  proxy <- leafletProxy("Map")
  not_selected <- setdiff(rv$regions, x)
  if(length(not_selected)) walk(not_selected, ~proxy %>% removeShape(layerId=paste0("selected_", .x)))
  walk(x, ~proxy %>%
    addPolygons(data=subset(rv$shp, REGION==.x),
      stroke=TRUE, fillOpacity=0.2, weight=1, group="selected", layerId=paste0("selected_", .x)))
  }
}, ignoreNULL=FALSE)

# observe map shape click and add or remove selected polygons and update region selectInput
observeEvent(input$Map_shape_click, {
  p <- input$Map_shape_click$id
  x <- input$regions
  if(is.null(x) || x[1]!=default_mapset){
    p1 <- strsplit(p, "_")[[1]][2]
    proxy <- leafletProxy("Map")
    
    if(substr(p, 1, 9)=="selected_"){
      proxy %>% removeShape(layerId=p)
    } else {
      proxy %>% addPolygons(data=subset(rv$shp, REGION==p), stroke=TRUE, fillOpacity=0.2, weight=1,
                            group="selected", layerId=paste0("selected_", p))
    }
    
    if(!is.null(p)){
      if(is.na(p1) && (is.null(x) || !p %in% x)){
        updateSelectInput(session, "regions", selected=c(x, p))
      } else if(!is.na(p1) && p1 %in% x){
        updateSelectInput(session, "regions", selected=x[x!=p1])
      }
    }
  }
})

# Toast-related observer for variable selection
observe({
  x <- NULL
  success <- paste(names(variables)[match(input$variable, variables)], "distributions available")
  input$rcps; input$gcms; input$regions; input$seasons
  isolate({
    if(is.null(input$rcps)) x <- "RCP selection missing"
    if(is.null(input$gcms)) x <- "GCM selection missing"
    if(!is.null(input$mapset) && input$mapset!=default_mapset && is.null(input$regions)) 
      x <- paste("No", tolower(mapset_labs()), "selected")
    if(is.null(input$seasons)) x <- "Season selection missing"
    if(is.null(x) && all(input$regions %in% rv$regions)){
      toastr_success(title="Data subset updated", success, timeOut=2500, preventDuplicates=TRUE)
    } else {
      toastr_error(title="Empty data set", x, timeOut=2500, preventDuplicates=TRUE)
    }
  })
})
