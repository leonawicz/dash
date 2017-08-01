# Modal-based inputs
#observeEvent(input$staticmap_btn, {
#  showModal(modalDialog(
#    title=names(mapsets)[match(input$mapset, mapsets)], footer=NULL,
#    img(src='Fire_Mgmt_Areas.png', align="center", style="width: 100%"),
#    size="l", easyClose=TRUE
#  ))
#})

# Map-related observers
# Observe selected mapset for regions list and shapefile reactive values
observeEvent(input$mapset, {
  if(is.null(input$mapset)){
    rv[["regions"]] <- regions_list_default
    rv[["shp"]] <- shp.list[[default_mapset]]
  } else {
    rv[["regions"]] <- locs[[input$mapset]]
    rv[["shp"]] <- shp.list[[input$mapset]]
  }
})

# observe region selectInput and update map polygons
observeEvent(input$regions, {
  update_mapset("selectInput", rv$shp, input$mapset, mapset_reg_id(), 
                input$regions, rv$regions, locs, locs2, default_mapset)
}, ignoreNULL=FALSE)

# observe map shape click and add or remove selected polygons and update region selectInput
observeEvent(input$Map_shape_click, {
  update_mapset("mapclick", selected_regions=input$regions, 
                click_id=input$Map_shape_click$id, session=session)
})

# Toast-related observer for variable selection
observe({
  x <- NULL
  success <- paste(names(variables)[match(input$variable, variables)], "distributions available")
  input$rcps; input$gcms; input$regions; input$seasons
  isolate({
    if(!is.null(input$mapset) && input$mapset!=default_mapset){
      if(is.null(input$rcps)) x <- "RCP selection missing"
      if(is.null(input$gcms)) x <- "GCM selection missing"
      if(is.null(input$regions)) 
        x <- paste("No", tolower(mapset_labs()), "selected")
      if(is.null(input$seasons)) x <- "Season selection missing"
      if(is.null(x) && all(input$regions %in% rv$regions)){
        toastr_success(title="Data subset updated", success, timeOut=2500, preventDuplicates=TRUE)
      } else {
        toastr_error(title="Empty data set", x, timeOut=2500, preventDuplicates=TRUE)
      }
    }
  })
})

# Observe button click for loading data
observeEvent(input$go_btn, {
  rv$go <- rv$go + 1
})

observe({
  x <- actionButton("overview_btn", "Overview", class="btn-block", icon("info-circle"))
  showNotification("Need an overview?", action=x, duration=NULL, id="overview_note", type="message")
})

observe({
  input$overview_btn
  isolate({
    if(!is.null(input$overview_btn) && input$overview_btn > 0){
      appintro(title=app_intro$title, message=app_intro$message, logo=app_intro$logo, toast.args=app_intro$toast.args)
    }
  })
})
    
observe({
  rv$go
  isolate({
  load_files <- function(path, files, src="local"){
    reg <- rv$regions
    readData <- if(src=="local") readRDS else s3readRDS
    readData_iterate <- function(i){
      readData(file.path(path, files[i, 5])) %>%
      mutate(RCP=factor(switch(files[i, 2], 
                               historical="Historical", 
                               rcp45="RCP 4.5", 
                               rcp60="RCP 6.0", 
                               rcp85="RCP 8.5"), levels=c("Historical", rcps)),
             Model=factor(ifelse(files[i, 3]=="ts40", cru, files[i, 3]), levels=c(cru, gcms)),
             Region=factor(basename(path), levels=reg),
             Var=factor(files[i, 1], levels=variables),
             Season=factor(files[i, 4], levels=as.character(unlist(seasons)))) %>%
      select(RCP, Model, Region, Var, Season, Year, Val, Prob)
    }
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    x <- vector("list", nrow(files))
    for(i in seq_along(x)){
      progress$inc(1/nrow(files), message="Loading data...", detail=basename(path))
      x[[i]] <- readData_iterate(i)
    }
    bind_rows(x) %>% rvtable
  }
  if(identical(files(), rv$current_files) & 
     identical(regions_selected(), rv$current_regions) & identical(cru_selected(), rv$cru)){
    rv$load_new_files <- FALSE
  } else {
    rv$current_files <- files()
    rv$current_regions <- regions_selected()
    rv$cru <- cru_selected()
    rv$load_new_files <- TRUE
  }
  if(rv$load_new_files){
    region_paths <- file.path(dataloc, "clim_2km_seasonal", input$mapset, regions_selected())
    rv$d <- map(region_paths, ~load_files(.x, files(), data_source)) %>% bind_rows  %>% droplevels
    rv$current_files <- files()
    rv$current_regions <- regions_selected()
    rv$cru <- cru_selected()
    rv$load_new_files <- FALSE
  }
  })
})

# Observe plots
ggObserve(session, input, rv, rv_plots, "ts_plot_dblclk", "ts_plot_brush", isolate(d_ts_brushed), "ts_x", NULL, "d_ts_b")
ggObserve(session, input, rv, rv_plots, "dec_plot_dblclk", "dec_plot_brush", isolate(d_dec_brushed), "dec_x", NULL, "d_dec_b", FALSE)
