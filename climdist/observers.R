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
  x <- input$regions
  if(is.null(x) || x[1]!=default_mapset){
    proxy <- leafletProxy("Map")
    not_selected <- setdiff(rv$regions, x)
    if(length(not_selected)) walk(not_selected, ~proxy %>% removeShape(layerId=paste0("selected_", .x)))
    if(length(x)){
      x2 <- as.character(locs2[[input$mapset]][match(x, locs[[input$mapset]])])
      walk2(x, x2, ~proxy %>%
        addPolygons(data=rv$shp[rv$shp[[mapset_reg_id()]]==.y,],
          stroke=TRUE, fillOpacity=0.2, weight=1, group="selected", layerId=paste0("selected_", .x)))
    }
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
      p2 <- as.character(locs2[[input$mapset]][match(p, locs[[input$mapset]])])
      proxy %>% addPolygons(data=rv$shp[rv$shp[[mapset_reg_id()]]==p2,], stroke=TRUE, fillOpacity=0.2, weight=1,
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

# Observe button click for loading data
observeEvent(input$go_btn, {
  if(input$go_btn==0) return()
  load_files <- function(path, files, src="local"){
    readData <- if(src=="local") readRDS else s3readRDS
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    x <- vector("list", nrow(files))
    for(i in seq_along(x)){
      progress$inc(1/nrow(files), message="Loading data...", detail=basename(path))
      x[[i]] <- readData(file.path(path, files[i, 5])) %>% 
            mutate(RCP=factor(switch(files[i, 2], 
                                     historical="Historical", 
                                     rcp45="RCP 4.5", 
                                     rcp60="RCP 6.0", 
                                     rcp85="RCP 8.5"), levels=c("Historical", rcps)),
                   Model=factor(ifelse(files[i, 3]=="ts40", cru, files[i, 3]), levels=c(cru, gcms)),
                   Region=factor(basename(path), levels=rv$regions),
                   Var=factor(files[i, 1], levels=variables),
                   Season=factor(files[i, 4], levels=seasons)) %>%
            select(RCP, Model, Region, Var, Season, Year, Val, Prob)
    }
    x <- bind_rows(x) %>% rvtable
  }
  if(identical(files(), rv$current_files) & 
     identical(regions_selected(), rv$current_regions) & identical(input$cru, rv$cru)){
    rv$load_new_files <- FALSE
  } else {
    rv$current_files <- files()
    rv$current_regions <- regions_selected()
    rv$cru <- input$cru
    rv$load_new_files <- TRUE
  }
  if(rv$load_new_files){
    region_paths <- file.path(dataloc, "clim_2km_seasonal", input$mapset, regions_selected())
    rv$d <- map(region_paths, ~load_files(.x, files(), data_source)) %>% bind_rows  %>% droplevels
    rv$current_files <- files()
    rv$current_regions <- regions_selected()
    rv$cru <- input$cru
    rv$load_new_files <- FALSE
  }
})
