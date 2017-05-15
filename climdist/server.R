library(rvtable)
lon <- -155
lat <- 65
tab_ids <- c("burnarea", "firefreq", "firesize", "vegarea", "vegage")
mods <- paste0("mod_", tab_ids)
default_mapset <- "AK-CAN"
regions_list_default <- locs$`FMZ Regions`
regions_selected_default <- regions_list_default[1]
cru.max.yr <- 2015
rcp.min.yr <- 2006

shinyServer(function(input, output, session) {
  
  mapset_workspace <- reactive({ 
    file.path(dataloc, "shp", paste0(input$mapset, ".rds"))
  })
  
  mapset_reg_id <- reactive({
    mapset_colIDs[match(input$mapset, mapsets)]
  })
  
  rv <- reactiveValues(d=NULL, current_files=NULL, current_regions=NULL, load_new_files=TRUE, 
          regions=regions_list_default, shp=readRDS(file.path(dataloc, "shp/FMZ Regions.rds")))
  
  load_map_file <- function(file, source="local"){
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    if(source=="local"){
      progress$set(message="Loading local data", value=1)
      shp <- readRDS(file)
      progress$set(message="Loading local data", value=2)
    }
    if(source=="aws"){
      progress$set(message="Fetching AWS data", value=1)
      shp <- s3readRDS(file)
      progress$set(message="Fetching AWS data", value=2)
    }
    rv[["regions"]] <- locs[[input$mapset]]
    rv[["shp"]] <- shp
  }
  
  observeEvent(input$mapset, {
    local_cache <- paste0("mapset_data_", input$mapset)
    if(is.null(mapset_workspace())){
      rv[["regions"]] <- regions_list_default
      rv[["shp"]] <- readRDS(file.path(dataloc, "shp/FMZ Regions.rds"))
    } else if(exists(local_cache, envir=.GlobalEnv)){
      rv[["regions"]] <- get(local_cache, envir=.GlobalEnv)$regions
      rv[["shp"]] <- get(local_cache, envir=.GlobalEnv)$shp
    } else {
      load_map_file(mapset_workspace(), source=data_source)
      assign(local_cache, list(regions=rv$regions, shp=rv$shp), envir=.GlobalEnv)
    }
  })

  mapset_labs <- reactive({ names(mapsets)[match(input$mapset, mapsets)] })
  
  output$mapset_regions <- renderUI({
    reg <- rv$regions
    mult <- FALSE
    if(input$mapset!=default_mapset){
      reg <- c("", reg)
      mult <- TRUE
    }
    if(length(mapset_labs()))
      selectInput("regions", mapset_labs(), choices=reg, selected=reg[1], multiple=mult, width="100%")
  })
  
  regions_selected <- reactive({
    if(is.null(input$regions)) return(regions_selected_default)
    input$regions
  })
  
  source("observers.R", local=TRUE) # map and region selectInput observers
  source("tour.R", local=TRUE) # introjs tour
  
  alpha_den <- reactive({ if(is.null(input$alpha_den)) 1 else input$alpha_den })
  alpha_ts <- reactive({ if(is.null(input$alpha_ts)) 0.1 else input$alpha_ts })
  facet_scales <- reactive({ if(is.null(input$facet_scales)) "fixed" else input$facet_scales })
  
  # Initialize map and add polygons
  mapSelect <- reactive({
    ptm <- proc.time()
    cat("Leaflet initialization time excluding renderLeaflet:\n")
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Generating zone map", value=0)
    akcan <- input$mapset==default_mapset
    if(akcan){
      z <- "Alaska and western Canada"
      z.id <- z
    } else {
      z <- as.character(rv$shp[[mapset_reg_id()]])
      idx <- match(z, locs2[[input$mapset]])
      z.id <- as.character(rv$regions[idx])
      z.lab <- names(rv$regions[idx])
    }
    n <- 1 + length(z)
    x <- leaflet() %>% addTiles() %>% setView(lon, lat, 4)
    progress$inc(1/n, detail="Basemap built")
    # Add background polygon region outlines after map is created
    if(!akcan){
      for(i in seq_along(z)){
        x <- x %>% addPolygons(data=rv$shp[rv$shp[[mapset_reg_id()]]==z[i],], stroke=TRUE, fillOpacity=0, weight=1,
          color="black", group="not_selected", layerId=z.id[i], label=z.lab[i],
          highlightOptions=highlightOptions(opacity=1, weight=2, fillOpacity=0, 
            bringToFront=FALSE, sendToBack=FALSE))
        progress$inc((i+1)/n, detail=paste("Adding polygon", i))
      }
    }
    print(proc.time() - ptm)
    x
  })
  
  output$Map <- renderLeaflet(mapSelect())
  outputOptions(output ,"Map", suspendWhenHidden=FALSE)
  
  metric <- reactive({ is.null(input$metric) || input$metric=="Metric" })
  i <- reactive({ list(rcps=input$rcps, gcms=input$gcms, reg=input$regions,
                       seasons=input$seasons, yrs=input$yrs,
                       reg.names=names(rv$regions)[match(input$regions, rv$regions)]) 
  })
  
  files <- reactive({
    rcps_string <- tolower(gsub("[ \\.]", "", input$rcps))
    models <- if(!is.null(input$cru) && input$cru && input$yrs[1] <= cru.max.yr)
      c("ts40", input$gcms) else input$gcms
    files <- expand.grid(input$variable, rcps_string, input$gcms, input$seasons, stringsAsFactors=FALSE)
    if("ts40" %in% models || input$yrs[1] < rcp.min.yr) 
      files <- rbind(expand.grid(input$variable, "historical", models, input$seasons, stringsAsFactors=FALSE), files)
    files <- files[!(files[, 3]=="ts40" & files[, 1] %in% c("tasmin", "tasmax")),] # temporarily missing files
    cbind(files, paste0(files[,1], "_", files[,2], "_", files[,3], "_", files[,4], ".rds"))
  })
  
  observeEvent(input$go_btn, {
    if(input$go_btn==0) return()
    load_files <- function(path, files){
      map(1:nrow(files), ~readRDS(file.path(path, files[.x, 5])) %>% 
            mutate(RCP=factor(switch(files[.x, 2], 
                                     historical="Historical", 
                                     rcp45="RCP 4.5", 
                                     rcp60="RCP 6.0", 
                                     rcp85="RCP 8.5"), levels=c("Historical", rcps)),
                   GCM=factor(ifelse(files[.x, 3]=="ts40", cru, files[.x, 3]), levels=c(cru, gcms)),
                   Region=factor(basename(path), levels=rv$regions),
                   Var=factor(files[.x, 1], levels=variables),
                   Season=factor(files[.x, 4], levels=seasons)) %>%
            select(RCP, GCM, Region, Var, Season, Year, Val, Prob)) %>% bind_rows %>% rvtable
    }
    print(files())
    if(identical(files(), rv$current_files) & identical(regions_selected(), rv$current_regions)){
      rv$load_new_files <- FALSE
    } else {
      rv$current_files <- files()
      rv$current_regions <- regions_selected()
      rv$load_new_files <- TRUE
    }
    if(rv$load_new_files){
      region_paths <- file.path(dataloc, "clim_2km_seasonal", input$mapset, regions_selected())
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(1, message="Loading data...", detail=NULL)
      rv$d <- map(region_paths, ~load_files(.x, files())) %>% bind_rows  %>% droplevels
      rv$current_files <- files()
      rv$current_regions <- regions_selected()
      rv$load_new_files <- FALSE
    }
  })
  
  noData <- reactive({ any(sapply(i(), is.null)) || is.null(rv$d) })
  
  d_sub <- reactive({
    input$go_btn
    isolate({
      if(is.null(rv$d)) return()
      withProgress({
        dots <- c("RCP", "GCM", "Region", "Var", "Season", "Year", "Val", "Prob")
        if(noData()){
          x <- slice(rv$d, 0)
        } else {
          getSubset <- function(x) filter(x, 
            RCP %in% c("Historical", i()[[1]]) & GCM %in% c(cru, i()[[2]]) &
              Region %in% i()[[3]] & Season %in% i()[[4]] &
              Year >= i()[[5]][1] & Year <= i()[[5]][2]) %>%
            select_(.dots=dots)
          x <- getSubset(rv$d)
        }
        x %>% droplevels
      }, message="Subsetting data...", value=1)
    })
  })
  
  d <- reactive({
    input$go_btn
    isolate({
      req(d_sub())
      m <- input$marginalize
      if(!is.null(m) && m!=""){
        m <- sort(m)
        m.lev <- map(m, ~levels(d_sub()[[.x]])) %>% 
          map(~.x[!.x %in% c("Historical", "CRU 4.0")])
        m <- m[map_int(m.lev, ~length(.x) > 1)]
        if(!length(m)) m <- NULL
      }
      
      d.args <- if(input$variable=="Precipitation") list(n=200, adjust=0.1, from=0) else list(n=200, adjust=0.1)
      s.args <- list(n=1000)
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      x <- d_sub() %>% rvtable
      step <- 1
      if(!is.null(m) && !"" %in% m){
        msg <- "Integrating variables..."
        for(i in seq_along(m)){
          step <- step + i
          detail <- paste0("Marginalizing over ", m[i], "s")
          progress$set(step, msg, detail)
          x <- marginalize(x, m[i], density.args=d.args, sample.args=s.args)
        }
      } else step <- 0
      progress$set(step + 1, message="Sampling distributions...", detail=NULL)
      x <- sample_rvtable(x, n=100)
      if(nrow(x) > 0){
        if(metric()){
          x <- mutate(x, Val=ifelse(Var=="pr", round(Val), round(Val, 1)))
        } else {
          x <- mutate(x, Val=ifelse(Var=="pr", round(Val/25.4, 3), round((9/5)*Val + 32, 1)))
        }
      }
      x
    })
  })
  
  yrs <- reactive({ seq(input$yrs[1], input$yrs[2]) })
  clrby <- reactive({ if(input$clrby=="") NULL else input$clrby })
  colorvec <- reactive({ if(is.null(clrby())) NULL else tolpal(length(unique(d()[[clrby()]]))) })
  preventPlot <- reactive({ is.null(d()) || nrow(d())==0 })
  plotHeight <- reactive({ if(preventPlot()) 0 else 300 })
  plotInteraction <- reactive({ interact(names(d())) })
  
  primeAxis <- reactive({
    v <- names(variables)[match(input$variable, variables)]
    if(metric()){
      z <- if(v=="Precipitation") paste(v,"(mm)") else bquote(.(paste(v, "("))~degree~C~")")
    } else {
      z <- if(v=="Precipitation") paste(v,"(in)") else bquote(.(paste(v, "("))~degree~F~")")
    }
    z
  })
  
  plot_dist <- reactive({
    input$go_btn
    isolate({
      distPlot(d(), input$variable, primeAxis(), clrby(), colorvec(), alpha_den(), 
        input$fctby, facet_scales(), yrs(), plotInteraction(), "density", preventPlot()) 
    })
  })
  plot_ts <- reactive({
    input$go_btn
    isolate({
      tsPlot(d(), input$variable, primeAxis(), clrby(), colorvec(), alpha_ts(), 
        input$fctby, facet_scales(), preventPlot())
    })
  })
  output$dist_plot <- renderPlot({ plot_dist() }, height=function() plotHeight())
  output$ts_plot <- renderPlot({ plot_ts() }, height=function() plotHeight())
  outputOptions(output, "dist_plot", suspendWhenHidden=FALSE)
  outputOptions(output, "ts_plot", suspendWhenHidden=FALSE)
})
