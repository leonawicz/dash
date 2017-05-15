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
  
  source("observers.R", local=TRUE) # map and region selectInput observers
  source("tour.R", local=TRUE) # introjs tour
  
  mapset_workspace <- reactive({ 
    file.path(dataloc, "shp", paste0(input$mapset, ".rds"))
  })
  
  mapset_reg_id <- reactive({
    mapset_colIDs[match(input$mapset, mapsets)]
  })
  
  rv <- reactiveValues(d=NULL, current_files=NULL, current_regions=NULL, load_new_files=TRUE, cru=NULL,
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
  
  alpha_den <- reactive({ if(is.null(input$alpha_den)) 1 else input$alpha_den })
  alpha_ts <- reactive({ if(is.null(input$alpha_ts)) 0.1 else input$alpha_ts })
  alpha_dec <- reactive({ if(is.null(input$alpha_dec)) 0.1 else input$alpha_dec })
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
  i <- reactive({
    cur_gcms <- input$gcms
    if(!is.null(input$cru) && input$cru) cur_gcms <- c("CRU 4.0", cur_gcms)
    list(rcps=input$rcps, gcms=cur_gcms, 
      reg=input$regions, seasons=input$seasons, yrs=input$yrs,
      reg.names=names(rv$regions)[match(input$regions, rv$regions)]) 
  })
  
  files <- reactive({
    rcps_string <- tolower(gsub("[ \\.]", "", input$rcps))
    models <- if(!is.null(input$cru) && input$cru && input$yrs[1] <= cru.max.yr)
      c("ts40", input$gcms) else input$gcms
    files <- expand.grid(input$variable, rcps_string, input$gcms, input$seasons, stringsAsFactors=FALSE)
    if("ts40" %in% models & input$yrs[1] >= rcp.min.yr){
      files <- rbind(expand.grid(input$variable, "historical", "ts40", input$seasons, stringsAsFactors=FALSE), files)
    } else if(input$yrs[1] < rcp.min.yr){ 
      files <- rbind(expand.grid(input$variable, "historical", models, input$seasons, stringsAsFactors=FALSE), files)
    }
    files <- files[!(files[, 3]=="ts40" & files[, 1] %in% c("tasmin", "tasmax")),] # temporarily missing files
    cbind(files, paste0(files[,1], "_", files[,2], "_", files[,3], "_", files[,4], ".rds"))
  })
  
  noData <- reactive({ any(sapply(i(), is.null)) || is.null(rv$d) })
  
  d_sub <- reactive({
    input$go_btn
    isolate({
      if(is.null(rv$d)) return()
      withProgress({
        dots <- c("RCP", "Model", "Region", "Var", "Season", "Year", "Val", "Prob")
        if(noData()){
          x <- slice(rv$d, 0)
        } else {
          getSubset <- function(x) filter(x, 
            RCP %in% c("Historical", i()[[1]]) & Model %in% c(cru, i()[[2]]) &
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
      x <- d_sub() %>% split(.$Year) %>% map(~rvtable(.x))
      n.steps <- length(x)
      step <- 0
      if(!is.null(m) && !"" %in% m){
        msg <- "Integrating variables..."
        progress$set(0, msg, detail=NULL)
        n.steps.marginal <- if(length(m)) n.steps*length(m) else n.steps
        for(i in seq_along(m)){
          for(j in seq_along(x)){
            step <- step + 1
            detail <- paste0("Marginalizing over ", m[i], "s: ", round(100*step/n.steps.marginal), "%")
            progress$inc(1/n.steps.marginal, msg, detail)
            x[[j]] <- marginalize(x[[j]], m[i], density.args=d.args, sample.args=s.args)
          }
        }
      }
      step <- 0
      msg <- "Sampling distributions..."
      progress$set(0, message=msg, detail=NULL)
      for(j in seq_along(x)){
        step <- step + 1
        progress$inc(1/n.steps, message=msg, detail=paste0(round(100*step/n.steps), "%"))
        x[[j]] <- sample_rvtable(x[[j]], n=10)
      }
      x <- bind_rows(x)
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
  plotHeight <- reactive({ if(preventPlot()) 0 else 400 })
  
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
      distPlot(d(), primeAxis(), clrby(), colorvec(), alpha_den(), 
        input$fctby, facet_scales(), yrs(), "density", preventPlot()) 
    })
  })
  plot_ts <- reactive({
    input$go_btn
    isolate({
      tsPlot(d(), primeAxis(), clrby(), colorvec(), alpha_ts(), 
        input$fctby, facet_scales(), preventPlot())
    })
  })
  plot_dec <- reactive({
    input$go_btn
    isolate({
      decPlot(d(), primeAxis(), clrby(), colorvec(), alpha_dec(), 
             input$fctby, facet_scales(), input$bptype, preventPlot())
    })
  })
  output$dist_plot <- renderPlot({ plot_dist() }, height=function() plotHeight())
  output$ts_plot <- renderPlot({ plot_ts() }, height=function() plotHeight())
  output$dec_plot <- renderPlot({ plot_dec() }, height=function() plotHeight())
  outputOptions(output, "dist_plot", suspendWhenHidden=FALSE)
  outputOptions(output, "ts_plot", suspendWhenHidden=FALSE)
  outputOptions(output, "dec_plot", suspendWhenHidden=FALSE)
})
