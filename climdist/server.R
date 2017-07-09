library(rvtable)
library(dtplyr)
library(data.table)
default_mapset <- "AK-CAN"
regions_list_default <- locs[[default_mapset]]
regions_selected_default <- regions_list_default[1]
cru <- "CRU 4.0"
cru.max.yr <- 2015
rcp.min.yr <- 2006
limit.sample <- TRUE # shrink final sampling by a factor of number of RCPs tmes number of GCMs
plottheme <- get_plottheme()
source("plots.R")
app_intro <- list(
  title=app_intro_title, message=app_intro_message, logo=app_intro_logo, 
  toast.args=list(extendedTimeOut=30000, progressBar=TRUE)
)

shinyServer(function(input, output, session) {
  source("observers.R", local=TRUE) # map and region selectInput observers
  source("tour.R", local=TRUE) # introjs tour
  
  mapset_reg_id <- reactive("NAME") #reactive({ mapset_colIDs[match(input$mapset, mapsets)] })
  
  rv <- reactiveValues(d=NULL, current_files=NULL, current_regions=NULL, load_new_files=TRUE, cru=NULL,
          regions=regions_list_default, shp=shp.list[[default_mapset]], go=1, intro_toast_done=FALSE)
  rv_plots <- reactiveValues(ts_x=NULL, ts_y=NULL, ts_brush=NULL, dec_x=NULL, dec_y=NULL, dec_brush=NULL)
  
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
      z <- "Alaska/western Canada"
      z.id <- z
      z.lab <- z
    } else {
      z <- as.character(rv$shp[[mapset_reg_id()]])
      idx <- match(z, locs2[[input$mapset]])
      z.id <- as.character(rv$regions[idx])
      z.lab <- names(rv$regions[idx])
    }
    xyzoom <- if(input$mapset %in% mapsets[1:2]) c(-135, 61, 3) else c(-155, 65, 4)
    n <- 1 + length(z)
    x <- leaflet() %>% addTiles() %>% setView(xyzoom[1], xyzoom[2], xyzoom[3])
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
    } else {
      x <- x %>% addPolygons(data=rv$shp, stroke=TRUE, opacity=1, fillOpacity=0.2, weight=2,
        group="not_selected", layerId=z.id[1], label=z.lab[1],
        highlightOptions=highlightOptions(opacity=1, weight=2, fillOpacity=0.2, 
         bringToFront=FALSE, sendToBack=FALSE))
      progress$inc((1+1)/n, detail=paste("Adding polygon", 1))
    }
    print(proc.time() - ptm)
    x
  })
  
  output$Map <- renderLeaflet(mapSelect())
  outputOptions(output ,"Map", suspendWhenHidden=FALSE)
  
  metric <- reactive({
    rv$go
    isolate( is.null(input$metric) || input$metric=="Metric" )
  })
  i <- reactive({
    cur_gcms <- input$gcms
    if(!is.null(input$cru) && input$cru && input$yrs[1] <= cru.max.yr) cur_gcms <- c(cru, cur_gcms)
    list(rcps=input$rcps, gcms=cur_gcms, 
      reg=regions_selected(), seasons=input$seasons, yrs=input$yrs,
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
    cbind(files, Var5=paste0(files[,1], "_", files[,2], "_", files[,3], "_", files[,4], ".rds"))
  })
  
  noData <- reactive({ any(sapply(i(), is.null)) || is.null(rv$d) })
  
  d_sub <- reactive({
    rv$go
    isolate({
      if(is.null(rv$d)) return()
      dots <- c("RCP", "Model", "Region", "Var", "Season", "Year", "Val", "Prob")
      if(noData()){
        x <- slice(rv$d, 0)
      } else {
        getSubset <- function(x) filter(x, 
          RCP %in% c("Historical", i()[[1]]) & Model %in% i()[[2]] &
            Region %in% i()[[3]] & Season %in% i()[[4]] &
            Year >= i()[[5]][1] & Year <= i()[[5]][2]) %>%
          select_(.dots=dots)
        x <- getSubset(rv$d)
      }
      x %>% droplevels
    })
  })
  
  d <- reactive({
    rv$go
    isolate({
      set.seed(1)
      req(d_sub())
      m <- input$marginalize # determine need for marginalization
      if(!is.null(m) && !"" %in% m){
        m <- sort(m)
        m.lev <- map(m, ~levels(d_sub()[[.x]])) %>% 
          map(~.x[!.x %in% c("Historical", cru)])
        m <- m[which(map_lgl(m.lev, ~length(.x) > 1))]
        if(!length(m)) m <- NULL
      }
      merge_vars <- !is.null(m) && !"" %in% m
      composite <- "Composite GCM"
      lev.rcps <- NULL
      if(input$yrs[1] < rcp.min.yr || (cru %in% i()[[2]] && input$yrs[1] <= cru.max.yr))
        lev.rcps <- "Historical"
      if(input$yrs[2] >= rcp.min.yr) lev.rcps <- c(lev.rcps, "Projected")
      
      d.args <- if(input$variable=="pr") list(n=200, adjust=0.1, from=0) else list(n=200, adjust=0.1)
      s.args <- list(n=100) # marginalize steps
      n.samples <- 100 # final sampling
      x <- d_sub()
      if(merge_vars & cru %in% i()[[2]]){ # split CRU from GCMs
        lev.models <- if("Model" %in% m) c(cru, composite) else i()[[2]]
        x.cru <- filter(x, Model==cru) %>% mutate(Model=factor(Model, levels=lev.models)) %>% 
          split(.$Year) %>% map(~rvtable(.x))
        x <- filter(x, Model!=cru)
      }
      if(!merge_vars){
        n.factor <- if(limit.sample) samplesize_factor(x, cru) else 1
      } else if(!cru %in% i()[[2]]){
        n.factor <- 1
      }
      x <- x %>% split(.$Year) %>% map(~rvtable(.x))
      n.steps <- length(x)
      step <- 0
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      if(merge_vars){ # marginalize (excludes CRU and historical GCM data)
        msg <- "Integrating variables..."
        progress$set(0, message=msg, detail=NULL)
        n.steps.marginal <- if(length(m)) n.steps*length(m) else n.steps
        for(i in seq_along(m)){
          for(j in seq_along(x)){
            step <- step + 1
            detail <- paste0("Marginalizing over ", m[i], "s: ", round(100*step/n.steps.marginal), "%")
            if(step %% 5 == 0 || step==n.steps.marginal) progress$set(step/n.steps.marginal, msg, detail)
            x[[j]] <- marginalize(x[[j]], m[i], density.args=d.args, sample.args=s.args)
          }
        }
      }
      if(merge_vars & cru %in% i()[[2]]){ # update factor levels (with CRU data)
        x.cru <- bind_rows(x.cru)
        x <- bind_rows(x) %>% ungroup()
        if("Model" %in% m) x <- mutate(x, Model=factor(lev.models[-1], levels=lev.models))
        if("RCP" %in% m){
          if(length(lev.rcps)==1) lev.rcps <- rep(lev.rcps, 2) # historical always present
          x.cru <- mutate(x.cru, RCP=factor(as.character(RCP), levels=unique(lev.rcps)))
          x <- mutate(x, RCP=factor(ifelse(Year < rcp.min.yr, lev.rcps[1], lev.rcps[2]), unique(lev.rcps)))
        }
        n.factor <- if(limit.sample) samplesize_factor(x, cru) else 1
        x <- bind_rows(x.cru, x) %>% split(.$Year) %>% map(~rvtable(.x))
      }
      step <- 0
      msg <- "Sampling distributions..."
      progress$set(0, message=msg, detail=NULL) # adjust sample based on number of RCPs and GCMs
      for(j in seq_along(x)){ # sample distributions
        step <- step + 1
        if(step %% 5 == 0 || step==n.steps) 
          progress$set(step/n.steps, message=msg, detail=paste0(round(100*step/n.steps), "%"))
        x[[j]] <- sample_rvtable(x[[j]], n=max(10, round(n.samples/n.factor)))
      }
      x <- bind_rows(x) %>% ungroup()
      if(merge_vars & !cru %in% i()[[2]]){ # update factor levels (no CRU data)
        if("Model" %in% m) x <- mutate(x, Model=factor(composite, levels=composite))
        if("RCP" %in% m)
          if(length(lev.rcps)==1) lev.rcps <- rep(lev.rcps, 2) # projected always present
          x <- mutate(x, RCP=factor(ifelse(Year < rcp.min.yr, lev.rcps[1], lev.rcps[2]), unique(lev.rcps)))
      }
      if(nrow(x) > 0){ # unit conversion
        if(metric()){
          x <- mutate(x, Val=ifelse(Var=="pr", round(Val), round(Val, 1)))
        } else {
          x <- mutate(x, Val=ifelse(Var=="pr", round(Val/25.4, 3), round((9/5)*Val + 32, 1)))
        }
        if(input$variable=="pr") x$Val[x$Val < 0] <- 0
        x <- mutate(x, Decade=factor( # Add decade factor column
          paste0(Year - Year %% 10, "s"), levels=paste0(unique(Year - Year %% 10), "s")))
      }
      x
    })
  })
  
  yrs <- reactive({ seq(input$yrs[1], input$yrs[2]) })
  clrby <- reactive({ if(is.null(input$clrby) || input$clrby=="") NULL else input$clrby })
  fctby <- reactive({ if(is.null(input$fctby) || input$fctby=="") NULL else input$fctby })
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
  
  d_ts_brushed <- reactive({
    d()
    x <- rv_plots$ts_x
    b <- input$ts_plot_brush
    isolate({
      if(is.null(b) & is.null(x)){
        y <- d()
      } else if(is.null(b) & !is.null(x)){
        y <- filter(d(), Year >= x[1] & Year <= x[2])
      } else y <- brushedPoints(d(), b)
      if(nrow(y)==0) y <- d()
      y
    })
  })
  
  d_dec_brushed <- reactive({
    d()
    x <- rv_plots$dec_x
    b <- input$dec_plot_brush
    isolate({
      if(is.null(b) & is.null(x)){
        y <- d()
      } else {
        dec <- d()$Decade
        lev <- levels(dec)
        intlev <- if(is.null(b) & !is.null(x)) round(x[1]):round(x[2]) else round(b$xmin):round(b$xmax)
        y <- slice(d(), dec %in% lev[intlev])
      }
      y
    })
  })
  
  plot_dist <- reactive({
    d_ts_brushed()
    input$plot_btn
    isolate({
      distPlot(d_ts_brushed(), primeAxis(), clrby(), colorvec(), alpha_den(), 
        fctby(), facet_scales(), "density", preventPlot(), plottheme) 
    })
  })
  plot_ts <- reactive({
    d()
    input$plot_btn
    rv_plots$ts_x
    isolate({
      tsPlot(d_ts_brushed(), primeAxis(), clrby(), colorvec(), alpha_ts(), 
        fctby(), facet_scales(), input$show_annual_means, input$show_annual_obs, preventPlot(), plottheme)
    })
  })
  plot_dec <- reactive({
    d()
    input$plot_btn
    rv_plots$dec_x
    isolate({
      decPlot(d_dec_brushed(), primeAxis(), clrby(), colorvec(), alpha_dec(), 
        fctby(), facet_scales(), input$bptype, limit.sample, preventPlot(), plottheme)
    })
  })
  output$dist_plot <- renderPlot({ plot_dist() }, height=function() plotHeight())
  output$ts_plot <- renderPlot({ plot_ts() }, height=function() plotHeight())
  output$dec_plot <- renderPlot({ plot_dec() }, height=function() plotHeight())
  
  sbArgs <- reactive({
    rnd <- if(d()$Var[1] %in% c("tas", "tasmin", "tasmax")) 1 else 0
    s.t <- 150
    s.v <- 75
    h <- 110
    w <- 90
    # if(!is.null(clrby())){
    #   n <- min(nlevels(d()[[clrby()]]), 2)
    #   s.t <- round(s.t / n)
    #   s.v <- round(s.v / n)
    #   h <- round(h / n)
    #   w <- round(w / n)
    # }
    list(rnd=rnd, h=h, w=w, s.t=s.t, s.v=s.v)
  })
  output$statBoxes1 <- renderUI({
    input$plot_btn
    x <- d_ts_brushed()
    isolate({
      stat_boxes_group(x, clrby(), rnd=sbArgs()$rnd, style="valueBox", height=paste0(sbArgs()$h, "px"), 
        width.icon=paste0(sbArgs()$w, "px"), text.size=sbArgs()$s.t, value.size=sbArgs()$s.v, prevent=preventPlot())
    })
  })
  
  output$statBoxes2 <- renderUI({
    input$plot_btn
    x <- d_dec_brushed()
    isolate({
      stat_boxes_group(x, clrby(), rnd=sbArgs()$rnd, style="valueBox", type="decadal", height=paste0(sbArgs()$h, "px"), 
        width.icon=paste0(sbArgs()$w, "px"), text.size=sbArgs()$s.t, value.size=sbArgs()$s.v, prevent=preventPlot())
    })
  })
  
  #outputOptions(output, "dist_plot", suspendWhenHidden=FALSE)
  outputOptions(output, "ts_plot", suspendWhenHidden=FALSE)
  outputOptions(output, "dec_plot", suspendWhenHidden=FALSE)
  outputOptions(output, "statBoxes1", suspendWhenHidden=FALSE)
  outputOptions(output, "statBoxes2", suspendWhenHidden=FALSE)
  
  output$dataLoadedSidebar <- renderUI({
    if(noData()) return()
    tagList(
      hr(style="margin: 12px;"),
      actionButton("help", "Take tour", style=action_btn_style, icon=icon("question-circle")),
      #bookmarkButton(style=action_btn_style)
      actionButton("fake", "Bookmark", style=action_btn_style, icon=icon("link")), # placeholder
      bsTooltip("fake", "Note: Server-side bookmarking not yet available.", placement="right", options=list(container="body")),
      hr(style="margin: 12px;"),
      h5(strong("Download current data set"), style="margin: 0px 12px 0px 12px;"),
      radioButtons("filetype", "File type:", c("csv", "json", "rds"), "rds", inline=T),
      downloadButton('downloadData', 'Download', style=action_btn_style),
      bsTooltip("filetype", "The csv and json file types are universal. The rds file type is familiar, specific and convenient to R users and results in a much smaller file size. If your data selections result in a large data set, rds is highly recommended. A 1.5 MB rds file is equivalent to ~100 MB json file for example.",
                placement="right", options=list(container='body')),
      bsTooltip("downloadData", "Download the currently loaded data set of climate distributions. Reprodicible sampling is performed on the probability densities and output in table form.",
                placement="right", options=list(container='body'))
    )
  })
  
  output$downloadData <- downloadHandler(
    filename=function(){ paste0("regional_climate_samples.", input$filetype) },
    content=function(file){
      type <- input$filetype
      if(type=="csv") write.csv(d(), file, row.names=FALSE, quote=FALSE)
      if(type=="json"){ sink(file=file); cat(jsonlite::toJSON(d())); sink() }
      if(type=="rds") saveRDS(d(), file)
    }
  )
})
