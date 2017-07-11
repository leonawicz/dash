library(rvtable)
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
    xyz <- split(c(rep(c(-135, 61, 3), 2), rep(c(-155, 65, 4), 8)), rep(1:10, each=3))
    names(xyz) <- mapsets
    build_mapset(rv$shp, rv$regions, mapset_reg_id(), input$mapset, mapsets, default_mapset, "Alaska/western Canada", locs2, xyz)
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
      req(d_sub())
      dist_data(d_sub(), input$variable, input$marginalize, seed=1, metric(), input$yrs, rcp.min.yr, cru.max.yr, i()[[2]], cru)
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
    isolate(brushed_data(d(), x, b))
  })
  
  d_dec_brushed <- reactive({
    d()
    x <- rv_plots$dec_x
    b <- input$dec_plot_brush
    isolate(brushed_data(d(), x, b, "decadal"))
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
  source("report.R", local=TRUE)
  output$dataLoadedSidebar <- renderUI({
    if(noData()) return()
    mar_ <- "margin: 15px;"
    mar_lr <- "margin: 0px 15px 0px 15px;"
    report <- NULL
    if(length(yrs()) >= 30){
      tip <- "Download a customized fact sheet based on currently selected data and displayed plots. See the FAQ section under the Information tab above for details."
      report <- tagList(
        hr(style=mar_),
        fluidRow(
          column(12,
            h5(strong("Download custom report"), style=mar_lr),
            tipify(downloadButton("report", "Fact sheet", style=action_btn_style), tip, placement="right", options=list(container="body"))
          )
        )
      )
    }
    tagList(
      hr(style=mar_),
      actionButton("help", "Take tour", style=action_btn_style, icon=icon("question-circle")),
      #bookmarkButton(style=action_btn_style)
      actionButton("fake", "Bookmark", style=action_btn_style, icon=icon("link")), # placeholder
      bsTooltip("fake", "Note: Server-side bookmarking not yet available.", placement="right", options=list(container="body")),
      hr(style=mar_),
      h5(strong("Download current data set"), style=mar_lr),
      radioButtons("filetype", "File type:", c("csv", "json", "rds"), "rds", inline=T),
      downloadButton("downloadData", "Data set", style=action_btn_style),
      bsTooltip("filetype", "The csv and json file types are universal. The rds file type is familiar, specific and convenient to R users and results in a much smaller file size. If your data selections result in a large data set, rds is highly recommended. A 1.5 MB rds file is equivalent to ~100 MB json file for example.",
                placement="right", options=list(container="body")),
      bsTooltip("downloadData", "Download the currently loaded data set of climate distributions. Reprodicible sampling is performed on the probability densities and output in table form.",
                placement="right", options=list(container="body")),
      report,
      hr(style=mar_),
      h5(strong("Download current plots"), style=mar_lr),
      map(c("Annual_series", "Decadal_boxplots", "Period_density"), 
          ~downloadButton(paste0("dlPlot_", tolower(.x)), gsub("_", " ", .x), style=action_btn_style))
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
  
  lapply(1:3, function(i, ids, plots){
    output[[paste0("dlPlot_", ids[i])]] <- downloadHandler(
      filename=function() { paste0(ids[i], ".pdf" ) },
      content=function(file){ pdf(file); print(plots[[i]]); dev.off() }
    ) }, 
    ids=c("annual_series", "decadal_boxplots", "period_density"), 
    plots=list(plot_ts(), plot_dec(), plot_dist())
  )
  
})
