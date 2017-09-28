library(rvtable)
plottheme <- get_plottheme()
source("plots.R")
app_intro <- list(
  title = app_intro_title, message = app_intro_message, logo = app_intro_logo, 
  toast_args = list(timeOut = 0, extendedTimeOut = 0)
)

shinyServer(function(input, output, session) {

  # reactive values
  rv <- reactiveValues(d=NULL, d_ts_b=NULL, d_dec_b=NULL, current_files=NULL, current_regions=NULL, 
                       load_new_files=TRUE, cru=NULL, regions=regions_list_default, 
                       shp=shp.list[[default_mapset]], go=1, intro_toast_done=FALSE)
  rv_plots <- reactiveValues(ts_x=NULL, ts_y=NULL, ts_brush=NULL, dec_x=NULL, dec_y=NULL, dec_brush=NULL)
  
  source("observers.R", local=TRUE) # source observers for maps, region inputs, interactive plots and data sets
  source("tour.R", local=TRUE) # introjs tour
  
  # map/region-related objects
  mapset_reg_id <- reactive("NAME") #reactive({ mapset_colIDs[match(input$mapset, mapsets)] })
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
  
  # Initialize map and add polygons
  mapSelect <- reactive({
    xyz <- split(c(rep(c(-135, 61, 3), 2), rep(c(-155, 65, 4), 8)), rep(1:10, each=3))
    names(xyz) <- mapsets
    build_mapset(rv$shp, rv$regions, mapset_reg_id(), input$mapset, mapsets, default_mapset, "Alaska/western Canada", locs2, xyz)
  })
  output$Map <- renderLeaflet(mapSelect())
  outputOptions(output ,"Map", suspendWhenHidden=FALSE)
  
  # reactive inputs
  alpha_den <- reactive({ if(is.null(input$alpha_den)) 1 else input$alpha_den })
  alpha_ts <- reactive({ if(is.null(input$alpha_ts)) 0.1 else input$alpha_ts })
  alpha_dec <- reactive({ if(is.null(input$alpha_dec)) 0.1 else input$alpha_dec })
  facet_scales <- reactive({ if(is.null(input$facet_scales)) "fixed" else input$facet_scales })
  metric <- reactive({ is.null(input$metric) || input$metric=="Metric" })
  cru_selected <- reactive({ cru %in% input$gcms })
  i <- reactive({
    list(rcps=input$rcps, gcms=input$gcms, 
      reg=regions_selected(), seasons=input$seasons, yrs=input$yrs,
      reg.names=names(rv$regions)[match(input$regions, rv$regions)]) 
  })
  
  # file loading
  files <- reactive({
    cruId <- "ts40"
    rcps_string <- tolower(gsub("[ \\.]", "", input$rcps))
    models <- input$gcms
    if(cru_selected() && input$yrs[1] <= cru.max.yr) models <- c(cruId, models[models != cru])
    files <- expand.grid(input$variable, rcps_string, models[models != cruId], input$seasons, stringsAsFactors=FALSE)
    if(cru_selected() & input$yrs[1] >= rcp.min.yr){
      files <- rbind(expand.grid(input$variable, "historical", cruId, input$seasons, stringsAsFactors=FALSE), files)
    } else if(input$yrs[1] < rcp.min.yr){ 
      files <- rbind(expand.grid(input$variable, "historical", models, input$seasons, stringsAsFactors=FALSE), files)
    }
    files <- files[!(files[, 3]==cruId & files[, 1] %in% c("tasmin", "tasmax")),] # temporarily missing files
    cbind(files, Var5=paste0(files[,1], "_", files[,2], "_", files[,3], "_", files[,4], ".rds"))
  })
  
  # data frames
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
  
  d_prepped <- reactive({
    rv$go
    isolate({
      req(d_sub())
      dist_data(d_sub(), input$variable, input$marginalize, seed=1, NULL, input$yrs, rcp.min.yr, cru.max.yr, i()[[2]], cru)
    })
  })
  
  d <- reactive({
    d_prepped()
    metric()
    isolate(clim_convert_round(d_prepped(), metric()))
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
  
  # plot-related reactive objects
  varName <- reactive({ names(variables)[match(input$variable, variables)] })
  yrs <- reactive({ seq(input$yrs[1], input$yrs[2]) })
  clrby_annual <- reactive({ if(is.null(input$clrby_annual) || input$clrby_annual=="") NULL else input$clrby_annual })
  fctby_annual <- reactive({ if(is.null(input$fctby_annual) || input$fctby_annual=="") NULL else input$fctby_annual })
  clrby_decadal <- reactive({ if(is.null(input$clrby_decadal) || input$clrby_decadal=="") NULL else input$clrby_decadal })
  fctby_decadal <- reactive({ if(is.null(input$fctby_decadal) || input$fctby_decadal=="") NULL else input$fctby_decadal })
  colorvec_annual <- reactive({ if(is.null(clrby_annual())) NULL else tolpal(length(unique(d()[[clrby_annual()]]))) })
  colorvec_decadal <- reactive({ if(is.null(clrby_decadal())) NULL else tolpal(length(unique(d()[[clrby_decadal()]]))) })
  preventPlot <- reactive({ is.null(d()) || nrow(d())==0 || any(is.na(d()$Val)) })
  plotHeight <- reactive({ if(preventPlot()) 0 else 400 })
  
  primeAxis <- reactive({
    v <- varName()
    if(metric()){
      z <- if(v=="Precipitation") paste(v,"(mm)") else bquote(.(paste(v, "("))~degree~C~")")
    } else {
      z <- if(v=="Precipitation") paste(v,"(in)") else bquote(.(paste(v, "("))~degree~F~")")
    }
    z
  })
  
  # reactive outputs: plots
  plot_dist <- reactive({
    rv$d_ts_b; rv_plots$ts_x; metric(); clrby_annual(); fctby_annual(); alpha_den(); facet_scales()
    isolate({
      distPlot(d_ts_brushed(), primeAxis(), clrby_annual(), colorvec_annual(), alpha_den(), 
        fctby_annual(), facet_scales(), "density", preventPlot(), plottheme) 
    })
  })
  plot_ts <- reactive({
    rv$d_ts_b; rv_plots$ts_x; metric(); clrby_annual(); fctby_annual(); alpha_ts()
    input$show_annual; input$fit_models; input$eq_pos; facet_scales()
    isolate({
      tsPlot(rv$d_ts_b, d(), varName(), primeAxis(), clrby_annual(), colorvec_annual(), alpha_ts(), 
        fctby_annual(), facet_scales(), input$show_annual, input$fit_models, input$eq_pos,
        preventPlot(), plottheme, rv_plots$ts_x)
    })
  })
  
  plot_dec <- reactive({
    rv$d_dec_b; rv_plots$dec_x; metric(); clrby_decadal(); fctby_decadal(); alpha_dec()
    input$bptype; facet_scales()
    isolate({
      decPlot(rv$d_dec_b, d(), primeAxis(), clrby_decadal(), colorvec_decadal(), alpha_dec(), 
        fctby_decadal(), facet_scales(), input$bptype, limit.sample, preventPlot(), plottheme, rv_plots$dec_x)
    })
  })
  output$dist_plot <- renderPlot({ plot_dist() }, height=function() plotHeight())
  output$ts_plot <- renderPlot({ plot_ts() }, height=function() plotHeight())
  output$dec_plot <- renderPlot({ plot_dec() }, height=function() plotHeight())
  
  # Observe plot for hiding launch overlay (must appear in code after plot_ts() is defined)
  observe(if(!is.null(plot_ts())) shinyjs::hide("fade-wrapper", anim=TRUE, animType="fade", time=1))
  
  # reactive outputs: stat boxes
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
    list(rnd=rnd, h=paste0(h, "px"), w=paste0(w, "px"), s.t=s.t, s.v=s.v)
  })
  
  lapply(1:2, function(i, x, p=c("annual", "decadal")){
    output[[paste0("statBoxes", i)]] <- renderUI({
      x <- if(i==1) d_ts_brushed() else d_dec_brushed()
      clr <- list(clrby_annual(), clrby_decadal())[[i]]
      metric()
      isolate({ # NOTE: if decide to adjust height by colorby()* reactives, more sbArgs()* reactives required
        stat_boxes_group(x, clr, rnd=sbArgs()$rnd, type=p[i], height=sbArgs()$h, 
          width.icon=sbArgs()$w, text.size=sbArgs()$s.t, value.size=sbArgs()$s.v, prevent=preventPlot())
      })
    })
  })
  
  slr <- reactive({
    x <- d_ts_brushed()
    if(!is.null(clrby_annual())){
      lev <- levels(x[[clrby_annual()]])
      x <- x %>% split(.[[clrby_annual()]])
      idx <- which(purrr::map_lgl(x, ~nrow(.x) > 0))
      x <- x[idx]
      lev <- lev[lev %in% names(x)]
      x <- x[match(lev, names(x))]
    } else {
      x <- list(x)
      names(x) <- "Selected data"
    }
    x <- x %>% map(~group_by(.x, Year) %>% summarise(Val=mean(Val))) %>% map(~lm(.x$Val ~ .x$Year))
    map(x, ~list(coef=round(summary(.x)$coefficients[, 1], 2), 
                 pval=round(summary(.x)$coefficients[, 4], 3), 
                 r2=round(summary(.x)$r.squared, 3)))
  })
  
  output$parsBoxes <- renderUI({
    x <- slr()
    id <- names(x)
    clrby <- clrby_annual()
    if(is.null(clrby)){
      clrs <- list(c("light-blue", "blue", "blue", "light-blue", "light-blue"))
    } else {
      clrs <- color_indexer(d_ts_brushed(), clrby)
      clrs <- transpose(map(1:5, ~substring(clrs, 2))) %>% map(~unlist(.x))
    }
    lmStatPanel <- function(x, id, clrs, value.size = 75, text.size = 150, drop_int = TRUE){
      text <- c("Intercept", "Slope", "p-value", "p-value", "R-squared")
      if(drop_int) text <- text[c(2, 4, 5)]
      text <- purrr::map(text, ~pTextSize(.x, text.size, margin=0))
      val <- purrr::map(unlist(x), ~pTextSize(.x, value.size))
      if(drop_int) val <- val[c(2, 4, 5)]
      if(drop_int){
        tabPanel(id,
          fluidRow(column(12, 
            valueBox(val[1], text[[1]],
                     icon=apputils::icon(list(src=statIcon("b1"), width="90px"), lib="local"), 
                     color=clrs[1], width=NULL),
            valueBox(val[2], text[[2]], 
                     icon=apputils::icon(list(src=statIcon("pvalue"), width="90px"), lib="local"), 
                     color=clrs[2], width=NULL),
            valueBox(val[3], text[[3]], 
                     icon=apputils::icon(list(src=statIcon("r2"), width="90px"), lib="local"), 
                     color=clrs[1], width=NULL)
          ))
        )
      } else {
        tabPanel(id,
          fluidRow(
            column(6, valueBox(val[1], text[[1]], 
                               icon=apputils::icon(list(src=statIcon("b0"), width="90px"), lib="local"), 
                               color=clrs[1], width=NULL)),
            column(6, valueBox(val[3], text[[3]], 
                               icon=apputils::icon(list(src=statIcon("pvalue"), width="90px"), lib="local"), 
                               color=clrs[3], width=NULL))
          ),
          fluidRow(
            column(6, valueBox(val[2], text[[2]], 
                               icon=apputils::icon(list(src=statIcon("b1"), width="90px"), lib="local"), 
                               color=clrs[2], width=NULL)),
            column(6, valueBox(val[4], text[[4]], 
                               icon=apputils::icon(list(src=statIcon("pvalue"), width="90px"), lib="local"), 
                               color=clrs[4], width=NULL))
          ),
          fluidRow(
            column(6, valueBox(val[5], text[[5]], 
                               icon=apputils::icon(list(src=statIcon("r2"), width="90px"), lib="local"), 
                               color=clrs[5], width=NULL))
          )
        )
      }
    }
    tps <- map(rev(seq_along(x)), ~lmStatPanel(x[[.x]], id[.x], clrs[[.x]], drop_int = TRUE))
    do.call(tabBox, c(tps, list(id="model_summary", selected=id[1], title=NULL, width=12, side="right")))
  })
  
  lapply(c("dist_plot", "ts_plot", "dec_plot", "statBoxes1", "statBoxes2", "parsBoxes"),
         function(x, o) outputOptions(o, x, suspendWhenHidden=FALSE), o=output)
  
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
            radioButtons("report_format", "File type:", c("pdf", "html"), "pdf", inline=T),
            tipify(downloadButton("report", "Fact sheet", style=action_btn_style), tip, placement="right", options=list(container="body")),
            bsTooltip("report_format", "Reports are available as pdf or html documents.", placement="right", options=list(container="body"))
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
      report
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
      content=function(file){ cairo_pdf(file, 12, 7); print(plots[[i]]); dev.off() }
    ) }, 
    ids=c("annual", "decadal", "period"), 
    plots=list(plot_ts(), plot_dec(), plot_dist())
  )
  
})
