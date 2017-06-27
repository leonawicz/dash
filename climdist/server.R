library(rvtable)
default_mapset <- "AK-CAN"
regions_list_default <- locs[[default_mapset]]
regions_selected_default <- regions_list_default[1]
cru.max.yr <- 2015
rcp.min.yr <- 2006

shinyServer(function(input, output, session) {
  
  source("observers.R", local=TRUE) # map and region selectInput observers
  source("tour.R", local=TRUE) # introjs tour
  
  mapset_reg_id <- reactive("NAME") #reactive({ mapset_colIDs[match(input$mapset, mapsets)] })
  
  rv <- reactiveValues(d=NULL, current_files=NULL, current_regions=NULL, load_new_files=TRUE, cru=NULL,
          regions=regions_list_default, shp=shp.list[[default_mapset]])

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
  
  output$ColorBy <- renderUI({
    x <- c("", "RCP", "Model", "Season", "Region")
    y <- input$marginalize
    x <- if(is.null(y)) x else setdiff(x, y)
    selectInput("clrby", "Color by", choices=x, selected="", width="100%")
  })
  output$FacetBy <- renderUI({
    x <- c("", "RCP", "Model", "Season", "Region")
    y <- input$marginalize
    x <- if(is.null(y)) x else setdiff(x, y)
    selectInput("fctby", "Facet by", choices=x, selected="", width="100%")
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
    input$go_btn
    isolate( is.null(input$metric) || input$metric=="Metric" )
  })
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
    cbind(files, Var5=paste0(files[,1], "_", files[,2], "_", files[,3], "_", files[,4], ".rds"))
  })
  
  noData <- reactive({ any(sapply(i(), is.null)) || is.null(rv$d) })
  
  d_sub <- reactive({
    input$go_btn
    isolate({
      if(is.null(rv$d)) return()
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
    })
  })
  
  d <- reactive({
    input$go_btn
    isolate({
      set.seed(1)
      req(d_sub())
      m <- input$marginalize
      if(!is.null(m) && !"" %in% m){
        m <- sort(m)
        m.lev <- map(m, ~levels(d_sub()[[.x]])) %>% 
          map(~.x[!.x %in% c("Historical", "CRU 4.0")])
        m <- m[which(map_lgl(m.lev, ~length(.x) > 1))]
        if(!length(m)) m <- NULL
      }
      d.args <- if(input$variable=="pr") list(n=200, adjust=0.1, from=0) else list(n=200, adjust=0.1)
      s.args <- list(n=100)
      x <- d_sub() %>% split(.$Year) %>% map(~rvtable(.x))
      n.steps <- length(x)
      step <- 0
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      #Sys.sleep(1)
      if(!is.null(m) && !"" %in% m){
        msg <- "Integrating variables..."
        progress$set(0, message=msg, detail=NULL)
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
        x[[j]] <- sample_rvtable(x[[j]], n=s.args$n)
      }
      x <- bind_rows(x)
      if(nrow(x) > 0){
        if(metric()){
          x <- mutate(x, Val=ifelse(Var=="pr", round(Val), round(Val, 1)))
        } else {
          x <- mutate(x, Val=ifelse(Var=="pr", round(Val/25.4, 3), round((9/5)*Val + 32, 1)))
        }
        if(input$variable=="pr") x$Val[x$Val < 0] <- 0
        x <- mutate(x, Decade=factor(
          paste0(Year - Year %% 10, "s"), levels=paste0(unique(Year - Year %% 10), "s")))
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
    d()
    input$plot_btn
    isolate({
      distPlot(d(), primeAxis(), clrby(), colorvec(), alpha_den(), 
        input$fctby, facet_scales(), yrs(), "density", preventPlot()) 
    })
  })
  plot_ts <- reactive({
    d()
    input$plot_btn
    isolate({
      tsPlot(d(), primeAxis(), clrby(), colorvec(), alpha_ts(), 
        input$fctby, facet_scales(), input$show_points, preventPlot())
    })
  })
  plot_dec <- reactive({
    d()
    input$plot_btn
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
  
  kilo_mega <- function(x){
    if(abs(x) < 1e4) paste0(x) else 
      if(abs(x) < 1e5) paste0(round(x/1000, 1), "K") else 
        if(abs(x) < 1e6) paste0(round(x/1000), "K") else
          paste0(round(x/1e6, 2), "M") 
  }
  
  output$statBoxes1 <- renderUI({
    x <- d()
    dec <- as.character(sort(unique(x$Decade)))
    if(length(dec) > 1) dec <- paste(dec[1], dec[length(dec)], sep=" - ")
    if(preventPlot() || nrow(x)==0) return()
    rnd <- if(x$Var[1]=="pr") 0 else 1
    x <- ungroup(x) %>% summarise_(.dots=list(
      Mean_=paste0("mean(Val)"),
      Min_=paste0("min(Val)"),
      Max_=paste0("max(Val)"),
      Median_=paste0("stats::median(Val)"),
      Pct25_=paste0("stats::quantile(Val, prob=0.25)"),
      Pct75_=paste0("stats::quantile(Val, prob=0.75)"),
      SD_=paste0("stats::sd(Val)")
    )) %>% round(rnd) %>% unlist %>% map_chr(~kilo_mega(.x))
    
    clrs <- c("light-blue", "light-blue", "blue", "light-blue", "blue", "blue")
    statval <- c(x[1:4], paste(x[5], "-", x[6]), x[7])
    statlab <- list(
      c("Mean", dec), c("Min", dec), c("Max", dec), c("Median", dec), c("IQR", dec), c("Std Dev", dec)
    )
    val <- map2(statval, c(rep(75, 4), 75, 75), ~pTextSize(.x, .y))
    text <- map2(statlab, rep(150, 6), ~pTextSize(.x, .y, margin=0))
    y <- list(
      mean=valueBox(val[[1]], text[[1]], icon=icon(list(src="stat_icon_normal_mean_white.png", width="90px"), lib="local"), color=clrs[1], width=NULL),
      min=valueBox(val[[2]], text[[2]], icon=icon(list(src="stat_icon_normal_min_white.png", width="90px"), lib="local"), color=clrs[2], width=NULL),
      max=valueBox(val[[3]], text[[3]], icon=icon(list(src="stat_icon_normal_max_white.png", width="90px"), lib="local"), color=clrs[3], width=NULL),
      med=valueBox(val[[4]], text[[4]], icon=icon(list(src="stat_icon_normal_median_white.png", width="90px"), lib="local"), color=clrs[4], width=NULL),
      iqr=valueBox(val[[5]], text[[5]], icon=icon(list(src="stat_icon_boxplot_iqr_white.png", width="90px"), lib="local"), color=clrs[5], width=NULL),
      sd=valueBox(val[[6]], text[[6]], icon=icon(list(src="stat_icon_normal_sd_white.png", width="90px"), lib="local"), color=clrs[6], width=NULL)
    )
    
    tagList(
      h4("Aggregate period statistics"),
      fluidRow(
        tags$head(tags$style(HTML(".small-box {height: 110px}"))),
        column(2, y$mean), column(2, y$sd), column(2, y$med), column(2, y$iqr), column(2, y$min), column(2, y$max)
      )
    )
  })
  
  output$statBoxes2 <- renderUI({
    x <- d()
    if(preventPlot() || nrow(x)==0) return()
    dots <- paste0("mean(Val)")
    pr <- x$Var[1]=="pr"
    rnd <- if(pr) 0 else 1
    x <- group_by(x, Decade) %>% summarise_(.dots=list(Decadal_mean=dots)) %>%
      rename(Val=Decadal_mean)
    v <- "Val"
    idx.mn <- which.min(x[[v]])
    idx.mx <- which.max(x[[v]])
    idx.dn <- if(nrow(x)==1) NA else seq(which.min(diff(x[[v]])), length.out=2)
    idx.up <- if(nrow(x)==1) NA else seq(which.max(diff(x[[v]])), length.out=2)
    tot <- tail(x[[v]], 1) - x[[v]][1]
    tot2 <- if(pr){
      ifelse(tot < 1 & tot > 0, 1, ifelse(tot < 0 & tot > -1, -1, round(tot)))
    } else round(tot, rnd)
    x <<- x
    pct <- if(!pr) NA else paste0(round(100*(tail(x[[v]], 1) / x[[v]][1] - 1)), "%")
    
    clrs <- c("light-blue", "blue", "light-blue", "blue", "light-blue", "blue")
    statval <- list(
      mn=kilo_mega(round(x[[v]][idx.mn], rnd)),
      mx=kilo_mega(round(x[[v]][idx.mx], rnd)),
      dn=if(is.na(idx.dn[1])) NA else kilo_mega(round(diff(x[[v]])[idx.dn[1]], rnd)),
      up=if(is.na(idx.up[1])) NA else kilo_mega(round(diff(x[[v]])[idx.up[1]], rnd)),
      totdif=kilo_mega(tot2),
      totpct=pct
    )
    
    src.dnup <- c("stat_icon_bar_deltaNeg_white.png", "stat_icon_bar_deltaPos_white.png")
    if(!is.na(statval$dn[1]) && statval$dn > 0) src.dnup[1] <- src.dnup[2]
    if(!is.na(statval$up[1]) && statval$up < 0) src.dnup[2] <- src.dnup[1]
    if(tot < 0){
      src.totals <- c("stat_icon_ts_deltaDec_white.png", "stat_icon_ts_deltaPctDec_white.png")
    } else {
      src.totals <- c("stat_icon_ts_deltaInc_white.png", "stat_icon_ts_deltaPctInc_white.png")
    }
    dec <- if(nrow(x)==1) paste(x$Decade[1]) else paste(x$Decade[c(1, nrow(x))], collapse=" - ")
    
    statlab <- list(
      c("Min", paste(x$Decade[idx.mn])),
      c("Max", paste(x$Decade[idx.mx])),
      c("Min growth", paste(x$Decade[idx.dn], collapse=" - ")),
      c("Max growth", paste(x$Decade[idx.up], collapse=" - ")),
      c("Total change", dec),
      c("% change", dec)
    )
    val <- map2(statval, 75, ~pTextSize(.x, .y))
    text <- map2(statlab, rep(150, 6), ~pTextSize(.x, .y, margin=0))
    y <- list(
      mn=valueBox(val[[1]], text[[1]], icon=icon(list(src="stat_icon_normal_min_white.png", width="90px"), lib="local"), color=clrs[1], width=NULL),
      mx=valueBox(val[[2]], text[[2]], icon=icon(list(src="stat_icon_normal_max_white.png", width="90px"), lib="local"), color=clrs[2], width=NULL),
      dn=valueBox(val[[3]], text[[3]], icon=icon(list(src=src.dnup[1], width="90px"), lib="local"), color=clrs[3], width=NULL),
      up=valueBox(val[[4]], text[[4]], icon=icon(list(src=src.dnup[2], width="90px"), lib="local"), color=clrs[4], width=NULL),
      totdif=valueBox(val[[5]], text[[5]], icon=icon(list(src=src.totals[1], width="90px"), lib="local"), color=clrs[5], width=NULL),
      totpct=valueBox(val[[6]], text[[6]], icon=icon(list(src=src.totals[2], width="90px"), lib="local"), color=clrs[6], width=NULL)
    )
    
    tagList(
      h4("Decadal averages: change over time"),
      fluidRow(
        tags$head(tags$style(HTML(".small-box {height: 110px}"))),
        column(2, y$totdif), column(2, y$totpct), column(2, y$dn), column(2, y$up), column(2, y$mn), column(2, y$mx)
      )
    )
  })
})
