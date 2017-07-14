uni <- reactive({
  v <- input$variable
  if(metric()){
    x <- if(v=="pr") "mm" else "degrees Celcius"
  } else {
    x <- if(v=="pr") "in" else "degrees fahrenheit"
  }
  x
})

report_stats <- reactive({ sbg_to_tables(d(), clrby(), sbArgs()$rnd) })
annual_plot_content <- reactive({
  x <- c("Means", "Observations")
  y <- input$show_annual
  if(all(x) %in% y) return("means and observations")
  if(x[1] %in% y) return("means")
  if(x[2] %in% y) return("observations")
  "data"
})

decadal_plot_content <- reactive({
  x <- c("Box plot", "Strip chart", "Overlay")
  y <- input$bptype
  if(y==x[1]) return("box plots")
  if(y==x[2]) return("strip chart")
  "box plots with spatial samples overlay"
})

report_doc_type <- reactive({
  switch(input$report_format, pdf=pdf_document(), html=html_document())
})

output$report <- downloadHandler(
  filename=function() { paste0("snap_downscaled_climate_custom_report.", input$report_format) },
  content=function(file){
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite=TRUE)
    params <- list(
      wd=getwd(),
      years=yrs(),
      n=length(yrs()),
      variable=input$variable,
      x=d(),
      units=uni(),
      regions=i()[[3]],
      regions.names=i()[[6]],
      seasons=input$seasons,
      plot_ts=plot_ts(),
      plot_den=plot_dist(),
      plot_dec=plot_dec(),
      cru=cru,
      clrby=clrby(),
      fctby=fctby(),
      tables=report_stats(),
      anncontent=annual_plot_content(),
      annlm="lm" %in% input$fit_models,
      deccontent=decadal_plot_content()
    )
    rmarkdown::render(tempReport, output_file=file, 
                      output_format=report_doc_type(), 
                      params=params, envir=new.env(parent=globalenv())
    )
  }
)
