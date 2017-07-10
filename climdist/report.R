uni <- reactive({
  v <- input$variable
  if(metric()){
    x <- if(v=="pr") "mm" else "degrees Celcius"
  } else {
    x <- if(v=="pr") "in" else "degrees fahrenheit"
  }
  x
})

output$report <- downloadHandler(
  filename="snap_downscaled_climate_custom_report.pdf",
  content=function(file){
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite=TRUE)
    params <- list(
      years=yrs(),
      n=length(yrs()),
      variable=input$variable,
      x=d(),
      units=uni(),
      regions=i()[[3]],
      regions.names=i()[[6]],
      plot_ts=plot_ts(),
      plot_den=plot_dist(),
      plot_dec=plot_dec(),
      cru=cru
    )
    rmarkdown::render(tempReport, output_file=file, params=params, envir=new.env(parent=globalenv())
    )
  }
)
