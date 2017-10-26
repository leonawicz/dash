tour.text <- read_md_paragraphs("text_tour.txt")
tour.pos <- c(rep("bottom", 13), "top", rep("bottom", 3), rep("top", 2), "bottom", "top")
tour.element <- c(
  "#controls", "#mapset + .selectize-control", "#Map", "#regions + .selectize-control",
  "#variable + .selectize-control",
  "#rcps + .selectize-control", "#gcms + .selectize-control", "#seasons + .selectize-control", ".js-irs-4", 
  "#marginalize + .selectize-control",
  "#go_btn", "#settings_btn",
  "#statBoxes1", "#ts_plot", "#clrby_annual + .selectize-control", "#fctby_annual + .selectize-control", 
  "#dlPlot_annual", "#parsBoxes", "#denbox", "#statBoxes2", "#dec_plot"
)
steps <- reactive({ data.frame(element=tour.element, intro=tour.text, position=tour.pos) })

# begin tour on button click
observeEvent(input$help, {
  tour.options <- list(steps=steps(), "showProgress"="true", "showStepNumbers"="false")
  tour.events <- list(
    "onchange"=I(paste0(
      tour_changeClass(1:20, "info", "climate"),
      tour_changeClass(1:19, "Decadal", "Annual"),
      tour_changeClass(20:21, "Annual", "Decadal"),
      collapse="\n"))
  )
  introjs(session, options=tour.options, events=tour.events)
})
