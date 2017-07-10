tour.text <- read_md_paragraphs("text_tour.txt")
tour.pos <- c("bottom", "right", "bottom", rep("left", 12), rep("top", 2), "left", rep("top", 2))
tour.element <- c(
  "#controls", "#mapset + .selectize-control", "#Map", "#regions + .selectize-control",
  "#variable + .selectize-control",
  "#rcps + .selectize-control", "#gcms + .selectize-control", "#seasons + .selectize-control", ".js-irs-3", 
  "#marginalize + .selectize-control", "#clrby + .selectize-control", "#fctby + .selectize-control",
  "#go_btn", "#settings_btn", "#plot_btn",
  "#statBoxes1", "#denbox", "#ts_plot", "#statBoxes2", "#dec_plot"
)
steps <- reactive({ data.frame(element=tour.element, intro=tour.text, position=tour.pos) })

# begin tour on button click
observeEvent(input$help, {
  tour.options <- list(steps=steps(), "showProgress"="true", "showStepNumbers"="false")
  tour.events <- list(
    "onchange"=I(paste0(
      tour_changeClass(1:20, "info", "climate"),
      tour_changeClass(1:18, "Decadal", "Annual"),
      tour_changeClass(19:20, "Annual", "Decadal"),
      collapse="\n"))
  )
  introjs(session, options=tour.options, events=tour.events)
})
