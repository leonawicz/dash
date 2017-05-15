faq <- source("faq.R", local=TRUE)[[1]]

function(request){
  dashboardPage(
    dashboardHeader(
      title="Climate Explorer",
      tags$li(class="dropdown",
        tags$a(href="http://snap.uaf.edu", target="_blank",
          tags$img(src="SNAP_acronym_100px.png", width="100%", alt="SNAP"), style="padding: 10px; margin: 0px;"))
      #tags$head(includeScript("ga-nwtapp.js"), includeScript("ga-allapps.js")),
    ),
    dashboardSidebar(
      useToastr(),
      introjsUI(),
      selectInput("mapset", "Change map layer", choices=mapsets, selected=mapsets[8], width="100%"),
      actionButton("staticmap_btn", "Detailed map", style=action_btn_style, icon("globe")),
      sidebarMenu(
        id="tabs",
        menuItem("Climate", icon=icon("sliders"), tabName="climate"),
        menuItem("Information", icon=icon("info-circle"), tabName="info")
      ),
      conditionalPanel("output.Map != null",
        actionButton("help", "Take tour", style=action_btn_style, icon=icon("question-circle")),
        #bookmarkButton(style=action_btn_style)
        actionButton("fake", "Bookmark", style=action_btn_style, icon=icon("link")), # placeholder
        bsTooltip("help", "Note: Tour incomplete, under development.", placement="right", options=list(container="body")),
        bsTooltip("fake", "Note: Server-side bookmarking not yet available.", placement="right", options=list(container="body"))
      ),
      #hr(style="margin: 15px;"),
      #fluidRow(
      #  column(12, 
      #    p("Global / semi-global options.", style="text-align: justify; margin: 0px 15px 0px 15px;"),
      #    selectInput("metric", "Units", c("US", "Metric"), width="100%"),
      #    selectInput("area_axis_scale", "Axis scale", c("x1", "x100", "x1000"), width="100%")
      #  )
      #),
      #bsTooltip("metric", "Units apply to all data.", placement="right", options=list(container="body")),
      #bsTooltip("area_axis_scale", "Scale affects plot axes for burn area, fire size and vegetation cover area.",
      #  placement="right", options=list(container="body")),
      #uiOutput("download_report")
      tags$footer(
        p(a(href="http://snap.uaf.edu", target="_blank",
          tags$img(src="SNAPDB_acronym_75px.png", width="20%", alt="SNAP Dashboards"), style="padding: 5px; margin: 0px;"),
        strong(em("SNAP Dashboards"))),
        style="position:absolute; align: left; bottom:0; width:100%; height:30px; color: white; padding: 5px; z-index: 1000;"
      )
    ),
    dashboardBody(
      includeCSS("www/styles.css"),
      tabItems(
        tabItem(tabName="climate",
              bsModal("settings", "Plot settings", "settings_btn", size="large",
                fluidRow(
                  column(4,
                    selectInput("metric", "Units", c("US", "Metric"), width="100%"),
                    selectInput("facet_scales", "Axis scales", choices=axis_scales, width="100%")
                  ),
                  column(4,
                    sliderInput("alpha_den", "Density transparency", 0.1, 1, 1, 0.1, sep="", width="100%"),
                    sliderInput("alpha_ts", "Series transparency", 0.1, 1, 0.1, 0.1, sep="", width="100%")
                  ),
                  column(4, checkboxInput("cru", "Include CRU 4.0 data", FALSE, width="100%"))
                )
              ),
              box(
                fluidRow(
                  column(5,
                         div(id="plot-container",
                             leafletOutput("Map", width="100%", height="400px"),
                             conditionalPanel("output.Map == null", 
                                              h4("Loading map", style="position: absolute; left: 0; top: 35%; right: 0; text-align: center;"),
                                              tags$img(src="spinner.gif", id="loading-spinner")
                             )
                         )
                  ),
                  column(7,
                         fluidRow(
                           column(4,
                             selectInput("variable", "Climate variable", choices=variables, selected=variables[1], width="100%")
                           ),
                           column(8,
                             uiOutput("mapset_regions")
                           )
                         ),
                         fluidRow(
                           column(4,
                             selectInput("rcps", "RCP", choices=rcps, selected=rcps[1], multiple=TRUE, width="100%")
                           ),
                           column(8,
                             selectInput("gcms", "GCM", choices=gcms, selected=gcms[1], multiple=TRUE, width="100%")
                           )
                         ),
                         fluidRow(
                           column(4,
                             selectInput("seasons", "Season", choices=seasons, selected=seasons[1], multiple=TRUE, width="100%")
                           ),
                           column(8,
                             sliderInput("yrs", "Years", min=period[1], max=period[2], value=c(2006, 2099), step=1, sep="", width="100%")
                           )
                         ),
                         fluidRow(
                             column(4,
                               selectInput("marginalize", "Merge distributions across", 
                                 choices=c("", "RCPs"="RCP", "Models"="GCM"), selected="", multiple=TRUE, width="100%")
                             ),
                             column(4,
                               selectInput("clrby", "Color by", choices=c("", "RCP", "Model", "Season", "Region"), selected="", width="100%")
                             ),
                             column(4,
                               selectInput("fctby", "Facet by", choices=c("", "RCP", "Model", "Season", "Region"), selected="", width="100%")
                             )
                         ),
                         fluidRow(
                           column(4, 
                             conditionalPanel(valid_input_selection,
                               actionButton("go_btn", "Build distributions", class="btn-block", icon("globe"))
                             )
                           ),
                           column(4,
                             actionButton("settings_btn", "Additional settings", class="btn-block", icon("gear"))
                           )
                         )
                  )
                ),
                title="Data selection", width=12, collapsible=TRUE
              ),
              fluidRow(
                column(5,
                  plotOutput("dist_plot", height="auto")
                ),
                column(7,
                  plotOutput("ts_plot", height="auto")
                )
              )
        ),
        tabItem(tabName="info",
          h2("Frequently asked questions"),
          faq,
          h2("Contact information"),
          p("For questions about this application, please email paul.duffy@neptuneinc.org")
        )
      )
    ),
    title="Climate Explorer"
  )
}
