library(shinycssloaders)
req_inputs <- inputs_not_null(req_inputs)

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
      use_apputils(TRUE, TRUE),
      update_toastr_css(list(width='70%', height='700px'), list(top='100px')),
      selectInput("mapset", "Change map layer", choices=mapsets, width="100%"),
      #actionButton("staticmap_btn", "Detailed map", style=action_btn_style, icon("globe")),
      sidebarMenu(
        id="tabs",
        menuItem("Climate", icon=icon("sliders"), tabName="climate"),
        menuItem("Information", icon=icon("info-circle"), tabName="info")
      ),
      uiOutput("dataLoadedSidebar"),
      dashboard_footer("http://snap.uaf.edu/", "snap_white_transparent_400h.png", "SNAP Dashboards")
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName="climate",
          bsModal("settings", "Additional settings", "settings_btn", size="large",
            h4("Data selection"),
            fluidRow(
              column(4, selectInput("metric", "Units", c("Metric", "US"), width="100%")),
              column(4, checkboxInput("cru", "Include CRU 4.0 data", FALSE, width="100%"))
            ),
            h4("Plot options"),
            fluidRow(
              column(4,
                sliderInput("alpha_den", "Period density transparency", 0.1, 1, 0.5, 0.1, sep="", width="100%"),
                selectInput("facet_scales", "Axis scales", choices=axis_scales, width="100%")
              ),
              column(4, 
                sliderInput("alpha_dec", "Decadal series transparency", 0.1, 1, 0.5, 0.1, sep="", width="100%"),
                selectInput("bptype", "Decadal distributions", c("Box plot", "Strip chart", "Overlay"), "Overlay", width="100%")
              ),
              column(4,
                sliderInput("alpha_ts", "Annual series transparency", 0.1, 1, 0.1, 0.1, sep="", width="100%"),
                checkboxGroupInput("show_annual", "Show points", c("Means", "Observations"), "Means", inline=TRUE, width="100%"),
                checkboxGroupInput("fit_models", "Statistical models", c("lm", "glm", "rlm", "gam", "loess"), "lm", inline=TRUE, width="100%"),
                selectInput("eq_pos", "Linear model summary", c("Top left", "Top right", "Bottom left", "Bottom right"), width="100%")
              )
            )
          ),
          div(id="controls", fluidRow(box(
            fluidRow(
              column(5,
                div(id="plot-container",
                  leafletOutput("Map", width="100%", height="400px"),
                  conditionalPanel("output.Map == null", 
                    h4("Loading map", style="position: absolute; left: 0; top: 32%; right: 0; text-align: center;"),
                    tags$img(src="resources/images/spinner.gif", id="loading-spinner")
                  )
                )
              ),
              column(7,
                fluidRow(
                 column(4,
                   selectInput("variable", "Climate variable", choices=variables, selected=variables[2], width="100%")
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
                       choices=c("", "RCPs"="RCP", "Models"="Model"), selected="", multiple=TRUE, width="100%")
                   ),
                   column(4, selectInput("clrby", "Color by", clrfctopts, width="100%")),
                   column(4, selectInput("fctby", "Facet by", clrfctopts, width="100%"))
                ),
                fluidRow(
                 column(4, 
                   conditionalPanel(req_inputs,
                     actionButton("go_btn", "Build distributions", class="btn-block btn-go", icon("signal")),
                     bsTooltip("go_btn", "Build probability distributions based on current data selections.")
                   )
                 ),
                 column(4,
                   actionButton("settings_btn", "Additional settings", class="btn-block", icon("gear")),
                   bsTooltip("settings_btn", "Addional specifications for data selection and plot formatting.")
                 ),
                 column(4,
                   actionButton("plot_btn", "Regenerate plots", class="btn-block", icon("line-chart")),
                   bsTooltip("plot_btn", "Regenerate plots quickly when only plot formatting has changed but data selection is the same.")
                 )
                )
              )
            ),
            title="Data selection", width=12, collapsible=TRUE
          ))),
          fluidRow(tabBox(
            tabPanel("Decadal",
              uiOutput("statBoxes2"),
              h4("Decadal distributions: box plots and observations"),
                fluidRow(
                  column(12,
                    withSpinner(
                      plotOutput("dec_plot", height="auto",
                        dblclick="dec_plot_dblclk", brush=brushOpts(id="dec_plot_brush", direction="x", resetOnNew=TRUE)))
                  )
               )
            ),
            tabPanel("Annual",
              uiOutput("statBoxes1"),
              fluidRow(box(
                withSpinner(
                  plotOutput("ts_plot", height="auto",
                    dblclick="ts_plot_dblclk", brush=brushOpts(id="ts_plot_brush", direction="x", resetOnNew=TRUE))), 
                title="Annual observations", width=12, collapsible=TRUE
              )),
              fluidRow(div(id="denbox", box(
                withSpinner(plotOutput("dist_plot", height="auto")), 
                title="Period density", width=12, collapsible=TRUE, collapsed=TRUE
              )))
            ), id="results", selected="Annual", title=NULL, width=12, side="right"
          )
        )),
        tabItem(tabName="info",
          about_app,
          h2("Frequently asked questions"),
          faq(faqs, bscollapse.args=list(id="faq", open="apps"), showcase.args=list(drop="climdist")),
          contactinfo(list(uaf="UAFLogo_A_286.png", iarc="iarc_375.jpg", snap="snap_fullcolor_400h.png")), br()
        )
      )
    ),
    title="Climate Explorer"
  )
}
