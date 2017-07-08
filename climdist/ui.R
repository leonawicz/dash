library(shinycssloaders)

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
      tags$style(HTML(
        '.content-wrapper,
        .right-side {
        background-color: #ffffff;
        }
        #toast-container.toast-top-center > div {
          overflow-y: auto;
          width: 70%;
          height: 700px;
        }
        .toast-top-center { 
        top: 100px;   
        margin: 0 auto;
        left: 115px;
        }'
      )),
      selectInput("mapset", "Change map layer", choices=mapsets, width="100%"),
      #actionButton("staticmap_btn", "Detailed map", style=action_btn_style, icon("globe")),
      sidebarMenu(
        id="tabs",
        menuItem("Climate", icon=icon("sliders"), tabName="climate"),
        menuItem("Information", icon=icon("info-circle"), tabName="info")
      ),
      uiOutput("dataLoadedSidebar"),
      tags$footer(
        a(href="http://snap.uaf.edu/", target="_blank",
            tags$img(src="snap_white_transparent_400h.png", width="100%"), style="align: center; padding: 0px; margin: 0px;"),
        p(strong(em("SNAP Dashboards")), style="text-align:center; padding: 0px; margin: 0px;"),
        style="position:absolute; align: center; bottom:0px; width:100%; height:160px; color: white; padding: 5px;"
      )
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
                    checkboxInput("show_annual_means", "Show annual means", TRUE, width="100%"),
                    checkboxInput("show_annual_obs", "Show annual observations", FALSE, width="100%")
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
                        tags$img(src="spinner.gif", id="loading-spinner")
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
                       conditionalPanel(valid_input_selection,
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
              uiOutput("statBoxes1"),
              fluidRow(box(
                column(5,
                  conditionalPanel("output.dist_plot !== null", 
                    withSpinner(plotOutput("dist_plot", height="auto"), type = 1)
                  )
                ),
                column(7,
                  conditionalPanel("output.ts_plot !== null", 
                    withSpinner(
                      plotOutput("ts_plot", height="auto",
                        dblclick="ts_plot_dblclk", brush=brushOpts(id="ts_plot_brush", direction="x", resetOnNew=TRUE)), 
                      type = 1)
                  )
                ),
                title="Period density and annual observations", width=12, collapsible=TRUE
              )),
              uiOutput("statBoxes2"),
              fluidRow(box(
                column(12,
                  conditionalPanel("output.dec_plot !== null", 
                    withSpinner(plotOutput("dec_plot", height="auto"), type = 1)
                  )
                ),
                title="Decadal distributions: box plots and observations", width=12, collapsible=TRUE
              ))
        ),
        tabItem(tabName="info",
          h2("About this application"),
          app_about,
          h2("Frequently asked questions"),
          faq(faqs, bscollapse.args=list(id="faq", open="apps"), showcase.args=list(drop="jfsp-v10")),
          contactinfo(list(uaf="UAFLogo_A_286.png", iarc="iarc_375.jpg", snap="snap_fullcolor_400h.png")), br()
        )
      )
    ),
    title="Climate Explorer"
  )
}
