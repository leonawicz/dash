library(shinycssloaders)
req_inputs <- inputs_not_null(req_inputs)

plot_opts_row <- function(id, clrfct=TRUE, dl="Download", w="100%"){
  x <- paste0(c("clrby_", "fctby_", "dlPlot_"), id)
  dl <- column(2, downloadButton(x[3], dl, class="btn-block"), offset=ifelse(clrfct, 6, 10))
  if(clrfct) fluidRow(
    column(2, selectInput(x[1], NULL, clropts, width=w)),
    column(2, selectInput(x[2], NULL, fctopts, width=w)), dl) else fluidRow(dl)
}

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
      useShinyjs(),
      do.call(update_toastr_css, intro_css_args),
      
      tags$head(tags$html(app_overlay(NULL, "snap_white.svg", "loading.png"))),
      tags$script("$(document).ready(function(){ $('#fade-wrapper').fadeIn(); });"),
      #actionButton("staticmap_btn", "Detailed map", style=action_btn_style, icon("globe")),
      sidebarMenu(
        id="tabs",
        menuItem("Climate", icon=icon("sliders"), tabName="climate"),
        menuItem("Information", icon=icon("info-circle"), tabName="info")
      ),
      uiOutput("dataLoadedSidebar"),
      dashboard_footer("http://snap.uaf.edu/", "snap_white.svg", "SNAP Dashboards")
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName="climate",
          bsModal("settings", "Additional plot settings", "settings_btn", size="large",
            fluidRow(
              column(4,
                h4("Annual time series"),
                sliderInput("alpha_ts", "Transparency", 0.1, 1, 0.1, 0.1, sep="", width="100%"),
                checkboxGroupInput("show_annual", "Show points", c("Means", "Observations"), "Means", inline=TRUE, width="100%"),
                checkboxGroupInput("fit_models", "Statistical models", c("lm", "glm", "rlm", "gam", "loess"), "lm", inline=TRUE, width="100%"),
                selectInput("eq_pos", "Linear model summary", c("Top left", "Top right", "Bottom left", "Bottom right"), width="100%")
              ),
              column(4,
                h4("Period density curves"),
                sliderInput("alpha_den", "Transparency", 0.1, 1, 0.5, 0.1, sep="", width="100%")
              ),
              column(4,
                h4("Decadal box plots"),
                sliderInput("alpha_dec", "Transparency", 0.1, 1, 0.5, 0.1, sep="", width="100%"),
                selectInput("bptype", "Decadal distributions", c("Box plot", "Strip chart", "Overlay"), "Overlay", width="100%")
              )
            ),
            hr(),
            h4("General options"),
            fluidRow(
              column(3, selectInput("metric", "Units", c("Metric", "US"), width="100%")),
              column(3, selectInput("facet_scales", "Axis scales", choices=axis_scales, width="100%"))
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
                    selectInput("mapset", "Change map layer", mapsets, width="100%")
                  ),
                  column(8,
                    uiOutput("mapset_regions")
                  )
                ),
                fluidRow(
                  column(4, selectInput("variable", "Climate variable", variables, variables[2], width="100%")),
                  column(4, selectInput("rcps", "RCP", rcps, rcps[1], multiple=TRUE, width="100%")),
                  column(4, selectInput("gcms", "Model", list(GCMs=gcms, Baseline=list(cru)), gcms[1], multiple=TRUE, width="100%"))
                ),
                fluidRow(
                  column(4, selectInput("seasons", "Season", seasons, seasons[[1]][1], multiple=TRUE, width="100%")),
                  column(8, sliderInput("yrs", "Years", period[1], period[2], c(2006, 2099), 1, sep="", width="100%"))
                ),
                fluidRow(
                  column(4, 
                    selectInput("marginalize", NULL, mergeopts, "", multiple=TRUE, width="100%"),
                    bsTooltip("marginalize", mergeopts_tooltip, placement="left")
                  ),
                  column(4, 
                    conditionalPanel(req_inputs,
                      actionButton("go_btn", "Build distributions", class="btn-block btn-go", icon("signal")),
                      bsTooltip("go_btn", "Build probability distributions based on current data selections.")
                    )
                  ),
                  column(4,
                    actionButton("settings_btn", "Additional settings", class="btn-block", icon("gear")),
                    bsTooltip("settings_btn", "Addional specifications for data selection and plot formatting.")
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
               ),
              plot_opts_row("decadal")
            ),
            tabPanel("Annual",
              uiOutput("statBoxes1"),
              fluidRow(box(
                withSpinner(
                  plotOutput("ts_plot", height="auto",
                    dblclick="ts_plot_dblclk", brush=brushOpts(id="ts_plot_brush", direction="x", resetOnNew=TRUE))),
                plot_opts_row("annual"),
                title="Annual observations", width=12, collapsible=TRUE
              )),
              fluidRow(div(id="denbox", box(
                withSpinner(plotOutput("dist_plot", height="auto")),
                plot_opts_row("period", FALSE),
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
