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
      selectInput("mapset", "Change map layer", choices=mapsets, width="100%"),
      #actionButton("staticmap_btn", "Detailed map", style=action_btn_style, icon("globe")),
      sidebarMenu(
        id="tabs",
        menuItem("Climate", icon=icon("sliders"), tabName="climate"),
        menuItem("Information", icon=icon("info-circle"), tabName="info")
      ),
      conditionalPanel("output.Map != null",
        actionButton("help", "Take tour", style=action_btn_style, icon=icon("question-circle")),
        #bookmarkButton(style=action_btn_style)
        actionButton("fake", "Bookmark", style=action_btn_style, icon=icon("link")), # placeholder
        bsTooltip("fake", "Note: Server-side bookmarking not yet available.", placement="right", options=list(container="body"))
      ),
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
                    sliderInput("alpha_den", "Period density transparency", 0.1, 1, 1, 0.1, sep="", width="100%"),
                    selectInput("metric", "Units", c("Metric", "US"), width="100%"),
                    checkboxInput("cru", "Include CRU 4.0 data", FALSE, width="100%")
                  ),
                  column(4,
                    sliderInput("alpha_ts", "Annual series transparency", 0.1, 1, 0.1, 0.1, sep="", width="100%"),
                    selectInput("facet_scales", "Axis scales", choices=axis_scales, width="100%"),
                    checkboxInput("show_points", "Show annual observations", FALSE, width="100%")
                  ),
                  column(4, 
                    sliderInput("alpha_dec", "Decadal series transparency", 0.1, 1, 0.5, 0.1, sep="", width="100%"),
                    selectInput("bptype", "Decadal distributions", c("Box plot", "Strip chart", "Overlay"), "Overlay", width="100%")
                  )
                )
              ),
              fluidRow(box(
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
                           choices=c("", "RCPs"="RCP", "Models"="Model"), selected="", multiple=TRUE, width="100%")
                       ),
                       column(4,
                         uiOutput("ColorBy")
                       ),
                       column(4,
                         uiOutput("FacetBy")
                       )
                    ),
                    fluidRow(
                     column(4, 
                       conditionalPanel(valid_input_selection,
                         actionButton("go_btn", "Build distributions", class="btn-block", icon("signal"))
                       )
                     ),
                     column(4,
                       actionButton("settings_btn", "Additional settings", class="btn-block", icon("gear"))
                     ),
                     column(4,
                       actionButton("plot_btn", "Regenerate plots", class="btn-block", icon("line-chart"))
                     )
                    )
                  )
                ),
                title="Data selection", width=12, collapsible=TRUE
              )),
              uiOutput("statBoxes1"),
              fluidRow(box(
                column(5,
                  plotOutput("dist_plot", height="auto")
                ),
                column(7,
                  plotOutput("ts_plot", height="auto")
                ),
                title="Period density and annual observations", width=12, collapsible=TRUE
              )),
              uiOutput("statBoxes2"),
              fluidRow(box(
                column(12,
                  plotOutput("dec_plot", height="auto")
                ),
                title="Decadal distributions: box plots and observations", width=12, collapsible=TRUE
              ))
        ),
        tabItem(tabName="info",
          h2("About this application"),
          p("This app provides rapid access to complete spatial distribution summaries for various climate variables.
            Probability distributions are estimated from downscaled historical and projected climate model outputs, 
            as well as from downscaled Climatological Research Unit (CRU) data for historical periods.
            Spatial probability distributions are conditioned on various geographical regions of interest.
            A number of different map sets are available, each of which contains related geographical subregions
            that can be selected and compared.", style="text-align:justify"),
          p("Data sets are stored in the Amazon Web Services S3 cloud environment. By accessing these data sets
          on demand, this app can quickly deliver full distributional information regarding climate projections based on
          user settings. The plots of densities and individual observations provide a much more complete picture of the 
          spatial variation in the climate series than relying strictly on select statistics.
          Distributional shape and properties such as modality are readily observable. With access to the distributions,
          any statistics of interest can be calcuated.", style="text-align:justify"),
          p("Additionally, when multiple spatially explcit data sets are requested by the user, the app offers the ability
            to compute marginal distributions for a climate variable by integrating out, for instance, multiple
            GCMs, RCPs, regions and/or years. By working with complete distributions rather than aggregate statistics from the start,
            it becomes possible to merge densities to yield these marginal probability distributions.", style="text-align:justify"),
          p("In total, the app offers 3.7 million spatial probability distributions among 82 unique geographic subdomains
          across more than 45,000 high resolution, spatially explicit climate maps.
          These maps cover all climate variables, time periods, GCMs and RCPs.
          Overall, the app provides users with direct access to a synthesis of map layers containing a total of approximately 200 billion pixels,
          all without the need to reduce spatial climate distributions to select, precomputed statistics.
          Then with some additonal in-app computing time at users' discretion, 
          the app also allows users to compute any number of marginal distributions of interest.", style="text-align:justify"),
          h2("Frequently asked questions"),
          faq,
          h2("Contact information"),
          HTML('
               <div style="clear: left;"><img src="https://www.gravatar.com/avatar/5ab20ebc3829054f8af7b1ea4a317269?s=128"
               alt="" style="float: left; margin-right:5px" /></div>
               <p>Matthew Leonawicz<br/>
               Statistician | useR<br/>
               <a href="http://leonawicz.github.io" target="_blank">Github.io</a> |
               <a href="http://blog.snap.uaf.edu" target="_blank">Blog</a> |
               <a href="https://twitter.com/leonawicz" target="_blank">Twitter</a> |
               <a href="http://www.linkedin.com/in/leonawicz" target="_blank">Linkedin</a> <br/>
               <a href="http://www.snap.uaf.edu/", target="_blank">Scenarios Network for Alaska and Arctic Planning</a>
               </p>'
          ),
          p("For questions about this application, please email mfleonawicz@alaska.edu")
        )
      )
    ),
    title="Climate Explorer"
  )
}
