app_intro_title <- "Explore and analyze spatial climate distributions"
app_intro_logo <- "snap_white.svg"
app_intro_message <- "
<p style='text-align:justify;'>Welcome to SNAP's Climate Analytics app. To get started, a default data set is loaded when you visit this app.
Press 'Build distributions' in the center of the page to reload after updating data selections.
You can see an overview of app features and capabilities below. 
If you are new to the app, taking the tour located in the sidebar is also recommended. Click to dismiss this message at any time.</p>
<ul style='padding: 15px; text-align: justify;'>
<li><h4>Full climate distributions, not just pre-aggregated stats.</h4>
<p>Drill down into SNAP's high-resolution downscaled climate data.
Access past and projected climate across Alaska and western Canada.</p></li>
<li><h4>Advanced analytics for informed decisions</h4>
<p>Integrate climate probability distributions across space, time and other factors. 
Fit models, analyze projections and bound future uncertainty.</p></li>
<li><h4>The data you specify. The output you need.</h4>
<p>Get customized output including dynamic reports generated on the fly in response to your unique specifications.</p></li>
<li><h4>Fully downloadable content at the click of a button</h4>
<p>Download data sets available in several file formats as well as pdf graphics and reports based on live content.</p></li>
<li><h4>Bookmarking</h4>
<p>Save the precise state of your application for later or to share specific data and results with colleagues.</p></li>
<li><h4>Get help</h4>
<p>The Information tab provides an overview and links to other resources.
Tooltips are available throughout the app. Go into deeper detail with the interactive tour.</p></li>
<li><h4>Sign in for more</h4>
<p>Parallel processing, even bigger data needs, and other features available to authenticated users.</p></li>
</ul>"

about_app <- tagList(
  h2("About this application"),
  HTML(read_md_paragraphs("text_about.txt", ptag = TRUE, collapse = TRUE)),
  app_citation("Matthew Leonawicz", 2017,
               title = "Web application for Alaska and western Canada downscaled GCM output",
               publisher = "Scenarios Network for Alaska and Arctic Planning, University of Alaska Fairbanks",
               url = "http://shiny.snap.uaf.edu/climdist")
)
