app_intro_title <- "Explore and analyze spatial climate distributions"
app_intro_logo <- "https://toolkit.climate.gov/sites/default/files/styles/large/public/snap_cmyk.png?itok=RYgSo91h"
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

app_about <- tagList(
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
    any statistics of interest can be calculated.", style="text-align:justify"),
  p("Additionally, when multiple spatially explicit data sets are requested by the user, the app offers the ability
    to compute marginal distributions for a climate variable by integrating out, for instance, multiple
    GCMs, RCPs, regions and/or years. By working with complete distributions rather than aggregate statistics from the start,
    it becomes possible to merge densities to yield these marginal probability distributions.", style="text-align:justify"),
  p("In total, the app offers 3.7 million spatial probability distributions among 82 unique geographic subdomains
    across more than 45,000 high-resolution, spatially explicit climate maps.
    These maps cover all climate variables, time periods, GCMs and RCPs.
    Overall, the app provides users with direct access to a synthesis of map layers containing a total of approximately 200 billion pixels,
    all without the need to reduce spatial climate distributions to select, precomputed statistics.
    Then with some additional in-app computing time at users' discretion, 
    the app also allows users to compute any number of marginal distributions of interest.
    The app exposes hundreds of gigabytes of data to the user and any slice of the data can be rapidly accessed.", style="text-align:justify")
)
