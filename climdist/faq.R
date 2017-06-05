bsCollapse(id="faq",
  bsCollapsePanel("What do the available distributions represent?",
    p("Probability distributions are available for four random variables: total monthly precipitation and mean monthly
    minimum, mean, and maximum daily temperature. For these four monthly climate variables, continuous probability densities represent
    the estimated distribution of values across space for a given spatial domain.
      ", style="text-align:justify"),
    style="info"),
  bsCollapsePanel("What are Fire Mgmt Zones?",
    p("These regions are the current Fire Management Zones for Alaska. For more information, please see the following link.", style="text-align:justify"),
    a("Bureau of Land Management / Alaska Fire Service - Alaska zone coverage maps", 
      href="https://afs.ak.blm.gov/fire-management/zones-alaska-zone-coverage-maps.php", target="_blank"),
    style="info"),
  bsCollapsePanel("What is a GCM?",
    p("General Circulation Models (GMCs) are used to depict how climate processes respond to the composition of various gases in the atmosphere.
      Using future projections of the composition of gases in the atmosphere, projections of future climate can be made.
      For this work, the GCMs provide different projections of future climate that are used to inform projections of future fire activity.
      If you are interested in exploring the range of burned area with these GCMs,
      the GFDL-CM3 and NCAR-CCSM4 models tend to correspond to the projections with the most area burned
      and the MRI-CGCM3 tends to have the least area burned. For more information, please see the following link.", style="text-align:justify"),
    a("Intergovernmental Panel on Climate Change GCM guide",
      href="http://www.ipcc-data.org/guidelines/pages/gcm_guide.html", target="_blank"),
    style="info"),
  bsCollapsePanel("What is an RCP?",
    p("Representative Concentration Pathways (RCPs) are used to characterize the consequences of different assumptions 
      about human population growth and economic development. In particular, economic development associated with energy usage 
      (e.g. how much and from what sources) is an important driver of future climate.
      We consider 3 RCPs here (4.5, 6.0, and 8.5). RCP 4.5 represents an aggressive reduction 
      in the emission of greenhouse gases like CO2 and methane. 
      RCP 8.5 represents significant increases in the population and a continuation of the use of energy sources
      that emit large quantities of green house gases. 
      RCP 6.0 lies somewhere in between.", style="text-align:justify"), 
      style="info")
)
