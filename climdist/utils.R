tolpal <- function(n){
  if(n==0) return()
  if(n >= 12) return(c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#ffad33", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499"))
  switch(n,
         "1"=c("#4477AA"),
         "2"=c("#4477AA", "#CC6677"),
         "3"=c("#4477AA", "#ffad33", "#CC6677"),
         "4"=c("#4477AA", "#117733", "#ffad33", "#CC6677"),
         "5"=c("#332288", "#88CCEE", "#117733", "#ffad33", "#CC6677"),
         "6"=c("#332288", "#88CCEE", "#117733", "#ffad33", "#CC6677","#AA4499"),
         "7"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#ffad33", "#CC6677","#AA4499"),
         "8"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#ffad33", "#CC6677","#AA4499"),
         "9"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#ffad33", "#CC6677", "#882255", "#AA4499"),
         "10"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#ffad33", "#661100", "#CC6677", "#882255", "#AA4499"),
         "11"=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#ffad33", "#661100", "#CC6677", "#882255", "#AA4499")
  )
}

plottheme <- theme(panel.grid.major = element_line(size = .5, color = "grey"),
        plot.title=element_text(hjust=0.5),
        axis.line=element_line(size=.7, color="black"),
        axis.ticks.length=unit(0.35,"cm"),
        legend.position="bottom",
        text = element_text(size=18),
        panel.spacing.x=unit(0.25,"cm"),
        plot.margin=unit(c(0.5, 1, 0.5, 0.5),"cm"),
        strip.text=element_text(size=14))

get_breaks <- function(yrs, n.facets=1){
  n <- length(yrs)
  if(n.facets==1) cols <- 1 else 
    if(n.facets %in% c(2,4)) cols <- 2 else cols <- 3
  if(n <= 15){
    if(cols < 2) return(yrs) else return(seq(yrs[1], yrs[n], by=2))
  }
  if(n <= 25){
    if(cols < 2) return(yrs) else return(seq(yrs[1], yrs[n], by=2))
  }
  if(n <= 50/cols) step <- 5 else 
    if(n <= 100/cols) step <- 10 else step <- 20
  a <- range(yrs[yrs %% step == 0])
  a <- seq(a[1], a[2], by=step)
  #if(a[1] - yrs[1] < )
  unique(c(yrs[1], a, yrs[n]))
}

pTextSize <- function(x, value, margin=NULL, default.value=100){
  if(length(x) > 1) value <- c(value, rep(default.value, length(x) - 1))
  style <- paste0("font-size: ", value, "%;")
  if(!is.null(margin)) style <- paste0(style, " margin: ", margin, "px;")
  x <- map2(x, style, ~tags$p(.x, style=.y))
  if(length(x)==1) return(x[[1]])
  class(x) <- c("shiny.tag.list", "list")
  x
}

interact <- function(x){
  grp <- c("RCP", "GCM", "Region", "Season")
  x <- grp[grp %in% x]
  if(!length(x)) return()
  paste0("interaction(", paste0(x, collapse=","), ")")
}

.getPosition <- function(jitter, cby, w=0.2, h=0, wd=0.75, dodgeable=FALSE){
  if(jitter) x <- position_jitter(width=w, height=h) else x <- "identity"
  if(!dodgeable) return(x)
  if(!is.null(cby)){
    if(jitter){
      x <- position_jitterdodge(jitter.width=w, jitter.height=h)
    } else {
      x <- position_dodge(width=wd)
    }
  }
  x
}

.colorFacet <- function(g, d, cby, clr, fby, scales){
  if(!is.null(clr)){
    g <- g + scale_fill_manual(values=clr, limits=levels(d[[cby]])) +
      scale_colour_manual(values=clr, limits=levels(d[[cby]]))
  }
  if(!is.null(fby)) g <- g + facet_wrap(as.formula(paste0("~", fby)), scales=scales)
  g
}

.app_img_link <- function(app_url, img_url, title, subtitle, height=200){
  HTML(paste0(
    '<div class="img_link_wrap">
      <img class="img_app" src="', img_url, '" width="100%" height="', height, '"/>
      <a href="', app_url, '" style="color:white;" target="_blank"
      <div class="img_hover_layer">
        <div class="img_hover">
          <h3><p>', title, '</p></h3>
          <h4><p class="img_hover">', subtitle, '</p></h4>
        </div>
      </div>
      </a>')) # contextual, must remove a closing div with mutliple inline calls
}

app_img_links <- function(app_url, img_url, title, subtitle, height=200, min.width=300, max.width=400, col.width=4){
  x <- purrr::map(seq_along(app_url),
    ~column(col.width, .app_img_link(app_url[.x], img_url[.x], title[.x], subtitle[.x], height),
            style=paste0("min-width: ", min.width, "px; max-width: ", max.width, "px; padding:5px;")))
  fluidRow(x, style="padding: 10px;")
}
