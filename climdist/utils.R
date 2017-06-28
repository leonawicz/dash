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
        text = element_text(size=14),
        panel.spacing.x=unit(0.25,"cm"),
        plot.margin=unit(c(0.5, 1, 0.5, 0.5),"cm"),
        strip.text=element_text(size=14))

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
