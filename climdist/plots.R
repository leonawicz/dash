distPlot <- function(data, xlb, clrby, clrvec, alpha, fctby, fct_scales, yrs, type, prevent){
  if(prevent) return()
  lgd_alpha <- guide_legend(override.aes=list(alpha=1))
  if(is.null(clrby)) clr <- "white" else clr <- "black"
  ylb <- if(yrs[1]==tail(yrs, 1)) paste(yrs[1], "density") else paste(yrs[1], "-", tail(yrs, 1), "density")
  g <- ggplot(data=data, aes_string("Val"))
  if(type=="density"){
    if(is.null(clrby)){
      g <- g + geom_density(fill="#A5A5A5", colour="white", alpha=alpha)
      g <- g + geom_line(colour="#3366FF", size=1, stat="density")
    } else {
      g <- g + geom_density(aes_string(fill=clrby), colour="white", alpha=alpha)
      g <- g + geom_line(aes_string(colour=clrby), size=1, stat="density")
    }
  } else {
    g <- g + geom_histogram(aes_string(fill=clrby), colour=clr, position="dodge", alpha=alpha)
  }
  g <- .colorFacet(g, data, clrby, clrvec, fctby, fct_scales)
  g + theme_bw(base_size=18) + plottheme + labs(x=xlb, y=ylb) + guides(colour=lgd_alpha)
    
}

tsPlot <- function(data, yrs, ylb, clrby, clrvec, alpha, fctby, fct_scales, ann_means, ann_obs, prevent){
  if(prevent) return()
  lgd_alpha <- guide_legend(override.aes=list(alpha=1))
  pos <- .getPosition(jitter=TRUE, clrby)
  n.facets <- if(is.null(fctby)) 1 else length(unique(data[[fctby]]))
  breaks <- get_breaks(yrs, n.facets)
  g <- ggplot(data=data, aes_string("Year", "Val", colour=clrby, fill=clrby))
  if(ann_obs) g <- g + geom_point(size=3, shape=21, color="black", alpha=alpha, position=pos)
  g <- g + geom_smooth(method="lm", size=1) + 
    geom_smooth(method="lm", colour="white", size=2, se=FALSE) + 
    geom_smooth(method="lm", size=1, se=FALSE)
  if(ann_means) g <- g + stat_summary(fun.y=mean, geom="line") + stat_summary(fun.y=mean, geom="point")
  g <- .colorFacet(g, data, clrby, clrvec, fctby, fct_scales)
  g +  theme_bw(base_size=18) + plottheme + theme(axis.text.x=element_text(angle=45, hjust=1)) + 
    labs(y=ylb) + guides(fill=lgd_alpha) +
    scale_x_continuous(limits=range(yrs), expand=c(0, 0), breaks=breaks, labels=breaks, minor_breaks=yrs)
}

decPlot <- function(data, ylb, clrby, clrvec, alpha, fctby, fct_scales, type, prevent){
  if(prevent) return()
  lgd_alpha <- guide_legend(override.aes=list(alpha=1))
  pos <- .getPosition(jitter=TRUE, clrby, dodgeable=TRUE)
  g <- g <- ggplot(data=data, aes_string("Decade", "Val", colour=clrby, fill=clrby))
  doBox <- type %in% c("Box plot", "Overlay")
  doStrip <- type %in% c("Strip chart", "Overlay")
  if(doStrip) shp.out <- NA else shp.out <- 21
  if(doBox){
    if(is.null(clrby)){
      g <- g + geom_boxplot(fill="gray", colour="black", alpha=alpha, outlier.shape=shp.out)
    } else {
      g <- g + geom_boxplot(colour="black", alpha=alpha, outlier.shape=shp.out)
    }
  }
  if(doStrip){
    if(is.null(clrby)){
      g <- g + geom_point(shape=21, fill="black", colour="black", position=pos, alpha=alpha)
    } else {
      g <- g + geom_point(shape=21, colour="black", position=pos, alpha=alpha)
    }
  }
  
  g <- .colorFacet(g, data, clrby, clrvec, fctby, fct_scales)
  g + theme_bw(base_size=18) + plottheme + theme(axis.text.x=element_text(angle=45, hjust=1)) +
    labs(y=ylb) + guides(fill=lgd_alpha)
}
