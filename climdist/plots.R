distPlot <- function(data, xlb, clrby, clrvec, alpha, fctby, fct_scales, yrs, type, prevent){
  if(prevent) return()
  lgd_alpha <- guide_legend(override.aes=list(alpha=1))
  if(is.null(clrby)) clr <- "white" else clr <- "black"
  ylb <- if(yrs[1]==tail(yrs, 1)) paste(yrs[1], "density") else paste(yrs[1], "-", tail(yrs, 1), "density")
  g <- ggplot(data=data, aes_string("Val"))
  if(type=="density"){
    g <- g + geom_line(aes_string(colour=clrby), stat="density", alpha=alpha)
  } else {
    g <- g + geom_histogram(aes_string(fill=clrby), colour=clr, position="dodge", alpha=alpha)
  }
  g <- .colorFacet(g, data, clrby, clrvec, fctby, fct_scales)
  g + theme_bw(base_size=14) + plottheme + labs(x=xlb, y=ylb) + guides(colour=lgd_alpha)
    
}

tsPlot <- function(data, ylb, clrby, clrvec, alpha, fctby, fct_scales, show_points, prevent){
  if(prevent) return()
  lgd_alpha <- guide_legend(override.aes=list(alpha=1))
  pos <- .getPosition(jitter=TRUE, clrby)
  g <- ggplot(data=data, aes_string("Year", "Val", colour=clrby, fill=clrby))
  if(show_points) g <- g + geom_point(size=3, shape=21, color="black", alpha=alpha, position=pos)
  g <- g + geom_smooth(method="lm", size=1) + geom_smooth(method="lm", colour="white", size=2, se=FALSE) + geom_smooth(method="lm", size=1, se=FALSE)
  g <- .colorFacet(g, data, clrby, clrvec, fctby, fct_scales)
  g +  theme_bw(base_size=14) + plottheme + labs(y=ylb) + guides(fill=lgd_alpha)
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
  g + theme_bw(base_size=14) + plottheme + labs(y=ylb) + guides(fill=lgd_alpha)
}
