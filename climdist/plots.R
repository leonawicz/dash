distPlot <- function(data, variable, xlb, clrby, clrvec, alpha, fctby, fct_scales, yrs, interact, type, prevent){
  if(prevent) return()
  lgd_alpha <- guide_legend(override.aes=list(alpha=1))
  if(is.null(clrby)) clr <- "white" else clr <- "black"
  ylb <- if(yrs[1]==tail(yrs, 1)) paste(yrs[1], "density") else paste(yrs[1], "-", tail(yrs, 1), "density")
  g <- ggplot(data=data, aes_string("Val", group=interact))
  if(type=="density"){
    g <- g + geom_line(aes_string(colour=clrby), stat="density", alpha=alpha)
  } else {
    g <- g + geom_histogram(aes_string(fill=clrby), colour=clr, position="dodge", alpha=alpha)
  }
  g <- g + theme_bw(base_size=14) #+ scale_y_continuous(expand = c(0, 0.5))
  g <- .colorFacet(g, data, clrby, clrvec, fctby, fct_scales)
  g + plottheme + labs(x=xlb, y=ylb) + guides(colour=lgd_alpha)
    
}

tsPlot <- function(data, variable, ylb, clrby, clrvec, alpha, fctby, fct_scales, prevent){
  if(prevent) return()
  lgd_alpha <- guide_legend(override.aes=list(alpha=1))
  pos <- .getPosition(jitter=TRUE, clrby)
  g <- ggplot(data=data, aes_string("Year", "Val", colour=clrby, fill=clrby)) + 
    geom_point(size=3, shape=21, color="black", alpha=alpha, position=pos)
  g <- g + theme_bw(base_size=14)
  g <- .colorFacet(g, data, clrby, clrvec, fctby, fct_scales)
  g + plottheme + labs(y=ylb) + guides(fill=lgd_alpha)
}
