distPlot <- function(data, xlb, clrby, clrvec, alpha, fctby, fct_scales, type, 
                     prevent, plottheme, condense_legend=FALSE){
  if(prevent) return()
  yrs <- range(data$Year)
  yrs <- if(length(unique(yrs)) > 1) seq(yrs[1], yrs[2]) else yrs[1]
  lgd_rows <- 1
  if(is.null(clrby)) clr <- "white" else clr <- "black"
  ylb <- if(length(yrs)==1) paste(yrs, "density") else paste(yrs[1], "-", tail(yrs, 1), "density")
  g <- ggplot(data=data, aes_string("Val"))
  if(type=="density"){
    if(is.null(clrby)){
      g <- g + geom_density(fill="#A5A5A5", colour="white", alpha=alpha)
      g <- g + geom_line(colour="#3366FF", size=1, stat="density")
    } else {
      lgd_rows <- if(condense_legend && nlevels(data[[clrby]]) > 2) 2 else 1
      g <- g + geom_density(aes_string(fill=clrby), colour="white", alpha=alpha)
      g <- g + geom_line(aes_string(colour=clrby), size=1, stat="density")
    }
  } else {
    g <- g + geom_histogram(aes_string(fill=clrby), colour=clr, position="dodge", alpha=alpha)
  }
  lgd <- guide_legend(override.aes=list(alpha=1), nrow=lgd_rows)
  g <- .colorFacet(g, data, clrby, clrvec, fctby, fct_scales)
  g + theme_bw(base_size=18) + plottheme + labs(x=xlb, y=ylb) + guides(fill=lgd)
    
}

tsPlot <- function(data, variable, ylb, clrby, clrvec, alpha, fctby, fct_scales, 
                   points, fitted_models, eq_pos, prevent, plottheme){
  if(prevent) return()
  lhs <- paste0("~~~~italic(hat(y)[", strsplit(variable, " ")[[1]][2], "])~`=`~")
  rhs <- "~italic(Year)"
  eq_pos <- strsplit(tolower(eq_pos), " ")[[1]]
  
  yrs <- range(data$Year)
  yrs <- if(length(unique(yrs)) > 1) seq(yrs[1], yrs[2]) else yrs[1]
  lgd_alpha <- guide_legend(override.aes=list(alpha=1))
  pos <- .getPosition(jitter=TRUE, clrby)
  n.facets <- if(is.null(fctby)) 1 else length(unique(data[[fctby]]))
  breaks <- get_breaks(yrs, n.facets)
  vars <- c("RCP", "Model", "Region", "Var", "Season", "Year")
  data2 <- group_by_(data, .dots=vars[vars %in% c(clrby, fctby, "Year")]) %>% summarise(Val=mean(Val))
  g <- ggplot(data=data, aes_string("Year", "Val", colour=clrby, fill=clrby))
  if("Observations" %in% points) g <- g + geom_point(size=3, shape=21, color="black", alpha=alpha, position=pos)
  
  g_fitted <- function(g, m){
    if(length(yrs) > 1) g + geom_smooth(data=data2, method=m, size=1) + 
    geom_smooth(data=data2, method=m, colour="white", size=2, se=FALSE) + 
    geom_smooth(data=data2, method=m, size=1, se=FALSE) else g
  }
  if(!is.null(fitted_models)){
    for(i in fitted_models) g <- g_fitted(g, i)
    if(eq_pos!="none" && "lm" %in% fitted_models){
      g <- g + ggpmisc::stat_poly_eq(data=data2, formula=y ~ x, eq.with.lhs=lhs, eq.x.rhs=rhs,
                 label.x.npc=eq_pos[2], label.y.npc=eq_pos[1],
                 aes(label=paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE, size=5)
    }
  }
  if("Means" %in% points){
    if(length(yrs) > 1) g <- g + geom_line(data=data2)
    g <- g + geom_point(data=data2)
  }
  g <- .colorFacet(g, data, clrby, clrvec, fctby, fct_scales)
  g +  theme_bw(base_size=18) + plottheme + theme(axis.text.x=element_text(angle=45, hjust=1)) + 
    labs(y=ylb) + guides(fill=lgd_alpha) +
    scale_x_continuous(limits=range(yrs), expand=c(0, 0), breaks=breaks, labels=breaks, minor_breaks=yrs)
}

decPlot <- function(data, ylb, clrby, clrvec, alpha, fctby, fct_scales, type, limit.sample, prevent, plottheme){
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
    set.seed(1)
    if(is.null(clrby)){
      g <- g + geom_point(data=sample_frac(data, 0.1), shape=21, fill="black", colour="black", position=pos, alpha=alpha)
    } else {
      n <- if(limit.sample & clrby %in% c("RCP", "Model")) nlevels(data[[clrby]]) else 1
      g <- g + geom_point(data=sample_frac(data, 0.1*n), shape=21, colour="black", position=pos, alpha=alpha)
    }
  }
  
  g <- .colorFacet(g, data, clrby, clrvec, fctby, fct_scales)
  g + theme_bw(base_size=18) + plottheme + theme(axis.text.x=element_text(angle=45, hjust=1)) +
    labs(y=ylb) + guides(fill=lgd_alpha)
}
