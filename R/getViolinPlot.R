#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getViolinPlot = function(data, measure = "predictive.accuracy", landscape = TRUE, prefix = NULL) {

  checkMeasure(measure = measure)

  temp = data[, c("flow.name", measure)]
  colnames(temp) = c("algo", "meas")

  if( measure == "usercpu.time.millis") {
    y.label = "log(Runtime)"
  } else if(measure == "usercpu.time.millis.training"){
    y.label = "log(Training time)"
  } else if(measure == "usercpu.time.millis") {
    y.label = "log(Testing time)"
  } else {
    y.label = gsub(measure, pattern="\\.", replacement=" ")
  }
  
  if(!is.null(prefix)) {
    y.label = paste(prefix, y.label)
  }

  if(measure %in% c("usercpu.time.millis",  "usercpu.time.millis.training", 
    "usercpu.time.millis.testing")) { 
    g = ggplot(data = temp, mapping = aes(x = as.factor(algo), y = log(meas), fill = algo))
  } else {
    g = ggplot(data = temp, mapping = aes(x = as.factor(algo), y = meas, fill = algo))
    g = g + scale_y_continuous(limits = c(0, 1))
  }

  g = g + geom_violin(trim = TRUE, scale = "width")
  g = g + geom_boxplot(outlier.colour = "black", outlier.size = 0.5, width = 0.2, fill = "white")
  g = g + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
  g = g + theme_bw() + theme(legend.position="none")
  g = g + xlab("Algorithms") + ylab(y.label)
  
  if(landscape) {
    g = g + coord_flip()
  }
  
  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------