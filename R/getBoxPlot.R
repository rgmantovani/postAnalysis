#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getBoxPlot = function(data, measure = "predictive.accuracy", landscape = FALSE, prefix = NULL) {

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
    g = ggplot(data = temp, mapping = aes(x = as.factor(algo), y = log(meas)))
  } else {
    g = ggplot(data = temp, mapping = aes(x = as.factor(algo), y = meas))
    g = g + scale_y_continuous(limits = c(0, 1))
  }
  g = g + stat_boxplot(geom ='errorbar')
  g = g + geom_boxplot(outlier.colour = "black", outlier.size = 0.5)
  g = g + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
  g = g + xlab("Algorithms")
  g = g + ylab(label = y.label)
  
  if(landscape) {
    g = g + coord_flip()
  }
  
  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
