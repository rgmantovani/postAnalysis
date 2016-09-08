#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getViolinPlot = function(data, measure = "predictive.accuracy", landscape = TRUE) {

  checkMeasure(measure = measure)

  temp = data[, c("flow.name", measure)]
  colnames(temp) = c("algo", "meas")
  
  if(measure %in% c("usercpu.time.millis",  "usercpu.time.millis.training", 
    "usercpu.time.millis.testing")) { 
 
    g = ggplot(data = temp, mapping = aes(x = as.factor(algo), y = log(meas), fill = algo))
    if( measure == "usercpu.time.millis") {
      g = g + ylab("log(Runtime)")
    } else if(measure == "usercpu.time.millis.training"){
      g = g + ylab("log(Training time)")
    } else{
      g = g + ylab("log(Testing time)")
    }
  } else {
    g = ggplot(data = temp, mapping = aes(x = as.factor(algo), y = meas, fill = algo))
    g = g + scale_y_continuous(limits = c(0, 1))
    g = g + ylab(gsub(measure, pattern="\\.", replacement=" "))
  }

  g = g + geom_violin(trim = TRUE, scale = "width")
  g = g + geom_boxplot(outlier.colour = "black", outlier.size = 0.5)
  g = g + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
  g = g + theme_bw() + theme(legend.position="none")
  g = g + xlab("Algorithms")
  
  if(landscape) {
    g = g + coord_flip()
  }
  
  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------