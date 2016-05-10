#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getAlgosViolinPlot = function(data, measure = "predictive.accuracy", landscape = FALSE) {

  if(measure %nin% c("predictive.accuracy", "area.under.roc.curve")) {
    stopf("Invalid performance measure!")
  }

  chart.meas = ifelse(measure == "predictive.accuracy", "Predictive accuracy", "AUC")
 
  df = data.frame(data)
  temp = df[, c("algo", measure)]
  colnames(temp)[2] = "meas"
  
  g = ggplot(data = temp, mapping = aes(x = as.factor(algo), y = meas))
  g = g + geom_violin(trim = TRUE, fill = '#FFCCFF')
  g = g + geom_boxplot(outlier.colour = "black", outlier.size = 0.5, width = 0.1)
  g = g + scale_y_continuous(limits = c(0, 1))
  g = g + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  g = g + ylab(chart.meas) + xlab("Algorithms") 

  if(landscape) {
    g = g + coord_flip()
  }

  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------