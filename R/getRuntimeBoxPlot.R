#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRuntimeBoxplot = function(data, landscape = FALSE) {

  df = data.frame(data)
  temp = df[, c("algo", "training.time", "testing.time")]
  temp$runtime = temp$training.time + temp$testing.time
  
  g = ggplot(data = temp, mapping = aes(x = as.factor(algo), y = log(runtime)))
  g = g + stat_boxplot(geom ='errorbar')
  g = g + geom_boxplot(outlier.colour = "black", outlier.size = 0.5)
  g = g + theme(text = element_text(size = 10), 
    axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
  g = g + ylab("log(Runtime)") + xlab("Algorithms")

  if(landscape) {
    g = g + coord_flip()
  }

  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
