#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRuntimeLinePlot = function(data) {

  temp = getRuntime(data)
  aux = temp[order(temp$total, decreasing = FALSE),]
  aux$alg = factor(aux$alg, levels = aux$alg)

  aux.final = melt(aux, id.vars = 4)   
  colnames(aux.final) = c("alg", "time", "value")

  g = ggplot(data=aux.final, aes(x=alg, y=log(value), 
    colour=time, group=time, linetype=time, shape=time))
  g = g + geom_line() + geom_point()
  g = g + theme(text = element_text(size=10), 
    axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  g = g + scale_colour_brewer(palette="Dark2")
  g = g + ylab("Average runtime (log scale)") + xlab("Algorithms")
  
  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
