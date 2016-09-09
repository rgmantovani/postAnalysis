#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getAlgosAvgPerfLinePlot = function(data, measures.list) {

  # Check measures
  aux = lapply(measures.list, function(meas){
    checkMeasure(measure = meas)
  })
  meas.names = do.call("c", measures.list)

  # Average performance by learner
  perf = lapply(measures.list, function(meas){
    return(getAvgPerformance(data = data, measure = meas))
  })
  perf = Reduce(function(...) merge(..., all=T), perf)
  new.names = paste0("avg_", gsub(meas.names, pattern = "\\.", replacement = "_"))
  colnames(perf) = c("algo", new.names)

  perf = perf[order(perf[,2], decreasing = TRUE), ]
  perf$algo = factor(perf$algo, levels = perf$algo)

  df = melt(perf)
  colnames(df)[2] = "Measure"

  g = ggplot(data = df, aes(x = algo, y = value, group = Measure, colour = Measure, 
    linetype = Measure, shape = Measure)) 
  g = g + geom_line() + geom_point() 
  g = g + theme(text = element_text(size=10),
   axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  g = g + scale_y_continuous(limits = c(0, 1))
  g = g + ylab("Average value") + xlab("Algorithms")
 
  return(g)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getAlgosAvgRankLinePlot = function(data, measures.list) {

  # Check measures
  aux = lapply(measures.list, function(meas){
    checkMeasure(measure = meas)
  })
  meas.names = do.call("c", measures.list)

  # Computing the Average Ranking
  rk.list = lapply(measures.list, function(meas){
    mat = getPerfMatrix(data = data, measure = meas)
    rk  = getRanking(mat = mat)$rk.avg 
    colnames(rk) = c("algo", paste0("rk_", gsub(meas, pattern = "\\.", replacement = "_")))
    return(rk)
  })
  rks = Reduce(function(...) merge(..., all=T), rk.list)
 
  df = rks[order(rks[,2], decreasing = FALSE), ]
  df$algo = factor(df$algo, levels = df$algo)
  
  df = melt(df) 
  colnames(df)[2] = "Measure"

  g = ggplot(data=df, aes(x=algo, y = value, group = Measure, colour = Measure, 
    linetype = Measure, shape = Measure)) 
  g = g + geom_line() + geom_point() 
  g = g + theme(text = element_text(size=10), 
    axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  g = g + scale_colour_brewer(palette="Dark2")
  g = g + ylab("Average ranking") + xlab("Algorithms")

  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
