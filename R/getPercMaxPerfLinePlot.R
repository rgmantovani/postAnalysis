#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getPercMaxPerfLinePlot = function(data, mat.acc, mat.auc, mat.comp) {

  tasks.acc   = getMaxPerfMatrix(mat.acc)
  tasks.auc   = getMaxPerfMatrix(mat.auc)
  tasks.comp  = getMaxPerfMatrix(mat.comp)

  temp.acc = data.frame(do.call("rbind",lapply(1:ncol(tasks.acc), function(j){
    ids = which(!is.na(tasks.acc[,j]))
    return(mean(tasks.acc[ids,j]))
  })))

  temp.auc = data.frame(do.call("rbind",lapply(1:ncol(tasks.auc), function(j){
    ids = which(!is.na(tasks.auc[,j]))
    return(mean(tasks.auc[ids,j]))
  })))

  temp.comp = data.frame(do.call("rbind",lapply(1:ncol(tasks.comp), function(j){
    ids = which(!is.na(tasks.comp[,j]))
    return(mean(tasks.comp[ids,j]))
  })))

  temp = cbind(temp.acc, temp.auc, temp.comp)
  temp$alg = colnames(mat.acc)
  colnames(temp)[1:3] = c("perc_max_acc", "perc_max_auc", "perc_max_auc_run")

  temp = temp[order(temp$perc_max_auc, decreasing = TRUE), ]
  temp$alg = factor(temp$alg, levels = temp$alg)
 
  df.p = melt(temp, id.vars = 4)
  colnames(df.p)[2] = "Measure"
  
  g = ggplot(data=df.p, aes(x=alg, y=value, group=Measure, colour=Measure, linetype=Measure, shape=Measure)) 
  g = g + geom_line() + geom_point() 
  g = g + guides(fill = FALSE)
  g = g + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
  g = g + scale_y_continuous(limits = c(0.0, 1))
  g = g + scale_colour_brewer(palette = "Set2")
  g = g + ylab("% of Max. Performance") + xlab("Algorithms")

  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
