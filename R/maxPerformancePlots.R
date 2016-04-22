#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# auxiliary function
percMaxPerfAux = function(mat) {

  best = lapply(1:nrow(mat), function(i){
    ids = which(!is.na(mat[i,]))
    return(max(mat[i, ids]))
  })
  df = data.frame(do.call("rbind", best))

  aux = lapply(1:nrow(mat), function(i){
    line = mat[i,] / best[i]
    return(line)
  })

  tasks = data.frame(do.call("rbind", aux))
  return(tasks)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
getMaxPerfByMeasure = function(data, tasks, chart = "boxplot", prefix = NULL) {

  ids = which(data$NumberOfClasses == 2)

  binary.tasks = unique(data[ids, "task.id"])
  bin.rows = which(rownames(tasks) %in% binary.tasks)

  multic.tasks = unique(data[-ids, "task.id"])
  mlt.rows = which(rownames(tasks) %in% multic.tasks)

  tasks = tasks[c(mlt.rows, bin.rows), ]
 
  if(chart != "boxplot") {
    tasks[is.na(tasks)] = 0
  }

  tasks$task = gsub("OpenML-Task-", "", rownames(tasks))
  rownames(tasks) = NULL

  df = melt(tasks, id.vars = ncol(tasks))
  colnames(df)[2] = "algo"

  if(chart != "boxplot" & chart != "heatmap") {
    stopf(" - Invalid type of plot: choose between \'boxplot\' and \'heatmap\' \n")
  }

  g = NULL

  if(chart == "heatmap") {

    df$task = as.factor(df$task)
    colnames(df)[3] = "percentage"

    g = ggplot(df, aes(x=as.factor(algo), y=task, fill=percentage))
    g = g + geom_tile() 
    g = g + scale_fill_gradient(low = "white", high = "black")
    g = g + scale_y_discrete(breaks = FALSE)
    g = g + theme(text = element_text(size = 10), axis.text.y = element_blank(), 
      axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
    g = g + ylab("Tasks") + xlab("Algorithms")
    g = g + ggtitle("Percentage of the maximum performance over all the tasks")

  } else if (chart == "boxplot") {

    g = ggplot(data = df, mapping = aes(x = as.factor(algo), y = value))
    g = g + geom_boxplot(outlier.colour = "black", outlier.size = 1)
    g = g + scale_y_continuous(limits = c(0, 1))
    g = g + theme(text = element_text(size = 10), 
      axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
    g = g + ylab(paste0("% of the maximum ", gsub("_", "", prefix))) + xlab("Algorithms")
  }

  return(g)
}


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getPercMaxPerfBoxplot = function(data, mat.acc, mat.auc) {

  tasks.acc  = percMaxPerfAux(mat.acc)
  tasks.auc  = percMaxPerfAux(mat.auc)
  
  # calling auxiliary plots
  r1 = getMaxPerfByMeasure(data = data, tasks = tasks.acc, chart = "boxplot", prefix = "acc_")
  r2 = getMaxPerfByMeasure(data = data, tasks = tasks.auc, chart = "boxplot", prefix = "auc_")
  
  double.bp = gridExtra::arrangeGrob(r1, r2, ncol = 1, nrow = 2)
  return(double.bp)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getPercentageMaxPerfPlot = function(data, mat.acc, mat.auc, mat.comp) {

  tasks.acc   = percMaxPerfAux(mat.acc)
  tasks.auc   = percMaxPerfAux(mat.auc)
  tasks.comp  = percMaxPerfAux(mat.comp)

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
