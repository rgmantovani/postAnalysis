#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

tasksInfoPlot = function(data) {

  tasks = unique(data$task.id)
 
  aux = lapply(tasks, function(task){
    sub = data[which(data$task.id == task), ]
    max.acc = max(sub$predictive.accuracy)
    max.auc = max(sub$area.under.roc.curve)
    maj.prop = max(sub$perMajClass)
    ret = c(max.acc, max.auc, maj.prop)
    return(ret)
  })

  # df = [task | max.acc | max.auc | % maj class ]
  df = data.frame(do.call("rbind", aux))
  colnames(df) = c("max_acc", "max_auc", "perc_maj")  
  df$tasks = tasks
  
  # sort increasing the % majclass (tasks)
  df = df[order(df$perc_maj, decreasing=FALSE),]
  df$tasks = factor(df$tasks, levels=df$tasks)

  df.final = melt(df, id.vars=4)
  colnames(df.final) = c("task", "Measure", "value")
  df.final$task = as.numeric(df.final$task)
 
  setEPS()
  postscript("plots/datasets_info_performance.eps", height=5, width=8)
  g = NULL
  g = ggplot(data=df.final, aes(x=task, y=value, group=Measure, 
    colour=Measure, shape=Measure)) 
  g = g + geom_point() 
  g = g + scale_colour_brewer(palette = "Dark2")
  g = g + ylab(" Maximum value / Majority Class") + xlab("OML Tasks")
  g = g + scale_x_continuous(limits = c(1, nrow(df)))
  print(g)
  dev.off()

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
percMaxAux = function(data, tasks, prefix = NULL) {

  ids = which(data$NumberOfClasses == 2)
  binary.tasks = unique(data[ids, "task.id"])
  multic.tasks = unique(data[-ids, "task.id"])

  bin.rows = which(rownames(tasks) %in% binary.tasks)
  mlt.rows = which(rownames(tasks) %in% multic.tasks)
  tasks = tasks[c(mlt.rows, bin.rows),]

  old.tasks = tasks
  tasks[is.na(tasks)] = 0
  tasks$task = gsub("OpenML-Task-", "", rownames(tasks))
  rownames(tasks) = NULL

  df = melt(tasks, id.vars=ncol(tasks))
  colnames(df)[2] = "alg"
  colnames(df)[3] = "percentage"
  df$task = as.factor(df$task)

  setEPS() 
  postscript(paste0("plots/", prefix, "perc_performance.eps"), height=11, width=10)
  g = ggplot(df, aes(x=alg, y=task, fill=percentage))
  g = g + geom_tile() 
  g = g + scale_fill_gradient(low="white", high="black")
  g = g + scale_y_discrete(breaks = FALSE)
  g = g + theme(text = element_text(size=10), axis.text.y = element_blank(), 
    axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  g = g + ylab("OpenML Tasks") + xlab("Learners")
  g = g + ggtitle("Percentage of the maximum performance overall OpenML tasks")
  print(g)
  dev.off()

  # Heatmap with only binary tasks
  tasks.bin = tasks[bin.rows,]
  df.bin = melt(tasks.bin, id.vars=ncol(tasks.bin))
  colnames(df.bin)[2] = "alg"
  colnames(df.bin)[3] = "percentage"
 
  setEPS() 
  postscript(paste0("plots/", prefix, "binary_perc_performance.eps"))
  g1 = ggplot(df.bin, aes(x=alg, y=task, fill=percentage))
  g1 = g1 + geom_tile()
  g1 = g1 + scale_fill_gradient(low="white", high="black")
  g1 = g1 + theme(text = element_text(size=10), axis.text.y = element_blank(), 
    axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  g1 = g1 + scale_y_discrete(breaks = FALSE)
  g1 = g1 + ylab("OpenML Tasks") + xlab("Learners")
  g1 = g1 + ggtitle("Percentage of the maximum performance overall OpenML tasks")
  print(g1)
  dev.off()

  # Heatmap with only multiclass tasks only
  tasks.mlt = tasks[mlt.rows,]
  df.mlt = melt(tasks.mlt, id.vars=ncol(tasks.mlt))
  colnames(df.mlt)[2] = "alg"
  colnames(df.mlt)[3] = "percentage"
 
  setEPS() 
  postscript(paste0("plots/", prefix, "multiclass_perc_performance.eps"))
  g2 = ggplot(df.mlt, aes(x=alg, y=task, fill=percentage))
  g2 = g2 + geom_tile() 
  g2 = g2 + scale_fill_gradient(low="white", high="black")
  g2 = g2 + theme(text = element_text(size=10), axis.text.y = element_blank(), 
    axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  g2 = g2 + scale_y_discrete(breaks = FALSE)
  g2 = g2 + ylab("OpenML Tasks") + xlab("Learners")
  g2 = g2 + ggtitle("Percentage of the maximum performance overall OpenML tasks.")
  print(g2)
  dev.off()

  # Boxplot for this measure
  df.tasks = old.tasks
  df.tasks$task = gsub("OpenML-Task-", "", rownames(df.tasks))
  rownames(df.tasks) = NULL

  df.perc = melt(df.tasks, id.vars = ncol(df.tasks))
  colnames(df.perc)[2] = "algo"
  
  # setEPS() 
  # postscript(paste0("plots/", prefix, "perc_performance_boxplot.eps"), height=4.5, width=9)
  g3 = NULL
  g3 = ggplot(data = df.perc, mapping = aes(x = as.factor(algo), y = value))
  g3 = g3 + geom_boxplot(outlier.colour = "black", outlier.size = 1)
  g3 = g3 + scale_y_continuous(limits = c(0, 1))
  g3 = g3 + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  g3 = g3 + ylab(paste0("% of the maximum ", gsub("_", "", prefix))) + xlab("Learners")
  # print(g3)
  # dev.off()

  return(g3)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

percMaxBests = function(mat){

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

percMaximumPerformance = function(data, mat.acc, mat.auc, mat.comp) {

  tasks.acc  = percMaxBests(mat.acc)
  tasks.auc  = percMaxBests(mat.auc)
  tasks.comp = percMaxBests(mat.comp)
  
  # calling auxiliary plots
  r1 = percMaxAux(data = data, tasks = tasks.acc, prefix = "acc_")
  r2 = percMaxAux(data = data, tasks = tasks.auc, prefix = "auc_")
  r3 = percMaxAux(data = data, tasks = tasks.comp, prefix = "auc_run_")

  setEPS() 
  postscript(paste0("plots/perc_performance_boxplot.eps"))
  ml = gridExtra::grid.arrange(r1, r2, ncol = 1, nrow = 2)
  dev.off()

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
 
  df.p = melt(temp, id.vars=4)
  colnames(df.p)[2] = "Measure"
  
  setEPS() 
  postscript(paste0("plots/perc_performance.eps"), height=6, width=8)
  g = NULL
  g = ggplot(data=df.p, aes(x=alg, y=value, group=Measure, colour=Measure, linetype=Measure, shape=Measure)) 
  g = g + geom_line() + geom_point() 
  g = g + guides(fill=FALSE)
  g = g + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  g = g + scale_y_continuous(limits = c(0.0, 1))
  g = g + scale_colour_brewer(palette="Set2")
  # g = g + ggtitle("Average percentage from the maximum performance value overall OpenML tasks.")
  g = g + ylab("% of max. performance") + xlab("mlr Learners")
  print(g)
  dev.off()  


}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
# coveragePlots(alg.coverage, task.coverage)
coveragePlots = function(temp, temp2) {

  aux = temp[order(temp$cov_alg, decreasing=TRUE),]
  aux$alg = factor(aux$alg, levels=aux$alg)

  setEPS()
  postscript("plots/alg_coverage_percentage.eps", height=4.5, width=10)
  g = NULL
  g = ggplot(data=aux, aes(x=alg, y=cov_alg, colour=1, group=1)) 
  g = g + geom_line() + geom_point()
  g = g + guides(fill=FALSE)
  g = g + guides(colour=FALSE)
  g = g + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  g = g + scale_y_continuous(limits = c(0, 1))
  g = g + ggtitle("Learners' coverage overall OpenML tasks")
  g = g + ylab("% of the total tasks") + xlab("mlr Learners")
  print(g)
  dev.off()


  aux2 = temp2[order(temp2$cov_task, decreasing=TRUE),]
  aux2$task = factor(aux2$task, levels=aux2$task)
  aux2$task = as.numeric(aux2$task)

  setEPS()
  postscript("plots/task_coverage_percentage.eps", height=4.5, width=10)
  g = NULL
  g = ggplot(data=aux2, aes(x=task, y=cov_task, colour=2, group=1)) 
  g = g + geom_line() + geom_point()
  g = g + guides(fill=FALSE)
  g = g + guides(colour=FALSE)
  g = g + scale_y_continuous(limits = c(0, 1))
  g = g + scale_x_continuous(limits = c(1, nrow(aux2)))
  g = g + ggtitle("OpenML tasks' coverage over all learners")
  g = g + ylab("% of the number of learners") + xlab("OpenML tasks")
  print(g)
  dev.off()

}

#--------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------

averagePerformance = function(data, rk.full) {

  algos = unique(data$algo)
  aux = lapply(algos, function(alg) {
    ids = which(data$algo == alg)
    acc = mean(data[ids, "predictive.accuracy"])

    real.ids = which(!is.na(data[ids, "area.under.roc.curve"]))
    auc = mean(data[ids, "area.under.roc.curve"][real.ids])
        
    auc_run = mean(data[ids, "area.under.roc.curve"][real.ids] - 
      ( log(1 + data[ids, "training.time"][real.ids] 
       + data[ids, "testing.time"][real.ids])) * 0.1)
        
    ret = c(acc, auc, auc_run)
    return(ret)
  })
  
  aux = data.frame(do.call("rbind", aux))
  aux$alg = algos
  colnames(aux) = c("avg_acc", "avg_auc", "avg_auc_run", "alg")
 
  aux = aux[order(aux$avg_auc, decreasing = TRUE), ]
  aux$alg = factor(aux$alg, levels = aux$alg)
  df1 = melt(aux, id.vars = 4)
  colnames(df1)[2] = "Measure"

  # Plot 1
  g1 = NULL
  g1 = ggplot(data=df1, aes(x=alg, y=value, group=Measure, colour=Measure, linetype=Measure, shape=Measure)) 
  g1 = g1 + geom_line() + geom_point() 
  g1 = g1 + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  g1 = g1 + scale_y_continuous(limits = c(0, 1))
  g1 = g1 + ylab("average value") + xlab("mlr Learners")
 

  df2 = rk.full[order(rk.full$rk_auc, decreasing = FALSE), ]
  df2$alg = factor(df2$alg, levels = df2$alg)
  df2 = melt(df2, id.vars = 1) 
  colnames(df2)[2] = "Measure"

  # Plot 2
  g2 = NULL
  g2 = ggplot(data=df2, aes(x=alg, y=value, group=Measure, colour=Measure, linetype=Measure, shape=Measure)) 
  g2 = g2 + geom_line() + geom_point() 
  g2 = g2 + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  g2 = g2 + scale_colour_brewer(palette="Dark2")
  g2 = g2 + ylab("average ranking") + xlab("mlr Learners")


  setEPS()
  postscript("plots/average_performance_two_plots.eps")
  p = grid.arrange(g1, g2)
  dev.off()

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

timingPlot = function(data) {

  temp = getRuntime(data)
  aux = temp[order(temp$total, decreasing=FALSE),]
  aux$alg = factor(aux$alg, levels=aux$alg)

  aux.final = melt(aux, id.vars=4)   
  colnames(aux.final) = c("alg", "time", "value")

  setEPS()
  postscript("plots/runtime_analysis.eps", height=4.5, width=10)
  g = NULL
  g = ggplot(data=aux.final, aes(x=alg, y=log(value), colour=time, group=time, linetype=time, shape=time))
  g = g + geom_line() + geom_point()
  g = g + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  g = g + ggtitle("Average runtime of the learners")
  g = g + ylab("Average time (log scale)") + xlab("mlr Learners")
  print(g)
  dev.off()

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

runtimeBoxplot = function(data) {

  df = data.frame(data)
  temp = df[, c("algo", "training.time", "testing.time")]
  temp$runtime = temp$training.time + temp$testing.time
  
  setEPS()
  postscript("plots/runtime_boxplot.eps", height=4.5, width=9)
  g = ggplot(data = temp, mapping = aes(x = as.factor(algo), y = log(runtime)))
  g = g + geom_boxplot(outlier.colour = "black", outlier.size = 1)
  g = g + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  g = g + ylab("log(Runtime)") + xlab("mlr Learners")
  g = g + ggtitle("Learners' performance on OpenML tasks")
  print(g)
  dev.off()

}


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

boxplotByAlg = function(data) {

  df = data.frame(data)
  temp = df[ ,c("algo", "area.under.roc.curve")]
  colnames(temp)[2] = "auc"
 
  # AUC boxplot
  bp1 = ggplot(data = temp, mapping = aes(x = as.factor(algo), y = auc))
  bp1 = bp1 + geom_boxplot(outlier.colour = "black", outlier.size = 1)
  bp1 = bp1 + scale_y_continuous(limits = c(0, 1))
  bp1 = bp1 + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  bp1 = bp1 + ylab("auc") + xlab("")
  bp1 = bp1 + ggtitle("Learners' performance on OpenML tasks")

  temp2 = df[, c("algo", "predictive.accuracy")]
  colnames(temp2)[2] = "acc"

  # ACC boxplot
  bp2 = ggplot(data = temp2, mapping = aes(x = as.factor(algo), y = acc))
  bp2 = bp2 + geom_boxplot(outlier.colour = "black", outlier.size = 1) 
  bp2 = bp2 + scale_y_continuous(limits = c(0, 1))
  bp2 = bp2 + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  bp2 = bp2 + ylab("predictive accuracy") + xlab("mlr Learners") 
  
  # saving composed boxplot (auc and acc)
  setEPS()
  postscript("plots/performance_boxplot.eps")
  ml = gridExtra::grid.arrange(bp1, bp2, ncol = 1, nrow = 2)
  dev.off()
 
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

plotRankingRows = function(rk, data, k = 5, prefix = NULL){

  data.runtime = getRuntime(data)
  all.learners = unique(data$algo)
  mat = rk$rk

  # k-best algorithms
  aux = lapply(1:nrow(mat), function(i){
    algos.ids = which(!is.na(mat[i,]))
    temp = mat[i, algos.ids]
    ret = sort(temp, decreasing = FALSE)

    if( length(algos.ids) == 1) {
      alg.names = colnames(mat)[algos.ids]
    } else{
      alg.names = names(ret)
    }

    obj = c(alg.names, rep(x=NA, times=(length(all.learners) - length(temp))))
    return(obj)
  })

  aux = data.frame(do.call("rbind", aux))
  
  #list of k-best
  temp = lapply(1:k, function(i){
    return(table(factor(aux[,i], levels = all.learners)))
  })

  # Df with the best learners
  rk.df = data.frame(do.call("cbind", temp))
  colnames(rk.df) = c(1:ncol(rk.df))
  rk.df$alg = rownames(rk.df)
  rownames(rk.df) = NULL

  # adding runtime on the data frame
  rk.df$runtime = 0
  for(i in 1:nrow(rk.df)){
    id = which(data.runtime$alg == rk.df$alg[i])
    if(length(id) != 0) {
      value = data.runtime$total[id]
      rk.df$runtime[i] = value
    }
  }

  df = melt(rk.df, id.vars = c(ncol(rk.df) - 1, ncol(rk.df)))
  colnames(df) = c("learner", "runtime", "rank", "value")
  df = df[which(df$value != 0),]
  
  old.prefix = prefix
  setEPS()
  postscript(paste0("plots/", old.prefix , "rank_counter.eps"), height=10, width=7)
  prefix = gsub("-", "", prefix)
  prefix = gsub("_", " ", prefix)

  g = ggplot(data = df, aes(x=learner, y=value, fill=runtime)) 
  g = g + geom_bar(position="dodge",stat="identity") + guides(fill=FALSE)
  g = g + scale_y_continuous(limits = c(0, max(df$value))) + facet_grid(rank ~ .)
  g = g + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) 
  g = g + ggtitle(paste0(prefix, "learners on OpenML tasks.")) + ylab("Occurences") + xlab("mlr Learner")
  g = g + scale_fill_gradient(high="red", low="grey40")
  print(g)
  dev.off()

  # Percentage version
  df2 = df
  df2$value = df2$value / length(unique(data$task.id))
  
  setEPS()
  postscript(paste0("plots/", old.prefix, "rank_percentage.eps"), height=10, width=7)
 
  g = ggplot(data = df2, aes(x=learner, y=value, fill=runtime)) 
  g = g + geom_bar(position="dodge",stat="identity") + guides(fill=FALSE)
  g = g + scale_y_continuous(limits = c(0, max(df2$value))) + facet_grid(rank ~ .)
  g = g + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) 
  g = g + ggtitle(paste0(prefix, "learners on OpenML tasks.")) + ylab("Occurences") + xlab("mlr Learner")
  g = g + scale_fill_gradient(high="red", low="grey40")
  print(g)
  dev.off()

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------