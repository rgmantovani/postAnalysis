#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

doTheAnalysis = function(data, k = 5) {

  # Making matrixes
  cat(" - Generating matrices: \n")
  mat.acc = getMatrix(data, column = "predictive.accuracy")
  mat.auc = getMatrix(data, column = "area.under.roc.curve")
  mat.comp = getMatrixAucRuntime(data, w = 0.1)
 

  cat(" - Generating rankings: \n")
  rk.acc = getRanking(mat.acc)
  rk.auc = getRanking(mat.auc) 
  rk.comp = getRanking(mat.comp) 
  
  # binding average ranking for each measure
  rk.full = data.frame(cbind(rk.acc$rk.mean, rk.auc$rk.mean[,2], rk.comp$rk.mean[,2]))
  colnames(rk.full) = c("alg", "rk_acc", "rk_auc", "rk_auc_run")
  
  # Calling data plots
  cat(" - Generating plots: \n")

  cat("\t@Plot: tasks info \n")
  task.plot = getTasksInfoPlot(data = data)  
  savePlotInEpsFile(g = task.plot, filename = "plots/DatasetsInfoPerformance", 
    height = 5, width = 8)


  cat("\t@Plot: Runtime plots \n")
  if(all(data$training.time == 0)) {
    cat(" \t   - Warning: no runtime - skipping \n")
   }else{

    runtime.bar.plot = getRuntimeBarPlot(data = data)
    savePlotInEpsFile(g = runtime.bar.plot, filename = "plots/RuntimeBarPlot", 
      height = 4.5, width = 10)

    runtime.boxplot = getRuntimeBoxPlot(data)
    savePlotInEpsFile(g = runtime.boxplot, filename = "plots/RuntimeBoxplot", 
      height = 4.5, width = 9)
  }

  
  cat("\t@Plot: Performances boxplot \n")
  algos.boxplot = getAlgosPerfBoxplot(data = data)
  savePlotInEpsFile(g = algos.boxplot, filename = "plots/AlgosPerformanceBoxplot")
 

  cat("\t@Plot: average peformance plots (rankings, accuracy) \n")
  algos.avg.plot = getAlgosAvegPerfLinePlot(data = data, rk.full = rk.full)
  savePlotInEpsFile(g = algos.avg.plot, filename = "plots/AlgosPerformanceLinePlot")


  cat("\t@Plot: ranking frequency \n")
  # percentage version
  rk.acc.perc.plot = getRankFrequencyPlot(rk = rk.acc, data = data, k = 5, version = "percentage")
  savePlotInEpsFile(g = rk.acc.perc.plot, filename = "plots/RankFrequencyAccPercentage", 
    height = 10, width = 7)

  # counter version
  rk.acc.count.plot = getRankFrequencyPlot(rk = rk.acc, data = data, k = 5, version = "counter")
  savePlotInEpsFile(g = rk.acc.count.plot, filename = "plots/RankFrequencyAccCounter", 
    height = 10, width = 7)

  # auc
  rk.auc.perc.plot = getRankFrequencyPlot(rk = rk.auc, data = data, k = 5, version = "percentage")
  savePlotInEpsFile(g = rk.acc.perc.plot, filename = "plots/RankFrequencyAucPercentage", 
    height = 10, width = 7)

  # auc - runtimme
  rk.comp.perc.plot = getRankFrequencyPlot(rk = rk.comp, data = data, k = 5, version = "percentage")
  savePlotInEpsFile(g = rk.acc.perc.plot, filename = "plots/RankFrequencyAucRuntimePercentage", 
    height = 10, width = 7)


  cat("\t@Plot: % of the maximum performance \n")
  
  # boxplot (double) - Auc and acc
  double.boxplot = getPercMaxPerfBoxplot(data, mat.acc, mat.auc)
  savePlotInEpsFile(g = double.boxplot, filename = "plots/PercMaxPerformanceBoxplot")

  # if I want a single boxplot for one measure (acc, for example)
  tasks.acc  = percMaxPerfAux(mat.acc)
  single.boxplot.acc = getMaxPerfByMeasure(data, tasks.acc, chart = "boxplot", prefix = "acc")
  savePlotInEpsFile(g = single.boxplot.acc, filename = "plots/PercMaxPerformanceBoxplotAccSigle",
    height = 5, width = 8)

  # for auc
  tasks.auc  = percMaxPerfAux(mat.auc)
  single.boxplot.auc = getMaxPerfByMeasure(data, tasks.auc, chart = "boxplot", prefix = "auc")
  savePlotInEpsFile(g = single.boxplot.auc, filename = "plots/PercMaxPerformanceBoxplotAucSigle",
    height = 5, width = 8)
  
  # If I want the heatmap for a measure
  single.heatmap.acc = getMaxPerfByMeasure(data, tasks.acc, chart = "heatmap", prefix = "acc")
  savePlotInEpsFile(g = single.heatmap.acc, filename = "plots/PercMaxPerformanceHeatmapAcc",
    height = 5, width = 8)

  # for the auc
  single.heatmap.auc = getMaxPerfByMeasure(data, tasks.auc, chart = "heatmap", prefix = "auc")
  savePlotInEpsFile(g = single.heatmap.auc, filename = "plots/PercMaxPerformanceHeatmapAuc",
    height = 5, width = 8)

  # line plot (both measures)
  max.perf.plot = getPercentageMaxPerfPlot(data, mat.acc, mat.auc, mat.comp)
  savePlotInEpsFile(g =  max.perf.plot, filename = "plots/PercentageMaximumPerformanceLinePlot", 
    height = 4, width = 8)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
