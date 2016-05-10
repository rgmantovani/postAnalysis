#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# Description: Data Analysis with results direct from OpenML

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

analysisFromOpenML = function() {

  devtools::load_all()
  subdir = "plots/openml_data/"
  if(!dir.exists(subdir)) {
    cat(" - Creating dir:", subdir, "\n")
    dir.create(subdir)
  }

  # data = getRunResultsByTag(tag = "randomBot") #defaults
  load(file = "data/small_set.RData")

  cat(" # Results Analysis: \n")
  cat(" - Generating matrices: \n")
  mat.acc  = getMatrix(data, column = "predictive.accuracy")
  mat.auc  = getMatrix(data, column = "area.under.roc.curve")
  mat.comp = getMatrixAucRuntime(data, w = 0.1)

  temp = data
  temp$runtime = data$training.time + data$testing.time
  mat.time = getMatrix(temp, column = "runtime")
 
  cat(" - Generating rankings: \n")
  rk.acc  = getRanking(mat.acc,  descending = TRUE)
  rk.auc  = getRanking(mat.auc,  descending = TRUE) 
  rk.comp = getRanking(mat.comp, descending = TRUE) 
  rk.time = getRanking(mat.time, descending = FALSE)
  
  # binding average ranking for each measure (rk with the 3 different measures)
  rk.full = data.frame(cbind(rk.acc$rk.mean, rk.auc$rk.mean[,2], rk.comp$rk.mean[,2]))
  colnames(rk.full) = c("alg", "rk_acc", "rk_auc", "rk_auc_run")
  
  # Calling data plots
  cat(" - Generating plots: \n")

  cat("\t@Plot: tasks info \n")
  task.plot = getTasksInfoPlot(data = data)  
  savePlotInEpsFile(g = task.plot, filename = paste0(subdir,"DatasetsInfoPerformance"), 
    height = 5, width = 8)

  cat("\t@Plot: Runtime plots \n")
  if(all(data$training.time == 0)) {
    cat(" \t   - Warning: no runtime - skipping \n")
  } else {

    #------------------------------------------------
    # Runtime Plots
    #------------------------------------------------

    runtime.line.plot = getRuntimeLinePlot(data = data)
    savePlotInEpsFile(g = runtime.line.plot, filename = paste0(subdir,"RuntimeLinePlot"), 
      height = 4.5, width = 10)

    runtime.boxplot.land = getRuntimeBoxplot(data = data, landscape = TRUE)
    savePlotInEpsFile(g = runtime.boxplot.land, filename = paste0(subdir,"RuntimeBoxplotLandscape"), 
      height = 8, width = 5)

    runtime.boxplot = getRuntimeBoxplot(data = data, landscape = FALSE)
    savePlotInEpsFile(g = runtime.boxplot, filename = paste0(subdir,"RuntimeBoxplot"), 
      height = 5, width = 9)

    runtime.heatmap = getRankingHeatMap(temp = rk.time$rk)
    savePlotInEpsFile(g = runtime.heatmap, filename = paste0(subdir,"RuntimeRankingHeatmap"), 
      height = 6, width = 11)
  }

  #------------------------------------------------
  # Performances plots 
  #------------------------------------------------

  cat("\t@Plot: Performances Violin and Box Plots \n")
  acc.viol = getAlgosViolinPlot(data = data, measure = "predictive.accuracy", landscape = FALSE)
  savePlotInEpsFile(g = acc.viol, filename = paste0(subdir,"AccAlgosViolinPlot"),
    height = 5, width = 12)

  acc.viol.land = getAlgosViolinPlot(data = data, measure = "predictive.accuracy", landscape = TRUE)
  savePlotInEpsFile(g = acc.viol.land, filename = paste0(subdir,"AccAlgosViolinPlotLandscape"),
    height = 10, width = 5)

  # not good !
  auc.viol = getAlgosViolinPlot(data = data, measure = "area.under.roc.curve", landscape = FALSE)
  savePlotInEpsFile(g = auc.viol, filename = paste0(subdir,"AucAlgosViolinPlot"),
      height = 5, width = 12)

  # not good !
  auc.viol.land = getAlgosViolinPlot(data = data, measure = "area.under.roc.curve", landscape = TRUE)
  savePlotInEpsFile(g = auc.viol.land, filename = paste0(subdir,"AucAlgosViolinPlotLandscape"),
      height = 10, width = 5)

  acc.box = getAlgosBoxPlot(data = data, measure = "predictive.accuracy", landscape = FALSE)
  savePlotInEpsFile(g = acc.box, filename = paste0(subdir,"AccAlgosBoxPlot"),
    height = 5, width = 12)

  acc.box.land = getAlgosBoxPlot(data = data, measure = "predictive.accuracy", landscape = TRUE)
  savePlotInEpsFile(g = acc.box.land, filename = paste0(subdir,"AccAlgosBoxPlotLandscape"),
    height = 10, width = 5)

  auc.box = getAlgosBoxPlot(data = data, measure = "area.under.roc.curve", landscape = FALSE)
  savePlotInEpsFile(g = auc.box, filename = paste0(subdir,"AucAlgosBoxPlot"),
    height = 5, width = 12)

  auc.box.land = getAlgosBoxPlot(data = data, measure = "area.under.roc.curve", landscape = TRUE)
  savePlotInEpsFile(g = auc.box.land, filename = paste0(subdir,"AucAlgosBoxPlotLandscape"),
    height = 10, width = 5)

  cat("\t@Plot: Average Peformance plots (rankings, accuracy) \n")
  algos.avg.plot = getAlgosAvegPerfLinePlot(data = data, rk.full = rk.full)
  savePlotInEpsFile(g = algos.avg.plot, filename = paste0(subdir,"AlgosPerformanceLinePlot"))


  #------------------------------------------------
  # Ranking Frequency plot
  #------------------------------------------------

  cat("\t@Plot: ranking frequency \n")
  # percentage version
  rk.acc.perc.plot = getRankFrequencyPlot(rk = rk.acc, data = data, k = 5, version = "percentage")
  savePlotInEpsFile(g = rk.acc.perc.plot, filename = paste0(subdir,"RankFrequencyAccPercentage"), 
    height = 10, width = 7)

  # counter version
  rk.acc.count.plot = getRankFrequencyPlot(rk = rk.acc, data = data, k = 5, version = "counter")
  savePlotInEpsFile(g = rk.acc.count.plot, filename = paste0(subdir,"RankFrequencyAccCounter"), 
    height = 10, width = 7)

  # auc
  rk.auc.perc.plot = getRankFrequencyPlot(rk = rk.auc, data = data, k = 5, version = "percentage")
  savePlotInEpsFile(g = rk.acc.perc.plot, filename = paste0(subdir,"RankFrequencyAucPercentage"), 
    height = 10, width = 7)

  # auc - runtimme
  rk.comp.perc.plot = getRankFrequencyPlot(rk = rk.comp, data = data, k = 5, version = "percentage")
  savePlotInEpsFile(g = rk.acc.perc.plot, filename = paste0(subdir,"RankFrequencyAucRuntimePercentage"), 
    height = 10, width = 7)

  #------------------------------------------------
  # Percentage of the Maximum Performance plots 
  #------------------------------------------------

  cat("\t@Plot: % of the maximum performance \n")
  
  # boxplot (double) - Auc and acc
  double.boxplot = getPercMaxPerfDoubleBoxplot(data = data, mat.acc = mat.acc, mat.auc = mat.auc)
  savePlotInEpsFile(g = double.boxplot, filename = paste0(subdir,"PercMaxPerformanceDoubleBoxplot"))

  # if I want a single boxplot for one measure (acc, for example)
  tasks.acc  = getMaxPerfMatrix(mat.acc)

  single.boxplot.acc = getMaxPerfSinglePlotByMeasure(data, tasks.acc, chart = "boxplot", prefix = "acc", 
    landscape = FALSE)
  savePlotInEpsFile(g = single.boxplot.acc, filename = paste0(subdir,"PercMaxPerfBoxplotAcc"),
    height = 4, width = 10)

  single.violin.acc = getMaxPerfSinglePlotByMeasure(data, tasks.acc, chart = "violin", prefix = "acc", 
    landscape = FALSE)
  savePlotInEpsFile(g = single.violin.acc, filename = paste0(subdir,"PercMaxPerfViolinPlotAcc"),
    height = 4, width = 10)

  # for auc ...
  tasks.auc  = getMaxPerfMatrix(mat.auc)
  single.boxplot.auc = getMaxPerfSinglePlotByMeasure(data, tasks.auc, chart = "boxplot", prefix = "auc")
  savePlotInEpsFile(g = single.boxplot.auc, filename = paste0(subdir,"PercMaxPerfBoxplotAuc"),
    height = 5, width = 8)
  
  # If I want the heatmap for a measure
  single.heatmap.acc = getMaxPerfSinglePlotByMeasure(data, tasks.acc, chart = "heatmap", prefix = "acc")
  savePlotInEpsFile(g = single.heatmap.acc, filename = paste0(subdir,"PercMaxPerfHeatmapAcc"),
    height = 6, width = 11)

  # for the auc
  single.heatmap.auc = getMaxPerfSinglePlotByMeasure(data, tasks.auc, chart = "heatmap", prefix = "auc")
  savePlotInEpsFile(g = single.heatmap.auc, filename = paste0(subdir,"PercMaxPerfHeatmapAuc"),
    height = 6, width = 11)

  # line plot (both measures)
  max.perf.plot = getPercMaxPerfLinePlot(data, mat.acc, mat.auc, mat.comp)
  savePlotInEpsFile(g =  max.perf.plot, filename = paste0(subdir,"PercMaxPerfLinePlot"), 
    height = 4, width = 8)

  cat(" All done !\n")

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

analysisFromOpenML()

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------