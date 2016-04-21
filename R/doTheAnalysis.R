#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

doTheAnalysis = function(data, k = 5) {

  cat(" - Generating coverage: \n")
  cat("\t@ Algorithm coverage: done \n")
  alg.coverage = coverageAlg(data)
  cat("\t@ Task coverage: done \n")
  task.coverage = coverageTask(data)
 
  # Making matrixes
  cat(" - Generating matrices: \n")
  
  mat.acc = getMatrix(data, column = "predictive.accuracy")
  cat("\t@ Accuracy matrix: done \n")

  mat.auc = getMatrix(data, column = "area.under.roc.curve")
  cat("\t@ AUC matrix: done \n")

  mat.comp = getMatrixAucRuntime(data, w = 0.1)
  mat.comp[mat.comp < 0] = 0
  cat("\t@ AUC-log(runtime) matrix: done \n")
 

  cat(" - Generating rankings: \n")
  cat("\t@ Acc ranking: done \n")
  rk.acc = getRanking(mat.acc)
  
  cat("\t@ AUC ranking: done \n")
  rk.auc = getRanking(mat.auc) 
  
  cat("\t@ AUC-log(runtime) ranking: done \n")
  rk.comp = getRanking(mat.comp) 
  
  rk.full = data.frame(cbind(rk.acc$rk.mean, rk.auc$rk.mean[,2], rk.comp$rk.mean[,2]))
  rk.full[,2] = as.numeric(as.character(rk.full[,2]))
  rk.full[,3] = as.numeric(as.character(rk.full[,3]))
  rk.full[,4] = as.numeric(as.character(rk.full[,4]))
  colnames(rk.full) = c("alg", "rk_acc", "rk_auc", "rk_auc_run")
  
  # Calling data plots
  cat(" - Generating plots: \n")

  cat("\t@Plot: tasks info \n")
  tasksInfoPlot(data)  

  cat("\t@Plot: timing plot \n")
  if(all(data$training.time == 0)) {
    cat(" \t   - Warning: no runtime - skipping \n")
  }else{
    timingPlot(data)
    runtimeBoxplot(data)
  }
  
  cat("\t@Plot: auc boxplot \n")
  boxplotByAlg(data)

  cat("\t@Plot: algorithm coverage \n")
  coveragePlots(alg.coverage, task.coverage)
 
  cat("\t@Plot: average peformance plots (rankings, accuracy) \n")
  averagePerformance(data = data, rk.full = rk.full)

  cat("\t@Plot: ranking frequency \n")
  plotRankingRows(rk.auc, data, k = k,  prefix = "best_auc_")
  plotRankingRows(rk.acc, data, k = k,  prefix = "best_acc_")
  plotRankingRows(rk.comp, data, k = k, prefix = "best_auc_run_")

  cat("\t@Plot: % of the maximum performance \n")
  percMaximumPerformance(data, mat.acc, mat.auc, mat.comp)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
