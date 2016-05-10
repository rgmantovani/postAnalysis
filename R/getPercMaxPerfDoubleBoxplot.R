#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getPercMaxPerfDoubleBoxplot = function(data, mat.acc, mat.auc) {

  tasks.acc  = getMaxPerfMatrix(mat.acc)
  tasks.auc  = getMaxPerfMatrix(mat.auc)
  
  # calling auxiliary plots
  r1 = getMaxPerfSinglePlotByMeasure(data = data, tasks = tasks.acc, chart = "boxplot", prefix = "acc")
  r2 = getMaxPerfSinglePlotByMeasure(data = data, tasks = tasks.auc, chart = "boxplot", prefix = "auc")
  
  double.bp = gridExtra::arrangeGrob(r1, r2, ncol = 1, nrow = 2)
  return(double.bp)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
