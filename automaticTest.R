#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# Description: Data Analysis with results direct from OpenML

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

automaticTest = function() {

  devtools::load_all()
  required_packages = c("devtools", "ggplot2", "reshape2", "gridExtra", "mlr", 
    "dplyr", "farff", "OpenML")
 
  checkPackages(pkgs = required_packages)

  # TODO: more than one tag
  # data = getExperimentsData(tag = "study_7", numRuns = 18000)
  data = getExperimentsData(tag = "study_14")
 
  # getting performance matrices for different performance measures
  mat.acc       = getPerfMatrix(data = data, measure = "predictive.accuracy")
  mat.auc       = getPerfMatrix(data = data, measure = "area.under.roc.curve")
  mat.f1        = getPerfMatrix(data = data, measure = "f.measure")
  mat.kappa     = getPerfMatrix(data = data, measure = "kappa")
  mat.runtime   = getPerfMatrix(data = data, measure = "usercpu.time.millis")
  mat.precision = getPerfMatrix(data = data, measure = "precision")
  mat.recall    = getPerfMatrix(data = data, measure = "recall")

  # performance weighted by the runtime
  mat.acc.w = getPerfMatrix(data = data, measure = "predictive.accuracy", weighted = TRUE)
  mat.auc.w = getPerfMatrix(data = data, measure = "area.under.roc.curve", weighted = TRUE)

  #------------------------------------------------
  # Tasks info plot
  #------------------------------------------------

  cat("\t@Plot: tasks info \n")
  tasks.plot = getTasksInfoPlot(data = data)  

  #------------------------------------------------
  # Performances plots 
  #------------------------------------------------

  acc.boxplot = getBoxPlot(data = data, measure = "predictive.accuracy")
  # getBoxPlot(data = data, measure = "usercpu.time.millis")
  # getBoxPlot(data = data, measure = "usercpu.time.millis.training")
  # getBoxPlot(data = data, measure = "usercpu.time.millis.testing")
  # getBoxPlot(data = data, measure = "f.measure")
  # getBoxPlot(data = data, measure = "kappa")
  

  acc.violin = getViolinPlot(data, measure = "predictive.accuracy")
  # getViolinPlot(data, measure = "area.under.roc.curve")
  # getViolinPlot(data, measure = "f.measure")
  # getViolinPlot(data, measure = "usercpu.time.millis")

  #------------------------------------------------
  # Runtime average line plot
  #------------------------------------------------
  runtime.data = getAvgRuntimeData(data = data)
  time.line = getRuntimeLinePlot(runtime.data = runtime.data)


  # ranking frequency
  cat(" - Generating rankings: \n")
  rk.acc   = getRanking(mat.acc, descending = TRUE)
  rk.auc   = getRanking(mat.auc, descending = TRUE) 
  # rk.f1    = getRanking(mat.f1, descending = TRUE)
  # rk.acc.w = getRanking(mat.acc.w, descending = TRUE) 
  # rk.time  = getRanking(mat.runtime, descending = FALSE)
 

  rk.fr1 = getRankFrequencyPlot(rk = rk.auc,  data = data, version = "percentage")
  rk.fr2 = getRankFrequencyPlot(rk = rk.auc, data = data, version = "counter")
 
  rk.hm = getRankingHeatMap(data = rk.acc$rk)
  # getRankingHeatMap(data = rk.time$rk)
  # getRankingHeatMap(data = rk.acc$rk)
  # getRankingHeatMap(data = rk.auc$rk)
  # getRankingHeatMap(data = rk.f1$rk)


  #------------------------------------------------
  # Maximum performance plots
  #------------------------------------------------

  scaled.mat.acc = scaleMatrix(mat = mat.acc)
  # scaled.mat.auc = scaleMatrix(mat = mat.auc)
  # scaled.mat.runtime = scaleMatrix(mat = mat.runtime)

  getMatrixBoxPlot(mat = scaled.mat.acc,    prefix = "predictive accuracy")
  getMatrixViolinPlot(mat = scaled.mat.acc, prefix = "predictive accuracy", landscape = TRUE)
  getMatrixHeatMap(mat = scaled.mat.acc,    prefix = "predictive accuracy")

  # Combined Plots - Performances plots 
  #------------------------------------------------

  measures.list = list("predictive.accuracy", "area.under.roc.curve", "f.measure")
  g1 = getAlgosAvgPerfLinePlot(data = data, measures.list = measures.list)
  g2 = getAlgosAvgRankLinePlot(data = data, measures.list = measures.list)

  # g3 = gridExtra::arrangeGrob(g1, g2, ncol = 1, nrow = 2) # To save in file
  gm = gridExtra::grid.arrange(g1, g2, ncol = 1, nrow = 2)


  matrices.list = list(mat.acc, mat.auc, mat.kappa, mat.f1, mat.precision, mat.recall) 
  measures.names = c("acc", "auc", "kappa", "f1", "precision", "recall")  
  g = getPercMaxPerfLinePlot(matrices.list = matrices.list, measures.names = measures.names)

  
  # ggsave(plot   = g, file   = paste0(filename, ".eps"), height = height, width  = width)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------



