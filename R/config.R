#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# OpenML version that works
# devtools::install_github("openml/r", ref = "05b8b97cc5ce6ea1b3f586818cfcf157b16a3cd4")

library('ggplot2')
library('reshape2')
library('gridExtra')  
library('mlr')
library('OpenML')
library('dplyr')
library('checkmate')

AVAILABLE.MEASURES = c("f.measure", "kappa", "mean.absolute.error", "precision", "recall", 
  "usercpu.time.millis", "area.under.roc.curve", "predictive.accuracy", "root.mean.squared.error")

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
