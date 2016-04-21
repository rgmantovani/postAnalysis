#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

library("ggplot2")
library("reshape2")
library("gridExtra")  
library("mlr")
library("OpenML")

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

loadAll = function() {

  files = list.files(path="R", pattern=".R$", full.names=TRUE)
  for(file in files) {
    source(file)
  }

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------


main = function() {
  
  loadAll()

  dir = "plots/"
  if(!dir.exists(dir)) {
    catf(" - Creating dir:", dir)
    dir.create(dir)
  }

  # data from file (study-7)
  filename = "data/full_data_results.RData"
  load(filename)

  # data from OpenML
  # data = getRunsResults(tag = "randomBot")

  # AUC must be numeric
  data$area.under.roc.curve = as.numeric(as.character(data$area.under.roc.curve))
  
  # Removing duplicated tasks (different tasks (ids) but they have the same setup (data, split) )
  ids = which(duplicated(data[ ,c("algo", "predictive.accuracy", "area.under.roc.curve", "dataset.id", "dataset.name")]))
  if(length(ids) > 0) {
    data = data[-ids, ]
  }
  
  # Generate results
  cat(" # Results Analysis: \n")
  doTheAnalysis(data = data, k = 5)
  
  cat(" All done !\n")

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

main()

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------