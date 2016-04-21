#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRunResultsByTag = function(tag) {

  if(is.null(tag)) {
    stop("You should specifiy a tag to get your OpenML runs!")
  }

  cat(" - Getting your data from OpenML \n")
  results  = listOMLRunEvaluations(tag = tag)
  time.ids = grep("time", colnames(results))
  has.time.measures = length(time.ids) > 0
  
  datasets = listOMLDataSets()
  flows    = listOMLFlows()

  fields = c("run.id", "task.id", "flow.id", "predictive.accuracy", "area.under.roc.curve")
  if(has.time.measures) {
    fields = c(fields, "usercpu.time.millis.training", "usercpu.time.millis.testing")
  }
  
  # data set for analysis
  temp = results[, fields]

  # Getting algorithm name from flows
  aux = lapply(1:nrow(temp), function(i) {
    
    # Get algorithms name from flow data frame
    id = which(flows$flow.id == temp$flow.id[i])
    if(length(id) == 0) {
      algo = NA
    } else {
      algo = flows$name[id]
    }

    # Get dataset information from datasets list
    task = getOMLTask(task.id = temp$task.id[i])
    data.id = which(datasets$did == task$input$data.set$desc$id)

    data.fields = c("did","name","NumberOfClasses", "NumberOfInstances", 
      "NumberOfFeatures", "MajorityClassSize")

    ret = cbind(algo, datasets[data.id, data.fields])
    return (ret)
  })

  data.temp = do.call("rbind", aux)
  temp = cbind(temp, data.temp)
  temp$perMajClass = temp$MajorityClassSize / temp$NumberOfInstances

  # Renaming coluns
  colnames(temp)[grep("did", colnames(temp))] = "dataset.id"
  colnames(temp)[grep("name", colnames(temp))] = "dataset.name"
  
  if(has.time.measures) {
    colnames(temp)[grep("usercpu.time.millis.training", colnames(temp))] = "training.time"
    colnames(temp)[grep("usercpu.time.millis.testing", colnames(temp))] = "testing.time"
  } else {
    temp$training.time = 0
    temp$testing.time = 0
  }

  cat(" - Returning your final data frame\n")

  return(temp)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------