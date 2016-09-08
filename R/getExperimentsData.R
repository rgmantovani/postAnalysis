#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getExperimentsData = function(tag, numRuns) {

  if(is.null(tag)) {
    stop("You should specifiy a tag to get your OpenML runs!")
  }

  if(numRuns < 1) {
    stop("You should specifiy a positive integer number of runs.")
  }

  # getting run results from OpenML
  results = do.call("rbind", 
    lapply(0:floor(numRuns/10000), function(i) {
      return(listOMLRunEvaluations(tag = tag, limit = 10000, offset = (10000 * i) + 1))
    })
  )

  sub.datasets = dplyr::select(.data = listOMLDataSets(), data.id, name, NumberOfInstances, 
    NumberOfFeatures, NumberOfClasses, MajorityClassSize)

  # getting dataset information from results
  aux = lapply(1:nrow(results), function(i) {
    task = getOMLTask(task.id = results$task.id[i])
    data.id = which(sub.datasets$data.id == task$input$data.set$desc$id)
    return (sub.datasets[data.id,])
  })

  data.info = do.call("rbind", aux)
  temp = cbind(results, data.info)
  temp$perMajClass = temp$MajorityClassSize / temp$NumberOfInstances
  temp$flow.name = sub("\\(.*", "", temp$flow.name)
  
  return(temp)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------