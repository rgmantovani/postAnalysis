#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getAlgoCoverage = function(data) {
  
  total = length(unique(data$task.id))
  tab = data.frame(cbind(table(data$algo), table(data$algo)/total))
  tab$algo = rownames(tab)
  rownames(tab) = NULL
  colnames(tab) = c("num_algs", "cov_alg", "alg")
  return(tab)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getTaskCoverage = function(data) {
  
  total = length(unique(data$algo))
  tab = data.frame(cbind(table(data$task.id), table(data$task.id)/total))
  tab$task = rownames(tab)
  rownames(tab) = NULL
  colnames(tab) = c("num_algs", "cov_task", "task")
  return(tab)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
