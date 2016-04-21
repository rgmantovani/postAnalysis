#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getMatrix = function(data, column = "predictive.accuracy"){

  cat(paste0("\t@ building ",column, " matrix \n"))

  all.learners = unique(data$algo)
  all.tasks = unique(data$task.id)

  mat = matrix(data = NA, nrow = length(all.tasks), ncol = length(all.learners), 
    dimnames = list(all.tasks, all.learners))
  
  # Matrices with the specified measure (by the column name specified)
  for(i in 1:nrow(data)) { 
    row.id = which(all.tasks == data[i,]$task.id)
    col.id = which(all.learners == data[i,]$algo)
    mat[row.id, col.id] = data[i, column]
  }
  
  # Removing algs with no execution (not being applied on all tasks)
  uniquelength = sapply(data.frame(mat), function(x) length(unique(x)))
  mat = subset(data.frame(mat), select = uniquelength > 1)
 
  return(mat)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getMatrixAucRuntime = function(data, w = 0.1){

  cat(paste0("\t@ building auc.weighted.by.runtime matrix \n"))

  all.learners = unique(data$algo)
  all.tasks = unique(data$task.id)

  mat = matrix(data = NA, nrow = length(all.tasks), ncol = length(all.learners), 
    dimnames = list(all.tasks, all.learners))
  
  temp = data
  temp$total.time.log = log(1 + temp$training.time + temp$testing.time)

  # Matrices with the specified measure (by the id arg)
  for(i in 1:nrow(temp)) { 
    row.id = which(all.tasks == temp[i,]$task.id)
    col.id = which(all.learners == temp[i,]$algo)
    mat[row.id, col.id] = temp$area.under.roc.curve[i] - (temp$total.time.log[i] * w)
  }
  
  # Removing algs with no execution (not being applied on all tasks)
  uniquelength = sapply(data.frame(mat), function(x) length(unique(x)))
  mat = subset(data.frame(mat), select = uniquelength > 1)
  mat[mat < 0] = 0
  
  return(mat)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------