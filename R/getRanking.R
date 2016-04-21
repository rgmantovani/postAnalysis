#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRanking = function(mat) {

  temp = mat
  for(i in 1:nrow(mat)) {
    ids =  which(!is.na(mat[i,]))
    temp[i,ids] = rank(1 - mat[i,ids])
  }

  # mean ranking by the measure
  aux = lapply(1:ncol(temp), function(i) {
    ids = which(!is.na(temp[,i]))
    return( mean(temp[ids,i]) )
  })

  rk.mean = data.frame(do.call("rbind", aux))
  rk.mean$alg = colnames(mat)
  colnames(rk.mean)[1] = "rk.mean"
  rk.mean = rk.mean[, c(2,1)]

  ret = list(rk = temp, rk.mean = rk.mean)
  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------