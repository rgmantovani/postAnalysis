#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getAvgRuntimeData = function(data) {

  temp = dplyr::select(.data = data, flow.name, usercpu.time.millis.training, 
    usercpu.time.millis.testing, usercpu.time.millis)

  algos = unique(temp$flow.name)
  aux = lapply(algos, function(alg) {
    d = temp[which(temp$flow.name == alg),]
    d = na.omit(d)
    ret = colMeans(d[,2:ncol(d)])
    return(ret)
  })

  temp = data.frame(do.call("rbind", aux))
  temp$alg = algos
  return(temp)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
