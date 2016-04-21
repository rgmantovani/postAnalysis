#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRuntime = function(data) {

  df = data.frame(data)
  temp = df[, c("algo", "training.time", "testing.time")]
  temp$total.time = temp$training.time + temp$testing.time

  # [alg | avg train | avg test | avg total]
  algos = unique(temp$algo)
  aux = lapply(algos, function(alg) {
    d = temp[which(data$algo == alg),]
    ret = c(mean(d$training.time), mean(d$testing.time), mean(d$total.time))
    return(ret)
  })

  temp = data.frame(do.call("rbind", aux))
  temp$alg = algos
  colnames(temp) = c("training", "testing", "total", "alg")

  return(temp)
}


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
