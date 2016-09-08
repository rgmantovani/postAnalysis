#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getTopAgainsBestPlot = function(mat, k = 2) {

  temp.rank = getRanking(mat = mat, descending = TRUE)
  top = temp.rank$rk.mean[order(temp.rank$rk.mean$rk.mean)[1:k], ]

  best = lapply(1:nrow(mat), function(i){
    ids = which(!is.na(mat[i,]))
    return(max(mat[i, ids]))
  })
  best = do.call("rbind", best)

  col.ids = which(colnames(mat) %in% top$alg)
  perf = mat[,col.ids]
  df = cbind(perf, best)
  df$task = rownames(df)
  df = df[order(df$best, decreasing=TRUE),]
  df$task = factor(df$task, levels = df$task)
  rownames(df) = NULL

  id = ncol(df) - 1
  df = cbind(df[, id], df[, -id])
  colnames(df)[1] = "best"
  df.final = melt(df, id.vars = ncol(df))
  colnames(df.final) = c("Task", "Algorithm", "value")
  df.final$Task = as.numeric(df.final$Task)

  g = ggplot(data = df.final, aes(x = Task, y = log(value), group = Algorithm, colour = Algorithm,
    linetype = Algorithm)) 
  g = g + geom_line() + geom_point(size = 0.2) + scale_colour_brewer(palette = "Dark2")
  g = g + ylab("log(value)") + xlab("Algorithms")
 
  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
