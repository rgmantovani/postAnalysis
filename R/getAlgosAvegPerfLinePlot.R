#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getAlgosAvegPerfLinePlot = function(data, rk.full) {

  algos = unique(data$algo)
  aux = lapply(algos, function(alg) {
    ids = which(data$algo == alg)
    acc = mean(data[ids, "predictive.accuracy"])

    real.ids = which(!is.na(data[ids, "area.under.roc.curve"]))
    auc = mean(data[ids, "area.under.roc.curve"][real.ids])
        
    auc_run = mean(data[ids, "area.under.roc.curve"][real.ids] 
      - ( log(1 + data[ids, "training.time"][real.ids] 
       + data[ids, "testing.time"][real.ids])) * 0.1)
        
    ret = c(acc, auc, auc_run)
    return(ret)
  })

  aux = data.frame(do.call("rbind", aux))
  aux$alg = algos
  colnames(aux) = c("avg_acc", "avg_auc", "avg_auc_run", "alg")
 
  aux = aux[order(aux$avg_auc, decreasing = TRUE), ]
  aux$alg = factor(aux$alg, levels = aux$alg)
  df1 = melt(aux, id.vars = 4)
  colnames(df1)[2] = "Measure"

  g1 = ggplot(data = df1, aes(x = alg, y = value, group = Measure, colour = Measure, 
    linetype = Measure, shape = Measure)) 
  g1 = g1 + geom_line() + geom_point() 
  g1 = g1 + theme(text = element_text(size=10),
   axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  g1 = g1 + scale_y_continuous(limits = c(0, 1))
  g1 = g1 + ylab("Average value") + xlab("Algorithms")
 

  df2 = rk.full[order(rk.full$rk_auc, decreasing = FALSE), ]
  df2$alg = factor(df2$alg, levels = df2$alg)
  df2 = melt(df2, id.vars = 1) 
  colnames(df2)[2] = "Measure"

  # Plot 2 (average rankings)
  g2 = NULL
  g2 = ggplot(data=df2, aes(x=alg, y = value, group = Measure, colour = Measure, 
    linetype = Measure, shape = Measure)) 
  g2 = g2 + geom_line() + geom_point() 
  g2 = g2 + theme(text = element_text(size=10), 
    axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  g2 = g2 + scale_colour_brewer(palette="Dark2")
  g2 = g2 + ylab("Average ranking") + xlab("Algorithms")

 double.g = gridExtra::arrangeGrob(g1, g2, ncol = 1, nrow = 2)
  return(double.g)
}


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
