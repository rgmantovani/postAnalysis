#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getAlgosPerformanceBoxplot = function(data) {

  df = data.frame(data)
  temp = df[ ,c("algo", "area.under.roc.curve")]
  colnames(temp)[2] = "auc"
 
  # AUC boxplot
  bp1 = ggplot(data = temp, mapping = aes(x = as.factor(algo), y = auc))
  bp1 = bp1 + geom_boxplot(outlier.colour = "black", outlier.size = 1)
  bp1 = bp1 + scale_y_continuous(limits = c(0, 1))
  bp1 = bp1 + theme(text = element_text(size=10), 
    axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  bp1 = bp1 + ylab("auc") + xlab("")
  bp1 = bp1 + ggtitle("Learners' performance overall tasks")

  temp2 = df[, c("algo", "predictive.accuracy")]
  colnames(temp2)[2] = "acc"

  # ACC boxplot
  bp2 = ggplot(data = temp2, mapping = aes(x = as.factor(algo), y = acc))
  bp2 = bp2 + geom_boxplot(outlier.colour = "black", outlier.size = 1) 
  bp2 = bp2 + scale_y_continuous(limits = c(0, 1))
  bp2 = bp2 + theme(text = element_text(size=10), 
    axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
  bp2 = bp2 + ylab("predictive accuracy") + xlab("Learners") 
  
  # saving composed boxplot (auc and acc)
  double.bp = gridExtra::arrangeGrob(bp1, bp2, ncol = 1, nrow = 2)

  return(double.bp) 
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------