#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getMaxPerfSinglePlotByMeasure = function(data, tasks, chart = "boxplot", 
  prefix = NULL, landscape = FALSE) {

  if(chart %nin% c("boxplot", "heatmap", "violin")) {
    stopf(" - Invalid type of plot: choose between \'boxplot\', \'heatmap\' or \'violin\'.\n")
  }

  ids = which(data$NumberOfClasses == 2)
  binary.tasks = unique(data[ids, "task.id"])
  bin.rows = which(rownames(tasks) %in% binary.tasks)

  multic.tasks = unique(data[-ids, "task.id"])
  mlt.rows = which(rownames(tasks) %in% multic.tasks)
  tasks = tasks[c(mlt.rows, bin.rows), ]
 
  if(chart != "boxplot") {
    tasks[is.na(tasks)] = 0
  }

  tasks$task = gsub("OpenML-Task-", "", rownames(tasks))
  rownames(tasks) = NULL

  df = melt(tasks, id.vars = ncol(tasks))
  colnames(df)[2] = "algo"

  g = NULL
  if(chart == "heatmap") {

    df$task = as.factor(df$task)
    colnames(df)[3] = "percentage"

    g = ggplot(df, aes(x = task, y = as.factor(algo), fill=percentage, colour=percentage))
    g = g + geom_tile() 
    g = g + scale_fill_gradient(low = "white", high = "black")
    g = g + scale_colour_gradient(low = "white", high = "black")
    g = g + scale_x_discrete(breaks = FALSE)
    g = g + theme(text = element_text(size = 10), axis.text.x = element_blank()) 
    g = g + xlab("Tasks") + ylab("Algorithms")
    g = g + ggtitle("Percentage of the maximum performance over all the tasks")

  } else if (chart == "boxplot") {

    g = ggplot(data = df, mapping = aes(x = as.factor(algo), y = value))
    g = g + stat_boxplot(geom ='errorbar')
    g = g + geom_boxplot(outlier.colour = "black", outlier.size = 0.5)
    g = g + theme(text = element_text(size = 10), 
      axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
    g = g + ylab(paste0("% of the maximum ", gsub("_", "", prefix))) + xlab("Algorithms")
    g = g + scale_y_continuous(limits = c(0, 1))
   
  } else {
    
    g = ggplot(data = df, mapping = aes(x = as.factor(algo), y = value))
    g = g + geom_violin(trim = TRUE, fill = '#FFCCFF')
    g = g + geom_boxplot(outlier.colour = "black", outlier.size = 0.5, width = 0.1)
     g = g + scale_y_continuous(limits = c(0, 1))
    g = g + theme(text = element_text(size = 10), 
      axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
    g = g + ylab(paste0("% of the maximum ", gsub("_", "", prefix))) + xlab("Algorithms")

  }

  if(landscape) {
      g = g + coord_flip()
  }
  
  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
