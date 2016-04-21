#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
# # coveragePlots(alg.coverage, task.coverage)
# coveragePlots = function(temp, temp2) {

#   aux = temp[order(temp$cov_alg, decreasing=TRUE),]
#   aux$alg = factor(aux$alg, levels=aux$alg)

#   setEPS()
#   postscript("plots/alg_coverage_percentage.eps", height=4.5, width=10)
#   g = NULL
#   g = ggplot(data=aux, aes(x=alg, y=cov_alg, colour=1, group=1)) 
#   g = g + geom_line() + geom_point()
#   g = g + guides(fill=FALSE)
#   g = g + guides(colour=FALSE)
#   g = g + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
#   g = g + scale_y_continuous(limits = c(0, 1))
#   g = g + ggtitle("Learners' coverage overall OpenML tasks")
#   g = g + ylab("% of the total tasks") + xlab("mlr Learners")
#   print(g)
#   dev.off()


#   aux2 = temp2[order(temp2$cov_task, decreasing=TRUE),]
#   aux2$task = factor(aux2$task, levels=aux2$task)
#   aux2$task = as.numeric(aux2$task)

#   setEPS()
#   postscript("plots/task_coverage_percentage.eps", height=4.5, width=10)
#   g = NULL
#   g = ggplot(data=aux2, aes(x=task, y=cov_task, colour=2, group=1)) 
#   g = g + geom_line() + geom_point()
#   g = g + guides(fill=FALSE)
#   g = g + guides(colour=FALSE)
#   g = g + scale_y_continuous(limits = c(0, 1))
#   g = g + scale_x_continuous(limits = c(1, nrow(aux2)))
#   g = g + ggtitle("OpenML tasks' coverage over all learners")
#   g = g + ylab("% of the number of learners") + xlab("OpenML tasks")
#   print(g)
#   dev.off()

# }

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
