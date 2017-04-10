# Analysis


# brute force factor analysis ---------------------------------------------

fa.list = lapply(df.list2num,brute.force.fa)

# Q8 exploratoin ----------------------------------------------------------
plot.na(q8,main="Missing data in Q8")
my.barplot.old(q8,main="Aggregates of Q8")
q8fa = brute.force.fa(q8nonanum,scores="regression",rotation="varimax")
plot.matrix(q8fa$`3factors`,main = "Minimum factors from Q8")
lapply(q8nonanum,table)


# Models for FA(Y) --------------------------------------------------------
# Predictor set 1: X in Q7
# q8 factor model
# Create combined data set with predicor
q7q8nona = na.omit(cbind(q7,q8))
q7q8nonanum = fact2num(q7q8nona)
# factor analyse Y, accounted for NAs in both X and Y
q8fa7 = brute.force.fa(q7q8nonanum[,grepl("q8",names(q7q8nonanum))],scores="regression",rotation="varimax")

y = q8fa7$`3factors`$scores
x = q7q8nona[,grepl("q7",names(q7q8nona))]
fk = model.list(x=x,y=y)
par(mfrow=c(3,3))
lapply(fk,eval.model)
# Predictor set2 



# Models for Y ------------------------------------------------------------


