# Analysis


# brute force factor analysis ---------------------------------------------

fa.list = lapply(df.list2num,brute.force.fa)

# Q8 exploratoin ----------------------------------------------------------
plot.na(q8,main="Missing data in Q8")
my.barplot.old(q8,main="Aggregates of Q8")
q8fa = brute.force.fa(q8nonanum,scores="regression",rotation="varimax")
q8fa3 = factanal(q8nonanum,factors=3)
q8fa4 = factanal(q8nonanum,factors=4)
q8fa5 = factanal(q8nonanum,factors=5)

plot.matrix(q8fa$`3factors`)
lapply(q8nonanum,table)



# Isolate scale -----------------------------------------------------------
plot.matrix2(q8fa3$loadings,scale=T)
