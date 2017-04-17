# analysis Q10


# brute force factor analysis ---------------------------------------------

fa.list = lapply(df.list2num,brute.force.fa)

# Q8 exploratoin ----------------------------------------------------------
plot.na(q10,main="Missing data in Q8")
my.barplot.old(q10,main="Aggregates of Q8")
q10fa = brute.force.fa(q10nonanum,scores="regression",rotation="varimax")
q10fa = factanal(q10nonanum,factors=3, scores="regression", rotation="varimax")
plot.matrix(q10fa,cutoff=.33,col=grey.colors(2),main="Cutoff .33")
lapply(q8nonanum,table)


# Q10 & Q11 ---------------------------------------------------------------

q10q11 = cbind(q10,q11)
q10q11nona = na.omit(q10q11)
q10q11nonanum = fact2num(q10q11nona)

q10q11fa = brute.force.fa(q10q11nonanum)
