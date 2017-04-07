# Analysis


# brute force factor analysis ---------------------------------------------

df.list2num = list()
df.list2num = df.list2[which(qlen!=1)]
df.list2num[names(df.list2num)[which(unlist(lapply(df.list2num,function(x)dim(x)[1])) == 0)]] = NULL
df.list2num = lapply(df.list2num,fact2num)
names(df.list2num) = paste0(names(df.list2num),"num")
list2env(df.list2num,envir = .GlobalEnv)

fa.list = lapply(df.list2num,brute.force.fa)
# list2env(fa.list,envir = .GlobalEnv)

# Q8 ----------------------------------------------------------------------
# Q8. FA
fa.list$q8nonanum
layout(matrix(1:9,nrow=3,byrow=T))
lapply(fa.list$q8nonanum,plot.matrix,col=grey.colors(3))
plot.matrix(factanal(q8nonanum,2),col=grey.colors(3))
plot.matrix(factanal(q8nonanum,4),col=grey.colors(3))
# plot.loadings(factanal(q8nonanum,2),col=3)
# plot.loadings(factanal(q8nonanum,4),col=4)



# Q7-Q8 relation ----------------------------------------------------------

q7q8nonanum = na.omit(fact2num(cbind(q7,q8)))






# Q7 ----------------------------------------------------------------------

fa.list$q7nonanum
lapply(fa.list$q7nonanum,plot.matrix,col=grey.colors(5))

# Hypothesis A: got got friends and family -> play games alone
q8q19 = cbind(q8, q19)
q8q19nona = na.omit(q8q19)
q8q19nonanum = fact2num(q8q19nona)
q8q19fa = brute.force.fa(q8q19nonanum)



# dev.set(max(dev.list()))


