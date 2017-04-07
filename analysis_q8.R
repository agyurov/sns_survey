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
par(mfrow=c(2,1))
lapply(fa.list$q8nonanum,plot.matrix,col=grey.colors(3))
# plot.matrix(factanal(q8nonanum,2),col=grey.colors(3))
plot.matrix(factanal(q8nonanum,4),col=grey.colors(3))
# plot.loadings(factanal(q8nonanum,2),col=3)
# plot.loadings(factanal(q8nonanum,4),col=4)



# Q7-Q8 relation ----------------------------------------------------------
q7q8nona = na.omit(cbind(q7,q8))
q7q8nonanum = na.omit(fact2num(q7q8nona))
plot.matrix(cor(q7q8nonanum),axes=F)
q7q8fa = brute.force.fa(q7q8nonanum)
lapply(q7q8fa,plot.matrix,col=grey.colors(5))
q8predq7 = clm.list(q7q8nona,resp=names(q7),pred=names(q8))
q8predq7accuracy = lapply(q8predq7,function(x) class.pred(x)$percentages)
# Predict Q7 with Q8_PCA!




# TO BE CONTINUED ---------------------------------------------------------


q8pcs = brute.force.pca(q8nonanum)
