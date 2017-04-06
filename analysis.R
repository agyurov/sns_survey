# Analysis


# brute force factor analysis ---------------------------------------------


df.list2num = list()
df.list2num = df.list2[which(qlen!=1)]
df.list2num[names(df.list2num)[which(unlist(lapply(df.list2num,function(x)dim(x)[1])) == 0)]] = NULL
df.list2num = lapply(df.list2num,fact2num)
names(df.list2num) = paste0(names(df.list2num),"num")
list2env(df.list2num,envir = .GlobalEnv)

fa.list = lapply(df.list2num,function(x)factanal(x,factors=floor(ncol(x)/2-1)))
names(fa.list) = paste0(names(fa.list),"fa")
# list2env(fa.list,envir = .GlobalEnv)

# Q8 ----------------------------------------------------------------------
# Q8a: PCA, FA
# Q8a1: remove outliers - none since scale questions. Normality assumption obv not met.
# scaling not required
q8pca = prcomp(q8nonanum)
summary(q8pca)
q8fa1 = factanal(q8nonanum,factors=3)
q8fa2 = factanal(q8nonanum,factors=4)
lapply(list(q8fa1,q8fa2),loadings)
# Hypothesis A: got got friends and family -> play games alone
q8q19 = cbind(q8,q19)
q8q19nona = na.omit(q8q19)

