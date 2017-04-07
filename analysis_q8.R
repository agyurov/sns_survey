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


# Hypothesis A: got got friends and family -> play games alone
q8q19 = cbind(q8, q19)
q8q19nona = na.omit(q8q19)
q8q19nonanum = fact2num(q8q19nona)
q8q19fa = brute.force.fa(q8q19nonanum)
