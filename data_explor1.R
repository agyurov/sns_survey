# Data exploration part 1

df.list = list()
j = 0
for(i in 1:length(qlen)){
  j = j + 1
  df.list[[j]] = as.data.frame(df2[,grepl(paste0("q",i,"."), names(df2),fixed=T)])
  names(df.list[[j]]) = names(df2)[grepl(paste0("q",i,"."), names(df2),fixed=T)]
}
names(df.list) = paste0("q",1:length(qlen))
df.list = lapply(df.list,as.data.frame)


# Relevel all questions

df.list = lapply(df.list,rename.level)
df.list.na = lapply(df.list,rename.level,include.na=T)
list2env(df.list,envir=.GlobalEnv)


par(mfrow=c(2,2))
for(i in 1:length(df.list.na)){
  if(ncol(df.list.na[[i]]) == 1) plot(df.list.na[[i]][,1],main=paste0("q",i,"\n",unique(full_questions_names)[i]),ylab="")
  else my.barplot(df.list.na[[i]],margins = 1,
                  main=paste0("q",i,"\n",unique(full_questions_names)[i]))
}
