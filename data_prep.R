# data preparation



# Fix column names --------------------------------------------------------

df0 = readWorksheet(loadWorkbook("SNSUseDataV1_stataVars.xls"),sheet=1)

# Rename casually repeated question name
names(df0)[grep("How.strongly.do.you.agree.or.disagree.with.the.following.statements.",names(df0))] =
  "How.strongly.do.you.agree.or.disagree.with.the.following.statements part2"

# Insert casually missing questions
df0left = df0[,1:grep("Since.you.first.started",names(df0))]
df0left = cbind(df0left,if.you.woke.up.tomorrow.and.fount.out.that = as.character(rep(NA,nrow(df0))))
df0right = df0[,(grep("Since.you.first.started",names(df0))+1):ncol(df0)]
df0right = cbind(df0right,if.you.have.any.comments.or.questions.about = as.character(rep(NA,nrow(df0))))
df0 = cbind(df0left,df0right)



# first move 1st three columns to the end....
firstthree = df0[,1:3]
df0 = df0[,-c(1:3)]
df0 = cbind(df0,firstthree)




names(df0) = tolower(names(df0))
names_strp_digits = gsub("[[:digit:]]","",names(df0))

# Rpeat question across Cols
namez = !names_strp_digits %in% "col"
full_questions = names_strp_digits

for(i in 2:ncol(df0)){
  if(is.na(full_questions[i])) break
  if(full_questions[i] == "col"){
    full_questions[i] = full_questions[i-1]
  }
}

# Idiotic unexplainable mistake in the last question

# Punctuation to underscore
full_questions = gsub("\\.$","",full_questions)
full_questions = gsub("\\.+"," ",full_questions)
full_questions_names = full_questions

# How many subquestions are there?
qlen = my.count(full_questions)
rownames(qlen) = NULL
q_numbers = rep(1:length(qlen),qlen)
long_questions = unlist(lapply(qlen,function(x)1:x))
question_numbers  = paste0("q",q_numbers,".",long_questions)
full_questions = paste0(question_numbers,"_",full_questions)

# Actual data names from 2nd row
x = df0[1,]
x = as.character(x)
x[(length(x)-2):length(x)] = names(df0)[(length(x)-2):length(x)]
shortnames = x
x = paste0(question_numbers,"_",x)
x = tolower(x)

# Set new data frame
df1 = df0
df1 = df1[-1,]
names(df1) = x
names(df1)

# Finally rename the added missing questions.....
names(df1)[grep("q14.1",names(df1))] = paste("q14.1_IfYouWokeUpTomorrowAndFoundOutThat")
names(df1)[grep("q31.1",names(df1))] = paste("q31.1_ifYouHaveAnyCommentsOrQuestions")

# convert the two missing columns temporarily to characters
df1[,grep("q14.1",names(df1))] = as.character(df1[,grep("q14.1",names(df1))])
df1[,grep("q31.1",names(df1))] = as.character(df1[,grep("q31.1",names(df1))])

# Check variable class ----------------------------------------------------

df2 = df1
str(df2)

# chr to ordered factor
chr2fctr = unlist(lapply(df2,is.character))

df2 = do.call(cbind.data.frame,lapply(df2[,unlist(lapply(df2,is.character))],function(x) factor(x,ordered=T)))
df2$q33.1_startdate= as.Date(df2$q33.1_startdate)
df2$q3.1_friendnum = as.numeric(df2$q3.1_friendnum)


# Dictionary ------------------------------------------------------------

questions = as.list(unique(full_questions_names))
names(questions) =paste0("q",1:length(unique(full_questions_names)),"_",unique(full_questions_names))

loc.shortnames = shortnames
for(i in 1:length(questions)){
  questions[[i]] = loc.shortnames[1:qlen[i]]
  loc.shortnames = loc.shortnames[-c(1:qlen[i])]
}


# Add to the bucket -------------------------------------------------------


bucket(full_questions_names, question_numbers,add=T)



# Examination of NAs ------------------------------------------------------

# NA pattern not dependent on snsusefreq
na.rows = apply(df2,1,function(x)sum(is.na(x)))
table(na.rows)
table(df2$q2.1_snsusefreq)
df3 = df2[df2$q2.1_snsusefreq != "1",]

# NA pattern not correlating with other variables
dfna = df2
dfna$nas = apply(df2,1,function(x)sum(is.na(x)))
dfna$nas
plot(dfna$nas)
focor = do.call(cbind.data.frame,lapply(dfna,function(x)as.numeric(as.character(x))))
cor.mat = cor(focor,use ="pairwise.complete.obs" )
plot.matrix(cor.mat,col=grey.colors(100,0,1))

# NA pattern in columns
na.cols = apply(df2,2,function(x)sum(is.na(x)))
plot.na(df2)



# add question data frames to environment ---------------------------------

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
df.list = lapply(df.list,rename.level,ordered = F)
df.list.na = lapply(df.list,rename.level,include.na=T, ordered = F)
list2env(df.list,envir=.GlobalEnv)


# factor data frames w/o NAs
df.list2 = df.list
names(df.list2) = paste0(names(df.list),"nona")
# how many complete rows for each question set
df.list2 = lapply(df.list2,na.omit)
unlist(lapply(df.list2,function(x)dim(x)[1]))
list2env(df.list2,envir=.GlobalEnv)

# numeric data frames w/o NAs 
df.list2num = list()
df.list2num = df.list2[which(qlen!=1)]
df.list2num[names(df.list2num)[which(unlist(lapply(df.list2num,function(x)dim(x)[1])) == 0)]] = NULL
df.list2num = lapply(df.list2num,fact2num)
names(df.list2num) = paste0(names(df.list2num),"num")
list2env(df.list2num,envir = .GlobalEnv)

# Demographic daa
names(df2)
demo.dat = df2[,115:124]
names(demo.dat)
