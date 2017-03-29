# data preparation



# Fix column names --------------------------------------------------------

df0 = readWorksheet(loadWorkbook("SNSUseDataV1_stataVars.xls"),sheet=1)

# Rename casually repeated question name
names(df0)[grep("How.strongly.do.you.agree.or.disagree.with.the.following.statements.",names(df0))] =
  "How.strongly.do.you.agree.or.disagree.with.the.following.statements part2"

# Insert casually missing questions
df0left = df0[,1:grep("Since.you.first.started",names(df0))]
df0left = cbind(df0left,if.you.woke.up.tomorrow.and.fount.out.that = as.character(rep("NA",nrow(df0))))
df0right = df0[,(grep("Since.you.first.started",names(df0))+1):ncol(df0)]
df0right = cbind(df0right,if.you.have.any.comments.or.questions.about = as.character(rep("NA",nrow(df0))))
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
