# functions


#
unique.data = function(df){
  return(lapply(df,unique))
}

# proper positions for text on plots
my.par = function(side,scale){
  environment(globalenv())
  side.opt = c("below","left","above","right")
  side.print = paste(side.opt,1:4,collapse=", ")
  if(!side %in% side.opt & !side %in% 1:4){
    print(paste("side must be on of: ",side.print,collapse=" "))
  }
  p = par("usr")
  if(side == "below" | side == 1){
    x = mean(p[1:2])
    y = p[3] - dist(p[3:4])*scale
  }
  if(side == "above" | side == 3){
    x = mean(p[1:2])
    y = p[4] + dist(p[3:4])*scale
  }
  #
  if(side == "left" | side == 2){
    y = mean(p[3:4])
    x = p[1] - dist(p[1:2])*scale
  }
  
  if(side == "right" | side == 4){
    y = mean(p[3:4])
    x = p[2] + dist(p[1:2])*scale
  }
  return(list(x=x,y=y))
}

# plot NA
plot.na = function(df,col=NULL,side=NULL,scale=NULL,...){
  df = is.na(df)
  # image(t(apply(df,2,rev)),col=grey.colors(2),yaxt="n",xaxt="n",xlab="rows",ylab="columns")
  if(is.null(col)){col=grey.colors(2)}
  image(t(apply(df,2,rev)),col = col,...)
}


# for(i in 1:length(full_questions_names)){
#   print(unique(full_questions_names)[i])
#   writeClipboard(unique(full_questions_names)[i])
#   readline("continue?")
# }

my.count = function(x){
  y = unique(x)
  out = list()
  for(i in 1:length(y)){
    out[i] = sum(x %in% y[i])
  }
  return(unlist(out))
}