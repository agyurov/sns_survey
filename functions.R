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
plot.na = function(df,col=NULL,...){
  df = is.na(df)
  if(is.null(col)){col=grey.colors(2)}
  image(t(apply(df,2,rev)),col=grey.colors(2),yaxt="n",xaxt="n",xlab="",ylab="",...)
  # title("Data image")
  p = my.par(3,.2)
  legend(p$x,p$y,
         xjust=.5,yjust=.5,c("Missing data"),xpd=NA,bty="n",fill=grey.colors(2)[2],cex=1.25)
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

# proper barplot
my.barplot = function(df,margins=NULL, ...){
  df = df[,!sapply(df,is.numeric)]
  if(1 %in% dim(df) | 0 %in% dim(df)){
    return()
  }
  ifelse(is.null(margins),margins<- -1,margins <-margins)
  par(mar=.pardefault$mar + margins)
  # layout(matrix(1:2,nrow=2,byrow=F),height=c(8,1))
  x = lapply(df,table)
  ps = barplot(as.matrix(x[[1]]),width=1/length(x),xlim=c(0,1),col=grey.colors(max(unlist(lapply(x,length)))),...)
  for(i in 2:length(x)){
    par(new=T)
    ps2 = barplot(as.matrix(x[[i]]),bty="n",yaxt="n",xaxt="n",xlim=c(0,1),space=(i-1)+.1,width=1/length(x),
                  col=grey.colors(max(unlist(lapply(x,length)))))
    ps = c(ps,ps2)
  }
  title()
  text(x=ps,y=par("usr")[3]-dist(par("usr")[3:4])/10,labels=names(df),xpd=NA,srt=45,col=2,cex=.75)
  # The proper method
  legend(par("usr")[2]+dist(par("usr")[1:2])/50,mean(par("usr")[3:4]),
         names(x[[which.max(unlist(lapply(x,length)))]]),
         levels(df[,1]),bty="n",xpd=NA,
         fill = rev(grey.colors(max(unlist(lapply(x,length))))),yjust=.5)
}

# rename levels to max level
rename.level = function(df,include.na = NULL){
  if(ncol(df)<2 & !is.factor(df[,1])){
    return(df)
  }
  max.level = which.max(unlist(lapply(df,function(x) length(levels(x)))))
  new.level = levels(df[,max.level])
  if(!is.null(include.na)){
    df = lapply(df,function(x,y) factor(as.character(x),new.level,ordered = T),y=new.level)
    df = do.call(cbind.data.frame,lapply(df,addNA))
  }
  if(is.null(include.na)){
    df = lapply(df,function(x,y) factor(as.character(x),new.level,ordered = T),y=new.level)
    df = do.call(cbind.data.frame,df)
  }
  
  return(df) 
}

