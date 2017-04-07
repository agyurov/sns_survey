# functions

# dev.set(dev.list()[???]])
#
unique.data = function(df){
  return(lapply(df,unique))
}

# plot.matrix
plot.matrix = function(x,...){
  image(t(apply(x,2,rev)),...)
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
my.barplot = function(df,margins=NULL, nas, ...){
  df = df[,!sapply(df,is.numeric)]
  if(1 %in% dim(df) | 0 %in% dim(df)){
    return()
  }
  if(is.null(margins)) margins<- -1
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
  legend(par("usr")[2]+dist(par("usr")[1:2])/50,mean(par("usr")[3:4]),
         names(x[[which.max(unlist(lapply(x,length)))]]),
         as.character(levels(df[,1])),bty="n",xpd=NA,
         fill = grey.colors(max(unlist(lapply(x,length)))),yjust=.5)
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

# goood stuff
.BucketEnv = new.env()
bucket = function(...,add = F,env = .BucketEnv,short=T,rmv=F){
  # Exit if add=F and empty bucket
  if(!add & length(as.list(env))==0 & !rmv){
    cat("Empty bucket, nothing to find here.\n")
    return(invisible(NULL))
  }
  # display items if add=F and !empty bucket
  if(!add & length(as.list(env))!=0 & length(as.character(as.list(substitute(list(...)))[-1]))==0 & !rmv){
    cat("In storage: \n")
    if(short){
      print(lapply(as.list(env),class))
    }
    if(!short){
      print(as.list(env))
    }
    return(invisible(NULL))
  }
  arg = as.character(as.list(substitute(list(...)))[-1])
  
  # Check for existence
  out = arg %in% ls(env)
  # -----------------------------------------
  # Return from bucket
  if(!add & !rmv){
    if(!all(out)){
      cat(paste0(paste0(arg[!out],collapse=", ")," not in the bucket. \n"))
      return(invisible(NULL))
    }
    if(all(out)){
      cat(paste0("Returning ",paste0(arg[out],collapse = ", "),".\n"))
      return(as.list(env)[arg[out]])
    }
    
  }
  # -----------------------------------------
  # Add to bucket
  if(add & !rmv){
    # Overwriting
    if(any(out)){
      for(i in arg[out]){
        ans = readline(paste0(i," already exist in bucket. Overwrrite? y/n: \n"))
        if(ans == "y"){
          assign(i,eval(parse(text = i)),envir = env)
          cat(paste0(i," added to the bucket!\n"))
        }
        if(ans == "n"){}
        if(!ans %in% c("y","n")){
          warning("Exitted function - answer me! ")
        }
      }
    }
    # Not overwriting, just adding
    for(i in arg[!out]){
      if(exists(i,envir = .GlobalEnv)){
        assign(i,eval(parse(text = i)),envir = env)
        cat(paste0(i," added to the bucket!\n"))
      }
      if(!exists(i,envir = .GlobalEnv)){
        cat(paste0("Object ",i," does not exist!\n"))
      }
    }
  }
  # Remove items from bucket
  if(rmv){
    for(i in arg){
      if(!all(out)){
        cat(paste0(paste0(arg[!out],collapse=", ")," not in the bucket. \n"))
      }
      if(all(out)){
        ans = readline(paste0("Are you sure you want to remove ",i," from the bucket. y/n: "))
        if(ans == "y"){
          rm(list=i,envir = env)
        }
      }
    }
  }
}

# save variables to data_file.RData
record = function(file){
  save.image(paste0(getwd(),"/",deparse(substitute(file)),".RData"))
}

# data frame factor to numeric
fact2num = function(y){
  return(do.call(cbind.data.frame,lapply(y[,unlist(lapply(y,is.factor))],function(x) as.numeric(as.character(x)))))
}

# Brute force FA with with pca and pval evaluation
brute.force.fa = function(x){
  nfac = sum(prcomp(x)$sdev > 1)
  nfac = min(floor(ncol(x)/2) - 1,nfac)
  fa.list = list()
  for(i in 1:nfac){
    fa.list[[i]]  = factanal(x,factors = i)
    names(fa.list)[i] = paste0(i,"factors")
    if(is.null(fa.list[[i]]$PVAL) || fa.list[[i]]$PVAL < .05){
      fa.list[[i]] = NULL
    }
  }
  return(fa.list[!sapply(fa.list,is.null)])
}