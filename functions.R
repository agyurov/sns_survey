# functions

# dev.set(dev.list()[???]])
#
unique.data = function(df){
  return(lapply(df,unique))
}

# plot.matrix
plot.matrix = function(x, cutoff=NULL, ...){
  if(class(x) == "factanal"){
    x = unclass(x$loadings)
    # if(!is.null(cutoff)){
    #   x[x<cutoff] = 0
    #   x[x != 0] = 1
    # }
    image(t(apply(x,2,rev)),axes=F, ...)
    lnx = par("usr")[2]-par("usr")[1]
    lny = par("usr")[4]-par("usr")[3]
    # abline(v = seq(par("usr")[1],par("usr")[2],by=lnx/ncol(x)),col=2)
    
    axis(1,at = lnx/ncol(x)*(0:(ncol(x)-1)),labels=paste("factor",1:ncol(x)),xpd=NA) # 1/2*ln/ncol(x)+
    axis(2,at = lny/nrow(x)*(0:(nrow(x)-1)),labels=rev(rownames(x)),las=1,xpd=NA)
    segments(seq(par("usr")[1],par("usr")[2],by=lnx/ncol(x)),rep(par("usr")[3],ncol(x)),
             seq(par("usr")[1],par("usr")[2],by=lnx/ncol(x)),rep(par("usr")[4],ncol(x)),col=2)
    title(paste0("Cutoff ",cutoff))
    return(invisible(NULL))
  }
  if(!is.null(cutoff)){
    x[x<cutoff] = 0
    col = x
    col[col != 0] = 1
  }
  image(t(apply(x,2,rev)), ...)
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
my.barplot.old = function(df, ...){
  df = df[,!sapply(df,is.numeric)]
  if(1 %in% dim(df) | 0 %in% dim(df)){
    return()
  }
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
rename.level = function(df,include.na = NULL, ordered){
  if(ncol(df)<2 & !is.factor(df[,1])){
    return(df)
  }
  max.level = which.max(unlist(lapply(df,function(x) length(levels(x)))))
  new.level = levels(df[,max.level])
  if(!is.null(include.na)){
    df = lapply(df,function(x,y) factor(as.character(x),new.level,ordered = ordered),y=new.level)
    df = do.call(cbind.data.frame,lapply(df,addNA))
  }
  if(is.null(include.na)){
    df = lapply(df,function(x,y) factor(as.character(x),new.level,ordered = ordered),y=new.level)
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
record = function(file = data_file){
  save.image(file)
}

# data frame factor to numerics
fact2num = function(y){
  return(do.call(cbind.data.frame,lapply(y[,unlist(lapply(y,is.factor))],function(x) as.numeric(as.character(x)))))
}

# Brute force PCA, returning the PCs
brute.force.pca = function(x){
  y = prcomp(x)
  return(y$x[, summary(y)$importance[1,] > 1])
}

# Brute force FA with with pca and pval evaluation
brute.force.fa = function(x,...){
  nfac = sum(prcomp(x)$sdev > 1)
  nfac = min(floor(ncol(x)/2) - 1,nfac)
  fa.list = list()
  for(i in 1:nfac){
    fa.list[[i]]  = factanal(x,factors = i,...)
    names(fa.list)[i] = paste0(i,"factors")
    if(is.null(fa.list[[i]]$PVAL) || fa.list[[i]]$PVAL < .05){
      fa.list[[i]] = NULL
    }
  }
  return(fa.list[!sapply(fa.list,is.null)])
}

# plot factor loadings
plot.loadings = function(x,...){
  y = unclass(x$loadings)
  apply(y,2, my.barplot, namez = rownames(y), ...)
  return(invisible(NULL))
}

# barplot with tilted labels
my.barplot = function(x, namez, ...){
  b = barplot(x, names.arg=NA,...)
  text(y=par("usr")[3],x = b, labels=namez,srt=45,xpd=NA,...)
}

# predicted classifications from CLM
class.pred = function(model){
  tbl = table(model$y,predict(model,type="class")$fit,useNA = "no")
  perc = round(diag(tbl)/rowSums(tbl),2)
  pergroup = rowSums(tbl)
  total = sum(pergroup)
  perc2 = round(perc*pergroup/total,2)
  return(list(table = tbl, percentages = perc,
              pop = pergroup, total = sum(perc2)))
}

# Predicting a questions. df of the form cbind(Q1,Q2)
model.list = function(pred,resp, ...){
  # x predictors
  # y responses
  clms = list()
  pred = as.data.frame(pred)
  resp = as.data.frame(resp)
  
  for(i in 1:ncol(resp)){
    df = cbind(pred,Y=resp[,i])
    frmla = as.formula(paste0("Y"," ~ ",paste0(names(pred),collapse=" + ")))
    # perform LM if numeric
    if(is.numeric(df$Y)){
      clms[[i]] = step(lm(formula=frmla,data=df,...),test="F",trace = 0)
    }
    # perform CLM if ordinal
    if(!is.numeric(df$Y)){
      clms[[i]] = step(clm(formula=frmla,data=df,...),test="Chisq",trace = 0)
    }
    cat(paste0("Estimating model ",i," of ", ncol(resp),"\n"))
  }
  names(clms) = names(resp) 
  return(clms)
}

# model.list + warnings parameter
model.listZ = function(pred,resp,exclude.warnings=F, ...){
  # x predictors
  # y responses
  clms = list()
  pred = as.data.frame(pred)
  resp = as.data.frame(resp)
  
  for(i in 1:ncol(resp)){
    cat(paste0("Estimating model ",i," of ", ncol(resp),"\n"))
    df = cbind(pred,Y=resp[,i])
    frmla = as.formula(paste0("Y"," ~ ",paste0(names(pred),collapse=" + ")))
    # perform LM if numeric
    if(is.numeric(df$Y)){
      clms[[i]] = step(lm(formula=frmla,data=df,...),test="F",trace = 0)
    }
    # perform CLM if ordinal
    if(!is.numeric(df$Y)){
      if(exclude.warnings){
        z <- has_warning(clms[[i]] <- step(clm(formula=frmla,data=df,...),test="Chisq",trace = 0))
        if(z){
          clms[i] = "bad"
          cat(paste0("CLM convergence code 1 for i = ",i,"...\n"))
        }
      }
      if(!exclude.warnings){
        clms[[i]] <- step(clm(formula=frmla,data=df,...),test="Chisq",trace = 0)
      }
    }
  }
  
  names(clms) = names(resp)
  # clms = clms[clms!="bad"]
  return(clms)
}

# Predict pred with resp, w/o overlapping. trims NAs. Wrapper of model.list
model.list.adv = function(pred,resp,...){
  newdf = na.omit(cbind(pred,resp))
  print(paste0("Dim new dat = ",paste0(dim(newdf),collapse=" ")))
  fk = model.listZ(pred = newdf[,!grepl(deparse(substitute(resp)),names(newdf))],
                  resp = newdf[,grepl(deparse(substitute(resp)),names(newdf))],...)
  return(model.list = fk)
}

# Predicting each var in a df
clm.each = function(x,...){
  clms = list()
  j = 0
  for(i in 1:ncol(x)){
    if(!"factor" %in% class(x[,i])){
      next 
    }
    j = j + 1
    cat(paste0("Estimating model ",i," of ", ncol(x),"\n"))
    frmla = as.formula(paste0(names(x)[i],"~."))
    clms[[j]] = step(clm(frmla , data =x,...),trace=0)
    names(clms)[[j]] = names(x)[i]
  }
  # names(clms) = names(x)
  return(clms)
}

# Evaluate model
eval.model = function(x,which = c(1:3, 5),...){
  if(class(x) == "lm"){
    plot(x,which=which,...)
    acf(resid(x),...)
    cat(paste0(lillie.test(resid(x))$p.value,"\n"))
    return(invisible(NULL))
  }
  if(class(x) == "clm"){
    return(class.pred(x))
  }
}

# ordered factor to un-ordered factor. non factors remain the same
ordfactor = function(x, ordered){
  if(is.factor(x)){
    return(factor(x, ordered = ordered))
  }else{
    return(x)
  }
}

# as.unordered applied to data frames
ordfactordf = function(x,ordered){
  return(do.call(cbind.data.frame,lapply(x,ordfactor,ordered = ordered)))
}

# pred.clm, CLM or list of CLMs
plot.clm = function(x,type="l",lwd=3,...){
  if(class(x)!="list")x = list(x)
  if("lm" %in% unlist(lapply(x,class))) {
    return(stop("Please supply CLM objects"))
  }
  plot(rep(1,length(x[[1]]$y)),pch=15,col="grey",bty="n",ylab="",yaxt="n",ylim=c(.8,.8+length(x)*.2),...)
  for(i in 1:length(x)){
    points(rep(1,length(x[[1]]$y)) +.2*i,col="grey",pch=15,...)
    z = x[[i]]
    zz = ordfactor(z$y,ordered = F)
    out = zz == predict(x[[i]],type="class")$fit
    out[!out] = NA
    points(out+.2*i - .2,lwd=lwd,pch=15,...)
    legend("bottom",c("Correct prediction","Wrong prediction"),fill=c(1,"grey"),bty="n",horiz=T,xpd=NA,...)
  }
  text(x = mean(par("usr")[1:2]),y = seq(.9,.9+length(x)*.2-.1,.2), labels=names(x),xpd=NA)
}
