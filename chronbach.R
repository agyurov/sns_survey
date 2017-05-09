# Chronbach's Alpha
# Only meaningful for SAME scale questions
 

# q8 ----------------------------------------------------------------------
# q8alpha = alpha.recursive(q8nonanum)
# Not meaningful for q8
# names(q8nonanum)[!names(q8nonanum) %in% names(q8alpha)]
# 
# q8alpha2 = alpha(q8nonanum)
# my.barplot(q8alpha2$alpha.drop$raw_alpha,namez = rownames(q8alpha2$alpha.drop),main="Q8",ylab=expression(alpha))

# q9 ----------------------------------------------------------------------
# Not meaningful for q9
# q9alpha = alpha.recursive(q9nonanum)
# names(q9nonanum)[!names(q9nonanum) %in% names(q9alpha)]
# 
# q9alpha2 = alpha(q9nonanum)
# my.barplot(q9alpha2$alpha.drop$raw_alpha,namez = rownames(q9alpha2$alpha.drop),main="Q9",ylab=expression(alpha))

# q10 ----------------------------------------------------------------------
q10alpha = alpha.recursive(q10nonanum)
dropped = !names(q10nonanum) %in% names(q10alpha)
names(q10nonanum)[dropped]

q10alpha2 = alpha(q10nonanum)
b = my.barplot(q10alpha2$alpha.drop$raw_alpha,ylim=c(0,1),col = grey((!dropped)*.3+.4),
               namez = rownames(q10alpha2$alpha.drop),main="Q10",ylab=expression(alpha))
lines(as.vector(b),q10fa$uniquenesses,col=2,lwd=2) #,axes=F,ann=F
legend(x=my.par("below",.1)$x,y=my.par("below",.1)$y,fill=c(grey(.4),2),
       c("Dropped","FA3 uniqueness"),xpd=NA,horiz=T,bty="n",xjust=.5)

# q19 ----------------------------------------------------------------------
q19alpha = alpha.recursive(q19nonanum)
dropped = !names(q19nonanum) %in% names(q19alpha)
names(q19nonanum)[dropped]

q19alpha2 = alpha(q19nonanum)
b = my.barplot(q19alpha2$alpha.drop$raw_alpha,ylim=c(0,1),col = grey((!dropped)*.3+.4),
               namez = rownames(q19alpha2$alpha.drop),main="Q19",ylab=expression(alpha))
lines(as.vector(b),q19fa2$uniquenesses,col=2,lwd=2) #,axes=F,ann=F
lines(as.vector(b),q19fa3$uniquenesses,col=3,lwd=2) #,axes=F,ann=F
lines(as.vector(b),q19fa4$uniquenesses,col=4,lwd=2) #,axes=F,ann=F
legend(x=my.par("below",.1)$x,y=my.par("below",.1)$y,fill=c(grey(.4),2:4),
       c("Dropped","FA2 uniqueness","FA3 uniqueness","FA4 uniqueness"),xpd=NA,horiz=T,bty="n",xjust=.5)


# q20 ----------------------------------------------------------------------
q20alpha = alpha.recursive(q20nonanum)
dropped = !names(q20nonanum) %in% names(q20alpha)
names(q20nonanum)[which(dropped)]

q20alpha2 = alpha(q20nonanum)
b = my.barplot(q20alpha2$alpha.drop$raw_alpha,ylim=c(0,1),col = grey((!dropped)*.3+.4),
               namez = rownames(q20alpha2$alpha.drop),main="Q20",ylab=expression(alpha))
lines(as.vector(b),q20fa2$uniquenesses,col=2,lwd=2) #,axes=F,ann=F
lines(as.vector(b),q20fa3$uniquenesses,col=3,lwd=2) #,axes=F,ann=F
lines(as.vector(b),q20fa4$uniquenesses,col=4,lwd=2) #,axes=F,ann=F
legend(x=my.par("below",.1)$x,y=my.par("below",.1)$y,fill=c(grey(.4),2:4),
       c("Dropped","FA2 uniqueness","FA3 uniqueness","FA4 uniqueness"),xpd=NA,horiz=T,bty="n",xjust=.5)




print.this = function(x){
  cat("---------------------\n")
  for(i in 1:length(x)){
    frmla = as.character(x[[i]]$formula)[-c(1:2)]
    frmla = paste0(frmla,collapse = " + ")
    frmla = paste0(names(x)[i]," ~ ",frmla)
    print.noquote(frmla)
    print.noquote(x[[i]]$beta)
  }
  cat("---------------------\n")
  return(invisible(NULL))
}
