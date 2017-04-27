# Analysis


# FA q8 -------------------------------------------------------------------

q8fa = brute.force.fa(q8nonanum, scores = "regression")
q8fa1 = factanal(q8nonanum, factors = 3, rotation = "promax")
print(q8fa$`3factors`, cutoff = .4)
print(q8fa1, cutoff = .3)



# FA q8 and q7 ------------------------------------------------------------

q7q8 = cbind(q7,q8)
q7q8nona = na.omit(q7q8)
q7q8nonanum = fact2num(q7q8nona)

q7q8fa = brute.force.fa(q7q8nonanum, scores = "regression") # identified 5 and 6 factors
frmla = formula(paste("~",paste(names(q7),collapse=" + ")))
q7q8fa0 = factanal(x = frmla, factors =2, data = q7q8nonanum, scores = "regression")

q7q8fa1 = factanal(q7q8nonanum, factors = 5, rotation = "varimax")
q7q8fa2 = factanal(q7q8nonanum, factors = 6, rotation = "varimax")
q7q8fa3 = factanal(q7q8nonanum, factors = 5, rotation = "promax")
q7q8fa4 = factanal(q7q8nonanum, factors = 6, rotation = "promax")

print(q7q8fa1, cutoff = .3)
print(q7q8fa2, cutoff = .3)
print(q7q8fa3, cutoff = .3)
print(q7q8fa4, cutoff = .3)

# par(mfrow=c(2,2))
# plot.matrix(q7q8fa1, cutoff = .3, main = "Cutoff .3")
# plot.matrix(q7q8fa2, cutoff = .3, main = "Cutoff .3")
# plot.matrix(q7q8fa3, cutoff = .3, main = "Cutoff .3")
# plot.matrix(q7q8fa4, cutoff = .3, main = "Cutoff .3")




# Q8 ~ Q7 clms ------------------------------------------------------------
q7fa = brute.force.fa(q7nonanum, scores = "regression")
q7q8clms = clm.each(q7q8nona) # not a single good model (not surprisingly)
q7q8clms.evals = lapply(q7q8clms, eval.model)

# naive models, predict q8 with q8 & FA(q7)
# naivedf = cbind(q7q8nona[, grepl("q8", names(q7q8nona))], q7q8fa0$scores)
# naive1 = clm.each(naivedf,link = "logit") # q8.5_beentertained
# naive2 = clm.each(naivedf,link = "probit") # q8.5_beentertained
# naive3 = clm.each(naivedf,link = "cauchit") # nothing
# naive4 = clm.each(naivedf,link = "cloglog") # q8.5_beentertained

# less naive models, predict q8 with q7
pred1q7q8 = q7q8nona[,grepl("q7",names(q7q8nona))]
resp1q7q8 = q7q8nona[,grepl("q8",names(q7q8nona))]
q7q8m1 = model.list(pred = pred1q7q8, resp = resp1q7q8, link = "logit") # q8.1_killtime
q7q8m2 = model.list(pred = pred1q7q8, resp = resp1q7q8, link = "probit") # q8.1_killtime, q8.5_beentertained
q7q8m3 = model.list(pred = pred1q7q8, resp = resp1q7q8, link = "cauchit") # nothing
q7q8m4 = model.list(pred = pred1q7q8, resp = resp1q7q8, link = "cloglog") # q8.1_killtime, q8.5_beentertained, q8.4_browsenoreason


# Q8, Q1 plots

plot(unlist(lapply(q7q8m1,function(x) eval.model(x)$total)),type="l",col=1
     ,ylim=c(.3,1),ylab="Accuracy",xaxt="n")
lines(unlist(lapply(q7q8m2,function(x) eval.model(x)$total)),col=2)
lines(unlist(lapply(q7q8m3,function(x) eval.model(x)$total)),col=3)
lines(unlist(lapply(q7q8m4,function(x) eval.model(x)$total)),col=4)
grid(10,2)
text(seq(1,length(q7q8m1),len=length(q7q8m1)),
     rep(par("usr")[3],length(q7q8m1)),
     labels=names(q7q8m1),xpd=NA,srt=45)
legend("top",horiz=T,c("logit","probit","cauchit","cloglog"),bty="n",fill=1:4)

plot.clm(q7q8m1,lwd=5)
plot.clm(q7q8m2,lwd=5)
plot.clm(q7q8m3,lwd=5)
plot.clm(q7q8m4,lwd=5)

# ... and vice versa

q7q8m11 = model.list(pred = resp1q7q8, resp = pred1q7q8, link = "logit") # q8.1_killtime
q7q8m21 = model.list(pred = resp1q7q8, resp = pred1q7q8, link = "probit") # q8.1_killtime, q8.5_beentertained
q7q8m31 = model.list(pred = resp1q7q8, resp = pred1q7q8, link = "cauchit") # nothing
q7q8m41 = model.list(pred = resp1q7q8, resp = pred1q7q8, link = "cloglog") # q8.1_killtime, q8.5_beentertained, q8.4_browsenoreason

plot(unlist(lapply(q7q8m11,function(x) eval.model(x)$total)),type="l",col=1
     ,ylim=c(.3,1),ylab="Accuracy",xaxt="n")
lines(unlist(lapply(q7q8m21,function(x) eval.model(x)$total)),col=2)
lines(unlist(lapply(q7q8m31,function(x) eval.model(x)$total)),col=3)
lines(unlist(lapply(q7q8m41,function(x) eval.model(x)$total)),col=4)
grid(10,2)
text(seq(1,length(q7q8m11),len=length(q7q8m11)),
     rep(par("usr")[3],length(q7q8m11)),
     labels=names(q7q8m11),xpd=NA,srt=45)
legend("top",horiz=T,c("logit","probit","cauchit","cloglog"),bty="n",fill=1:4)


plot.clm(q7q8m11,lwd=5)
plot.clm(q7q8m21,lwd=5)
plot.clm(q7q8m31,lwd=5)
plot.clm(q7q8m4,lwd=5)





# Q10 ~ Q19 ---------------------------------------------------------------

q10q19 = cbind(q10,q19)
q10q19nona = na.omit(q10q19)
q10q19nonanum = fact2num(q10q19nona)

# dim 68x19. Require FA prior to modelling
q10q19fa = brute.force.fa(q10q19nonanum,scores="regression")

# less naive models, predict q10 with q19
pred1q10q19 = q10q19nona[,grepl("q19",names(q10q19nona))]
# pred1q10q19 = q10q19fa$`4factors`$scores
# pred1q10q19 = q10q19fa$`5factors`$scores
# pred1q10q19 = q10q19fa$`6factors`$scores
# pred1q10q19 = q10q19fa$`7factors`$scores
# pred1q10q19 = q10q19fa$`8factors`$scores

resp1q10q19 = q10q19nona[,grepl("q10",names(q10q19nona))]
q10q19m1 = model.list(pred = pred1q10q19, resp = resp1q10q19, link = "logit")
q10q19m2 = model.list(pred = pred1q10q19, resp = resp1q10q19, link = "probit")
q10q19m3 = model.list(pred = pred1q10q19, resp = resp1q10q19, link = "cauchit")
q10q19m4 = model.list(pred = pred1q10q19, resp = resp1q10q19, link = "cloglog")

# Q10, Q19 plots

plot(unlist(lapply(q10q19m1,function(x) eval.model(x)$total)),type="l",col=1
     ,ylim=c(.3,1),ylab="Accuracy",xaxt="n",xlab="")
lines(unlist(lapply(q10q19m2,function(x) eval.model(x)$total)),col=2)
lines(unlist(lapply(q10q19m3,function(x) eval.model(x)$total)),col=3)
lines(unlist(lapply(q10q19m4,function(x) eval.model(x)$total)),col=4)
grid(10,2)
text(seq(1,length(q10q19m1),len=length(q10q19m1)),
     rep(par("usr")[3],length(q10q19m1)),
     labels=names(q10q19m1),xpd=NA,srt=45)
legend("top",horiz=T,c("logit","probit","cauchit","cloglog"),bty="n",fill=1:4)

# more plots
plot.clm(q10q19m1,lwd=5)
plot.clm(q10q19m2,lwd=5)
plot.clm(q10q19m3,lwd=5)
plot.clm(q10q19m4,lwd=5)


# ... and vice versa

q10q19m11 = model.list(pred = resp1q10q19, resp = pred1q10q19, link = "logit") # q8.1_killtime
q10q19m21 = model.list(pred = resp1q10q19, resp = pred1q10q19, link = "probit") # q8.1_killtime, q8.5_beentertained
q10q19m31 = model.list(pred = resp1q10q19, resp = pred1q10q19, link = "cauchit") # nothing
q10q19m41 = model.list(pred = resp1q10q19, resp = pred1q10q19, link = "cloglog") # q8.1_killtime, q8.5_beentertained, q8.4_browsenoreason

plot(unlist(lapply(q10q19m11,function(x) eval.model(x)$total)),type="l",col=1
     ,ylim=c(.3,1),ylab="Accuracy",xaxt="n")
lines(unlist(lapply(q10q19m21,function(x) eval.model(x)$total)),col=2)
lines(unlist(lapply(q10q19m31,function(x) eval.model(x)$total)),col=3)
lines(unlist(lapply(q10q19m41,function(x) eval.model(x)$total)),col=4)
grid(10,2)
text(seq(1,length(q10q19m11),len=length(q10q19m11)),
     rep(par("usr")[3],length(q10q19m11)),
     labels=names(q10q19m11),xpd=NA,srt=45)
legend("top",horiz=T,c("logit","probit","cauchit","cloglog"),bty="n",fill=1:4)

# more plots
plot.clm(q10q19m11,lwd=5)
plot.clm(q10q19m21,lwd=5)
plot.clm(q10q19m31,lwd=5)
plot.clm(q10q19m41,lwd=5)







# Q10 ~ Q20 ---------------------------------------------------------------

q10q20 = cbind(q10,q20)
q10q20nona = na.omit(q10q20)
q10q20nonanum = fact2num(q10q20nona)

# dim 68x20. Require FA prior to modelling
q10q20fa = brute.force.fa(q10q20nonanum,scores="regression")

# less naive models, predict q10 with q20
pred1q10q20 = q10q20nona[,grepl("q20",names(q10q20nona))]
# pred1q10q20 = q10q20fa$`4factors`$scores
# pred1q10q20 = q10q20fa$`5factors`$scores
# pred1q10q20 = q10q20fa$`6factors`$scores
# pred1q10q20 = q10q20fa$`7factors`$scores
# pred1q10q20 = q10q20fa$`8factors`$scores

resp1q10q20 = q10q20nona[,grepl("q10",names(q10q20nona))]
q10q20m1 = model.list(pred = pred1q10q20, resp = resp1q10q20, link = "logit")
q10q20m2 = model.list(pred = pred1q10q20, resp = resp1q10q20, link = "probit")
q10q20m3 = model.list(pred = pred1q10q20, resp = resp1q10q20, link = "cauchit")
q10q20m4 = model.list(pred = pred1q10q20, resp = resp1q10q20, link = "cloglog")

# Q10, Q20 plots

plot(unlist(lapply(q10q20m1,function(x) eval.model(x)$total)),type="l",col=1
     ,ylim=c(.3,1),ylab="Accuracy",xaxt="n",xlab="")
lines(unlist(lapply(q10q20m2,function(x) eval.model(x)$total)),col=2)
lines(unlist(lapply(q10q20m3,function(x) eval.model(x)$total)),col=3)
lines(unlist(lapply(q10q20m4,function(x) eval.model(x)$total)),col=4)
grid(10,2)
text(seq(1,length(q10q20m1),len=length(q10q20m1)),
     rep(par("usr")[3],length(q10q20m1)),
     labels=names(q10q20m1),xpd=NA,srt=45)
legend("top",horiz=T,c("logit","probit","cauchit","cloglog"),bty="n",fill=1:4)

# more plots
plot.clm(q10q20m1,lwd=5)
plot.clm(q10q20m2,lwd=5)
plot.clm(q10q20m3,lwd=5)
plot.clm(q10q20m4,lwd=5)


# ... and vice versa

q10q20m11 = model.list(pred = resp1q10q20, resp = pred1q10q20, link = "logit") # q8.1_killtime
q10q20m21 = model.list(pred = resp1q10q20, resp = pred1q10q20, link = "probit") # q8.1_killtime, q8.5_beentertained
q10q20m31 = model.list(pred = resp1q10q20, resp = pred1q10q20, link = "cauchit") # nothing
q10q20m41 = model.list(pred = resp1q10q20, resp = pred1q10q20, link = "cloglog") # q8.1_killtime, q8.5_beentertained, q8.4_browsenoreason

plot(unlist(lapply(q10q20m11,function(x) eval.model(x)$total)),type="l",col=1
     ,ylim=c(.3,1),ylab="Accuracy",xaxt="n")
lines(unlist(lapply(q10q20m21,function(x) eval.model(x)$total)),col=2)
lines(unlist(lapply(q10q20m31,function(x) eval.model(x)$total)),col=3)
lines(unlist(lapply(q10q20m41,function(x) eval.model(x)$total)),col=4)
grid(10,2)
text(seq(1,length(q10q20m11),len=length(q10q20m11)),
     rep(par("usr")[3],length(q10q20m11)),
     labels=names(q10q20m11),xpd=NA,srt=45)
legend("top",horiz=T,c("logit","probit","cauchit","cloglog"),bty="n",fill=1:4)

# more plots
plot.clm(q10q20m11,lwd=5)
plot.clm(q10q20m21,lwd=5)
plot.clm(q10q20m31,lwd=5)
plot.clm(q10q20m41,lwd=5)







# Add models to bucket ----------------------------------------------------


