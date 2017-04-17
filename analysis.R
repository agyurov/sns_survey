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
naivedf = cbind(q7q8nona[, grepl("q8", names(q7q8nona))], q7q8fa0$scores)
naive1 = clm.each(naivedf,link = "logit") # q8.5_beentertained
naive2 = clm.each(naivedf,link = "probit") # q8.5_beentertained
naive3 = clm.each(naivedf,link = "cauchit") # nothing
naive4 = clm.each(naivedf,link = "cloglog") # q8.5_beentertained

# less naive models, predict q8 with q7
pred1 = q7q8nona[,grepl("q7",names(q7q8nona))]
resp1 = q7q8nona[,grepl("q8",names(q7q8nona))]
lessnaive1 = model.list(x = pred1, y = resp1, link = "logit") # q8.1_killtime
lessnaive2 = model.list(x = pred1, y = resp1, link = "probit") # q8.1_killtime, q8.5_beentertained
lessnaive3 = model.list(x = pred1, y = resp1, link = "cauchit") # nothing
lessnaive4 = model.list(x = pred1, y = resp1, link = "cloglog") # q8.1_killtime, q8.5_beentertained, q8.4_browsenoreason


# Q10 ---------------------------------------------------------------------
# H0: single latent factor - nope
q10fa = brute.force.fa(q10nonanum)
q10fa = factanal(q10nonanum, factors = 3)

# H1: q10 ~ q11
q10q11 = cbind(q10,q11)
q10q11nona = na.omit(q10q11)
q10q11nonanum = fact2num(q10q11nona)

pred = q10q11nona[,grepl("q11",names(q10q11nona))]
resp = q10q11nona[,grepl("q10",names(q10q11nona))]
q10m1 = model.list(x = pred, y = resp, link = "logit")
q10m2 = clm.each(q10q11nona, link = "cloglog") # logit nope, probit nope, cauchit nope, cloglog - q10.3_browsephoto




