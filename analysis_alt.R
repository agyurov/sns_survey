# Alternative analysis
demo.dat = ordfactordf(demo.dat,ordered = F)
m0 = model.list.adv(pred = demo.dat, resp = q10, exclude.warnings = T)

m1 = model.list.adv(pred = demo.dat, resp = q11, exclude.warnings = T)
# lapply(m1,summary)
# lapply(m1,eval.model)
# plot.clm(m1)

m2 = model.list.adv(pred = demo.dat, resp = q12, exclude.warnings = T)


m3 = model.list.adv(pred = demo.dat, resp = q13, exclude.warnings = T)

m4 = model.list.adv(pred = demo.dat, resp = q15, exclude.warnings = T)

m5 = model.list.adv(pred = demo.dat, resp = q16, exclude.warnings = T)

# q17 to omany NAs
m6 = model.list.adv(pred = demo.dat, resp = q18, exclude.warnings = T, link = "logit")

m7 = model.list.adv(pred = demo.dat, resp = q19, exclude.warnings = T, link = "cloglog")

m8 = model.list.adv(pred = demo.dat, resp = q20, exclude.warnings = T)

m9 = model.list.adv(pred = demo.dat, resp = q5, exclude.warnings= T)

m10 = model.list.adv(pred = demo.dat, resp = q6, exclude.warnings= T)

m11 = model.list.adv(pred = demo.dat, resp = q7, exclude.warnings= T)

m12 = model.list.adv(pred = demo.dat, resp = q8, exclude.warnings= T)

m13 = model.list.adv(pred = demo.dat, resp = q9, exclude.warnings= T)


# lapply(paste0("m",1:8),function(x) eval(parse(text = x)))
lapply(paste0("m",0:13),function(x) length(eval(parse(text = x))))


model.names = paste0("m",0:13)

models = lapply(model.names,function(x) eval(parse(text = x)))
models.null = which(unlist(lapply(models,length))==0)
models = models[-models.null]
names(models) = model.names[-models.null]






  

  