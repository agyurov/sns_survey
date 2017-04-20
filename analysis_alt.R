# Alternative analysis


m1 = model.list.adv(pred = demo.dat, resp = q11, exclude.warnings = T)
lapply(m1,summary)
lapply(m1,eval.model)
plot.clm(m1)


m2 = model.list.adv(pred = demo.dat, resp = q12, exclude.warnings = T)
lapply(m2,summary)
lapply(m2,eval.model)
plot.clm(m2)


m3 = model.list.adv(pred = demo.dat, resp = q13, exclude.warnings = T)
lapply(m3,summary)
lapply(m3,eval.model)
plot.clm(m3)

m4 = model.list.adv(pred = demo.dat, resp = q15, exclude.warnings = T)
lapply(m4,summary)
lapply(m4,eval.model)
plot.clm(m4)

m5 = model.list.adv(pred = demo.dat, resp = q16, exclude.warnings = T)
lapply(m5,summary)
lapply(m5,eval.model)
plot.clm(m5)


# q17 to omany NAs
m6 = model.list.adv(pred = demo.dat, resp = q18, exclude.warnings = T)
lapply(m6,summary)
lapply(m6,eval.model)
plot.clm(m6)

m7 = model.list.adv(pred = demo.dat, resp = q19, exclude.warnings = T)
lapply(m7,summary)
lapply(m7,eval.model)
plot.clm(m7)

m8 = model.list.adv(pred = demo.dat, resp = q20, exclude.warnings = T)
lapply(m8,summary)
lapply(m8,eval.model)
plot.clm(m8)
