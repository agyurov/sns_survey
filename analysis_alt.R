# Alternative analysis


m1 = model.list.adv(pred = demo.dat, resp = q11)
lapply(m1,summary)
lapply(m1,eval.model)
plot.clm(m1)


m2 = model.list.adv(pred = demo.dat, resp = q12)
lapply(m2,summary)
lapply(m2,eval.model)
plot.clm(m2)


m3 = model.list.adv(pred = demo.dat, resp = q13)
lapply(m3,summary)
lapply(m3,eval.model)
plot.clm(m3)

m4 = model.list.adv(pred = demo.dat, resp = q15)
lapply(m4,summary)
lapply(m4,eval.model)
plot.clm(m4)

m5 = model.list.adv(pred = demo.dat, resp = q16)
lapply(m5,summary)
lapply(m5,eval.model)
plot.clm(m5)


# q17 to omany NAs
m6 = model.list.adv(pred = demo.dat, resp = q17)
lapply(m6,summary)
lapply(m6,eval.model)
plot.clm(m6)
