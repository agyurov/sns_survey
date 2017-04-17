# Analysis Q19

# Labeling q19 ISEL
q19labels = c("B","A","T","A","B","A","B","T","B","T","A","T")
names(q19) = paste0(names(q19),"_",q19labels)
names(q19nona) = paste0(names(q19nona),"_",q19labels)
names(q19nonanum) = paste0(names(q19nonanum),"_",q19labels)
unique(q19labels)

# Overview
par(mar=.pardefault$mar + c(2,0,0,8))
my.barplot.old(q19nona)

# Factor analysis ---------------------------------------------------------
q19fa2 = factanal(q19nonanum, factors = 2)
q19fa3 = factanal(q19nonanum, factors = 3)
q19fa4 = factanal(q19nonanum, factors = 4)
q19fa5 = factanal(q19nonanum, factors = 5)
q19fa6 = factanal(q19nonanum, factors = 6)
q19fa7 = factanal(q19nonanum, factors = 7)


# FA plots ----------------------------------------------------------------
par(mar = .pardefault$mar + c(0,4,-2,0),mfrow=c(3,1))
plot.matrix(q19fa2,cutoff=.15,col=grey.colors(3))
plot.matrix(q19fa3,cutoff=.33,col=grey.colors(3))
plot.matrix(q19fa4,cutoff=.32,col=grey.colors(3))


# h0 teset hypothesis that the Groups are governed by the same latent variables
h0 = '
      GroupA =~ q19.2_isel2_A + q19.4_isel4_A + q19.6_isel6_A + q19.11_isel11_A
      GroupB =~ q19.1_isel1_B + q19.5_isel5_B + q19.7_isel7_B + q19.9_isel9_B
      GroupT =~ q19.3_isel3_T + q19.8_isel8_T + q19.10_isel10_T + q19.12_isel12_T
      '
m0 = cfa(model = h0, data = q19nona)

