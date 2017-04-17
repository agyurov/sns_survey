# Analysis Q20

# Brute force FA




# Overview ----------------------------------------------------------------
par(mar = .pardefault$mar + c(2,0,0,8))
my.barplot.old(q20)


# FA ----------------------------------------------------------------------

q20fa2 = factanal(q20nonanum, factors=2)
q20fa3 = factanal(q20nonanum, factors=3)
q20fa4 = factanal(q20nonanum, factors=4)
q20fa5 = factanal(q20nonanum, factors=5)
q20fa6 = factanal(q20nonanum, factors=6)



# FA plots ----------------------------------------------------------------

# for 2 factors it doesn't look good
plot.matrix(q20fa3, cutoff=.4,col=grey.colors(2),main="3 factors, cutoff .4")
# for 4 factors, the fourth factor is redundant with f1 and f2. Minimum cutoff 
# for 4th factor to be relevant
plot.matrix(q20fa4, cutoff=.25,col=grey.colors(2), main="cutoff .25")


