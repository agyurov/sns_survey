# Chronbach's Alpha
 

# q8 ----------------------------------------------------------------------
q8alpha = alpha.recursive(q8nonanum)
names(q8nonanum)[!names(q8nonanum) %in% names(q8alpha)]

q8alpha2 = alpha(q8nonanum)
my.barplot(q8alpha2$alpha.drop$raw_alpha,namez = rownames(q8alpha2$alpha.drop),main="Q8",ylab=expression(alpha))

# q9 ----------------------------------------------------------------------
q9alpha = alpha.recursive(q9nonanum)
names(q9nonanum)[!names(q9nonanum) %in% names(q9alpha)]

q9alpha2 = alpha(q9nonanum)
my.barplot(q9alpha2$alpha.drop$raw_alpha,namez = rownames(q9alpha2$alpha.drop),main="Q9",ylab=expression(alpha))

# q10 ----------------------------------------------------------------------
q10alpha = alpha.recursive(q10nonanum)
names(q10nonanum)[!names(q10nonanum) %in% names(q10alpha)]

q10alpha2 = alpha(q10nonanum)
my.barplot(q10alpha2$alpha.drop$raw_alpha,namez = rownames(q10alpha2$alpha.drop),main="Q10",ylab=expression(alpha))

# q19 ----------------------------------------------------------------------
q19alpha = alpha.recursive(q19nonanum)
names(q19nonanum)[!names(q19nonanum) %in% names(q19alpha)]

q19alpha2 = alpha(q19nonanum)
my.barplot(q19alpha2$alpha.drop$raw_alpha,namez = rownames(q19alpha2$alpha.drop),main="Q19",ylab=expression(alpha))

# q20 ----------------------------------------------------------------------
q20alpha = alpha.recursive(q20nonanum)
names(q20nonanum)[!names(q20nonanum) %in% names(q20alpha)]

q20alpha2 = alpha(q20nonanum)
my.barplot(q20alpha2$alpha.drop$raw_alpha,namez = rownames(q20alpha2$alpha.drop),main="Q20",ylab=expression(alpha))
