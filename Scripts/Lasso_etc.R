# Simple playing around with the lars package (lasso and least angle regression)
# and some principal components regression

require(lars)

data(diabetes)
attach(diabetes)

lars1 = lars(x,y, type = "lar")
lasso = lars(x,y, type = "lasso")

plot(lars1)

summary(lars1)


lars1_cv = cv.lars(x,y)


# Principal components regression
lm1 = lm(y~x)

summary(lm1)

pcomp = princomp(x)

pcomp_models = lapply(1:ncol(x), function(index){
	temp = lm(y~pcomp$scores[,1:index])
	return(temp)
})

r2_vals = lapply(pcomp_models, function(model_in){
	return(summary(model_in)$r.squared)
})

plot(unlist(r2_vals))
abline(h = summary(lm1)$r.squared, col = "red", lty = 2)

# Don't see a drastic increase in r^2 after inclusion of the first 4
# principal component scores. Thus, dimensionality could be reduced here
# without a severe loss of information

detach(diabetes)
