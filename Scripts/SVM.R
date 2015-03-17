require(MASS)
require(e1071)

# Global constants
mu0 = c(rep(0, 10))
mu1 = c(rep(1,5), rep(0,5))
y = as.factor(c(rep(0, 50), rep(1, 50)))


many_error_rates = sapply(1:1000, function(i){
	sample0 = sapply(1:50, function(i){
		return(mvrnorm(1, mu0, diag(length(mu0))))
	})

	sample1 = sapply(1:50, function(i){
		return(mvrnorm(1, mu1, diag(length(mu0))))
	})

	x = t(cbind(sample0, sample1))

	svm1 = svm(y ~ x, scale = FALSE)

	error = 1 - mean(svm1$fitted == y)
	return(error)
})

# Approximate test error rate (via repeated sampling) 
# for a SVM with a radial kernel
mean(many_error_rates)



many_error_rates = sapply(1:1000, function(i){
	sample0 = sapply(1:50, function(i){
		return(mvrnorm(1, mu0, diag(length(mu0))))
	})

	sample1 = sapply(1:50, function(i){
		return(mvrnorm(1, mu1, diag(length(mu0))))
	})

	x = t(cbind(sample0, sample1))

	svm1 = svm(y ~ x, scale = FALSE, kernel = "linear")

	error = 1 - mean(svm1$fitted == y)
	return(error)
})

# Approximate test error rate (via repeated sampling) 
# for a SVM with a linear kernel
mean(many_error_rates)


many_error_rates = sapply(1:1000, function(i){
	sample0 = sapply(1:50, function(i){
		return(mvrnorm(1, mu0, diag(length(mu0))))
	})

	sample1 = sapply(1:50, function(i){
		return(mvrnorm(1, mu1, diag(length(mu0))))
	})

	x = t(cbind(sample0, sample1))

	logit = glm(y ~ x, family = "binomial")

	error = 1 - mean(round(logit$fitted.values) == y)
	return(error)
})

# Approximate test error rate (via repeated sampling) 
# for logistic regression 
mean(many_error_rates)





