# Data provided by ESL course for testing
load("/Users/jmwerner1123/Dropbox/GitHub/ML2015/Scripts/5.R.RData")

attach(Xy)

## Chapter 5 Quiz questions

#1

lm1 = lm(y~X1+X2)
summary(lm1)

#2

matplot(Xy,type="l")

#3

boot1 = function(B){
	beta1 = rep(0,B)
	for(i in 1:B){
		new_data = Xy[sample(1:1000, 1000, replace = TRUE),]
		new_lm = lm(y ~ X1 + X2, data = new_data)
		beta1[i] = coefficients(new_lm)[2]
	}
	return(sd(beta1))
}

boot2 = function(B){
	beta1_2 = sapply(1:B, function(i){
		new_data = Xy[sample(1:1000, 1000, replace = TRUE),]
		new_lm = lm(y ~ X1 + X2, data = new_data)
		return(coefficients(new_lm)[2])
	})
	return(sd(beta1_2))
}

# Time test (for fun)
times = sapply(c(1000, 5000, 10000), function(B_in){
	t1 = system.time(bs_1 <- boot1(B_in))
	t2 = system.time(bs_2 <- boot2(B_in))
	return(c(t1[3], t2[3]))
})

# Final answer
B_in = 10000
boot2(B_in)


#4

lb = 0:9*100 + 1
ub = 1:10*100
B = 1000

beta1 = rep(0,B)
for(i in 1:B){
	sample_index = sample(1:10, 10, replace = TRUE)
	new_rows = unlist(lapply(sample_index, function(j){return(lb[j]:ub[j])}))
	new_data = Xy[new_rows,]
	new_lm = lm(y ~ X1 + X2, data = new_data)
	beta1[i] = coefficients(new_lm)[2]
}

sd(beta1)

