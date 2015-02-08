data(iris)

#train_indices = sample(1:150, 125)
train_indices = 1:150 # Train with full data set for now
# Use sepal length and sepal width for our two input data vectors

train = iris[train_indices,c("Sepal.Length", "Sepal.Width")]
train_response = iris[train_indices, c("Species")] == "setosa"  # Turn into binary response


# Try 1, solving as a generalized eigenvalue problem. Abandoned due to frustration
#  http://www.cs.rpi.edu/~zaki/www-new/uploads/Dmcourse/Main/chap22.pdf
# D = split(train, train_response)
# mu = lapply(D, function(input){return(apply(input, 2, mean))})
# B = (mu[[1]] - mu[[2]]) %*% t(mu[[1]] - mu[[2]])
# Z = lapply(1:2, function(input){return(D[[input]] - matrix(rep(1, nrow(D[[input]]))) %*%  mu[[input]])})
# S_list = lapply(1:2, function(input){
# 	mat = as.matrix(Z[[input]])
# 	return(t(mat) %*% mat)
# })
# S = S_list[[1]] + S_list[[2]]
# # w = eigen(solve(S) %*% B)


# Try 2

# Hard code example for K=2
# Formulas referenced from ESL p 109 (Hastie, Tibshirani)
K = 2
N = length(train_response)
N1 = sum(train_response)
N2 = sum(!train_response)

pihat1 = N1 / N
pihat2 = N2 / N

muhat1 = apply(train[train_response,], 2, sum) / N1
muhat2 = apply(train[!train_response,], 2, sum) / N2

Z1 = cbind(train[train_response, 1] - muhat1[1], train[train_response, 2] - muhat1[2])
Z2 = cbind(train[!train_response, 1] - muhat2[1], train[!train_response, 2] - muhat2[2])

S1 = t(Z1) %*% Z1
S2 = t(Z2) %*% Z2
sigmahat = 1/(N-K)*(S1+S2)

# The functions below will call parameter estimates calculated above from global (I know...shame)

# Finding the decision border by writing x2 as a function of x1 (calculated with some linear algebra)
border = function(x1){
	sinv = solve(sigmahat)
	M = muhat2-muhat1
	right_side = 1/2*t(muhat2) %*% sinv %*% muhat2 - 1/2*t(muhat1) %*% sinv %*% muhat1 + log(N1/N) - log(N2/N)
	piece2 = x1*(sinv[1,1]*M[1] + sinv[1,2]*M[2])
	denom = sinv[2,1]*M[1] + sinv[2,2]*M[2]
	return((right_side - piece2) / denom)
}

indices = as.numeric(train_response) + 1
plot(train, pch = c(1,2)[indices], col = c("red", "blue")[indices])
in_vect = c(4,8)
y_vals = sapply(in_vect, border)
points(in_vect, y_vals, col = "grey", lwd = 2, type = "l")


lda_classification = function(one_row){
	sinv = solve(sigmahat)
	right_side = 1/2*t(muhat2) %*% sinv %*% muhat2 - 1/2*t(muhat1) %*% sinv %*% muhat1 + log(N1/N) - log(N2/N)
	left_side = as.matrix(one_row) %*% sinv %*% (muhat2-muhat1)
	return(left_side < right_side)
}

class_vect = sapply(1:150, function(i){return(lda_classification(train[i,]))})
correct_class_rate = sum(class_vect == train_response)/N


