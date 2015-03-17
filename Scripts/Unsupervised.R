
# k-means clustering for 2 dimensions

###
# Needed functions
###

# Within cluster variance for given cluster x,y values
wcv = function(x,y){
	all_pairs = combn(1:length(x), 2)
	lengths = sapply(1:ncol(all_pairs), function(i){
		d1 = (x[all_pairs[1,i]] - x[all_pairs[2,i]])^2
		d2 = (y[all_pairs[1,i]] - y[all_pairs[2,i]])^2
		return(d1 + d2)
	})
	return(sum(all_pairs) / length(x))
}

# Find centroids (group-wise means)
get_centroids = function(x,y,ind){
	results = lapply(unique(ind), function(val){
		return(c(mean(x[ind == val]), mean(y[ind == val])))
	})
	return(results)
}

# Find distances from every point to centroid
distances_to_centroid = function(x, y, roids){
	results = sapply(1:length(roids), function(pt){
		return((x-roids[[pt]][1])^2 + (y-roids[[pt]][2])^2)
	})
	return(results)
}

# Find the new labels based on distances (ie closest - or smallest distances - means relabeling as that class)
new_labels = function(distances){
	results = sapply(1:nrow(distances), function(i){
		return(which.min(distances[i,]))
	})
	return(factor(results, levels = 1:ncol(distances)))
}

# Plots centroid coordinates with their correct color
plot_centroids = function(x_in, y_in, roids_in, labs_in){
	d = apply(distances_to_centroid(x_in, y_in, roids_in), 2, which.min)
	sapply(1:length(roids_in), function(i){
		# Correction to be sure centroid color matches points
		points(roids_in[[i]][1], roids_in[[i]][2], col = as.numeric(labs_in[d[i]]) + 1, pch = "X", cex = 3)
	})
}

########################################################

# make some data to play with
x = c(rnorm(35, 5), rnorm(35, 8))
y = c(rnorm(35, 6, 2), rnorm(35, 1))

k = 5

########################################################

par(mfrow = c(2,2))
plot(x,y, main = "All data")

# Initial random assignment of 1:k labels
labels = factor(sample(1:k, length(x), replace = TRUE), levels = 1:k)

# Plot data and centroids
plot(x, y, pch = as.numeric(labels), col = as.numeric(labels) + 1, main = "Iteration 1")
cents = get_centroids(x, y, labels)
for(i in 1:length(cents)){
	points(cents[[i]][1], cents[[i]][2], col = i + 1, pch = "X", cex = 3)
}


# Find distances and new labels
distances_1 = distances_to_centroid(x, y, cents)
labels_2 = new_labels(distances_1)

plot(x, y, pch = as.numeric(labels_2), col = as.numeric(labels_2) + 1, main = "Iteration 2")
new_cents = get_centroids(x, y, labels_2)
plot_centroids(x, y, new_cents, labels_2)



wcv_past = rep(0, k)
wcv_present = sapply(1:k, function(i){
	# wcv will fail if 1 or fewer points are passed to it, check here for that
	if(sum(labels_2 == i) > 1){
		return(wcv(x[labels_2 == i],y[labels_2 == i]))
	}else{
		return(0)
	}
})
current_labs = labels_2

while(sum(wcv_past == wcv_present) != k){
	wcv_past = wcv_present
	cent = get_centroids(x, y, current_labs)
	dist = distances_to_centroid(x, y, cent)
	current_labs = new_labels(dist)
	wcv_present = sapply(1:k, function(i){
		# wcv will fail if 1 or fewer points are passed to it, check here for that
		if(sum(current_labs == i) > 1){
			return(wcv(x[current_labs == i],y[current_labs == i]))
		}else{
			return(0)
		}
	})
}

plot(x, y, pch = as.numeric(current_labs), col = as.numeric(current_labs) + 1, main = "Final Result")
final_cents = get_centroids(x, y, current_labs)
plot_centroids(x, y, final_cents, current_labs)


# With further study, I would run this algorithm for all j <= k and
#  find the lowest sum of within cluster variances (as discussed in the book)
#  this algorithm as written risks converging on local total wcv over all clusters


########################################################
########################################################

###
# Quiz things/ playing with pca regression section
###

load("/Users/jmwerner1123/Dropbox/GitHub/ML2015/Scripts/10.R.RData")

# Here we have n = 300, p = 200 so dimensional reduction is necessary for 
# linear regression

all_x_data = rbind(x,x.test)

vars = apply(all_x_data, 2, var)
plot(vars)

# All cols have approximately the same variance, so we won't scale

pca1 = prcomp(all_x_data, scale = FALSE)

# scree plot
plot(pca1, type = "lines")

# Proportion of variance 
summary(pca1)
sum(pca1$sdev[1:5]^2)/sum(pca1$sdev^2)

# PCA regression
x.pca = pca1$x[1:300,1:5]
pca.predict.x = data.frame(pca1$x[301:1300, 1:5])

pca_df = data.frame(y, x.pca)
pca_lm = lm(y~., data = pca_df)

pca_predict = predict(pca_lm, pca.predict.x)

mse_prediction = mean((pca_predict - y.test)^2)

# OLS 

alldata = data.frame(y, x)

ols1 = lm(y~., data = alldata)

ols_pred = predict(ols1, x.test)

mse_ols = mean((ols_pred - y.test)^2)
