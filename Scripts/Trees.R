# This is a simple example of finding the first split in a tree (circa ESL lecture notes)

# Make some play data
n = 75
x = rbinom(n, 25, .7)
y = rexp(n) + rnorm(n)

#This is mean to be a quantitative response (even though it looks categorical)
label = sample(c(1,2,3), n, replace = TRUE, prob = c(.2,.3,.5)) 

plot(x,y, col = label)

###
calc_rss = function(labels, box_indicators){
	total = sum((labels[box_indicators] - mean(labels[box_indicators]))^2) + 
			sum((labels[!box_indicators] - mean(labels[!box_indicators]))^2)

	return(total)
}
###

xvals = sort(unique(x))
yvals = sort(unique(y))

results_x_splits = sapply(1:(length(xvals)-1), function(i){
	return(calc_rss(label, x > mean(xvals[i:(i+1)])))
})

results_y_splits = sapply(1:(length(yvals)-1), function(i){
	return(calc_rss(label, y > mean(yvals[i:(i+1)])))
})

ymin = min(results_y_splits)
xmin = min(results_x_splits)

if(ymin < xmin){
	splitter = yvals[which.min(results_y_splits)]
	abline(h = splitter)
}else{
	splitter = xvals[which.min(results_x_splits)]
	abline(v = splitter)
}


