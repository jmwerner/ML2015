# Author: Jeremy Werner

# Begin with naive 2 dimensional data set (with the intent of eventually expanding to n dimensional data sets)

using RDatasets

iris = dataset("datasets", "iris")

# PetalLength and PetalWidth will be used for 2D example

function euclidean_distance(vector1, vector2)
	diff = vector1 - vector2
	sqrt(sum(diff .* diff))
end # distance

function voting_function(vector_in::Array)
	counts_map = countmap(vector_in)
	indicator = sortperm(collect(values(counts_map)), rev = true)[1]
	collect(keys(counts_map))[indicator]
end

# Returns array of categorical predictions for the predict_matrix input
function kNN(X::Matrix, y::Array, k::Integer, predict_matrix::Matrix, distance_metric::Function, voting_scheme::Function)
	N = size(predict_matrix)[1]
	predictions = Array(ASCIIString, N)
	for i = 1:N
		distances = map(j -> distance_metric(X[j,:], predict_matrix[i,:]), 1:(size(X)[1]))
		indexes = sortperm(distances)
		predictions[i] = voting_scheme(y[indexes[1:k]])
	end
	predictions
end # kNN


##########
# Driver #
##########
x_all = hcat(iris[:PetalLength], iris[:PetalWidth])

subset_n = 120
x_train = x_all[1:subset_n, 1:2]
x_test = x_all[(subset_n + 1):150, 1:2]
y_in = array(iris[:Species][1:subset_n])

test_predictions = kNN(x_train, y_in, 3, x_test, euclidean_distance, voting_function)

