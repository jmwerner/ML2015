# Author: Jeremy Werner

using MultivariateStats

euclidean_distance(vector1::Array, vector2::Array) = √(sumabs2(vector1 - vector2))
	
function voting_function(vector_in::Array)
	counts_map = countmap(vector_in)
	k = [item[1] for item in counts_map]
	v = [item[2] for item in counts_map]
	indicators = sortperm(v, rev = true)
	if(length(indicators) > 1)
		if(k[indicators[1]] == k[indicators[2]])
			warn("Voting Tie! Please select k that is not a multiple of the number of output classes")
		end
	end
	k[indicators[1]]
end

# Takes matrix (with rows as observations) and converts it to an array of observations as arrays
function data_converter(input::Matrix)
	(rows,cols) = size(input)
	output = Array(Any, rows)
	for k in 1:rows
		output[k] = input[k,:]
	end
	output
end

# Returns array of categorical predictions for the predictors input
function kNN(X::Matrix, y::Array, k::Integer, predictors::Matrix)
	N = size(predictors, 1)
	predictions = Array(ASCIIString, N)
	train_size = size(X,1)
	distances = Array(Float64, train_size)
	
	if ndims(X) > 1
		if size(X,2) > 2
		 	pca_model = fit(PCA, transpose(X))
		 	X_array = data_converter(transpose(transform(pca_model, transpose(X))))
		 	predict_array = data_converter(transpose(transform(pca_model, transpose(predictors))))
		else
			X_array = data_converter(X)
			predict_array = data_converter(predictors)
		end
	else
		X_array = data_converter(X)
		predict_array = data_converter(predictors)
	end

	for i = 1:N
		# Use a distance metric and voting scheme of your choice here (as defined above)
		for j in 1:train_size
			distances[j] = euclidean_distance(X_array[j], predict_array[i])
		end
		indexes = sortperm(distances)
		predictions[i] = voting_function(y[indexes[1:k]])
	end
	predictions
end # kNN

