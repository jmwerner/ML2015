# Author: Jeremy Werner

# Start in the same directory as kNN.jl
include("kNN.jl")

using RDatasets

iris = dataset("datasets", "iris")


# PetalLength and PetalWidth will be used for 2D example
x_all = hcat(iris[:PetalLength], iris[:PetalWidth])

subset_n = 120
x_train = x_all[1:subset_n, 1:2]
x_test = x_all[(subset_n + 1):150, 1:2]
y_in = array(iris[:Species][1:subset_n])

real_y = array(iris[:Species][(subset_n+1):(size(iris)[1])])
n1 = length(real_y)

# Fitting for k=2,6
test_predictions_2 = kNN(x_train, y_in, 2, x_test)
test_predictions_6 = kNN(x_train, y_in, 6, x_test)

# Classification rates
class_2 = sum(real_y .== test_predictions_2) / n1
class_6 = sum(real_y .== test_predictions_6) / n1


# Higher dimensional test

x_all = hcat(iris[:PetalLength], iris[:PetalWidth], iris[:SepalLength], iris[:SepalWidth])

subset_n = 120
x_train = x_all[1:subset_n, 1:4]
x_test = x_all[(subset_n + 1):150, 1:4]
y_in = array(iris[:Species][1:subset_n])

test_predictions_3 = kNN(x_train, y_in, 3, x_test)
class_3_big = sum(real_y .== test_predictions_3) / n1

