kNN Readme
=====

This script will run a kNN classification algorithm and predict classification outputs for data sets. Currently, it only performs PCA to reduce dimensionality if data with more than 2 columns is given. It will reduce the data to the default number of components returned by PCA from the `MultivariateStats` module. 

Status
-----
Running and finished for now

To Do / Potential Future Work
-----
1. ~~Adapt for higher than 2-dimensional data sets~~
2. ~~Fix ties in both the voting function and distance function - left distance for now because of rarity (add warning messages as well)~~
3. ~~Clean up specific data types for inputs/outputs of functions~~
4. ~~Add execution examples (example is currently embeded in the script itself)~~
5. Add an inverse distance voting scheme 
6. Calculate a pairwise distance matrix instead of doing it in an inefficient loop
