# Test validate.R function

# clear memory
rm(list=ls());

# set working directory
setwd('~/Repo/ds_boilerplate/');

# random forest library
library('randomForest');
library('caret');

# load validation function
source('Code/utils/validate.R');


# RANDOM FOREST
# Constants
NTREE = 20; NODESIZE = 5; MTRY = 2;
set.seed(42L); # because this is the answer!

# BINARY CASE
iris_binary = iris;
iris_binary$bin_class = rep(0, nrow(iris_binary));
iris_binary$bin_class[iris$Species=="setosa"] = 1;
iris_binary$bin_class = as.factor(iris_binary$bin_class);
iris_binary = subset(iris_binary, select=-Species);

# split dataset in train and test sets
folds = createFolds(y=iris_binary$bin_class, k=5, returnTrain = TRUE);

# for fold # q only
i = 1;
rf = randomForest(x=iris_binary[folds[[i]],1:4], y = iris_binary$bin_class[folds[[i]]], 
                  ntree = NTREE, nodesize = NODESIZE, mtry = MTRY);

# do prediction
test_index = setdiff(1:nrow(iris), folds[[i]]);

# prediction as factor, or matrix of probobilities, or as an one dim vector of probability
predicted_f = predict(rf, iris_binary[test_index, 1:4], type = "response");
predicted_m = predict(rf, iris_binary[test_index, 1:4], type = "prob");
predicted_b = predicted_m[,2]*runif(length(test_index));
predicted_randb = runif(length(test_index));

# groundtruth as factor and numeric vector
groundtruth_f = iris_binary$bin_class[test_index];
groundtruth_v = as.numeric(levels(groundtruth_f))[groundtruth_f];
    
# validate results
valid = validate(predicted_f, groundtruth_v, verbose=TRUE, debug=TRUE);
valid = validate(predicted_m, groundtruth_v, verbose=TRUE, debug=TRUE);
valid = validate(predicted_b, groundtruth_v, verbose=TRUE, debug=TRUE);
valid = validate(predicted_randb, groundtruth_f, verbose=TRUE, debug=TRUE, th=0.4);


# NORMAL CASE for IRIS dataset
rf = randomForest(x=iris[folds[[i]],1:4], y = iris$Species[folds[[i]]], 
                  ntree = NTREE, nodesize = NODESIZE, mtry = MTRY);

# do prediction
test_index = setdiff(1:nrow(iris), folds[[i]]);

# prediction as factor, or matrix of probobilities, or as an one dim vector of probability
predicted_f = predict(rf, iris[test_index, 1:4], type = "response");
predicted_m = predict(rf, iris[test_index, 1:4], type = "prob");
predicted_missing_class = predict(rf, iris[test_index, 1:4], type = "response");
predicted_missing_class[predicted_missing_class=="setosa"] = "virginica";
predicted_missing_class = as.factor(as.character(predicted_missing_class));

# groundtruth as factor and numeric vector
groundtruth_f = iris$Species[test_index];
groundtruth_v = as.numeric(groundtruth_f);

# predicted in as matrix
valid = validate(predicted_m, groundtruth_f, verbose=TRUE, debug=TRUE);
# predictor in a factor form
valid = validate(predicted_f, groundtruth_f, verbose=TRUE, debug=TRUE);
# predictor with a missing class
valid = validate(predicted_missing_class, groundtruth_f, verbose=TRUE, debug=TRUE);


