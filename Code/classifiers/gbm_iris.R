# Train GBM classifier on Iris dataset

## clear memory
rm(list=ls());


## load GBM library
library('gbm');
# load validation function
source('D:/Repo/ds_boilerplate/Code/utils/validate.R');

# Constants for GBM
set.seed(42L); # because this is the answer!
GBM_SHRINKAGE  = 0.01;
GBM_MINOBS  =  5;
GBM_DEPTH = 2;
numOfTrees= 15;

## split dataset in train and test sets
k_folds = 5;
folds = createFolds(iris$Species, k=k_folds, returnTrain = TRUE);


## Train and Test for every fold
f_measure = rep(NA, k_folds);
for (i in 1:k_folds) {
     
     # train Random Forest in parallel
     gbm_model <- tryCatch({
          gbm(Species ~ ., data = iris[folds[[i]],], 
              n.tree=numOfTrees, shrinkage = GBM_SHRINKAGE, interaction.depth = GBM_DEPTH, 
              n.minobsinnode = GBM_MINOBS, verbose=T,
              distribution='multinomial')     
     }, error = function(err) {
          message(err);
          return(NULL);
     });
     
     if (!is.null(gbm_model)) {
          # do prediction
          test_index = setdiff(1:nrow(iris), folds[[i]]);
          test_predicted = predict(gbm_model, iris[test_index,], type = "response", n.trees = numOfTrees)[,,1];
          
          # validate results
          valid = validate(test_predicted, as.numeric(iris$Species[test_index]), verbose=TRUE);
          f_measure[i] = valid$w_f_measure;
     };
};

best.iter <- gbm.perf(gbm_model, method="OOB");
print(best.iter)

# print average weigthed F-measure
print(mean(f_measure, na.rm = TRUE));
