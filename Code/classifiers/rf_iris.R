# Train random forest classifier on Iris dataset in parallel settings
# clear memory
rm(list=ls());

# parallel computing
library(foreach);
if (.Platform$OS.type == "unix") {
     library(doMC);
     registerDoMC();     
} else { # windows
     library(doParallel);
     registerDoParallel();
};
# random forest library
library('randomForest');

# load validation function
source('D:/Repo/ds_boilerplate/Code/utils/validate.R');

# Constants
NTREE = 20;
NODESIZE = 5;
MTRY = 2;
set.seed(42L); # because this is the answer!
# parallel execution
n_cores = parallel:::detectCores()/2;
ntree_per_core = floor(NTREE/n_cores);

# split dataset in train and test sets
k_folds = 5;
folds = createFolds(iris$Species, k=k_folds, returnTrain = TRUE);

# for every fold
f_measure = rep(NA, k_folds);
for (i in 1:k_folds) {
     
     # train Random Forest in parallel
     rf <- tryCatch({
          foreach(ntree=rep(ntree_per_core, n_cores), .combine=combine, .multicombine=TRUE, .packages='randomForest') %dopar% {
               randomForest(x=iris[folds[[i]],1:4], y = iris$Species[folds[[i]]], ntree = ntree, nodesize = NODESIZE, mtry = MTRY);
          };     
     }, error = function(err) {
          message(err);
          return(NULL);
     });
     
     if (!is.null(rf)) {
          # do prediction
          test_index = setdiff(1:nrow(iris), folds[[i]]);
          test_predicted = predict(rf, iris[test_index,1:4], type = "response");
          
          # validate results
          valid = validate(test_predicted, iris$Species[test_index], verbose=TRUE);
          f_measure[i] = valid$w_f_measure;
     };

};

# show features importance
f_imp = varImpPlot(rf);

# print average weigthed F-measure
print(mean(f_measure, na.rm = TRUE));
