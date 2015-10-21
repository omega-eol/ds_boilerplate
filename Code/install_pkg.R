# list of most common packages that I usually use

# update all old packages first
update.packages();

# to support parallel computation
install.packages("codetools");
install.packages("foreach");
install.packages("doMC");
install.packages("parallel");

# sparse matrix
install.packages("Matrix");

# c++ support
install.packages("Rcpp");
install.packages("RcppEigen");
install.packages("RcppArmadillo");

# DB support
# make sure that libpq-dev package is installed!
install.packages("RPostgreSQL");

# Optimization
install.packages("DEoptim");
install.packages("GenSA");

# classifiers
install.packages("randomForest");
install.packages("gbm");
install.packages('rpart');

# ds support
install.packages('caret');