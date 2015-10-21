# 2 sample t-test
# for example see here: http://www.evanmiller.org/ab-testing/t-test.html
# here are testing the following:
# H0: m1-m2 = 0
# H1: m1-m2 != 0

## clear workspace
rm(list=ls());

## We are given two distributions
## Question: Does the average value differ across two groups?

## Distribution #1
m1 = 58.175;
sd1 = 28.017;
n1 = 1000;
## Distribution #2
m2 = 58.71;
sd2 = 11.841;
n2 = 1000;

# set alpha = 1-confedence level
alpha = 0.05;

## plot distributions
data1 = rnorm(n=n1, mean=m1, sd=sd1);
data2 = rnorm(n=n2, mean=m2, sd=sd2);
hist(data1, breaks = 100, col=rgb(0,0,1,1/4), main="Two Groups");
hist(data2, breaks = 100, col=rgb(1,0,0,1/4), add=T);

## load t-test funcion
source('D:/Repo/ds_boilerplate/Code/ab_tests/tst_test.R');
res = tst_test(m1, sd1, n1, m2, sd2, n2, m0=0, alpha=0.05, verbose=TRUE);

# compare the results with standard R function
t.test(data1, data2, alternative = "two.sided", var.equal = FALSE, conf.level = 1-alpha);
