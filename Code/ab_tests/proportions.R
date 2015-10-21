# Given a baseline convertion rate of 10% and desire lift of 1%,
# find needed sample size for each group (assume groups are the same size)

## clear workspace
rm(list=ls());

## Define variables
p1 = 0.1;
lift = 0.01;

## then desired convertion rate will be..
p2 = p1*(1+lift);


## set test parameters
##
## Statistical power of the test (1-Betta): This is the fraction of A/B experiments 
## in which the 1% (lift) relative change will actually be detectable, if it really exists.
## 
## Significance level (Alpha): This is the fraction A/B experiments which will 
## report a 1% (lift) relative change, even though there is no underlying difference
stat_power = 0.8;
alpha = 0.05;

## Compute power of test, or determine need sample size per group
res = power.prop.test(n = NULL, p1 = p1, p2 = p2, sig.level = alpha,
                      power = stat_power, alternative = "two.sided", strict = TRUE);
print(res);
n = round(res$n);
message('Sample size per group is ', n, '.');

## let`s assume that we have
people_per_day = 150000; # 150K visits per day

## how long do we need to run the test?
n_days = round(2*n/people_per_day);
## it looks like we would need to run the test for 19 days, but I would recommend round this number to
## whole weeks just to eleminate "best week days effect"
n_weeks = ceiling(n_days/7);
