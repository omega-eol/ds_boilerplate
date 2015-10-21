## clear workspace
rm(list=ls());

# find required sample size for each group given
# baseline average income and std
m1 = 1.10;
sd = 0.2;
lift = 0.01;

## them second mean is
m2 = m1*(1+lift);

## other parameters of the test
alpha = 0.05;
stat_power = 0.8;

res = power.t.test(n=NULL, delta = m2-m1, sd = sd, sig.level = alpha, power = stat_power,
                   type = "two.sample", alternative = "two.sided");