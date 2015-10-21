# two sample t-test
# Input example
# # group 1 - treatment
# m1 = 4.1; # sample mean
# s1 = 0.9; # sample standard deviation
# n1 = 17; # number of samples
# 
# # group 2 - control
# m2 = 3.7;
# s2 = 0.9;
# n2 = 19;
# 
# alpha = 0.05;
# m0 = 0;
tst_test = function(m1, s1, n1, m2, s2, n2, m0=0, alpha=0.05, 
                    equal.variance=FALSE, verbose=TRUE, plotChart=FALSE) {
     
     # find standard error and # degrees of freedom
     if (equal.variance) {
          # we assume equal variance
          se = sqrt((1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2)); 
          df = n1+n2-2;
     } else {
          # variaces are different
          # standard error of the difference for two independent random samples
          ve = s1*s1/n1 + s2*s2/n2;
          se = sqrt(ve);
          
          # welch-satterthwaite digrees of freedom
          df = ve^2/((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1));
     };

     # find margin of error
     me = qt(alpha/2, df)*se;
     d = m1 - m2;
     
     # confedence interval
     x1 = d - me; x2 = d + me;
     conf_interval = c(min(x1, x2), max(x1, x2));
     
     # find t statistics
     t = (m1-m2-m0)/se; # t-statistics
     
     # find p-value and prepare the res table
     p_value = 2*pt(-abs(t),df);
     res <- c(d, se, t, p_value, df); 
     names(res) <- c("Difference of Means", "Std Error", "t", "p-value", "Degree of Freedom");
     
     if (verbose) {
          print(res);
          message("Confedence interval: (", conf_interval[1], ", ", conf_interval[2], ")");
     };
     
     if (plotChart) {
          t_data = rt(n=4000, df);
          hist(t_data, breaks = 100)
          # TODO finish this part
     };
     
     res_l = list("report" = res, "confedence_interval" = conf_interval);
     return(res_l);
}