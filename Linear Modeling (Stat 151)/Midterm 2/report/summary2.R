
> summary(ftDropOutliers)

Call:
lm(formula = medv ~ rm + ptratio + crim, data = bost[-which(pValues < 
    bonferroni), ])

Residuals:
     Min       1Q   Median       3Q      Max 
-18.2124  -2.7641   0.1068   2.5272  14.9498 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -6.22829    3.29065  -1.893    0.059 .  
rm           8.00451    0.33196  24.113  < 2e-16 ***
ptratio     -1.14914    0.10668 -10.772  < 2e-16 ***
crim        -0.20903    0.02566  -8.147 3.06e-15 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.7 on 495 degrees of freedom
Multiple R-squared:  0.7187,	Adjusted R-squared:  0.717 
F-statistic: 421.5 on 3 and 495 DF,  p-value: < 2.2e-16


> sink()
