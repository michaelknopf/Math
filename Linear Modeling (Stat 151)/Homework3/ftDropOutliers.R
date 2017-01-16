> summary(ftDropOutliers)

Call:
lm(formula = BODYFAT ~ AGE + WEIGHT + HEIGHT + THIGH, data = body[-c(39, 
    42), ])

Residuals:
     Min       1Q   Median       3Q      Max 
-10.6253  -3.4532  -0.0025   3.4547  11.1239 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 40.67324   12.72816   3.196  0.00158 ** 
AGE          0.14813    0.02930   5.055 8.44e-07 ***
WEIGHT       0.19536    0.02797   6.984 2.68e-11 ***
HEIGHT      -1.09459    0.15516  -7.054 1.77e-11 ***
THIGH        0.23070    0.14127   1.633  0.10373    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.969 on 245 degrees of freedom
Multiple R-squared:  0.5883,	Adjusted R-squared:  0.5816 
F-statistic: 87.52 on 4 and 245 DF,  p-value: < 2.2e-16