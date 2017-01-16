> summary(ft)

Call:
lm(formula = BODYFAT ~ AGE + WEIGHT + HEIGHT + THIGH, data = body)

Residuals:
     Min       1Q   Median       3Q      Max 
-17.3699  -3.9361  -0.0351   3.6796  16.0833 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.07425   10.30553  -0.104   0.9171    
AGE          0.18901    0.03033   6.233 1.97e-09 ***
WEIGHT       0.12373    0.02734   4.526 9.37e-06 ***
HEIGHT      -0.46074    0.10478  -4.397 1.63e-05 ***
THIGH        0.36546    0.14952   2.444   0.0152 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5.329 on 247 degrees of freedom
Multiple R-squared:  0.5349,	Adjusted R-squared:  0.5273 
F-statistic: 71.01 on 4 and 247 DF,  p-value: < 2.2e-16