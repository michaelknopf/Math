Call:
lm(formula = medv ~ londist + crim + zn + chas + nox + rm + dis + 
    rad + tax + ptratio + b + lstat, data = bost[-which(pValues < 
    bonferroni), ])

Residuals:
     Min       1Q   Median       3Q      Max 
-14.0883  -2.4031  -0.4924   1.9151  19.5268 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  32.696599   4.792329   6.823 2.64e-11 ***
londist      26.030654   6.866851   3.791 0.000169 ***
crim         -0.109809   0.029404  -3.734 0.000210 ***
zn            0.040032   0.012079   3.314 0.000988 ***
chas          1.891099   0.775444   2.439 0.015093 *  
nox         -18.028122   3.194950  -5.643 2.84e-08 ***
rm            4.285432   0.373691  11.468  < 2e-16 ***
dis          -1.904177   0.231625  -8.221 1.82e-15 ***
rad           0.255340   0.056781   4.497 8.62e-06 ***
tax          -0.011343   0.003019  -3.757 0.000193 ***
ptratio      -0.988666   0.115556  -8.556  < 2e-16 ***
b             0.007434   0.002409   3.086 0.002146 ** 
lstat        -0.454664   0.043963 -10.342  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.226 on 490 degrees of freedom
Multiple R-squared:  0.7836,	Adjusted R-squared:  0.7783 
F-statistic: 147.8 on 12 and 490 DF,  p-value: < 2.2e-16