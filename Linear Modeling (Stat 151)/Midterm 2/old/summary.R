
Call:
lm(formula = medv ~ londist + crim + zn + chas + nox + rm + dis + 
    rad + tax + ptratio + b + lstat, data = bost)

Residuals:
     Min       1Q   Median       3Q      Max 
-14.5088  -2.7227  -0.5072   2.0255  25.1846 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  38.943085   5.222764   7.456 4.03e-13 ***
londist      31.815269   7.542918   4.218 2.93e-05 ***
crim         -0.121201   0.032378  -3.743 0.000203 ***
zn            0.045717   0.013299   3.438 0.000636 ***
chas          2.399272   0.843489   2.844 0.004634 ** 
nox         -19.619022   3.517087  -5.578 4.01e-08 ***
rm            3.578099   0.403079   8.877  < 2e-16 ***
dis          -2.230291   0.252866  -8.820  < 2e-16 ***
rad           0.303072   0.062357   4.860 1.58e-06 ***
tax          -0.010693   0.003326  -3.214 0.001392 ** 
ptratio      -0.988743   0.127321  -7.766 4.73e-14 ***
b             0.007750   0.002655   2.919 0.003671 ** 
lstat        -0.552666   0.047182 -11.714  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.658 on 493 degrees of freedom
Multiple R-squared:  0.7496,	Adjusted R-squared:  0.7435 
F-statistic:   123 on 12 and 493 DF,  p-value: < 2.2e-16

