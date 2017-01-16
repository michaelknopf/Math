
Call:
lm(formula = BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + HIP + 
    THIGH + FOREARM + WRIST, data = data[-c(39, 224), ])

Residuals:
    Min      1Q  Median      3Q     Max 
-8.8086 -2.7012 -0.2672  2.6823  9.4454 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -20.34883   10.65212  -1.910 0.057280 .  
AGE           0.06947    0.02809   2.473 0.014076 *  
WEIGHT       -0.07242    0.03658  -1.980 0.048890 *  
NECK         -0.34846    0.20756  -1.679 0.094474 .  
ABDOMEN       0.84245    0.06672  12.626  < 2e-16 ***
HIP          -0.13094    0.12821  -1.021 0.308135    
THIGH         0.27692    0.11753   2.356 0.019269 *  
FOREARM       0.35477    0.17874   1.985 0.048293 *  
WRIST        -1.59434    0.46618  -3.420 0.000735 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.886 on 241 degrees of freedom
Multiple R-squared:  0.7523,	Adjusted R-squared:  0.7441 
F-statistic:  91.5 on 8 and 241 DF,  p-value: < 2.2e-16

