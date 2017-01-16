
> summary(backward())

Call:
lm(formula = BODYFAT ~ ., data = data[, vars])

Residuals:
     Min       1Q   Median       3Q      Max 
-10.0574  -2.7411  -0.1912   2.6929   9.4977 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -20.06213   10.84654  -1.850  0.06558 .  
AGE           0.05922    0.02850   2.078  0.03876 *  
WEIGHT       -0.08414    0.03695  -2.277  0.02366 *  
NECK         -0.43189    0.20799  -2.077  0.03889 *  
ABDOMEN       0.87721    0.06661  13.170  < 2e-16 ***
HIP          -0.18641    0.12821  -1.454  0.14727    
THIGH         0.28644    0.11949   2.397  0.01727 *  
FOREARM       0.48255    0.17251   2.797  0.00557 ** 
WRIST        -1.40487    0.47167  -2.978  0.00319 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.965 on 243 degrees of freedom
Multiple R-squared:  0.7467,	Adjusted R-squared:  0.7383 
F-statistic: 89.53 on 8 and 243 DF,  p-value: < 2.2e-16


> summary(forward())

Call:
lm(formula = BODYFAT ~ ., data = data[, allvars[vars]])

Residuals:
     Min       1Q   Median       3Q      Max 
-10.0574  -2.7411  -0.1912   2.6929   9.4977 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -20.06213   10.84654  -1.850  0.06558 .  
ABDOMEN       0.87721    0.06661  13.170  < 2e-16 ***
WEIGHT       -0.08414    0.03695  -2.277  0.02366 *  
WRIST        -1.40487    0.47167  -2.978  0.00319 ** 
FOREARM       0.48255    0.17251   2.797  0.00557 ** 
NECK         -0.43189    0.20799  -2.077  0.03889 *  
AGE           0.05922    0.02850   2.078  0.03876 *  
THIGH         0.28644    0.11949   2.397  0.01727 *  
HIP          -0.18641    0.12821  -1.454  0.14727    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.965 on 243 degrees of freedom
Multiple R-squared:  0.7467,	Adjusted R-squared:  0.7383 
F-statistic: 89.53 on 8 and 243 DF,  p-value: < 2.2e-16


> summary(adjRsqr())

Call:
lm(formula = BODYFAT ~ ., data = data[, vars])

Residuals:
     Min       1Q   Median       3Q      Max 
-10.0574  -2.7411  -0.1912   2.6929   9.4977 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -20.06213   10.84654  -1.850  0.06558 .  
AGE           0.05922    0.02850   2.078  0.03876 *  
WEIGHT       -0.08414    0.03695  -2.277  0.02366 *  
NECK         -0.43189    0.20799  -2.077  0.03889 *  
ABDOMEN       0.87721    0.06661  13.170  < 2e-16 ***
HIP          -0.18641    0.12821  -1.454  0.14727    
THIGH         0.28644    0.11949   2.397  0.01727 *  
FOREARM       0.48255    0.17251   2.797  0.00557 ** 
WRIST        -1.40487    0.47167  -2.978  0.00319 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.965 on 243 degrees of freedom
Multiple R-squared:  0.7467,	Adjusted R-squared:  0.7383 
F-statistic: 89.53 on 8 and 243 DF,  p-value: < 2.2e-16


> summary(AIC())

Call:
lm(formula = BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + HIP + 
    THIGH + FOREARM + WRIST, data = data[, allvars])

Residuals:
     Min       1Q   Median       3Q      Max 
-10.0574  -2.7411  -0.1912   2.6929   9.4977 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -20.06213   10.84654  -1.850  0.06558 .  
AGE           0.05922    0.02850   2.078  0.03876 *  
WEIGHT       -0.08414    0.03695  -2.277  0.02366 *  
NECK         -0.43189    0.20799  -2.077  0.03889 *  
ABDOMEN       0.87721    0.06661  13.170  < 2e-16 ***
HIP          -0.18641    0.12821  -1.454  0.14727    
THIGH         0.28644    0.11949   2.397  0.01727 *  
FOREARM       0.48255    0.17251   2.797  0.00557 ** 
WRIST        -1.40487    0.47167  -2.978  0.00319 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.965 on 243 degrees of freedom
Multiple R-squared:  0.7467,	Adjusted R-squared:  0.7383 
F-statistic: 89.53 on 8 and 243 DF,  p-value: < 2.2e-16


> summary(BIC())

Call:
lm(formula = BODYFAT ~ WEIGHT + ABDOMEN + FOREARM + WRIST, data = data[, 
    allvars])

Residuals:
    Min      1Q  Median      3Q     Max 
-9.8002 -2.8728 -0.1545  2.8980  8.3845 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -31.29679    6.70886  -4.665 5.06e-06 ***
WEIGHT       -0.12557    0.02292  -5.479 1.05e-07 ***
ABDOMEN       0.92137    0.05192  17.747  < 2e-16 ***
FOREARM       0.44638    0.16822   2.654 0.008480 ** 
WRIST        -1.39177    0.40991  -3.395 0.000799 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.021 on 247 degrees of freedom
Multiple R-squared:  0.7351,	Adjusted R-squared:  0.7308 
F-statistic: 171.4 on 4 and 247 DF,  p-value: < 2.2e-16


> summary(Mallow())

Call:
lm(formula = BODYFAT ~ ., data = data[, vars])

Residuals:
     Min       1Q   Median       3Q      Max 
-10.0574  -2.7411  -0.1912   2.6929   9.4977 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -20.06213   10.84654  -1.850  0.06558 .  
AGE           0.05922    0.02850   2.078  0.03876 *  
WEIGHT       -0.08414    0.03695  -2.277  0.02366 *  
NECK         -0.43189    0.20799  -2.077  0.03889 *  
ABDOMEN       0.87721    0.06661  13.170  < 2e-16 ***
HIP          -0.18641    0.12821  -1.454  0.14727    
THIGH         0.28644    0.11949   2.397  0.01727 *  
FOREARM       0.48255    0.17251   2.797  0.00557 ** 
WRIST        -1.40487    0.47167  -2.978  0.00319 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.965 on 243 degrees of freedom
Multiple R-squared:  0.7467,	Adjusted R-squared:  0.7383 
F-statistic: 89.53 on 8 and 243 DF,  p-value: < 2.2e-16
