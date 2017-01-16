body = read.csv("BodyFat.csv")
#ft = lm(BODYFAT ~ AGE + WEIGHT + HEIGHT + I(AGE + 10*HEIGHT), data = body)
ft = lm(BODYFAT ~ AGE + WEIGHT + HEIGHT, data = body)
summary(ft)

Output:
Coefficients:
               Estimate Std. Error t value Pr(>|t|)    
(Intercept)    17.72142   6.92955   2.557   0.0111 *  
AGE            0.15583    0.02739   5.690 3.57e-08 ***
WEIGHT         0.18373    0.01216  15.107  < 2e-16 ***
HEIGHT        -0.55099    0.09904  -5.563 6.85e-08 ***