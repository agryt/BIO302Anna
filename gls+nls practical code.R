#### GLS AND NLS PRACTICAL ####

#### GLS ####

# using the built in data "iris"

# Q: Make a plot to show how `Petal.Length` varies between species.

as_tibble(iris)               # showing iris as a tibble
iris <- as_tibble(iris)       # naming the tibble iris
iris                          # showing what we now called iris

iris %>% 
  group_by(Species) %>% 
  select(Petal.Length) %>% 
  ggplot(aes(x = Species, y = Petal.Length)) + geom_boxplot()


# Q: Find the variance of `Petal.Length` for each species.

iris %>% 
  group_by(Species) %>% 
  summarise(variance_petal.length = var(Petal.Length))

# Answer:
# Setosa: 0.0302
# Versicolor: 0.221
# Virginica: 0.305


# Q: Fit an anova using `lm` between `Petal.Length` and species and examine the diagnostic plots.

# making a linear model
mod <- lm(Petal.Length ~ Species, data = iris)

# performing anova
anova(mod)
# output:
#Analysis of Variance Table

#Response: Petal.Length
#Df Sum Sq Mean Sq F value    Pr(>F)    
#Species     2 437.10 218.551  1180.2 < 2.2e-16 ***
#  Residuals 147  27.22   0.185                      
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(mod)
# output:
#Call:
#  lm(formula = Petal.Length ~ Species, data = iris)

#Residuals:
#  Min     1Q Median     3Q    Max 
#-1.260 -0.258  0.038  0.240  1.348 

#Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)        1.46200    0.06086   24.02   <2e-16 ***
#  Speciesversicolor  2.79800    0.08607   32.51   <2e-16 ***
#  Speciesvirginica   4.09000    0.08607   47.52   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.4303 on 147 degrees of freedom
#Multiple R-squared:  0.9414,	Adjusted R-squared:  0.9406 
#F-statistic:  1180 on 2 and 147 DF,  p-value: < 2.2e-16

# making diagnostic plots
par(mfrow=c(2,2))
plot(mod)
# residuals vs fitted is almost a straight line -> good
# normal Q-Q is almost at the lines -> good
# scale-location line is fairly horisontal -> good
# constant leverage -> not sure, is not covered in BIO300B


# Q: Fit a `gls` for the same model. Have the coefficients changed?

# loading library with gls
library(nlme)

# nlme is in conflict with a tidyverse package, the easiest is to restart r, here i have included nlme:: in front of gls

# making a generalised least squared model
mod2 <- nlme::gls(Petal.Length ~ Species, data = iris)

# performing anova
anova(mod2)
# output: 
#Denom. DF: 147 
#numDF   F-value p-value
#(Intercept)     1 11439.118  <.0001
#Species         2  1180.161  <.0001

summary(mod2)
#Generalized least squares fit by REML
#Model: Petal.Length ~ Species 
#Data: iris 
#AIC      BIC   logLik
#189.0054 200.9671 -90.5027

#Coefficients:
#                  Value  Std.Error  t-value p-value
#(Intercept)       1.462 0.06085848 24.02294       0
#Speciesversicolor 2.798 0.08606689 32.50960       0
#Speciesvirginica  4.090 0.08606689 47.52118       0

#Correlation: 
#  (Intr) Spcsvrs
#Speciesversicolor -0.707        
#Speciesvirginica  -0.707  0.500 

#Standardized residuals:
#  Min          Q1         Med          Q3         Max 
#-2.92795509 -0.59953366  0.08830341  0.55770573  3.13244720 

#Residual standard error: 0.4303345 
#Degrees of freedom: 150 total; 147 residual

# A: the coefficients did not change (much)


# Q: Fit a `gls` for the same model but allow the variance to be different for each species

# making the model
mod3 <- nlme::gls(Petal.Length ~ Species, data = iris, weights = varIdent(form = ~ +1|Species))

# performing the anova
anova(mod3)
# output:
#Denom. DF: 147 
#            numDF  F-value p-value
#(Intercept)     1 9025.476  <.0001
#Species         2 1843.702  <.0001

summary(mod3)
# output:
#Generalized least squares fit by REML
#Model: Petal.Length ~ Species 
#Data: iris 
#AIC      BIC   logLik
#137.0802 155.0228 -62.5401

#Variance function:
#  Structure: Different standard deviations per stratum
#Formula: ~+1 | Species 
#Parameter estimates:
#  setosa versicolor  virginica 
#1.000000   2.705864   3.177947 

#Coefficients:
#                  Value  Std.Error  t-value p-value
#(Intercept)       1.462 0.02455979 59.52820       0
#Speciesversicolor 2.798 0.07084850 39.49272       0
#Speciesvirginica  4.090 0.08182262 49.98617       0

#Correlation: 
#  (Intr) Spcsvrs
#Speciesversicolor -0.347        
#Speciesvirginica  -0.300  0.104 

#Standardized residuals:
#  Min          Q1         Med          Q3         Max 
#-2.68135872 -0.73402753  0.08697309  0.70029465  2.52211264 

#Residual standard error: 0.1736639 
#Degrees of freedom: 150 total; 147 residual


# Q: Use `AIC` to test if this is a better model.

# comparing the two gls models:
anova(mod2, mod3)
# output:
#     Model df      AIC      BIC   logLik   Test L.Ratio p-value
#mod2     1  4 189.0054 200.9671 -90.5027                       
#mod3     2  6 137.0802 155.0228 -62.5401 1 vs 2 55.9252  <.0001

# A: the AIC is lower for the more complex model -> is useful to allow the variance to vary between species (mod3 is better)


#### NLS ####

# Q: Import data amount.csv
amount <- readr::read_csv("amount.csv")
# the readr::read_csv is usually a better way to import than read.csv


# Q: Do a non-linear regression
# first need to load library nlme
library(nlme)
# making the model
amount.nls <- nls(amount ~ b0 + b1 * exp(b2 * calcium), data = amount, start = c(b0 = 0, b1 = 20, b2 = -1))
summary(amount.nls)
# output: 
##Formula: amount ~ b0 + b1 * exp(b2 * calcium)

##Parameters:
##   Estimate Std. Error t value Pr(>|t|)    
##b0   0.4206     2.1141   0.199 0.844676    
##b1  43.5721    10.7301   4.061 0.000813 ***
##b2  -0.3435     0.1270  -2.705 0.015025 *  
##  ---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##Residual standard error: 6.139 on 17 degrees of freedom

##Number of iterations to convergence: 6 
##Achieved convergence tolerance: 8.991e-06

# plotting the model
library(ggplot2)
ggplot(cbind(amount, fit = fitted(amount.nls)), aes(x = calcium, y = amount)) + geom_point() + geom_line(aes(y = fit))


# Q: Interpret the results
# the estimate column shows the least squares estimates
# the std. error column shows the standard errors of these estimates
# the t value column shows the ratio of each parameter estimate to its standard error
# The residual standard deviation is the estimate of sigma
# this is from: http://stat.wvu.edu/~jharner/courses/stat512/docs/Nonlinear-Regression.pdf


# What is the expected value if calcium = 10?
coefs <- coef(amount.nls)
Ca <- 10
coefs["b0"] + coefs["b1"] * exp(coefs["b2"] * Ca)

# A: b0 = 1.825287