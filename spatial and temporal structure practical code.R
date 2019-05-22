#### SPACIAL AND TEMPORAL STRUCTURE PRACTICAL ####

# 1. Simulate a 100-observation autocorrelated timeseries with arima.sim, with a first order autoregressive coefficient of 0.5. Also make a time vector of 1:100
y1 <- arima.sim(list(ar = 0.5), n = 100)        # ar = autoregressive
x <- 1:100


# 2. Plot the data.
plot(y1)


# 3. Regress the timeseries against time with an OLS model. Does the model appear to be statistically significant?
# OLS = ordinary least squares
ols_model <- lm(y1 ~ x)
summary(ols_model)
# p-value = 0.0779
# it is not significant

# this does the same, but this is how he did it:
mod1 <- lm(y1~time(y1))
summary(mod1)


# 4. Plot the model diagnostics, including an acf and pacf of the residuals.
acf(resid(mod1))
# this gives a graph showing the ACF
pacf(resid(mod1))
# this gives a graph showing the partial ACF


# 5. Use the Durbin-Watson test to test the residuals for autocorrelation.
# loading the lmtest library
library(lmtest)
dwtest(mod1)
# result:
# DW = 0.96394
# p-value = 4.712e-08
# alternative hypothesis: true autocorrelation is greater than 0
# there is positive autocorrelation because DW < 2


# 6. Fit a gls with an appropriate correlation structure. Is this a better model? How have the p-value and effect size changed?
# loading library nlme that gls is a part of
library(nlme)
# making the model
year <- time(y1)
gls_mod <- gls(y1 ~ year, corr = corAR1())
summary(gls_mod)
# p-value: 0.3584

#making a gls model without the correlation to compare this to
gls_mod2 <- gls(y1 ~ year)
summary(gls_mod2)
# p-value: 0.0779

# performing anova comparing the two models mod1 and gls_mod
anova(gls_mod, gls_mod2)
# gls_mod has more degrees of freedom and lower AIC, so it is better (p-value <.0001)


# 7. Repeat the above 1000 times and find how autocorrelation affects the distribution of the p-value and the effect size.
library(tidyverse)
library(broom)
library(broom.mixed)

# p-value:
mod1000 <- rerun(.n = 1000, tibble(x = 1:100, y = arima.sim(list(ar = 0.5), n = 100))) %>% 
  map(~ gls(y ~ x, corr = corAR1(), data = .)) %>% 
  map_df(tidy) %>% 
  filter((term == "x"))

# plotting this to see the p-values
ggplot(mod1000, aes(x = p.value)) + geom_histogram()
# this shows the distribution of p-values, which is quite even for all p-value intervals

# effect size:
mod_n <- 
    rerun(.n = 1000, tibble(x = 1:100, y = arima.sim(list(ar = 0.5), n = 100))) %>% 
      map(~ gls(y ~ x, corr = corAR1(), data = .)) %>% 
      map_df(tidy) %>% 
  filter(term == "x") 

mod_n %>% 
  mutate(sig = p.value < 0.05) %>% 
  ggplot(aes(x = p.value, fill = sig)) +
  geom_histogram()




#### REAL DATA ####


# 1. The built-in dataset LakeHuron has annual lake level data from 1885 to 1972 Load the data with the command data(LakeHuron)
data("LakeHuron")


# 2. Plot the data.
plot(LakeHuron)


# 3. Regress the LakeHuron lake level against year using a linear model. Is there a significant trend?
# regression -> lm
# making the model
reg <- lm(LakeHuron ~ time(LakeHuron), data = LakeHuron)
# anova to test it
anova(reg)
# p-value: 3.545e-08
summary(reg)
# p-value: 3.55e-08
# there is a significant trend


# 4. Plot the autocorrelation and partial autocorrelation functions for the residuals from this regression. Interpret them.
acf(resid(reg))
# this gives a graph showing the ACF
pacf(resid(reg))
# this gives a graph showing the partial ACF
# this data is correlated because the ACF follows the pattern of the weight matrix for correlated observations almost exactly, and the partial ACF is close to 0


# 5. Fit an autoregressive models to the residuals. Compare the results with your interpretation of the PACF plot.
ar(x = resid(reg))
# results:
#Coefficients:
#     1        2  
#0.9714  -0.2754 
# this shows the rho values
# these values support that it is autoregressive


# 6. Fit a gls model using a corAR1 correlation structure. Test if the correlation structure is necessary. Is the trend significant?
year <- time(LakeHuron)
# gls model WITH autocorrelation
gls_lh <- gls(LakeHuron ~ year, corr = corAR1())
summary(gls_lh)
# p-value: 0.1282

# gls model WITHOUT autocorrelation
gls_lh2 <- gls(LakeHuron ~ year)
summary(gls_lh2)
# p-value: 0

# comparing the two gls models:
anova(gls_lh, gls_lh2)
# gls_h, the model with autocorrelation has a higher df and lower AIC, so it is better
# this difference is significant (p-value: <.0001)

# by using
summary(gls_lh)
# the p-value is 0.1282 for the slope, meaning the trend is not significant


# 7. Fit a gls model using a corARMA correlation structure with two AR terms. Is this model an improvement?

gls_arma <- gls(LakeHuron ~ year, corr = corARMA(p = 2))
summary(gls_arma)
# the parameter estimates Phi1 =  1.0203418 and Phi2 = -0.2741249 are similar to the coefficients from the ar model in task 5
# p-value: 0.0223
# this p-value is lower than the p-value for gls_lh, so this is a better model