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


