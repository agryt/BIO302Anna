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
