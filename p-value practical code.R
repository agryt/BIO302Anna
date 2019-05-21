#### P-VALUE PRACTICAL ####

# NONE OF THIS IS RIGHT

# loading tidyverse
library(tidyverse)

# loading broom
library(broom)

# choosing random numbers
set.seed(42)    # guarantees reproducibility, put in a random number
x <- 1:20
y <- rnorm(20)

# creating a model
mod <- lm(y ~ x)

# performing an anova analyses
anova(mod)
summary(mod)

# p-value is 0.1252
# effect size (r^2) is 0.1256

# this does the same (I think), but with broom
glance(mod)
tidy(mod)

# we want to do this many times
mod1000 <- rerun(.n = 1000, tibble(x = 1:20, y = rnorm(20))) %>%  # n=1000 to run this 1000 times
  map(~ lm(y ~ x, data = .)) %>% 
  map_df(glance) 

nbins <- 20
ggplot(mod1000, aes(x = p.value)) + geom_histogram(binwidth = 1/nbins, center = 1/nbins/2) + geom_hline(yintercept = 1000/nbins, colour = "red")


# finding a small effect
mod1001 <- rerun(.n = 1000, tibble(x = 1:20, y = rnorm(20, x*0.3))) %>% 
  map(~ lm(y ~ x, data = .)) %>% 
  map_df(glance) 

nbins <- 20
ggplot(mod1001, aes(x = p.value)) + geom_histogram(binwidth = 1/nbins, center = 1/nbins/2) + geom_hline(yintercept = 1000/nbins, colour = "red")


mod1001 %>% mutate(sig = p.value < 0.05) %>% 
  ggplot(aes(x = 0.3, fill = sig)) + geom_histogram() + geom_vline(xintercept = 0, colour = "grey50", linetype = "dashed")
