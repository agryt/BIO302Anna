#### P-VALUE PRACTICAL ####

# loading tidyverse
library(tidyverse)

# loading broom
library(broom)

#### some random data ####

# choosing random numbers
set.seed(42)    # guarantees reproducibility, put in a random number
dat <- tibble(
  x = 1:20, 
  y = rnorm(20))

# creating a model
mod <- lm(y ~ x, data = dat)

# performing an anova analyses
anova(mod)
summary(mod)

# p-value is 0.1252
# effect size (r^2) is 0.1256

# this does the same (I think), but with broom
glance(mod)
tidy(mod)

#### lots of random data ####

# we want to do this many times
rerun(.n = 2, tibble(x = 1:20, y = rnorm(20))) %>%  # do 2 first to check if it works
  map(~ lm(y ~ x, data = .)) %>% 
  map_df(glance)

# making the model:
mod1000 <- rerun(.n = 1000, tibble(x = 1:20, y = rnorm(20))) %>%  # n=1000 to run this 1000 times
  map(~ lm(y ~ x, data = .)) %>% 
  map_df(tidy) %>% 
  filter(term == "x")

nbins <- 20
ggplot(mod1000, aes(x = p.value)) + 
  geom_histogram(binwidth = 1/nbins, center = 1/nbins/2) + # make bins centered correctly
  geom_hline(yintercept = 1000/nbins, colour = "red")
# this gives a histogram with a red line at 50 showing the distribution of p-values

mod1000 %>% 
  mutate(sig = p.value < 0.05) %>% 
  ggplot(aes(x = estimate, fill = sig)) +
  geom_histogram() +
  geom_vline(xintercept = 0, colour = "grey50", linetype = "dashed")
# this gives a histogram showing how often the p-value will be true and false

#### finding a small effect ####

effect_sizes <- c(0, 0.01, 0.05, 0.15) # 0.05 will give a change of 1 over the 0-20 x range == standard deviation

# making the model:
mod_effect <- effect_sizes %>% 
  set_names() %>% 
  map_df(~ {
    rerun(.n = 1000, tibble(x = 1:20, y = rnorm(20, mean = x * .))) %>% 
    map(~ lm(y ~ x, data = .)) %>% 
    map_df(tidy)
  }, .id = "effect") %>% 
  filter(term == "x")

nbins <- 20
ggplot(mod_effect, aes(x = p.value, colour = effect)) +
  geom_freqpoly(binwidth = 1/nbins, center = 1/nbins/2) +
  geom_hline(yintercept = 1000/nbins, colour = "grey50", linetype = "dashed") +
  lims(x = c(0, 1))
# this gives a plot showing the p-value on the x-axis and count on the y-axis for each effect size

mod_effect %>% 
  mutate(sig = p.value < 0.05) %>% 
  ggplot(aes(x = estimate, fill = sig)) +
  geom_histogram() +
  geom_vline(data = tibble(effect = effect_sizes), aes(xintercept = effect), linetype = "dashed", colour = "grey50") +
  facet_wrap(~ effect)
# this gives a plot showing how often the p-value will be true and false for each effect size with the estimate on the x-axis

#### effect of sample size ####

sample_sizes <- c(20, 40, 80, 160)
effect <- 0.01

# making the model:
mod_n <- sample_sizes %>%
  set_names() %>%
  map_df(~{
    rerun(.n = 1000, tibble(x = seq(1, 20, length = .), y = rnorm(., mean = x * effect))) %>% 
      map(~lm(y ~ x, data = .)) %>% 
      map_df(tidy)
  }, .id = "n") %>%
  filter(term == "x") %>% 
  mutate(n = factor(n, levels = sample_sizes))

nbins <-  20
ggplot(mod_n, aes(x = p.value, colour = n)) +
  geom_freqpoly(binwidth = 1/nbins, center = 1/nbins/2) +
  geom_hline(yintercept = 1000/nbins, colour = "grey50", linetype = "dashed") +
  lims(x = c(0, 1))
# this gives a plot showing the count of each p-value for each n

mod_n %>% 
  mutate(sig = p.value < 0.05) %>% 
  ggplot(aes(x = estimate, fill = sig)) +
  geom_histogram() +
  geom_vline(xintercept = effect, linetype = "dashed", colour = "grey50") +
  facet_wrap(~ n)
# this gives a plot showing true and false significance for each p-value with estimate on x-axis

#### effect of p-hacking ####

# making the model
p_hack <- rerun(.n = 1000, tibble(x = 1:20, y = rnorm(20))) %>% 
  map_df(~{
    data <- .
    mod0 <- lm(y ~ x, data = data)
    n  <- 1#counter
    while(glance(mod0)$p.value > 0.05 & n <=3){#while conditions are true - do expression
      data <- data %>% slice(-which.max(abs(resid(mod0))))#remove point with largest absolute residual
      mod0 <- lm(y ~ x, data = data)
      n <- n + 1#increment counter
    }
    tidy(mod0)
  }) %>% 
  filter(term == "x")

nbins <-  20
p_hack %>% 
  ggplot(aes(x = p.value)) +
  geom_histogram(binwidth = 1/nbins, center = 1/nbins/2) +
  geom_hline(yintercept = 1000/nbins, colour = "red") 
# shows a clear effect of p-hacking (because it is above the red line?)

p_hack %>% 
  ggplot(aes(x = p.value)) +
  geom_histogram(binwidth = 0.01, center = 0.005) + 
  xlim(0, 0.1) 
# shows good evidence of p-hacking because there are many values close to threshold (0.05)

# P-hacking can be detected by an excess of p-values just below p = 0.05 ([Head et al. 2015](https://doi.org/10.1371/journal.pbio.1002106)). Can you detect p-hacking in your output?