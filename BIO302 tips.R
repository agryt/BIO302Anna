# loading tidyverse
library("tidyverse")




#### PIPES ####
# how to get pipes ( %>% ): control + shift + m
# pipes say "this goes inside here:"

data(BCI, package = "vegan")
plot(sort(colSums(BCI), decreasing = TRUE))
x11() <- opens plot in new window

# more readable version:
plot(
  sort(
    colSums(BCI)           # this sums all the columns
    , decreasing = TRUE
    )
  )

# could also do this:
x1 <- colSums(BCI)
x2 <- sort(x1, decreasing = TRUE)
plot(x2)
# this will be very confusing when you have long codes (due to naming)

# could change it to this:
x <- colSums(BCI)
x <- sort(x, decreasing = TRUE)
plot(x)
# this is bad because a fault can happen in the middle and you don't know where

#solution: use PIPES!

BCI %>% 
  colSums() %>% 
  sort(decreasing = TRUE) %>%      #the line above goes into the bracket even with the text
  plot()
# the lines use the line above




#### ONE TABLE FUNCTIONS ####
# select, filter, mutate, group_by, summarise, slice, count, arrange, nest

#select

as_tibble(iris)               # showing iris as a tibble
iris <- as_tibble(iris)       # naming the tibble iris
iris                          # showing what we now called iris

# selecting for the Sepal.Length and Species
iris %>% select(Sepal.Length, Species)
select(iris, Sepal.Length, Species)   #this is the other way to say this (without pipes)

# pipe to view() -> shows all the values
.Last.value %>% view()

# can get rid of something by putting minus in front of it
iris %>% select(-Sepal.Width)     # this removes Sepal.Width

# NONE OF THIS CHANGES THE IRIS DATASET!!!

# using colon it ises the factors to and from the chosen ones
iris %>% select(Sepal.Length:Petal.Length)    # shows f.o.m Sepal.Length t.o.m Petal.Length

# renaming columns
iris %>% rename(sepal.length = Sepal.Length)  # new name equals old name
# can put multiple in one, like this:
iris %>% rename(sepal.length = Sepal.Length, spp = Species)
# if you want to use space in names, need to use 'n a m e'


# filter
iris %>% filter(Sepal.Length > 5, Petal.Length < 2)   # shows only the ones who fulfill this
# can use this with other commands, for example:
iris %>% filter(Sepal.Length > 5, Petal.Length < 2) %>% 
  select(Species)


# mutate
iris %>% mutate(petal.area = Petal.Length * Petal.Width)
# here we made a new column showing the petal area
iris %>% mutate(Species = toupper(Species))     # this capitalizes the species names
# similarly: tolower changes it to lowercase


# group_by
iris %>% group_by(Species)
# this tells us that we have 3 species

iris %>% group_by(Species) %>% 
  summarise(mean_petal_length = mean(Petal.Length))
# mean_petal_length is the name we gave it, will show mean petal length for all species

iris %>% group_by(Species) %>% 
  summarise(mean_petal_length = mean(Petal.Length), sd = sd(Petal.Length))
# now we also get standard deviation, named "sd"
# we don't need to name things! could just do this:
iris %>% group_by(Species) %>% 
  summarise(mean(Petal.Length), sd(Petal.Length))
# but it is nice to name things, and useful

iris %>% group_by(Species) %>% 
  mutate(mean_petal_length = mean(Petal.Length))
# this adds a column showing the mean petal length

# can add ungroup
iris %>% group_by(Species) %>% 
  mutate(mean_petal_length = mean(Petal.Length)) %>% 
  ungroup()
# now we no longer gets the info about there being 3 groups
# now if we summarise, it doesn't get sorted into the 3 groups
  # for example: when we want the mean, we will get the mean for ALL species
  # (unlike before)


#arrange
iris %>% arrange(Petal.Length)          # arranges by petal length, small to large
iris %>% arrange(desc(Petal.Length))    # arranges by petal length, large to small

iris %>% 
  group_by(Species) %>%         # groups into species
  arrange(Petal.Length) %>%     # arranges by petal length (small to large)
  slice(1)      # taking out the top 1 row, can do slice(1:3) to show top 3 for all species
# this means that the smallest flower of each species is shown!


# nest
iris %>% group_by(Species) %>% nest()
# this sorts the data for each species into new tibbles
  # we have tibbles inside a tibble

# can use mutate with this
iris %>% 
  group_by(Species) %>% 
  nest() %>% 
  mutate(mod = map(data, ~lm(Sepal.Length ~ Sepal.Width, data = .)))
# map is part of tidyverse, what did it do?
# mutate makes a new column
# we are now making one linear model per species

# we want to see what is in the new tibbles
iris %>% 
  group_by(Species) %>% 
  nest() %>%             # nest into tibbles
  mutate(mod = map(data, ~lm(Sepal.Length ~ Sepal.Width, data = .))) %>% # put linear models on each of these tibbles (aka the species)
  mutate(coef = map(mod, broom::tidy)) %>%    # uses broom to extract coefficient from each of the models
  unnest(coef)       # this is to be able to see the data, pulls the coefficients out of the model, only coef is unnested




#### RESHAPING DATA ####
# make tidy data
# gather, spread

# gather
iris %>% 
  rownames_to_column() %>% 
  gather(key = variable, value = measurement, -Species, -rowname) %>% 
      # key is ? and value is data
  group_by(Species, variable) %>% 
  summarise(mean = mean(measurement))

# can plot this:
iris %>% 
  rownames_to_column() %>% 
  gather(key = variable, value = measurement, -Species, -rowname) %>% 
  ggplot(aes(x = variable, y = measurement, fill = Species)) + geom_violin()



#### TWO TABLE FUNCTION ####
# left_join, full_join, semi_join, anti_join




#### N TABLE FUNCTIONS ####
# bind_rows, bind_cols, crossing