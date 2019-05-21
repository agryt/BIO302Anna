#### TIDYVERSE PRACTICAL ####

#### EXPLORING THE DATA ####

# loading tidyverse
library(tidyverse)

# importing the data, the 03.csv file
sykkel <- read_csv('03.csv')          # imports the data and calls it 'sykkel'
sykkel                                # shows the imported data


# Q: Which is the most popular starting station?

sykkel %>% 
  count(start_station_name) %>%       # counts the number of each start station name
  arrange(desc(n)) %>%                # arranges the number from highest to lowest
  slice(1)                            # shows only the top row, aka the highest value

# A: Møllendalsplass was used as start station 1502 times, which was the highest value


# Q: Plot the number of hires and returns from each station

# hires will be the number of times this station is start station
# returns will be the number of times this station is end station
# we want to know how many times a station has been used as start station, and how many times the same station has been used as end station
# we want to know this for ALL station
# given a station, how many times was this used as end station and start station

# plot showing numbers per start station:
sykkel %>% 
  count(start_station_name) %>% 
  ggplot(aes(x = n, y = start_station_name)) + geom_point()

# plot showing numbers per end station:
sykkel %>% 
  count(end_station_name) %>% 
  ggplot(aes(x = n, y = end_station_name)) + geom_point()


# TRIED TO GET EVERYTHING INTO ONE DIAGRAM:

# make new dataset with number of starts per station:
start_sykkel <- sykkel %>%  
  count(start_station_name) %>% 
  rename(start_n = n)

# make new dataset with number of ends per station:
end_sykkel <- sykkel %>% 
  count(end_station_name) %>% 
  rename(end_n = n)

# joining the two tables:
joined_sykkel <- inner_join(start_sykkel, end_sykkel, by = c("start_station_name" = "end_station_name"))

# need to use gather to join them

# making a bar chart

# DID NOT MANAGE THIS


# Q: Which is the most popular pair of start and end stations?

sykkel %>% 
  count(start_station_name, end_station_name) %>% 
  arrange(desc(n)) %>% 
  slice(1)

# A: From Møllendalsplass to Nonneseterplass had 231 trips, was most popular


# Q: What was the longest/shortest duration of hire?

# need to make a new column with end time minus start time
sykkel %>% 
  mutate(duration = ended_at - started_at) %>%     # making the new column
  select(ended_at, started_at, duration)           # want to see only these 3 columns

# finding shortest duration:
sykkel %>% 
  arrange(duration) %>% 
  slice(1:5) %>%          # to see the top 5 shortest duration
  select(duration)
# A: shortest duration was 61 seconds

# finding longest duration:
sykkel %>% 
  arrange(desc(duration)) %>% 
  slice(1:5) %>% 
  select(duration)
# A: longest duration was 164 999 seconds, which is ~ 46 hours


# Q: Plot the distribution of hire duration.

# time should be on the x axis, number on the y axis
# plotting:
sykkel %>% 
  count(duration) %>% 
  ggplot(aes(x = duration, y = n)) + geom_line() + coord_trans(x="log10")
# coord_trans(x="log10") makes the x axis logarithmic


# Q: What is the median duration of hire from each station?
sykkel %>% 
  group_by(start_station_name) %>% 
  summarise(median_duration = median(duration)) %>% 
  view()

# A: answers can be seen by viewing this


# Q: Map this information

sykkel %>% 
  group_by(start_station_name, start_station_latitude, start_station_longitude) %>% 
  summarise(median_duration = median(duration)) %>% 
  ggplot(aes(x = start_station_longitude, y = start_station_latitude)) + geom_point()
# this gives a coordinate system with dots


# Q: Are there any significant differences in duration between stations.

sykkel %>% count(start_station_name) %>% view() # needed to see order of stations

lm(data=sykkel, duration~start_station_name) %>% 
  anova
# p-value is 8.925e-08, so there is a significant difference
# but between what stations?
lm(data=sykkel, duration~start_station_name) %>% 
  summary()
# here it is possible to see that for the first station, ADO arena, the duration is significantly different from the stations Høgskulen på Vestlandet, Lysverket, and Verftet.

# it is possible to check this for all stations, see lecture 4 in BIO300B


# Q: How far does a typical cyclist travel?

sykkel %>% 
  group_by(start_station_latitude, start_station_longitude, end_station_latitude, end_station_longitude) %>% 
  mutate(distance = sqrt((start_station_latitude - end_station_latitude)^2 + (start_station_longitude - end_station_longitude)^2), abs(distance)) %>% 
  ungroup() %>% 
  summarise(mean_distance = mean(distance))

# A: in latitude + longitude, the mean distance is 0.0152


# Q: What is the relationship between distance travelled and time taken?

sykkel %>% 
  mutate(distance = sqrt((start_station_latitude - end_station_latitude)^2 + (start_station_longitude - end_station_longitude)^2), abs(distance)) %>% 
  ggplot(aes(x = distance, y = duration)) + geom_point() + stat_smooth(method = "lm", formula = y~x, se = F) 
# the regression line (blue) shows the linear relationship


# Q: How fast was the fastest cyclist (for simplicity assume a straight line of travel)

sykkel %>% 
  mutate(speed = abs(sqrt((start_station_latitude - end_station_latitude)^2 + (start_station_longitude - end_station_longitude)^2)) / duration) %>% 
  arrange(desc(speed)) %>% 
  slice(1:5) %>% 
  select(speed)

# A: the fastest person travelled at coordinates/seconds = 0.000117



#### DAY AND TIME ####

# With help from the `lubridate` package

# Q: How does the number of hires vary throughout the day and week?

