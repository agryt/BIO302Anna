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

# plot everything in one plot (from solution):
sykkel %>% 
  select(start_station_name, end_station_name) %>% 
  gather(key = key, value = station) %>% 
  count(key, station) %>% 
  filter(n > 10) %>%  # this is to remove stations used very little
  mutate(key = factor(key, levels = c("start_station_name", "end_station_name"))) %>% # this is to force order of bars
  ggplot(aes(x = station, y = n, fill = key)) +
  geom_col(position = position_dodge()) +
  scale_fill_discrete(labels = c("start_station_name" = "Hire", "end_station_name" = "Return")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# this gives a bar chart showing both hire and return for each station with >10 check outs and check ins


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
# code from solution:
sykkel %>% 
  select(duration) %>% 
  slice(which.max(duration)) %>% 
  mutate(duration = duration/60/60) # to get answer in hours


# Q: Plot the distribution of hire duration.

# time should be on the x axis, number on the y axis
# plotting:
sykkel %>% 
  count(duration) %>% 
  ggplot(aes(x = duration, y = n)) + geom_line() + coord_trans(x="log10")
# coord_trans(x="log10") makes the x axis logarithmic

# code from solution:
sykkel %>% 
  ggplot(aes(x = duration/60)) + # /60 to get minutes
  geom_histogram() +
  xlim(0, 60)  # to only get hires from 0 to 60 min


# Q: What is the median duration of hire from each station?
sykkel %>% 
  group_by(start_station_name) %>% 
  summarise(median_duration = median(duration)) %>% 
  view()
# in solution he removed stations with <10 hires, i have not

# A: answers can be seen by viewing this


# Q: Map this information

sykkel %>% 
  group_by(start_station_name, start_station_latitude, start_station_longitude) %>% 
  summarise(median_duration = median(duration)) %>% 
  ggplot(aes(x = start_station_longitude, y = start_station_latitude)) + geom_point()
# this gives a coordinate system with dots representing all stations

# code from solution:
med_dur <- sykkel %>% 
  group_by(start_station_name, start_station_latitude, start_station_longitude) %>% 
  filter(n() > 10) %>% # removes stations with very few hires
  summarise(med = median(duration)/60) # /60 to get minutes

library(ggrepel)

ggplot(med_dur, aes(x = start_station_longitude, y = start_station_latitude, size = med)) +
  geom_point(colour = "grey50") +
  scale_size_area(name = "Median duration") +
  ggrepel::geom_text_repel(aes(label = start_station_name), size = 3)
# shows map with dots representing each station with >10 hires, dot size representing medium hire duration in minutes


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

# code from solution, compares ALL stations selected for:
mod_dur <- sykkel %>% 
  group_by(start_station_name, start_station_latitude, start_station_longitude) %>% 
  filter(n() > 10, # removes stations with very few hires
         duration < 60 * 60) %>% # removes outliers
  lm(duration ~ start_station_name, data = .)
anova(mod_dur)
# p-value is 2.2e-16 so there is a significant difference 


# Q: How far does a typical cyclist travel?

sykkel %>% 
  group_by(start_station_latitude, start_station_longitude, end_station_latitude, end_station_longitude) %>% 
  mutate(distance = sqrt((start_station_latitude - end_station_latitude)^2 + (start_station_longitude - end_station_longitude)^2), abs(distance)) %>% 
  ungroup() %>% 
  summarise(mean_distance = mean(distance))

# A: in latitude + longitude, the mean distance is 0.0152

# code from solution:
library(fields)
distances <- sykkel %>% 
  mutate(distance = fields::rdist.earth.vec( # have long/lat so need great circle distances
    x1 = cbind(start_station_longitude, start_station_latitude), 
    x2 = cbind(end_station_longitude, end_station_latitude), 
    miles = FALSE
  ))
# great circle distance is the shortest distance between two points on the surface of a sphere, measured along the surface of the sphere

distances %>% 
  ggplot(aes(x = distance)) +
  geom_histogram() +
  labs(x = "Distance km")
# this gives a plot showing a histogram of the distances travelled in km


# Q: What is the relationship between distance travelled and time taken?

sykkel %>% 
  mutate(distance = sqrt((start_station_latitude - end_station_latitude)^2 + (start_station_longitude - end_station_longitude)^2), abs(distance)) %>% 
  ggplot(aes(x = distance, y = duration/60)) + geom_point() + stat_smooth(method = "lm", formula = y~x, se = F) + ylim(0, 60)
# the regression line (blue) shows the linear relationship
# here the distance is in coordinates, so not good
# duration/60 to get minutes, ylim to not show the extremely long ones

# code from solution:
ggplot(distances, aes(x = distance, y = duration/60)) +
  geom_point(alpha = 0.2) +
  ylim(0, 60)


# Q: How fast was the fastest cyclist (for simplicity assume a straight line of travel)

sykkel %>% 
  mutate(speed = abs(sqrt((start_station_latitude - end_station_latitude)^2 + (start_station_longitude - end_station_longitude)^2)) / duration) %>% 
  arrange(desc(speed)) %>% 
  slice(1:5) %>% 
  select(speed)

# A: the fastest person travelled at coordinates/seconds = 0.000117
# this is not a very useful answer, tells us nothing...

# code from solution:
distances %>% 
  mutate(speed = distance/(duration/60/60)) %>% # will get km/hour
  select(start_station_name, end_station_name, speed) %>% 
  arrange(desc(speed)) %>% 
  slice(1:5)

# A: the fastest cyclist travelled at 28.3 km/hour (!)


#### DAY AND TIME ####

# With help from the `lubridate` package

# Q: How does the number of hires vary throughout the day and week?

library(lubridate)
sykkel %>% 
  mutate(hour = hour(started_at), day = wday(started_at, label = TRUE)) %>% 
  count(day, hour) %>% 
  ggplot(aes(x = hour, y = n, colour = day)) +
  geom_line()
# gives a plot showing the number of hires per hour for every day of the week
# there is a significant difference between weekdays and weekend days
# on weekdays there are peaks around 7 and 15
# on weekends the number of hires is quite evenly high between 11 and 17


# Q: How does the average speed vary through the day?
distances %>% 
  filter(wday(started_at, week_start = 7) %in% 2:6) %>%  # to only look at weekdays
  mutate(hour = hour(started_at)) %>% 
  mutate(speed = distance/(duration/60/60)) %>%  # to get speed in km/hour
  ggplot(aes(x = hour, y = speed, group = hour)) +
  geom_boxplot()
# get a bloxplot showing the speed every hour for weekdays

# A: the mean does not vary much, but appears to be a bit higher early in the day


# Q: Are some stations more popular at different times of day for hire and return?

# hires per hour:
sykkel %>% 
  mutate(hour = hour(started_at)) %>% 
  group_by(start_station_name, hour) %>% 
  count() %>% 
  group_by(hour) %>% 
  filter(between(hour, 5, 22)) %>%  # to show hires only between 5 and 22
  mutate(percent = n/sum(n) * 100) %>% 
  group_by(start_station_name) %>% 
  filter(sum(n) > 400) %>%  # to remove little used stations
  ggplot(aes(x = hour, y = percent, colour = start_station_name)) +
  geom_line()
# this gives a plot showing the percentage of hires per hour for each station with hires > 400

# returns per hour:
sykkel %>% 
  mutate(hour = hour(ended_at)) %>% 
  group_by(end_station_name, hour) %>% 
  count() %>% 
  group_by(hour) %>% 
  filter(between(hour, 5, 22)) %>%  # to show hires only between 5 and 22
  mutate(percent = n/sum(n) * 100) %>% 
  group_by(end_station_name) %>% 
  filter(sum(n) > 400) %>%  # to remove little used stations
  ggplot(aes(x = hour, y = percent, colour = end_station_name)) +
  geom_line()
# this gives a plot showing the percentage of returns per hour for each station with returns > 400

# hires per day
sykkel %>% 
  mutate(day = wday(started_at, label = TRUE)) %>% 
  group_by(start_station_name, day) %>% 
  count() %>% 
  group_by(day) %>% 
  mutate(percent = n/sum(n) * 100) %>% 
  group_by(start_station_name) %>% 
  filter(sum(n) > 400) %>%  # to remove little used stations
  ggplot(aes(x = day, y = percent, colour = start_station_name, group = start_station_name)) +
  geom_line()
# this gives a plot showing the percentage of hires per day for each station with hires > 400

# returns per day
sykkel %>% 
  mutate(day = wday(ended_at, label = TRUE)) %>% 
  group_by(end_station_name, day) %>% 
  count() %>% 
  group_by(day) %>% 
  mutate(percent = n/sum(n) * 100) %>% 
  group_by(end_station_name) %>% 
  filter(sum(n) > 400) %>%  # to remove little used stations
  ggplot(aes(x = day, y = percent, colour = end_station_name, group = end_station_name)) +
  geom_line()
# this gives a plot showing the percentage of returns per day for each station with returns > 400