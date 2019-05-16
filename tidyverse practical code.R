#### TIDYVERSE PRACTICAL ####

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

