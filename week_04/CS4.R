#CS4_1st_approach
# install.packages("nycflights13")
# install.packages("dplyr")

library("tidyverse")
library("nycflights13")
library("dplyr")

view(flights)
view(airports)

farthest_airport <- flights %>%
arrange(desc(distance)) %>%
  slice(1)

farthest_airports_name <- farthest_airport %>%
left_join(airports, by=c("dest"="faa")) %>%
  select(destination=name)

farthest_airport <- as.character(farthest_airports_name$destination)

