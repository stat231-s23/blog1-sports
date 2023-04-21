#This is a file wrangling the dataset for the NFL dataset
#import packages
library(maps)
library(tidyverse)
library(sf)

#import the nfl draft dataset
nfl <- read_csv("dataforNFL/draft-data-20-years.csv")

#import the map
states_map <- maps::map("state", regions = ".", plot = FALSE, fill = TRUE) %>%
  st_as_sf()
data(state)

# select only the variables we need
nfl2 <- nfl %>%
  select(Tm, Player, College, DraftYear, Yrs)

