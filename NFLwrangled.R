#This is a file wrangling the dataset for the NFL dataset
#import packages
library(maps)
library(tidyverse)
library(sf)
library(ggplot2)
library(car)
library(RColorBrewer)

#import the NFL draft dataset
nfl <- read_csv("dataforNFL/nfl_draft_picks.csv")

#filter the players that were picked only in the 1st round 
#and only look at years 2017-2021
nflrounds <- nfl%>% 
  filter(Round == 1)%>%
  filter(Year > 2005)

#add states to the dataset 
nfl_with_states <- nflrounds %>%
mutate(
  state_cd = case_when(
    School == "Alabama" ~ "AL",
    School == "Alabama State" ~ "AL",
    School == "Arizona State" ~ "AZ",
    School == "Arizona" ~ "AZ",
    School == "Arkansas" ~"AR", 
    School == "Auburn" ~ "AL", 
    School == "Baylor" ~ "TX", 
    School == "Boise State" ~ "ID", 
    School == "Boston College" ~ "MA", 
    School == "Brigham Young" ~ "UT", 
    School == "Buffalo" ~ "NY",
    School == "California" ~ "CA",
    School == "Central Florida" ~ "FL", 
    School == "Central Michigan" ~ "MI",
    School == "Clemson" ~ "SC", 
    School == "Colorado" ~ "CO",
    School == "Connecticut" ~ "CT", 
    School == "Delaware" ~ "DE", 
    School == "Duke" ~ "NC", 
    School == "East Carolina" ~ "NC",
    School == "Florida" ~ "FL", 
    School == "Florida State" ~ "FL", 
    School == "Fresno State" ~ "CA", 
    School == "Georgia" ~ "GA", 
    School == "Houston" ~ "TX", 
    School == "Idaho" ~ "ID", 
    School == "Illinois" ~ "IL", 
    School == "Iowa" ~ "IA", 
    School == "Kansas" ~ "KS", 
    School == "Kansas State" ~ "KS", 
    School == "Kentucky" ~ "KY", 
    School == "Louisiana State" ~ "LA",
    School == "Louisiana Tech" ~ "LA",
    School == "Louisville" ~ "KY", 
    School == "Maryland" ~ "MD", 
    School == "Memphis" ~ "TN", 
    School == "Miami (FL)" ~ "FL", 
    School == "Michigan" ~ "MI", 
    School == "Michigan State" ~ "MI", 
    School == "Minnesota" ~ "MN", 
    School == "Mississippi" ~ "MS", 
    School == "Mississippi State" ~ "MS", 
    School == "Missouri" ~ "MO", 
    School == "Nebraska" ~ "NE", 
    School == "North Carolina" ~ "NC", 
    School == "North Carolina State" ~ "NC",
    School == "North Dakota State" ~ "ND",
    School == "Northern Illinois" ~ "IL",
    School == "Northwestern" ~ "IL", 
    School == "Notre Dame" ~ "IN", 
    School == "Ohio State" ~ "OH", 
    School == "Oklahoma" ~ "OK", 
    School == "Oklahoma State" ~ "OK",
    School == "Oregon" ~ "OR", 
    School == "Oregon State" ~ "OR", 
    School == "Penn State" ~ "PA", 
    School == "Pittsburgh" ~ "PA",
    School == "Purdue" ~ "IN", 
    School == "Rutgers" ~ "NJ", 
    School == "San Diego State" ~ "CA", 
    School == "South Carolina" ~ "SC", 
    School == "Stanford" ~ "CA", 
    School == "Syracuse" ~ "NY", 
    School == "Temple" ~ "PA", 
    School == "Tennessee" ~ "TN", 
    School == "Tennessee State" ~ "TN", 
    School == "Texas" ~ "TX", 
    School == "Texas A&M" ~ "TX", 
    School == "Texas Christian" ~ "TX", 
    School == "Texas Tech" ~ "TX",
    School == "Texas-San Antonio" ~ "TX", 
    School == "Troy" ~ "AL", 
    School == "Tulsa" ~ "OK", 
    School == "UCLA" ~ "CA", 
    School == "USC" ~ "CA", 
    School == "Utah" ~ "UT", 
    School == "Utah State" ~ "UT", 
    School == "Vanderbilt" ~ "TN", 
    School == "Virginia" ~ "VA", 
    School == "Virginia Tech" ~ "VA", 
    School == "Wake Forest" ~ "NC", 
    School == "Washington" ~ "WA", 
    School == "Washington State" ~ "WA", 
    School == "Western Michigan" ~ "MI", 
    School == "West Virginia" ~ "WV", 
    School == "Wisconsin" ~ "WI", 
    School == "Wyoming" ~ "WY") 
  )

#import the map
states_map <- maps::map("state", regions = ".", plot = FALSE, fill = TRUE) %>%
  st_as_sf()
state_info <- data.frame(state_full = tolower(state.name)
                         , state_cd = state.abb)

#add full name of states to NFL dataset and merge with mapping dataset
nfl_final <- nfl_with_states %>%
  left_join(state_info, by = "state_cd") %>%
  right_join(states_map, by = c("state_full"="ID"))

#create plot on US country map
ggplot(nfl_final, aes(geometry=geom, fill = Overall)) +
  geom_sf(color = "black") +
  theme_void() +
  labs(fill = "Overall Draft Pick (1-32)"
       , frame.colour = "black"
       , barwidth = 15
       , barheight = 1.5
       , title = "  U.S. College Football Athletes Drafted in the First Round (from 2006 to 2021)") +
         theme(legend.position="bottom") +
  scale_fill_distiller(palette = "Spectral", direction = 1) 


#user interactive table 
library(DT)
nfl_final %>% 
  select(Name, Position, Overall, School, Year,) %>% 
  datatable(colnames = c("Name", "Position", "Overall Draft Pick", "School", "Year"),
            rownames = head(nfl_final),
            filter = 'top',
            options = list(pageLength = 10, autoWidth = TRUE))

