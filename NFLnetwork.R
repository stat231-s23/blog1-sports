library(ggnetwork)
library(igraph)
library(tidyverse)


nfl <- read_csv("dataforNFL/nfl_draft_picks.csv")
nflrounds <- nfl%>% 
  filter(Round == 1)%>%
  filter(Year > 2010)
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
nfl_igraph_full <- nfl_with_states%>% 
  select(School, Team, everything()) %>%
  graph_from_data_frame(directed = TRUE)
gorder(nfl_igraph_full)

nfl_network_full <- ggnetwork(nfl_igraph_full)
#plot the network
ggplot(data=nfl_network_full
       , aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed", length = unit(8, "pt")),
             color = "blue") +
  geom_nodes() +
  geom_nodelabel(aes(label = name)) +
  theme_blank() 


ggplot(data=nfl_network_full, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "lightgray", curvature = .2) +
  geom_nodes(aes(size=Pick, color = Year)) +
  geom_nodelabel_repel(aes(label = name, color = Year)) +
  labs(size = "Year") +
  guides(color = "none") +
  theme_blank() +
  theme(legend.position = "bottom")
