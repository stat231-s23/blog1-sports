#This is a file wrangling the dataset for the NHL dataset
#import packages
library(maps)
library(tidyverse)

#import the nhl draft dataset
nhl <- read_csv("dataforNHL/nhldraft.csv")

#mutate to change nationalities to coutries on maps
nhlcountrynames <- nhl %>%
  mutate_if(is.numeric, ~replace_na(.,0))%>%
  mutate(nationality = ifelse(nationality == "CA", paste("Canada"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "US", paste("United States"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "FI", paste("Finland"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "SE", paste("Sweden"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "RU", paste("Russia"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "CZ", paste("Czech"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "FR", paste("France"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "CH", paste("Switzerland"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "DE", paste("Germany"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "SK", paste("Slovakia"), nationality ))




#add points per game to the skaters
nhlcountrynames1 <- nhlcountrynames %>%
  mutate(ppg = points/games_played)

#take draft data so players are a minimum 5 years removed from being drafted and in the 21century
nhlpost2000to2017years <- nhlcountrynames1 %>%
  filter(year < 2018)%>%
  filter(year > 1999)

#wrangle to filter people who have played more than 80 games
nhlrelevant <- nhlpost2000to2017years %>%
  filter(games_played > 79)

#mutate to create a variable that divides games played by point shares
nhlmosteffective <- nhlrelevant %>%
  mutate(winshares = point_shares/games_played)%>%
  arrange(desc(winshares))


#make variable that multiplies pick in draft to winshares
nhlmosteffectivetotal1 <- nhlmosteffective %>%
  mutate(valueofpick = winshares * overall_pick)%>%
  arrange(desc(valueofpick))

#make variable that multiplies pick in draft to winshares
nhlmosteffectiveskater1 <- nhlmosteffectivetotal1 %>%
  mutate(valueofskater = winshares * overall_pick * ppg)%>%
  arrange(desc(valueofskater))