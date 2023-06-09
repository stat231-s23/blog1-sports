#This is a file wrangling the dataset for the NHL dataset
#import packages
library(maps)
library(tidyverse)
library(sf)

#import the nhl draft dataset
nhl <- read_csv("dataforNHL/nhldraft.csv")

#import the map
world_map <- maps::map("world", plot = FALSE, fill = TRUE) %>%
  st_as_sf()

#mutate to change nationalities to coutries on maps
nhlcountrynames <- nhl %>%
  mutate_if(is.numeric, ~replace_na(.,0))%>%
  mutate(nationality = ifelse(nationality == "CA", paste("Canada"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "US", paste("USA"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "FI", paste("Finland"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "SE", paste("Sweden"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "RU", paste("Russia"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "CZ", paste("Czech Republic"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "FR", paste("France"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "CH", paste("Switzerland"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "DE", paste("Germany"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "SK", paste("Slovakia"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "AT", paste("Austrian"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "AU", paste("Australia"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "DK", paste("Denmark"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "LV", paste("Latvia"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "UA", paste("Ukraine"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "ZA", paste("South Africa"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "EE", paste("Estonia"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "SI", paste("Slovenia"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "NL", paste("Netherlands"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "BY", paste("Belarus"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "GB", paste("UK"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "IT", paste("Italy"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "KZ", paste("Kazakhstan"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "NO", paste("Norway"), nationality ))%>%
  mutate(nationality = ifelse(nationality == "PL", paste("Poland"), nationality ))




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

# combine the map to the countries for sskaters
worldtotalmap <- nhlmosteffectiveskater1 %>%
  left_join(world_map, by = c("nationality" = "ID"), multiple = "all")%>%
  mutate_if(is.numeric, ~replace_na(.,0))


#summarize into countries
playerworldranking <- worldtotalmap %>%
  filter(position != "G")%>%
  group_by(nationality, geom) %>%
  summarise(avgpickvalue = mean(valueofskater, na.rm = TRUE))%>%
  arrange(desc(avgpickvalue))
  

#make graph to represent graph of best valued picks for skaters
ggplot(playerworldranking, aes(geometry = geom, fill = avgpickvalue)) +
  geom_sf() +
  theme_void() +
  labs(fill = "Value Of Pick Ranking"
       , title = "Best Valued Skater Picks Based on Home Country")

# make a plot for where the best valued goalies stem from
goaliedata <- nhlmosteffectivetotal1 %>%
  filter(position == "G")%>%
  select(year, overall_pick, team, player, nationality, games_played, 
         goalie_wins, goalie_losses, point_shares, save_percentage, 
         goals_against_average, valueofpick)%>%
  mutate(winpct = goalie_wins/games_played)

goalieworldmap <- goaliedata %>%
  left_join(world_map, by = c("nationality" = "ID"), multiple = "all")%>%
  mutate_if(is.numeric, ~replace_na(.,0))


goalieworldranking <- goalieworldmap %>%
  group_by(nationality, geom) %>%
  summarise(avgpickvalue = mean(valueofpick, na.rm = TRUE))%>%
  arrange(desc(avgpickvalue))

ggplot(goalieworldranking, aes(geometry = geom, fill = avgpickvalue)) +
  geom_sf() +
  theme_void() +
  labs(fill = "Value Of Pick Ranking"
       , title = "Best Valued Goaltender Picks Based on Home Country")




# RESET THE DATA AND LOOK AT WHAT TEAM PICK THE BEST VALUED PICKS

#import the north American graph



#add points per game to the skaters
nhlppg <- nhl %>%
  mutate(ppg = points/games_played)%>%
  mutate_if(is.numeric, ~replace_na(.,0))

#take draft data so players are a minimum 5 years removed from being drafted and in the 21century
nhlpost2000to2017years1 <- nhlppg %>%
  filter(year < 2018)%>%
  filter(year > 1999)

nhleffectiveness <- nhlpost2000to2017years1 %>%
  mutate(winshares = point_shares/games_played)%>%
  arrange(desc(winshares))

#group by the team and point shares
nhlteamranking <- nhleffectiveness %>%
  group_by(team) %>%
  summarise(avgpickvalue = mean(winshares, na.rm = TRUE))%>%
  arrange(desc(avgpickvalue))

