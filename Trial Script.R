# Honors Thesis Work
library(tidyverse)
library(nbastatR)
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072*2)
bref_stats<-bref_players_stats(seasons = 2009:2018, tables = c("totals", "per_game", "advanced", "per_minute", "per_poss"))

Shooting_Scrape <- read_csv("~/Desktop/Fall 2021/Honors Thesis Work/Basketball Reference Scrape.csv")
# Evaluate if we have the correct number of players)
not_traded<-bref_stats%>%filter(slugTeamBREF != "TOT")
traded<-bref_stats%>%filter(slugTeamBREF == "TOT")
traded_twice <- traded%>%filter(nchar(slugTeamsBREF) >9)
traded_thrice<-traded_twice%>%filter(nchar(slugTeamsBREF) > 15)

Count_Games<-bref_stats%>%filter(countGames > 30)
try1<-game_data%>%filter(gp>30)

# Load NBA data through NBA.com
trial<-teams_players_stats(seasons = 2009:2018, types = "player", tables = c("general"), measures = c('Base', 'Advanced', "Misc", 'Scoring', 'Usage'), mode = "Per100Possessions")
tria2<-teams_players_stats(seasons = 2009:2018, types = "player", tables = c("shots"), measures = c('Base', 'Advanced', "Misc", 'Scoring', 'Usage'), mode = "Per100Possessions")

bref_stats<-bref_players_stats(seasons = 2009:2018, tables = c("per_poss"))

Trial1<-trial[1:10,]
Trial2<-trial[11:20,]
Trial3<-trial[21:30,]
Trial4<-trial[31:40,]
Trial5<-trial[41:50,]

load_data<-function(dataset){
  game_data<-data.frame()
  for(n in (1:nrow(dataset))){
    year = dataset[[4]][[n]]
    df = dataset[[7]][[n]]
    df$Season = year
    game_data <- rbind(game_data, df)
    print(n)
  }
  return(game_data)
}

data1<-load_data(Trial1)
data2<-load_data(Trial2)
data3<-load_data(Trial3)
data4<-load_data(Trial4)
data5<-load_data(Trial5)

master<-inner_join(data1, data2, by = c('idPlayer', 'idTeam', 'Season'), suffix = c('_Base', '_Adv'))%>%
  inner_join(data3, by = c('idPlayer', 'idTeam', 'Season'), suffix = c('', '_Misc'))%>%
  inner_join(data4, by = c('idPlayer', 'idTeam', 'Season'), suffix = c('', '_Scoring'))%>%
  inner_join(data5, by = c('idPlayer', 'idTeam', 'Season'), suffix = c('', '_Usage'))

master%>%filter(gp_Base >30)

### Show an example of a player who was traded midseason
player <- bref_stats%>%filter(namePlayer == "Evan Turner")

### Density Chart
plot1<-ggplot(Shooting_Scrape, aes(x = G))+
  geom_density()
plot2<-ggplot(bref_stats, aes(x = countGames))+
  geom_density()
plot3<-ggplot(not_traded, aes(x = countGames))+
  geom_density()
plot1

