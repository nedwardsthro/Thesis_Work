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

# Examining Discrepancies Between Our Data and the Original Paper's Data
not_traded <- bref_stats %>%
  filter(slugTeamBREF != "TOT")
traded <- bref_stats %>%
  filter(slugTeamBREF == "TOT")
traded_twice <- traded %>%
  filter(nchar(slugTeamsBREF) >9)
traded_thrice <- traded_twice %>%
  filter(nchar(slugTeamsBREF) > 15)
Count_Games <- bref_stats %>%
  filter(countGames > 30)

9/21/21 - Downloaded Basketball Reference Data and set up github

603 rows of traded players, 39 double traded, 1 triple traded
603+39+1
643 +4762

5512 in the original Sloan Article. there are 5405 in ours. In data that matters (players with more than 30 games), we have slightly 68 more observations, likely again due to the traded feature, but working the opposite direction now. Say a player plays 25 games with one team and 25 games with another. The Sloan article won't pick that player up because he didn't play more than 30 games for either team. We will pick that player up because it will show up as 50 games for us. 

Because of the data structure (our data having one line per season even if the player is traded vs. their data likely having multiple lines per season if the player is traded), our density plots will be different.

Update 10/6 below

See new Geom_density with scraped data

Used Mclust
Also useful mclustBIC and mclustBICupdate
mclustModel

Potentially looking into doing dimension reduction in the future?
  
  11/9/21

Initial model and second model don't really match. Even my initial model doesn't really match the output from the Sloan paper. 
Heat map seemed to relatively work. I messed around a little bit with a k means model just for the heck of it. Things to look at for next week are more heat map stuff and also how certain we are each player is in a certain group? Is that how our analysis can be a little bit different? Sum across teams and that way players with multiple parts show up?

