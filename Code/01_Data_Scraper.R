# Libraries
library(tidyverse)
library(nbastatR)

# Basketball Reference Stats
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

## Original 10 Seasons
bref_stats <- bref_players_stats(
  seasons = 2009:2018,
  tables = c("totals", "per_game", "advanced", "per_minute", "per_poss")
)
write_csv(bref_stats, file = "bref_stats.csv")

## New 3 Seasons
bref_stats_new <- bref_players_stats(
  seasons = 2019:2021,
  tables = c("totals", "per_game", "advanced", "per_minute", "per_poss")
)
write_csv(bref_stats_new, file = "bref_stats_new.csv")


# Adding Player Heights
bref_stats_total <- rbind(bref_stats, bref_stats_new)
player_IDS <- bref_stats_total %>% select(idPlayerNBA) %>% unique()

for (i in 1:length(player_IDS$idPlayerNBA)) {
  player = player_profiles(player_ids = player_IDS$idPlayerNBA[i])
  player = player %>% select(idPlayer, namePlayer, heightInches)
  Player_Profile <- rbind(Player_Profile, player)
  print(i)
}

Player_Profile <- Player_Profile %>% unique()
write_csv(Player_Profile, "Player_Profile.csv")

# Shooting Data from Basketball Reference Data was manually scraped and can be found in Basketball_Referece_Scrape and Basketball_Reference_Scrape_New
