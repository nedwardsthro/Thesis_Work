# Honors Thesis Work
library(tidyverse)
library(nbastatR)
stats_2018<-bref_players_stats(seasons = 2018, tables = c("totals", "per_game", "advanced","per_minute", "per_poss"),join_data = TRUE)
bref_stats<-bref_players_stats(seasons = 2008:2018, tables = c("totals", "per_game", "advanced", "per_minute", "per_poss"))
