# Read In Data
combined_standings <- read_csv("Data/Raw_Data/Combined_Standings.csv")
bref_stats <- read_csv("Data/Raw_Data/bref_stats.csv")
bref_stats_new <- read_csv("Data/Raw_Data/bref_stats_new.csv")

## Column identifiers for reading the Shooting Scrape Data
columns<-list("c","c","i","c","i","i","d","d","c","d","d","d","d","d","d","d","d","d","d","d","d","d","d","d", "d", "d", "d", "d", "d")
Shooting_Scrape <- read_csv("Data/Raw_Data/Basketball_Reference_Scrape.csv", col_types = columns)
Shooting_Scrape_New <- read_csv("Data/Raw_Data/Basketball_Reference_Scrape_New.csv", col_types = columns)
Player_Profile <- read_csv("Data/Raw_Data/Player_Profile.csv")

# Shooting Data Clean
## Clean Original 10 Seasons
Shooting_Scrape <- Shooting_Scrape %>%
  separate(Player, c("Player_Name", "Player_ID"), sep = "\\\\")
Shooting_Scrape$yearSeason <- as.double(Shooting_Scrape$Year %>%
                                          str_extract(pattern = "[^-]+$"))%>%
  group_by(Player_Name, Age) %>%
  filter(G == max(G))

## Clean New 3 Seasons
Shooting_Scrape_New <- Shooting_Scrape_New %>%
  separate(Player, c("Player_Name", "Player_ID"), sep = "\\\\")
Shooting_Scrape_New$yearSeason <- as.double(Shooting_Scrape_New$Year %>%
                                              str_extract(pattern = "[^-]+$"))%>%
  group_by(Player_Name, Age) %>%
  filter(G == max(G))

## Combine all 13 Seasons
Shooting_Scrape_Total <- rbind(Shooting_Scrape, Shooting_Scrape_New)

### Vectors to be used later in selecting necessary columns
order<-c("heightInches","pctORB","pctDRB","pctAST", "pctSTL", "pctBLK","pctTOV", "ptsPerMinute", "pctUSG", "ratioPER", "pctFTRate", "pctFT", "fgaPerMinute", "FG% by Distance 2P", "FG% by Distance (3P)", "% of FG Ast'd 2P", "% of FGA by Distance 3P", "Corner 3s %3PA", "% of FG Ast'd 3P", "Dunks", "% of FGA by Distance (0-3)", "% of FGA by Distance (3-10)", "% of FGA by Distance (10-16)", "% of FGA by Distance (16-3P)")
order2<-c("namePlayer.x","slugSeason", "groupPosition", "countGames", "countGamesStarted","Tm")

# Combining Basketball Reference Stats, Player Heights, and Shooting Scrape into Master Data frame
## Creating Master Data frame for First 10 Seasons
master<-inner_join(
  bref_stats, 
  Shooting_Scrape, 
  by = c("yearSeason" = "yearSeason", "slugPlayerBREF"= "Player_ID"))
master <- left_join(master, Player_Profile, by = c("idPlayerNBA" = "idPlayer"))
### A few players didn't have player heights from the scrape so I have to manually add them
master<-master%>%
  mutate(heightInches = case_when(
    namePlayer.x == "Andres Nocioni" ~ 79,
    namePlayer.x == "Anthony Randolph" ~ 82,
    namePlayer.x == "David Andersen" ~ 83,
    namePlayer.x == "Dionte Christmas" ~ 77,
    namePlayer.x == "Frank Mason III" ~ 71,
    namePlayer.x == "Gustavo Ayon" ~ 82,
    namePlayer.x == "Jeff Adrien" ~ 79,
    namePlayer.x == "Joey Dorsey" ~ 80,
    namePlayer.x == "Rudy Fernandez" ~ 78,
    namePlayer.x == "Samardo Samuels" ~ 81,
    namePlayer.x == "TJ Leaf" ~ 82,
    namePlayer.x == "Tobias Harris" ~ 80,
    namePlayer.x == "Tyler Hansbrough" ~ 81,
    namePlayer.x == "Victor Claver" ~ 81,
    namePlayer.x == "Vitor Luiz Faverani" ~ 83,
    TRUE ~ heightInches
  ))
### Selecting numeric columns
nums <- unlist(lapply(master, is.numeric))
### Filtering for more than 30 games, selecting the necessary columns
master_data <- master[nums]%>%
  filter(countGames > 30)%>%
  select(order)%>%
  mutate(across(everything(), .fns = ~replace_na(., 0)))%>%
  mutate(`% of FGA by Distance (10-3P)` = `% of FGA by Distance (10-16)` + `% of FGA by Distance (16-3P)`)%>%
  select(-c(`% of FGA by Distance (10-16)`,`% of FGA by Distance (16-3P)`))

## Creating Master Data Frame for 3 New Seasons
new_master<-inner_join(
  bref_stats_new, 
  Shooting_Scrape_New, 
  by = c("yearSeason" = "yearSeason", "slugPlayerBREF"= "Player_ID")
)
new_master <- left_join(new_master, Player_Profile, by = c('idPlayerNBA' = "idPlayer"))
### A few players didn't have player heights from the scrape so I have to manually add them
new_master<-new_master%>%
  mutate(heightInches = case_when(
    namePlayer.x == "Frank Mason III" ~ 71,
    namePlayer.x == "Landry Shamet" ~ 82,
    namePlayer.x == "Nicolas Claxton" ~ 79,
    namePlayer.x == "PJ Dozier" ~ 80,
    namePlayer.x == "TJ Leaf" ~ 82,
    namePlayer.x == "Tobias Harris" ~ 80,
    TRUE ~ heightInches
  ))
### Filtering for more than 30 games, selecting the necessary columns
new_master_data <- new_master[nums]%>%
  filter(countGames > 30)%>%
  select(order)%>%
  mutate(across(everything(), .fns = ~replace_na(., 0)))%>%
  mutate(`% of FGA by Distance (10-3P)` = `% of FGA by Distance (10-16)` + `% of FGA by Distance (16-3P)`)%>%
  select(-c(`% of FGA by Distance (10-16)`,`% of FGA by Distance (16-3P)`))

# Combine into a single master data frame
## In data exploration, I found that some observations that were a decimal percentage stayed as a decimal. 
## For instance, a BLK percentage of 0.9% entered as .9 even though it should be 0.009 in this dataframe
### This cleans up those errors
combined_master_data <- rbind(master_data,new_master_data)
combined_master_data <- combined_master_data%>%
  mutate(pctBLK = case_when(
    pctBLK >= .2 ~ pctBLK/100,
    pctBLK == .100 ~ pctBLK/100,
    TRUE~ pctBLK),
    pctORB = case_when(
      pctORB >= .3 ~ pctORB/100,
      TRUE~pctORB),
    pctAST = case_when(
      pctAST == .7 ~ pctAST/100,
      TRUE~pctAST),
    pctSTL = case_when(
      pctSTL >= .2 ~ pctSTL/100,
      TRUE~ pctSTL),
    heightInches = case_when(
      heightInches == 0 ~ 72,
      TRUE ~ heightInches)
  )

# Create labels for master, new_master, and combined_master data frames 
master_labels <- master%>%
  filter(countGames > 30)%>%
  select(all_of(order2))
new_master_labels <- new_master%>%
  filter(countGames > 30)%>%
  select(all_of(order2))
combined_master_labels <- rbind(master_labels, new_master_labels)
## Write CSVs
write_csv(combined_master_data, file = "combined_master_data.csv")
write_csv(combined_master_labels, file = "combined_master_labels.csv")
write_csv(master_labels, file = "master_labels.csv")
write_csv(new_master_labels, file = "new_master_labels.csv")

# Scale Combined Master Data and then separate into the original 10 seasons and the new 3 seasons
scaled_combined_master_data <- data.frame(scale(combined_master_data))
scaled_master_data <- scaled_combined_master_data[1:3676,]
scaled_new_master_data <- scaled_combined_master_data[3677:4801,]

## Write CSV's
write_csv(scaled_combined_master_data, file = "scaled_combined_master_data.csv")
write_csv(scaled_master_data, file = "scaled_master_data.csv")
write_csv(scaled_new_master_data, file = "scaled_new_master_data.csv")

# Data Reduction for New Models
reduced_combined_data <- combined_master_data%>%
  mutate(`% of FGA by Distance (0-10)` = `% of FGA by Distance (0-3)` + `% of FGA by Distance (3-10)`)%>%
  select(1,2,3,4,7,8,9,11,14,15,17,24,23)

# Scale the Reduced Data
reduced_scaled_combined_data <- data.frame(scale(reduced_combined_data))
reduced_scaled_data <- reduced_scaled_combined_data[1:3676,]
reduced_scaled_new_data <- reduced_scaled_combined_data[3677:4801, ]

# Write CSV's
write_csv(reduced_combined_data, file = "reduced_combined_data.csv")
write_csv(reduced_scaled_combined_data, file = "reduced_scaled_combined_data.csv")
write_csv(reduced_scaled_data, file = "reduced_scaled_data.csv")
write_csv(reduced_scaled_new_data, file = "reduced_scaled_new_data.csv")

# Analyzing Redundancy in PER
library(Hmisc)
redun( ~ pctORB + pctDRB + pctAST + pctTOV + ptsPerMinute + ratioPER, data = combined_master_data, r2 = 0.8) 
