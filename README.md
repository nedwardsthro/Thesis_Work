Thesis Work
================
Noah Edwards-Thro

9/21/21 - Downloaded Basketball Reference Data and set up github

603 rows of traded players, 39 double traded, 1 triple traded 603+39+1
643 +4762

5512 in the original Sloan Article. there are 5405 in ours. In data that
matters (players with more than 30 games), we have slightly 68 more
observations, likely again due to the traded feature, but working the
opposite direction now. Say a player plays 25 games with one team and 25
games with another. The Sloan article won’t pick that player up because
he didn’t play more than 30 games for either team. We will pick that
player up because it will show up as 50 games for us.

Because of the data structure (our data having one line per season even
if the player is traded vs. their data likely having multiple lines per
season if the player is traded), our density plots will be different.

Update 10/6 below

See new Geom\_density with scraped data

Used Mclust Also useful mclustBIC and mclustBICupdate mclustModel

Potentially looking into doing dimension reduction in the future?

``` r
library(tidyverse)
library(nbastatR)
library(mclust)
```

``` r
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072*2)

bref_stats <- bref_players_stats(
  seasons = 2009:2018,
  tables = c("totals", "per_game", "advanced", "per_minute", "per_poss")
  )

write_csv(bref_stats, file = "bref_stats.csv")

bref_stats_new <- bref_players_stats(
  seasons = 2019:2021,
  tables = c("totals", "per_game", "advanced", "per_minute", "per_poss")
  )

write_csv(bref_stats_new, file = "bref_stats_new.csv")
```

``` r
bref_stats <- read_csv("bref_stats.csv")
bref_stats_new <- read_csv("bref_stats_new.csv")
```

``` r
columns<-list("c","c","i","c","i","i","d","d","c","d","d","d","d","d","d","d","d","d","d","d","d","d","d","d", "d", "d", "d", "d", "d")
Shooting_Scrape <- read_csv("Basketball Reference Scrape.csv", col_types = columns)
Shooting_Scrape_New <- read_csv("~/Desktop/Fall 2021/Honors Thesis Work/Basketball Reference Scrape2.csv", col_types = columns)
```

``` r
Shooting_Scrape <- Shooting_Scrape %>%
  separate(Player, c("Player_Name", "Player_ID"), sep = "\\\\")
Shooting_Scrape$yearSeason <- as.double(Shooting_Scrape$Year %>%
                                          str_extract(pattern = "[^-]+$"))
Shooting_Scrape <- Shooting_Scrape %>%
  group_by(Player_Name, Age) %>%
  filter(G == max(G))

Shooting_Scrape_New <- Shooting_Scrape_New %>%
  separate(Player, c("Player_Name", "Player_ID"), sep = "\\\\")
Shooting_Scrape_New$yearSeason <- as.double(Shooting_Scrape_New$Year %>%
                                          str_extract(pattern = "[^-]+$"))
Shooting_Scrape_New <- Shooting_Scrape_New %>%
  group_by(Player_Name, Age) %>%
  filter(G == max(G))

Player_Profile <- read_csv("Player_Profile.csv")
```

    ## Rows: 1487 Columns: 3

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): namePlayer
    ## dbl (2): idPlayer, heightInches

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Shooting_Scrape_Total <- rbind(Shooting_Scrape, Shooting_Scrape_New)
bref_stats_total <- rbind(bref_stats, bref_stats_new)
player_IDS <- bref_stats_total%>%select(idPlayerNBA)%>%unique()
player_IDS <- player_IDS[1281:1509,]

for (i in 1:length(player_IDS$idPlayerNBA)){
  player = player_profiles(player_ids = player_IDS$idPlayerNBA[i])
  player = player%>%select(idPlayer, namePlayer, heightInches)
  Player_Profile <- rbind(Player_Profile, player)
  print(i)
}
Player_Profile <- Player_Profile%>%unique()
write_csv(Player_Profile, "Player_Profile.csv")
```

# Evaluate if we have the correct number of players)

``` r
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
```

### Density Chart

``` r
plot1 <- ggplot(Shooting_Scrape, aes(x = G)) +
  geom_density()
plot2 <- ggplot(bref_stats, aes(x = countGames)) +
  geom_density()
plot1
```

``` r
order<-c(115,54,55,57,58,59,60,47,61,50,53,9,73,101,106,107,100,111,108,109,96,97,98,99)
order2<-c(2,1,3,11,12)
```

``` r
master<-inner_join(
  bref_stats, 
  Shooting_Scrape, 
  by = c("yearSeason" = "yearSeason", "slugPlayerBREF"= "Player_ID"))
master <- left_join(master, Player_Profile, by = c("idPlayerNBA" = "idPlayer"))
nums <- unlist(lapply(master, is.numeric))
master2 <- master[nums]
master2 <- master2%>%
  filter(countGames >= 30)
master3 <- master2[order]
master5 <- master3 %>%
  mutate(across(everything(), .fns = ~replace_na(., 0)))
master6 <- master%>%
  filter(countGames >= 30)%>%
  select(all_of(order2))
```

``` r
new_master<-inner_join(
  bref_stats_new, 
  Shooting_Scrape_New, 
  by = c("yearSeason" = "yearSeason", "slugPlayerBREF"= "Player_ID")
)
new_master <- left_join(new_master, Player_Profile, by = c('idPlayerNBA' = "idPlayer"))
new_master2 <- new_master%>%
  filter(countGames >= 30)
new_master3 <- new_master2[nums]
new_master4 <- new_master3[order]
new_master5 <- new_master4 %>%
  mutate(across(everything(), .fns = ~replace_na(., 0)))
```

    ## Warning: One or more parsing issues, see `problems()` for details

``` r
new_master5 <- rbind(master5, new_master5)
new_master6 <- new_master2[order2]
new_master6 <- rbind(master6, new_master6)
```

``` r
library(visdat)
vis_miss(new_master5, cluster = TRUE)
```

``` r
start_time <- Sys.time()
m <- Mclust(master5, G = 9)
end_time <- Sys.time()
end_time - start_time
```

    ## Time difference of 10.08734 secs

``` r
master7 <- master5
master7$first_pred <- m$classification
master7 <- cbind(master6, master7)
d <- purrr::map(1:9, ~ master7%>%select_if(is.numeric) %>% filter(first_pred == {{.x}}))
```

``` r
start_time <- Sys.time()
m2 <- Mclust(new_master5, G = 9)
end_time <- Sys.time()
end_time - start_time
```

    ## Time difference of 3.711612 mins

``` r
new_master7 <- new_master5
new_master7$second_pred <- m2$classification
new_master7 <- cbind(new_master6, new_master7)
d2 <- purrr::map(1:9, ~ new_master7%>%select_if(is.numeric) %>% filter(second_pred == {{.x}}))
```

``` r
new_predictions <- predict(m, newdata = new_master5[3711:4855,])
new_master7 <- new_master5
first_pred <- c(m$classification, new_predictions$classification)
new_master7$first_pred <- first_pred
new_master7 <- cbind(new_master6, new_master7)
prop.table(table(new_master7$first_pred))
```

    ## 
    ##          1          2          3          4          5          6          7 
    ## 0.05643666 0.26838311 0.13491246 0.07723996 0.03048404 0.19382080 0.10669413 
    ##          8          9 
    ## 0.12193615 0.01009269

``` r
pred_1_median<-map_df(d, ~ .x %>% summarise(across(everything(), median)))
pred_1_mean<-map_df(d, ~ .x %>% summarise(across(everything(), mean)))
pred_2_median<-map_df(d2, ~ .x %>% summarise(across(everything(), median)))
pred_2_mean<-map_df(d2, ~ .x %>% summarise(across(everything(), mean)))
```

``` r
prop.table(table(m$classification))
prop.table(table(m2$classification))
```
