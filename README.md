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

11/9/21

Initial model and second model don’t really match. Even my initial model
doesn’t really match the output from the Sloan paper. Heat map seemed to
relatively work. I messed around a little bit with a k means model just
for the heck of it. Things to look at for next week are more heat map
stuff and also how certain we are each player is in a certain group? Is
that how our analysis can be a little bit different? Sum across teams
and that way players with multiple parts show up?

``` r
library(tidyverse)
library(nbastatR)
library(mclust)
library(mlr)
library(rsample)
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
order2<-c(2,1,3,11,12,109)
```

``` r
master<-inner_join(
  bref_stats, 
  Shooting_Scrape, 
  by = c("yearSeason" = "yearSeason", "slugPlayerBREF"= "Player_ID"))
master <- left_join(master, Player_Profile, by = c("idPlayerNBA" = "idPlayer"))
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
set.seed(21)
start_time <- Sys.time()
m <- Mclust(master5, G = 9)
end_time <- Sys.time()
end_time - start_time
```

    ## Time difference of 5.303455 mins

``` r
master7 <- master5
master7$first_pred <- m$classification
master7 <- cbind(master6, master7)
d <- purrr::map(1:9, ~ master7%>%select_if(is.numeric) %>% filter(first_pred == {{.x}}))
```

``` r
set.seed(23)
start_time <- Sys.time()
m2 <- Mclust(new_master5, G = 9)
end_time <- Sys.time()
end_time - start_time
```

    ## Time difference of 9.385853 mins

``` r
new_predictions <- predict(m, newdata = new_master5[3711:4855,])
new_master7 <- new_master5
first_pred <- c(m$classification, new_predictions$classification)
new_master7$first_pred <- first_pred
new_master7$second_pred <- m2$classification
new_master7 <- cbind(new_master6, new_master7)
d2 <- purrr::map(1:9, ~ new_master7%>%select_if(is.numeric) %>% filter(second_pred == {{.x}}))
prop.table(table(new_master7$first_pred))
```

    ## 
    ##          1          2          3          4          5          6          7 
    ## 0.15818744 0.17981462 0.08074150 0.11719876 0.08321318 0.03316169 0.22100927 
    ##          8          9 
    ## 0.09577755 0.03089598

``` r
new_master7 <- new_master7%>%
  mutate(First_Prediction = case_when(
    first_pred == 1 ~ "Floor General",
    first_pred == 2 ~ "Stretch Forward",
    first_pred == 3 ~ "Mid Range Big",
    first_pred == 4 ~ "Traditional Center",
    first_pred == 5 ~ "Ball Dominant Scorer",
    first_pred == 6 ~ "Versatile Role Player",
    first_pred == 7 ~ "High Usage Guard",
    first_pred == 8 ~ "Skilled Forward",
    first_pred == 9 ~ "Three Point Shooting Guard"))
new_master7 <- new_master7%>%
  mutate(Second_Prediction = case_when(
    second_pred == 1 ~ "Mid Range Big",
second_pred == 2 ~ "Stretch Forward",
    second_pred == 3 ~ "Skilled Forward",
    second_pred == 4 ~ "High Usage Guard",
    second_pred == 5 ~ "Ball Dominant Scorer",
    second_pred == 6 ~ "Traditional Center",
    second_pred == 7 ~ "Versatile Role Player",
    second_pred == 8 ~ "Floor General",
    second_pred == 9 ~ "Three Point Shooting Guard"))
new_master7 <- new_master7%>%
  mutate(Match = (First_Prediction == Second_Prediction))
```

``` r
zvalues <- rbind(m$z, new_predictions$z)
zvalues <- cbind(new_master6, zvalues)
zvalues <- zvalues%>%
  rename("Floor General" = `1`,
         "Stretch Forward" = `2`,
         "Mid Range Big" = `3`,
         "Traditional Center" = `4`,
         "Ball Dominant Scorer" = `5`,
         "Versatile Role Player" = `6`,
         "High Usage Guard" = `7`,
         "Skilled Forward" = `8`,
         "Three Point Shooting Guard" = `9`)

Teams2<-zvalues%>%
  group_by(Tm, slugSeason)%>%
  summarize(Floor_General = sum(`Floor General`),
            Stretch_Forward = sum(`Stretch Forward`),
            Mid_Range_Big = sum(`Mid Range Big`), 
            Traditional_Center = sum(`Traditional Center`),
            Ball_Dominant_Scorer = sum(`Ball Dominant Scorer`),
            Versatile_Role_Player = sum(`Versatile Role Player`),
            High_Usage_Guard = sum(`High Usage Guard`),
            Skilled_Forward = sum(`Skilled Forward`), 
            Three_Point_Shooting_Guard = sum(`Three Point Shooting Guard`))
```

    ## `summarise()` has grouped output by 'Tm'. You can override using the `.groups` argument.

``` r
Teams2 <- Teams2%>%pivot_longer(cols = c(3:11))
```

``` r
set.seed(123)
cv = vfold_cv(master5, v = 5)
wss <- function(df) {
  model = kmeans(df, centers = 5, nstart = 10 )
  print(summary(model))
}
k.values <- 1:10
wss(data_frame(cv[1,1]))
wss_values <- map_df(cv$splits, .f = wss(.))
plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
wss <- function(df) {
  kmeans(df, centers = 5, nstart = 10 )$tot.withinss
}
```

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

``` r
ggplot(Teams2%>%filter(Tm == "GSW"), aes(x = slugSeason, y = name, fill = value)) +
  geom_tile()+
  scale_fill_gradient(low="pink", high="red")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, vjust = .5))
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
