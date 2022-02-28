# Model 1 and 2 Predictions

scaled_new_predictions <- predict(scaled_m, newdata = scaled_new_master_data)
original_pred <- c(scaled_m$classification, scaled_new_predictions$classification)
new_pred <- scaled_m2$classification
scaled_combined_preds <- cbind(original_pred, new_pred, combined_master_labels, scaled_combined_master_data)

# Grouping dataframe into clusters to investigate each cluster
scaled_d <- purrr::map(1:9, ~ scaled_combined_preds%>%select_if(is.numeric) %>% filter(original_pred == {{.x}}))
scaled_d2 <- purrr::map(1:9, ~ scaled_combined_preds%>%select_if(is.numeric) %>% filter(new_pred == {{.x}}))

## Giving clusters labels
scaled_combined_preds <- scaled_combined_preds%>%
  mutate(First_Prediction = case_when(
    original_pred == 1 ~ "Traditional Center",
    original_pred == 2 ~ "Three Point Shooting Guard",
    original_pred == 3 ~ "Versatile Role Player",
    original_pred == 4 ~ "Stretch Forward",
    original_pred == 5 ~ "Skilled Forward",
    original_pred == 6 ~ "Mid Range Big",
    original_pred == 7 ~ "Floor General",
    original_pred == 8 ~ "Ball Dominant Scorer",
    original_pred == 9 ~ "High Usage Guard"),
    New_Prediction = case_when(
      new_pred == 1 ~ "Traditional Center",
      new_pred == 2 ~ "Three Point Shooting Guard",
      new_pred == 3 ~ "Stretch Forward",
      new_pred == 4 ~ "Skilled Forward",
      new_pred == 5 ~ "Versatile Role Player",
      new_pred == 6 ~ "High Usage Guard",
      new_pred == 7 ~ "Floor General",
      new_pred == 8 ~ "Ball Dominant Scorer",
      new_pred == 9 ~ "Mid Range Big"), 
    data_time = case_when(
      slugSeason == "2018-19" ~ "New", 
      slugSeason == "2019-20" ~ "New", 
      slugSeason == "2020-21" ~ "New", 
      TRUE ~ "Old"))

# Analyzing Cluster Groups
scaled_d_mean<-map_df(scaled_d, ~ .x %>% summarise(across(everything(), mean)))
scaled_d2_mean<-map_df(scaled_d2, ~ .x %>% summarise(across(everything(), mean)))


# Reduced Model (Models 3 and 4) Predictions
reduced_new_predictions <- predict(scaled_m3, newdata = reduced_scaled_new_data)
reduced_original_preds <- c(scaled_m3$classification, reduced_new_predictions$classification)
reduced_new_preds <- scaled_m4$classification
reduced_preds <- cbind(reduced_original_preds, reduced_new_preds, combined_master_labels, reduced_scaled_combined_data)

# Grouping data frame into clusters to investigate each cluster
reduced_scaled_d <- purrr::map(1:9, ~ reduced_preds%>%select_if(is.numeric) %>% filter(reduced_original_preds == {{.x}}))
reduced_scaled_d2 <- purrr::map(1:9, ~ reduced_preds%>%select_if(is.numeric) %>% filter(reduced_new_preds == {{.x}}))
reduced_scaled_d_mean<-map_df(reduced_scaled_d, ~ .x %>% summarise(across(everything(), mean)))
reduced_scaled_d2_mean<-map_df(reduced_scaled_d2, ~ .x %>% summarise(across(everything(), mean)))

# Giving Clusters Labels
reduced_preds <- reduced_preds %>%
  mutate(
    First_Prediction = case_when(
      reduced_original_preds == 1 ~ "Mid Range Big",
      reduced_original_preds == 2 ~ "High Usage Guard",
      reduced_original_preds == 3 ~ "Skilled Forward",
      reduced_original_preds == 4 ~ "Stretch Forward",
      reduced_original_preds == 5 ~ "Three Point Shooting Guard",
      reduced_original_preds == 6 ~ "Traditional Center",
      reduced_original_preds == 7 ~ "Floor General",
      reduced_original_preds == 8 ~ "Versatile Role Player",
      reduced_original_preds == 9 ~ "Ball Dominant Scorer"
    ),
    New_Prediction = case_when(
      reduced_new_preds == 1 ~ "Mid Range Big",
      reduced_new_preds == 2 ~ "Floor General",
      reduced_new_preds == 3 ~ "Stretch Forward",
      reduced_new_preds == 4 ~ "Skilled Forward",
      reduced_new_preds == 5 ~ "Traditional Center",
      reduced_new_preds == 6 ~ "High Usage Guard",
      reduced_new_preds == 7 ~ "Three Point Shooting Guard",
      reduced_new_preds == 8 ~ "Ball Dominant Scorer",
      reduced_new_preds == 9 ~ "Versatile Role Player"
    ),
    data_time = case_when(
      slugSeason == "2018-19" ~ "New",
      slugSeason == "2019-20" ~ "New",
      slugSeason == "2020-21" ~ "New",
      TRUE ~ "Old"
    )
  )

# Less Clusters, Reduced Model (Models 5 and 6) Predictions
six_reduced_new_predictions <- predict(scaled_m5, newdata = reduced_scaled_new_data)
six_reduced_original_preds <- c(scaled_m5$classification, six_reduced_new_predictions$classification)
six_reduced_new_preds <- scaled_m6$classification
six_reduced_preds <- cbind(six_reduced_original_preds, six_reduced_new_preds, combined_master_labels, reduced_scaled_combined_data)

# Grouping data frame into clusters to investigate each cluster
six_reduced_scaled_d <- purrr::map(1:6, ~ six_reduced_preds%>%select_if(is.numeric) %>% filter(six_reduced_original_preds == {{.x}}))
six_reduced_scaled_d2 <- purrr::map(1:6, ~ six_reduced_preds%>%select_if(is.numeric) %>% filter(six_reduced_new_preds == {{.x}}))
six_reduced_scaled_d_mean<-map_df(six_reduced_scaled_d, ~ .x %>% summarise(across(everything(), mean)))
six_reduced_scaled_d2_mean<-map_df(six_reduced_scaled_d2, ~ .x %>% summarise(across(everything(), mean)))


# Giving Clusters Labels
six_reduced_preds <- six_reduced_preds %>%
  mutate(
    First_Prediction = case_when(
      six_reduced_original_preds == 1 ~ "Skilled Big",
      six_reduced_original_preds == 2 ~ "Complimentary Guard",
      six_reduced_original_preds == 3 ~ "Three Point Shooter",
      six_reduced_original_preds == 4 ~ "Ball Dominant Scorer",
      six_reduced_original_preds == 5 ~ "Traditional Center",
      six_reduced_original_preds == 6 ~ "Versatile Role Player"
    ),
    New_Prediction = case_when(
      six_reduced_new_preds == 1 ~ "Traditional Center",
      six_reduced_new_preds == 2 ~ "Ball Dominant Scorer",
      six_reduced_new_preds == 3 ~ "Three Point Shooter",
      six_reduced_new_preds == 4 ~ "Skilled Big",
      six_reduced_new_preds == 5 ~ "Versatile Role Player",
      six_reduced_new_preds == 6 ~ "Versatile Role Player"
    ),
    data_time = case_when(
      slugSeason == "2018-19" ~ "New",
      slugSeason == "2019-20" ~ "New",
      slugSeason == "2020-21" ~ "New",
      TRUE ~ "Old"
    )
  )


write_csv(six_reduced_preds, "Data/Predictions/six_reduced_preds.csv")
write_csv(reduced_preds, "Data/Predictions/reduced_preds.csv")
write_csv(scaled_combined_preds, "Data/Predictions/scaled_combined_preds.csv")

six_reduced_preds%>%
  summarize(Perc = sum(First_Prediction == New_Prediction) / nrow(six_reduced_preds))
reduced_preds%>%
  summarize(Perc = sum(First_Prediction == New_Prediction) / nrow(reduced_preds))
scaled_combined_preds%>%
  summarize(Perc = sum(First_Prediction == New_Prediction) / nrow(scaled_combined_preds))

# Seeing how models with original variables and reduced variables compared
models_1_4 <- inner_join(scaled_combined_preds%>%
                          select(namePlayer.x, slugSeason, First_Prediction, New_Prediction, data_time),
                        reduced_preds%>%
                          select(namePlayer.x, slugSeason, First_Prediction, New_Prediction, data_time), 
                        by = c("namePlayer.x", "slugSeason", "data_time"))

save(models_1_4, file = "RDAs/model_1_4.rda")
models_1_4%>%
  group_by(First_Prediction.x, First_Prediction.y)%>%
  summarize(n = n())%>%
  mutate(Perc = n/sum(n))%>%
  filter(First_Prediction.x == First_Prediction.y)%>%
  select(First_Prediction.x, Perc)%>%
  rename(Cluster = First_Prediction.x, Percentage = Perc)%>%
  kable()

scaled_mid <- scaled_combined_preds%>%
  select(namePlayer.x, slugSeason, First_Prediction, New_Prediction, data_time)%>%
  mutate(Data = "Original Variables")
reduced_mid <- reduced_preds%>%
  select(namePlayer.x, slugSeason, First_Prediction, New_Prediction, data_time)%>%
  mutate(Data = "Reduced Variables")
combined_1_4 <- rbind(scaled_mid, reduced_mid)
save(combined_1_4, file = "RDAs/combined_1_4.rda")


# Seeing how models with original variables and reduced variables, smaller number of cluster models compared
models_1_6 <- inner_join(scaled_combined_preds%>%
                           select(namePlayer.x, slugSeason, First_Prediction, New_Prediction, data_time),
                         six_reduced_preds%>%
                           select(namePlayer.x, slugSeason, First_Prediction, New_Prediction, data_time), 
                         by = c("namePlayer.x", "slugSeason", "data_time"))
save(models_1_6, file = "RDAs/models_1_6.rda")





