# Z values for First Model
zvalues <- rbind(scaled_m$z, scaled_new_predictions$z)
zvalues <- cbind(combined_master_labels, zvalues)
zvalues <- zvalues%>%
  rename("Traditional Center" = `1`,
         "Three Point Shooting Guard" = `2`,
         "Versatile Role Player" = `3`,
         "Stretch Forward" = `4`,
         "Skilled Forward" = `5`,
         "Mid Range Big" = `6`,
         "Floor General" = `7`,
         "Ball Dominant Scorer" = `8`,
         "High Usage Guard" = `9`)

#zvalues <- zvalues%>%
#  rowwise()%>%
#  mutate(Maximum_Score = max(c(`Skilled Forward`, `Versatile Wing`, `Stretch Forward`, `Floor General`, `Traditional Center`, `Mid Range Scorer`, `Three Point Shooting Guard`, `Ball Dominant Scorer`, `Versatile Big Man`)))

Teams<-zvalues%>%
  group_by(Tm, slugSeason)%>%
  summarize("Floor General" = sum(`Floor General`),
            "Stretch Forward" = sum(`Stretch Forward`),
            "Mid Range Big" = sum(`Mid Range Big`), 
            "Traditional Center" = sum(`Traditional Center`),
            "Ball Dominant Scorer" = sum(`Ball Dominant Scorer`),
            "Versatile Role Player" = sum(`Versatile Role Player`),
            "High Usage Guard"= sum(`High Usage Guard`),
            "Skilled Forward" = sum(`Skilled Forward`), 
            "Three Point Shooting Guard" = sum(`Three Point Shooting Guard`))%>%
  ungroup()
Teams_Total <- Teams%>%
  mutate(Totals = rowSums(Teams%>%select(3:11)))%>%
  mutate(Season = as.numeric(str_sub(slugSeason, start = 1, end = 4))+1)

Standings <- combined_standings%>%
  select(season, team_short, win_pct)
Teams_Total<-inner_join(Standings, Teams_Total, by = c("season" = "Season", "team_short" = "Tm"))

Teams2<-Teams%>%
  pivot_longer(cols = c(3:11))%>%
  group_by(Tm, slugSeason)%>%
  summarize(name = name, 
            value = value,
            Normalized = value/sum(value))%>%
  ungroup()
Teams_Total <- Teams_Total%>%
  mutate_at(.vars = c(5:13), funs(. / Totals))

Seasons <- zvalues%>%
  group_by(slugSeason)%>%
  summarize("Floor General" = sum(`Floor General`),
            "Stretch Forward" = sum(`Stretch Forward`),
            "Mid Range Big" = sum(`Mid Range Big`), 
            "Traditional Center" = sum(`Traditional Center`),
            "Ball Dominant Scorer" = sum(`Ball Dominant Scorer`),
            "Versatile Role Player" = sum(`Versatile Role Player`),
            "High Usage Guard"= sum(`High Usage Guard`),
            "Skilled Forward" = sum(`Skilled Forward`), 
            "Three Point Shooting Guard" = sum(`Three Point Shooting Guard`))%>%
  ungroup()%>%
  pivot_longer(cols = c(2:10))%>%
  group_by(slugSeason)%>%
  summarize(slugSeason = slugSeason,
            name = name,
            value = value, 
            Normalized = value/sum(value))


# Z values for Reduced Model
reduced_zvalues <- rbind(scaled_m3$z, reduced_new_predictions$z)
reduced_zvalues <- cbind(combined_master_labels, reduced_zvalues)
reduced_zvalues <- reduced_zvalues%>%
  rename("Mid Range Big" = `1`,
         "High Usage Guard" = `2`,
         "Skilled Forward" = `3`,
         "Stretch Forward" = `4`,
         "Three Point Shooting Guard" = `5`,
         "Traditional Center" = `6`,
         "Floor General" = `7`,
         "Versatile Role Player" = `8`,
         "Ball Dominant Scorer" = `9`)

Teams3<-reduced_zvalues%>%
  group_by(Tm, slugSeason)%>%
  summarize(Floor_General = sum(`Floor General`),
            Stretch_Forward = sum(`Stretch Forward`),
            Mid_Range_Big = sum(`Mid Range Big`), 
            Traditional_Center = sum(`Traditional Center`),
            Ball_Dominant_Scorer = sum(`Ball Dominant Scorer`),
            Versatile_Role_Player = sum(`Versatile Role Player`),
            High_Usage_Guard = sum(`High Usage Guard`),
            Skilled_Forward = sum(`Skilled Forward`), 
            Three_Point_Shooting_Guard = sum(`Three Point Shooting Guard`))%>%
  ungroup()
Teams_Total4 <- Teams3%>%
  mutate(Totals = rowSums(Teams3%>%select(3:11)))%>%
  mutate(Season = as.numeric(str_sub(slugSeason, start = 1, end = 4))+1)

Standings <- combined_standings%>%
  select(season, team_short, win_pct)
Teams_Total4<-inner_join(Standings, Teams_Total4, by = c("season" = "Season", "team_short" = "Tm"))

Teams5<-Teams3%>%
  pivot_longer(cols = c(3:11))%>%
  group_by(Tm, slugSeason)%>%
  summarize(name = name, 
            value = value,
            Normalized = value/sum(value))%>%
  ungroup()
Teams_Total4 <- Teams_Total4%>%
  mutate_at(.vars = c(5:13), funs(. / Totals))

Seasons2 <- reduced_zvalues%>%
  group_by(slugSeason)%>%
  summarize(Floor_General = sum(`Floor General`),
            Stretch_Forward = sum(`Stretch Forward`),
            Mid_Range_Big = sum(`Mid Range Big`), 
            Traditional_Center = sum(`Traditional Center`),
            Ball_Dominant_Scorer = sum(`Ball Dominant Scorer`),
            Versatile_Role_Player = sum(`Versatile Role Player`),
            High_Usage_Guard = sum(`High Usage Guard`),
            Skilled_Forward = sum(`Skilled Forward`), 
            Three_Point_Shooting_Guard = sum(`Three Point Shooting Guard`))%>%
  ungroup()%>%
  pivot_longer(cols = c(2:10))%>%
  group_by(slugSeason)%>%
  summarize(slugSeason = slugSeason,
            name = name,
            value = value, 
            Normalized = value/sum(value))

# Z values for Six Group Model
six_zvalues <- rbind(scaled_m$z, scaled_new_predictions$z)
six_zvalues <- cbind(combined_master_labels, six_zvalues)
six_zvalues <- six_zvalues%>%
  rename("Skilled Big" = `1`,
         "Complimentary Guard" = `2`,
         "Three Point Shooter" = `3`,
         "Ball Dominant Scorer" = `4`,
         "Traditional Center" = `5`,
         "Versatile Role Player" = `6`)

Teams_Six<-six_zvalues%>%
  group_by(Tm, slugSeason)%>%
  summarize(Complimentary_Guard = sum(`Complimentary Guard`), 
            Traditional_Center = sum(`Traditional Center`),
            Ball_Dominant_Scorer = sum(`Ball Dominant Scorer`),
            Versatile_Role_Player = sum(`Versatile Role Player`),
            Skilled_Big = sum(`Skilled Big`), 
            Three_Point_Shooter = sum(`Three Point Shooter`))%>%
  ungroup()
Teams_Total_Six <- Teams_Six%>%
  mutate(Totals = rowSums(Teams%>%select(3:8)))%>%
  mutate(Season = as.numeric(str_sub(slugSeason, start = 1, end = 4))+1)

Standings <- combined_standings%>%
  select(season, team_short, win_pct)
Teams_Total<-inner_join(Standings, Teams_Total, by = c("season" = "Season", "team_short" = "Tm"))

Teams2_Six<-Teams_Six%>%
  pivot_longer(cols = c(3:8))%>%
  group_by(Tm, slugSeason)%>%
  summarize(name = name, 
            value = value,
            Normalized = value/sum(value))%>%
  ungroup()
Teams_Total_Six <- Teams_Total_Six%>%
  mutate_at(.vars = c(5:10), funs(. / Totals))

Seasons_Six <- six_zvalues%>%
  group_by(slugSeason)%>%
  summarize(Complimentary_Guard = sum(`Complimentary Guard`), 
            Traditional_Center = sum(`Traditional Center`),
            Ball_Dominant_Scorer = sum(`Ball Dominant Scorer`),
            Versatile_Role_Player = sum(`Versatile Role Player`),
            Skilled_Big = sum(`Skilled Big`), 
            Three_Point_Shooter = sum(`Three Point Shooter`))%>%
  ungroup()%>%
  pivot_longer(cols = c(2:7))%>%
  group_by(slugSeason)%>%
  summarize(slugSeason = slugSeason,
            name = name,
            value = value, 
            Normalized = value/sum(value))

