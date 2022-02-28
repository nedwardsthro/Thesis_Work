# Density Plot of Z-Values for First Model
zvalues_density <- ggplot(zvalues, aes(x = Maximum_Score))+
  geom_density()

# Heat Map of Clusters Demonstrated by the Golden State Warriors
Warriors_Plot <- ggplot(Teams2%>%filter(Tm == "GSW"), aes(x = slugSeason, y = name, fill = Normalized)) +
  geom_tile()+
  scale_fill_gradient(low="pink", high="red")+
  labs(title = "Golden State Warriors Cluster Heat Map", y = "Cluster", x = "Season", fill = "Percentage")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, vjust = .5))
save(Warriors_Plot, file = "RDAs/Warriors_Plot")


# Heat Map of Clusters over the NBA for all 13 Seasons
Seasons_Plot <- ggplot(Seasons, aes(x = slugSeason, y = name, fill = Normalized)) +
  geom_tile()+
  scale_fill_gradient(low="pink", high="red")+
  labs(x = "Season", y = "Cluster", fill = "Proportion", title = "Proportion of Cluster Across the NBA Over 13 Seasons")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, vjust = .5))

# Heat Map of Clusters over the NBA for all 13 Seasons (Reduced_Data)
Seasons_Plot_Reduced <- ggplot(Seasons2, aes(x = slugSeason, y = name, fill = Normalized)) +
  geom_tile()+
  scale_fill_gradient(low="pink", high="red")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, vjust = .5))

# Heat Map of Clusters over the NBA for all 13 Seasons (Six Groups)
Seasons_Plot_Six <- ggplot(Seasons_Six, aes(x = slugSeason, y = name, fill = Normalized)) +
  geom_tile()+
  scale_fill_gradient(low="pink", high="red")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, vjust = .5))

