# Library
library(mclust)

# First Model - Based on 10 Seasons (Kalman and Bosch Replication)
set.seed(21)
scaled_m <- Mclust(scaled_master_data, G = 9)

# Second Model - Based on 13 Seasons
set.seed(21)
scaled_m2 <- Mclust(scaled_combined_master_data, G = 9)

# Third Model - Based on 10 Seasons, reduced variables
set.seed(21)
scaled_m3 <- Mclust(reduced_scaled_data, G = 9)

# Fourth Model - Based on 13 seasons, reduced variables
set.seed(21)
scaled_m4 <- Mclust(reduced_scaled_combined_data, G = 9)

# Fifth Model - Based on 10 Seasons, reduced variables
set.seed(21)
scaled_m5 <- Mclust(reduced_scaled_data, G = 6)

# Sixth Model - Based on 13 seasons, reduced variables
set.seed(21)
scaled_m6 <- Mclust(reduced_scaled_combined_data, G = 6)

# Seventh Model - Based on 10 Seasons, reduced variables
set.seed(21)
scaled_m5 <- Mclust(scaled_master_data, G = 6)

# Eight Model - Based on 13 seasons, reduced variables
set.seed(21)
scaled_m6 <- Mclust(scaled_combined_master_data, G = 6)
