setwd("/Users/dphnrome/Documents/Git/Predict_Crime/")
setwd("C:/Users/dhadley/Documents/GitHub/Predict_Crime")

library(lubridate)
library(tidyr)
library(dplyr)
library(broom) # augments d with model variables
library(ggplot2)
library(ggmap)
library(randomForest)

#### Boston data ####
# Import data from Socrata
# Using their api
# dBRaw <- read.csv(url(
#   "http://data.cityofboston.gov/resource/7cdf-6fgx.csv?$limit=
#   20000&incident_type_description=RESIDENTIAL%20BURGLARY"))
# 
# # Write the dataset so I don't have to keep importing from Socrata
# dB <- dBRaw
# write.csv(dBRaw, file = "./data/bos.csv")

d <- read.csv("./data/bos.csv")

# Prepare to create x, y
d$Loc <- gsub("\\(", "", d$Location)
d$Loc <- gsub("\\)", "", d$Loc)

d <- d %>%
  tbl_df()  %>% # Convert to tbl class - easier to examine than dfs
  mutate(dateTime = mdy_hms(FROMDATE, tz='EST')) %>% 
  separate(Loc, c("y", "x"), ",") %>%
  mutate(x = as.numeric(x), y = as.numeric(y))

# K means
# This is how we group crimes on a map.
# It may be more convenient to use reporting areas, but often those bisect a cluster
clust <- d %>%
  ungroup %>% dplyr::select(x, y) %>% kmeans(30)


# Add cluster variable back to the data frame with the last n clusters
# We use the last 'n' clusters because we will use those to train the model
# And ultimately we will predict future clusters based on the last n clusters
# n is specified in the for loop
# I went with 50 because I suspect that will be enough for prediction
c <- augment(clust, d) %>% select(.cluster)

for(i in 1:50){
  c[[paste('lag', i, sep="_")]] <- lag(c[[i]])
}

c$X.1 <- d$X.1

d <- merge(d, c, by='X.1')

d <- mutate(d, cluster = .cluster)


# To get the variables to build the model
# noquote(paste("lag_", 1:50," +", sep=''))


fit <- randomForest(cluster ~ lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + lag_7 + lag_8 + lag_9 + lag_10, 
                    data=d, importance=TRUE, ntree=500, na.action = na.omit)


#### Use maps to inspect the clusters ####
# There's no science to this - I went with 30 because they seemed appropriate on the map

# Dot map centered on Boston
map.center <- geocode("Dudley Square, Boston, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 12, color='bw')
SHmap + geom_point(data=d, aes(x=x, y=y, color=d$.cluster))


# Dot map centered on Boston
map.center <- geocode("Dorchester, Boston, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 14, color='bw')
SHmap + geom_point(data=d, aes(x=x, y=y, color=d$.cluster, size=16))





# Useful function from 
# https://github.com/dgrtwo/dgrtwo.github.com/blob/master/_R/2015-01-16-kmeans-free-lunch.Rmd
plot_kmeans <- function(dat, k) {
  clust <- dat %>% ungroup %>% dplyr::select(x, y) %>% kmeans(k)
  ggplot(augment(clust, dat), aes(x, y)) + geom_point(aes(color = .cluster)) +
    geom_point(aes(x1, x2), data = tidy(clust), size = 10, shape = "x") +
    labs(color = "K-means assignments")
}
