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
  arrange(dateTime) %>%
  separate(Loc, c("y", "x"), ",") %>%
  mutate(x = as.numeric(x), y = as.numeric(y),
         order = seq(1, nrow(d)))

# K means
# This is how we group crimes on a map.
# It may be more convenient to use reporting areas, but often those bisect a cluster
clust <- d %>%
  ungroup %>% dplyr::select(x, y) %>% kmeans(20)


# Add cluster variable back to the data frame with the last n clusters
# We use the last 'n' clusters because we will use those to train the model
# And ultimately we will predict future clusters based on the last n clusters
# n is specified in the for loop
# I went with 30 because I suspect that will be enough for prediction
c <- augment(clust, d) %>% select(.cluster)

for(i in 1:30){
  c[[paste('lag', i, sep="_")]] <- lag(c[[i]])
}

c$order <- d$order

d <- merge(d, c, by='order')

# Use this to find the weekly and monthly mode 
# Which will hopefully be predictive
modeStat = function(vals, ...) {
  return(as.numeric(names(which.max(table(vals)))))
}

# Note sure how to index by names, so replace x:y with proper numbers for
# lag_1 : lag_7 using names(d)
d <- d  %>% 
  mutate(cluster = .cluster)  %>%  #the ."var name" throws off some functions
  mutate(weeklyMode = apply(d[, 27:33], 1, modeStat),
         weeklyModeLag = apply(d[34:40], 1, modeStat),
         monthlyMode = apply(d[, 27:56], 1, modeStat)) %>%
  mutate(weeklyMode = as.factor(as.numeric(weeklyMode)),
         weeklyModeLag = as.factor(as.numeric(weeklyModeLag)),
         monthlyMode = as.factor(as.numeric(monthlyMode)))
         


# To get the variables to build the model
# noquote(paste("lag_", 1:50," +", sep=''))

training <- filter(d, Year=="2014")
testing <- d  %>% filter(Year=="2015") 

model <- randomForest(cluster ~ lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + lag_7 + lag_8 + lag_9 + lag_10 +
                      weeklyMode + weeklyModeLag + monthlyMode + order + DAY_WEEK,
                    data=training, importance=TRUE, ntree=500, na.action = na.omit)


varImpPlot(model)

testing$clusterPredicted <- predict(model, testing)

results <- testing %>% 
  select(clusterPredicted, cluster) %>%
  mutate(correct = ifelse(clusterPredicted == cluster, "Correct", "Incorret"))

table(results$correct)

comparison <- testing %>%
  select(cluster, weeklyMode) %>%
  mutate(correct = ifelse(weeklyMode == cluster, "Correct", "Incorret"))
  
table(comparison$correct)






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




vals <- lag(d$.cluster)


# Useful function from 
# https://github.com/dgrtwo/dgrtwo.github.com/blob/master/_R/2015-01-16-kmeans-free-lunch.Rmd
plot_kmeans <- function(dat, k) {
  clust <- dat %>% ungroup %>% dplyr::select(x, y) %>% kmeans(k)
  ggplot(augment(clust, dat), aes(x, y)) + geom_point(aes(color = .cluster)) +
    geom_point(aes(x1, x2), data = tidy(clust), size = 10, shape = "x") +
    labs(color = "K-means assignments")
}
