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


# Add cluster variable back to the data frame 
c <- augment(clust, d) %>% select(.cluster)
c$order <- d$order

d <- merge(d, c, by='order')

d <- d  %>% tbl_df() %>% mutate(cluster = .cluster) #the ."var name" throws off some functions

remove(c)


#### Narrow it down to one cluster and start looking for patterns there #####
copy <- d
d <- filter(d, cluster == "5")

# Date, which I will use to group the observations
# And then combine them with all days in the sequence
# The goal is to be able to predict whether a BnE will occur on a given day
d$date <- as.Date(d$dateTime)
d$hour <- hour(d$dateTime)

days <- d %>%
  group_by(date) %>%
  summarise(Events = n())

allDays <- seq.Date(from=d$date[1], to = d$date[nrow(d)], b='days')
allDays <- allDays  %>%  as.data.frame() 
colnames(allDays)[1] = "date"

# After this we will have a df with every date and how many BnEs
d = merge(days, allDays, by='date', all=TRUE)
d[is.na(d)] <- 0

remove(allDays, days)

d$id <- seq(1, nrow(d))

# Now get lag, which will probably be more predictive than time between last event(s)
c <- as.data.frame(d$Events)

for(i in 1:15){
  c[[paste('lag', i, sep="_")]] <- lag(c[[i]])
}

colnames(c)[1] = "Events"
c <- c  %>% select(-Events) %>%
  mutate(rowMeans(c))

colnames(c)[16] <- "AvgTwoWeeks"

c$id <- d$id

d = merge(d, c, by="id")

remove(c)

## Add some more time variables to improve prediction
d <- d %>%
  mutate(monthDay = day(date),
         weekDay = wday(date),
         month = month(date),
         year = year(date))


# To get the variables to build the model
# noquote(paste("lag_", 1:50," +", sep=''))
names(d)

# training <- filter(d, year=="2014")
# testing <- d  %>% filter(year=="2015") 

training <- d[1:700,]
testing <- d[701:1127,]

model <- randomForest(Events ~ lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + 
                        lag_6 + lag_7 + lag_8 + lag_9 + lag_10 +
                        AvgTwoWeeks + id + monthDay + weekDay +  month,
                    data=training, importance=TRUE, ntree=500, na.action = na.omit)


varImpPlot(model)

testing$EventsPredicted <- predict(model, testing)

results <- testing %>% 
  select(EventsPredicted, Events) %>%
  mutate(EventsPredicted = round(EventsPredicted)) %>%
  mutate(falsePositive = ifelse(EventsPredicted > 0 & Events <= 0, "Yes", "No"),
         falseNegative = ifelse(EventsPredicted <=0 & Events > 0, "Yes", "No"),
         correct = ifelse(falsePositive == "Yes" | falseNegative == "Yes", "No", "Yes"))

table(results$correct)
table(results$falsePositive)
table(results$falseNegative)

comparison <- testing %>% 
  select(Events, AvgTwoWeeks) %>%
  mutate(EventsPredicted = round(AvgTwoWeeks)) %>%
  mutate(falsePositive = ifelse(EventsPredicted > 0 & Events <= 0, "Yes", "No"),
         falseNegative = ifelse(EventsPredicted <=0 & Events > 0, "Yes", "No"),
         correct = ifelse(falsePositive == "Yes" | falseNegative == "Yes", "No", "Yes"))
  
table(comparison$correct)
table(results$falsePositive)
table(results$falseNegative)






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




