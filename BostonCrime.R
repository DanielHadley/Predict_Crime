setwd("/Users/dphnrome/Documents/Git/Predict_Crime/")

library(lubridate)
library(tidyr)
library(dplyr)
library(broom) # augments d with model variables

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
  separate(Loc, c("x", "y"), ",")

# 20 means
clust <- d %>%
  ungroup %>% dplyr::select(x, y) %>% kmeans(20)  

# Add cluster variable back to d
d <- augment(clust, d)


# Useful function from 
# https://github.com/dgrtwo/dgrtwo.github.com/blob/master/_R/2015-01-16-kmeans-free-lunch.Rmd
plot_kmeans <- function(dat, k) {
  clust <- dat %>% ungroup %>% dplyr::select(x, y) %>% kmeans(k)
  ggplot(augment(clust, dat), aes(x, y)) + geom_point(aes(color = .cluster)) +
    geom_point(aes(x1, x2), data = tidy(clust), size = 10, shape = "x") +
    labs(color = "K-means assignments")
}


library(ggmap)
# Dot map centered on Boston
map.center <- geocode("Boston, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 16)
SHmap + geom_point(
  aes(x=d$x, y=d$y),size = 10, alpha = .7, bins = 26, color="red", 
  data = d) 