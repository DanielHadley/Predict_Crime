

#### Boston data ####
Import data from Socrata
Using their api
dBRaw <- read.csv(url(
  "http://data.cityofboston.gov/resource/7cdf-6fgx.csv?$limit=20000&incident_type_description=RESIDENTIAL%20BURGLARY"))



Duplicate the dataset so I don't have to keep importing from Socrata
dB <- dBRaw
write.csv(dBRaw, file = "./data/bos.csv")

dB <- read.csv("./data/bos.csv")