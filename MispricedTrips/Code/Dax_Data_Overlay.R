### PROJECT 2: WHAT ARE MISPRICED TRIPS CURRENTLY?
### Define trips that are mispriced. Find them.
library(ggplot2)
library(ggmap)
library(scales)
library(geohash)
library(dplyr)
library(tidyr)
library(stringr)

######################################################################################################
##################################### DATA MANIPULATION ##############################################
######################################################################################################

# data <- read.csv("~/Documents/grab_data_may2018.csv", stringsAsFactors=FALSE)

data <- read.csv("~/Downloads/LATEST hc_sgjggs_june_random_sample_ucp.csv", stringsAsFactors=FALSE)
colnames(data)[17] <- "bookings"
data <- data[, c("streamtime", "pickup_geohash",
                 "pickuplatitude", "pickuplongitude",
                 "bookings")]

data$bookings <- ifelse(data$bookings == 0, 0, 1) #if bookings==0 then 0, else 1
#extract date and time
data$datetime <- substring(data$streamtime, 1, 19)
pb.date <- as.POSIXct(data$datetime, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
data$datetime <- format(pb.date, tz="Singapore", usetz=TRUE)
data$time <- substring(data$datetime, 12, 19)
data$hour <- as.numeric(substring(data$time, 1, 2))

colnames(data)[2] <- "geohash"

# #calculate surge
# data$surge <- data$finalfare / data$nsnsfare
# data$surge_scaled <- rescale(data$surge, 
#                              to = c(0.001, 0.1), 
#                              from = range(data$surge, na.rm = TRUE, finite = TRUE))
# extract day
# data$day <- as.numeric(substring(data$datetime, 9, 10))
# convert lat and lon to geohash
# data$geohash <- gh_encode(data$pickuplatitude, data$pickuplongitude)
# data$geohashlatitude <- gh_decode(data$geohash)[, 1]
# data$geohashlongitude <- gh_decode(data$geohash)[, 2]

#final data
# data <- data[, c(9, 10, 13, 17, 18, 19, 20, 21, 22, 23)] #remove unecessary data
booked_rides <- subset(data, bookings == 1) #didn't subset bookings==1 because bookings range from 1 to 27 lol
unbooked_rides <- subset(data, bookings == 0)

#converting data to geohash (aggregating for neater visualisation)
groupby_geohash <- data %>% 
  group_by(geohash, bookings, hour) %>%
  tally(sort=TRUE) 

data_by_geohash <- groupby_geohash %>%
  spread(bookings, n) %>%
  na.omit() %>%
  # remove data with very low Booked compared to Unbooked (e.g Jurong Island)
  # filter(`0` > 600) %>% 
  mutate(potentialDemand = `0` / (`0`+`1`)) %>%
  mutate(avg_unbooked = `0`/31) %>% #no. unbooked per day
  mutate(lat = gh_decode(geohash)[, 1]) %>% #convert geohash to lat
  mutate(lng = gh_decode(geohash)[, 2]) %>% #convert geohash to lng
  arrange(desc(`0`))

#converting dax_data to geohash (aggregating for neater visualisation)

dax_data_by_geohash <- dax_data[, c(1, 2, 4, 6)]
dax_data_by_geohash <- dax_data_by_geohash %>%
  group_by(start_geohash, hour_of_day) %>%
  summarise(avg_no_idle_dax = round(sum(distinct_idle_dax_count)/31), #per day
            avg_total_idle_minutes = round(sum(idle_minutes)/31)) %>% #per day
  mutate(lat = gh_decode(start_geohash)[, 1]) %>% #convert geohash to lat
  mutate(lng = gh_decode(start_geohash)[, 2]) %>% #convert geohash to lng
  arrange(desc(avg_no_idle_dax))

######################################################################################################
############################# UNBOOKED, BOOKED RIDES WITH SURGE ######################################
######################################################################################################

##################################
##### MAP FOR 12 AM (HOUR 0) #####
##################################
# Subset
data0 <- subset(data, hour == 0)
booked_rides0 <- subset(booked_rides, hour == 0)
unbooked_rides0 <- subset(unbooked_rides, hour ==0)

# Generate Map Background
bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

# Open PDF Device
pdf("Unbooked_Booked_Rides_with_Surge_12am.pdf", onefile = TRUE)
for (i in 1:31) {
  
  #subset data for the day
  data_ <- subset(data0, day == i)
  booked_rides_ <- subset(booked_rides0, day == i)
  unbooked_rides_ <- subset(unbooked_rides0, day == i)
  
  # Plot Map
  g <- ggmap(map, extent = "panel") +
    geom_point(aes(x = pickuplongitude, y = pickuplatitude),
               data = unbooked_rides_,
               colour = "red",
               alpha = 0.4,
               size = 0.7,
               stroke = 0) +
    geom_point(aes(x = pickuplongitude, y = pickuplatitude),
               data = booked_rides_,
               colour = "green",
               alpha = 0.4,
               size = 0.7,
               stroke = 0) +
    geom_point(aes(x= pickuplongitude, y = pickuplatitude, alpha = data_$surge_scaled),
               data = data_,
               colour = "deepskyblue",
               size = 0.7,
               stroke = 0) +
    scale_alpha(range = c(0.001, 0.1),
                name = "Surge",
                labels = NULL) +
    ggtitle(paste0("May ", i, ", 12am: Unbooked and Booked Rides With Surge Pricing"))
  
  
  g <- g + 
    theme(axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())
  
  print(g)
  
}
dev.off()



################################
##### MAP FOR 8AM (HOUR 8) #####
################################
# Subset
data8 <- subset(data, hour == 8)
booked_rides8 <- subset(booked_rides, hour == 8)
unbooked_rides8 <- subset(unbooked_rides, hour == 8)

# Generate Map Background
bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

# Open PDF Device
pdf("Unbooked_Booked_Rides_with_Surge_8am.pdf", onefile = TRUE)
for (i in 1:31) {
  
  #subset data for the day
  data8_ <- subset(data8, day == i)
  booked_rides8_ <- subset(booked_rides8, day == i)
  unbooked_rides8_ <- subset(unbooked_rides8, day == i)
  
  # Plot Map
  g <- ggmap(map, extent = "panel") +
    geom_point(aes(x = pickuplongitude, y = pickuplatitude),
               data = unbooked_rides8_,
               colour = "red",
               alpha = 0.4,
               size = 0.7,
               stroke = 0) +
    geom_point(aes(x = pickuplongitude, y = pickuplatitude),
               data = booked_rides8_,
               colour = "green",
               alpha = 0.4,
               size = 0.7,
               stroke = 0) +
    geom_point(aes(x= pickuplongitude, y = pickuplatitude, alpha = data8_$surge),
               data = data8_,
               colour = "deepskyblue",
               size = 0.7,
               stroke = 0) +
    scale_alpha(limits= c(0.4, 5.25),
                range = c(0.001, 1),
                name = "Surge",
                labels = NULL) +
    ggtitle(paste0("May ", i, ", 8am: Unbooked and Booked Rides With Surge Pricing"))
  
  
  g <- g + 
    theme(axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = c(0.95, 0.15))
  
  print(g)
  
}
dev.off()


#################################################
##### MAP FOR 8AM (HOUR 8) WITH light SURGE #####
#################################################
# Subset
data8 <- subset(data, hour == 8)
booked_rides8 <- subset(booked_rides, hour == 8)
unbooked_rides8 <- subset(unbooked_rides, hour == 8)

# Generate Map Background
bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

# Open PDF Device
pdf("Unbooked_Booked_Rides_with_Light-Surge_8am.pdf", onefile = TRUE)
for (i in 1:31) {
  
  #subset data for the day
  data8_ <- subset(data8, day == i)
  booked_rides8_ <- subset(booked_rides8, day == i)
  unbooked_rides8_ <- subset(unbooked_rides8, day == i)
  
  # Plot Map
  g <- ggmap(map, extent = "panel") +
    geom_point(aes(x = pickuplongitude, y = pickuplatitude),
               data = unbooked_rides8_,
               colour = "red",
               alpha = 0.4,
               size = 0.7,
               stroke = 0) +
    geom_point(aes(x = pickuplongitude, y = pickuplatitude),
               data = booked_rides8_,
               colour = "green",
               alpha = 0.4,
               size = 0.7,
               stroke = 0) +
    geom_point(aes(x= pickuplongitude, y = pickuplatitude, alpha = data8_$surge),
               data = data8_,
               colour = "deepskyblue",
               size = 0.7,
               stroke = 0) +
    scale_alpha(limits= c(0.4, 5.25),
                range = c(0.001, 0.5),
                name = "Surge",
                labels = NULL) +
    ggtitle(paste0("May ", i, ", 8am: Unbooked and Booked Rides With Surge Pricing"))
  
  
  g <- g + 
    theme(axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = c(0.95, 0.15))
  
  print(g)
  
}
dev.off()


################################
##### MAP FOR 6PM (HOUR 18) #####
################################
# Subset
data18 <- subset(data, hour == 18)
booked_rides18 <- subset(booked_rides, hour == 18)
unbooked_rides18 <- subset(unbooked_rides, hour == 18)

# Generate Map Background
bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

# Open PDF Device
pdf("Unbooked_Booked_Rides_with_Surge_6pm.pdf", onefile = TRUE)
for (i in 1:31) {
  
  #subset data for the day
  data18_ <- subset(data18, day == i)
  booked_rides18_ <- subset(booked_rides18, day == i)
  unbooked_rides18_ <- subset(unbooked_rides18, day == i)
  
  # Plot Map
  g <- ggmap(map, extent = "panel") +
    geom_point(aes(x = pickuplongitude, y = pickuplatitude),
               data = unbooked_rides18_,
               colour = "red",
               alpha = 0.4,
               size = 0.7,
               stroke = 0) +
    geom_point(aes(x = pickuplongitude, y = pickuplatitude),
               data = booked_rides18_,
               colour = "green",
               alpha = 0.4,
               size = 0.7,
               stroke = 0) +
    geom_point(aes(x= pickuplongitude, y = pickuplatitude, alpha = data18_$surge),
               data = data18_,
               colour = "deepskyblue",
               size = 0.7,
               stroke = 0) +
    scale_alpha(limits= c(0.4, 5.25),
                range = c(0.001, 1),
                name = "Surge",
                labels = NULL) +
    ggtitle(paste0("May ", i, ", 6pm: Unbooked and Booked Rides With Surge Pricing"))
  
  
  g <- g + 
    theme(axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = c(0.95, 0.15))
  
  print(g)
  
}
dev.off()


################################
##### MAP FOR WHOLE OF MAY #####
################################

# Subset
# data15may <- subset(data, day == 15)
# booked_rides15may <- subset(booked_rides, day == 15)
# unbooked_rides15may <- subset(unbooked_rides, day == 15)

# Generate Map Background
bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

# Open PDF Device
pdf("Unbooked_Booked_Rides_with_Surge_MAY.pdf", onefile = TRUE)
for (i in 0:23) {
  
  #subset data by hour
  data_ <- subset(data, hour == i)
  booked_rides_ <- subset(booked_rides, hour == i)
  unbooked_rides_ <- subset(unbooked_rides, hour == i)
  
  # Plot Map
  g <- ggmap(map, extent = "panel") +
    geom_point(aes(x = pickuplongitude, y = pickuplatitude),
               data = unbooked_rides_,
               colour = "red",
               alpha = 0.4,
               size = 0.7,
               stroke = 0) +
    geom_point(aes(x = pickuplongitude, y = pickuplatitude),
               data = booked_rides_,
               colour = "green",
               alpha = 0.4,
               size = 0.7,
               stroke = 0) +
    geom_point(aes(x= pickuplongitude, y = pickuplatitude, alpha = data_$surge_scaled),
               data = data_,
               colour = "deepskyblue",
               size = 0.7,
               stroke = 0) +
    scale_alpha(range = c(0.001, 0.1),
                name = "Surge",
                labels = NULL) +
    ggtitle(paste0("MAY Hour ", i, ": Unbooked and Booked Rides With Surge Pricing"))
  
  
  g <- g + 
    theme(axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = c(0.95, 0.15))
  
  print(g)
  
}
dev.off()


######################################################################################################
####################################### IDLE DAX #####################################################
######################################################################################################

#############################################################
################## Overlaying Dax Data ######################
#############################################################

dax_data <- read.csv("~/Documents/may_jg_dax_idle_by_geohash-latest.csv", stringsAsFactors=FALSE) #load data
dax_data <- dax_data[, -c(2, 6, 7, 8)] #remove cityid, online_seconds and transit_seconds, online_dax_count
dax_data$hour_of_day <- as.numeric(substring(dax_data$datehourlocal, 12, 13)) #extract hour of day
dax_data$day <- as.numeric(substring(dax_data$datehourlocal, 9, 10)) #extract date of day

# Cleaning data
dax_data <- dax_data[complete.cases(dax_data), ] #remove NAs
dax_data <- subset(dax_data, idle_seconds <= 86400) #remove data where idle time > 24 hrs
dax_data <- subset(dax_data, idle_seconds != 0) #remove data where idle_seconds = 0
dax_data$idle_minutes <- (dax_data$idle_seconds/60)
#convert idle seconds to minutes
#divide total no. idle hours over no. of dax to normalize the result
dax_data <- dax_data[, c(2, 5, 6, 7, 8, 9)] #remove unecessary columns

##########################
##### MAP FOR HOUR 0 #####
##########################
# Subset
dax0 <- subset(dax_data, hour_of_day == 0) #subset data for specified hour
dax0 <- cbind(dax0, gh_decode(dax0$start_geohash)[, c(1,2)]) #convert geohash to lat and lon and cbind to df
dax0 <- dax0[, -c(1, 2)] #remove unecessary columns

# Generate Map Background
bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

# Open PDF Device
pdf("Dax_Idle_Hour0.pdf", onefile = TRUE)
for (i in 1:31) {
  
  dax0_ <- subset(dax0, day == i) #subset data for the day
  
  # Plot Map
  g <- ggmap(map, extent = "panel") +
    geom_point(aes(x = lng, y = lat, alpha = dax0_$idle_minutes_per_dax),
               data = dax0_,
               colour = "yellow",
               stroke = 0,
               shape = 15,
               size = 5) +
    scale_alpha_continuous(limits=c(0, 30),
                name = "Idle Minutes\nper Dax") +
    ggtitle(paste0("May ", i, ", 12am : Idle Dax"))
  
  g <- g + theme(axis.text = element_blank(),
                   axis.line = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title = element_blank(),
                 legend.position = c(0.95, 0.15)) +
    guides(colour = guide_legend(nrow=2))
  
  print(g)
  
}
dev.off()



################################
##### MAP FOR 8AM (HOUR 8) #####
################################
# Subset
dax8 <- subset(dax_data, hour_of_day == 8) #subset data for specified hour
dax8 <- cbind(dax8, gh_decode(dax8$start_geohash)[, c(1,2)]) #convert geohash to lat and lon and cbind to df
dax8 <- dax8[, -c(3, 4)] #remove unecessary columns

# Generate Map Background
bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

# Open PDF Device
pdf("Dax_Idle_8am_NEW.pdf", onefile = TRUE)
for (i in 1:31) {
  
  dax8_ <- subset(dax8, day == i) #subset data for the day
  
  # Plot Map
  g <- ggmap(map, extent = "panel") +
    geom_point(aes(x = lng, y = lat, alpha = dax8_$idle_minutes, size = dax8_$distinct_idle_dax_count),
               data = dax8_,
               colour = "yellow",
               stroke = 0,
               shape = 15) +
    scale_alpha_continuous(breaks = c(0, 500, 1000),
                           name = "Idle Minutes") +
    scale_size(limits = c(0, 900), range = c(2,7),
               name = "No. Idle Dax") +
    ggtitle(paste0("May ", i, ", 8am: Idle Dax"))
  
  g <- g + theme(axis.text = element_blank(),
                 axis.line = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 legend.position = c(0.95, 0.15)) +
    guides(colour = guide_legend(nrow=2))
  
  print(g)
  
}
dev.off()



################################
##### MAP FOR 6PM (HOUR 18) #####
################################
# Subset
dax18 <- subset(dax_data, hour_of_day == 18) #subset data for specified hour
dax18 <- cbind(dax18, gh_decode(dax18$start_geohash)[, c(1,2)]) #convert geohash to lat and lon and cbind to df
dax18 <- dax18[, -c(3, 4)] #remove unecessary columns

# Generate Map Background
bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

# Open PDF Device
pdf("Dax_Idle_6pm_NEW.pdf", onefile = TRUE)
for (i in 1:31) {
  
  dax18_ <- subset(dax18, day == i) #subset data for the day
  
  # Plot Map
  g <- ggmap(map, extent = "panel") +
    geom_point(aes(x = lng, y = lat, alpha = dax18_$idle_minutes, size = dax18_$distinct_idle_dax_count),
               data = dax18_,
               colour = "yellow",
               stroke = 0,
               shape = 15) +
    scale_alpha_continuous(breaks = c(0, 500, 1000),
                           name = "Idle Minutes") +
    scale_size(limits = c(0, 900), range = c(2,7),
               name = "No. Idle Dax") +
    ggtitle(paste0("May ", i, ", 6pm: Idle Dax"))
  
  g <- g + theme(axis.text = element_blank(),
                 axis.line = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 legend.position = c(0.95, 0.15)) +
    guides(colour = guide_legend(nrow=2))
  
  print(g)
  
}
dev.off()


################################
##### MAP FOR 7PM (HOUR 19) #####
################################
# Subset
dax19 <- subset(dax_data, hour_of_day == 19) #subset data for specified hour
dax19 <- cbind(dax19, gh_decode(dax19$start_geohash)[, c(1,2)]) #convert geohash to lat and lon and cbind to df
dax19 <- dax19[, -c(3, 4)] #remove unecessary columns

# Generate Map Background
bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

# Open PDF Device
pdf("Dax_Idle_7pm.pdf", onefile = TRUE)
for (i in 1:31) {
  
  dax19_ <- subset(dax19, day == i) #subset data for the day
  
  # Plot Map
  g <- ggmap(map, extent = "panel") +
    geom_point(aes(x = lng, y = lat, alpha = dax19_$idle_minutes, size = dax19_$distinct_idle_dax_count),
               data = dax19_,
               colour = "yellow",
               stroke = 0,
               shape = 15) +
    scale_alpha_continuous(breaks = c(0, 500, 1000),
                           name = "Idle Minutes") +
    scale_size(limits = c(0, 900), range = c(2,7),
               name = "No. Idle Dax") +
    ggtitle(paste0("May ", i, ", 7pm: Idle Dax"))
  
  g <- g + theme(axis.text = element_blank(),
                 axis.line = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 legend.position = c(0.95, 0.15)) +
    guides(colour = guide_legend(nrow=2))
  
  print(g)
  
}
dev.off()

################################
##### MAP FOR WHOLE OF MAY #####
################################
# Subset
dax_data_may <- cbind(dax_data, (gh_decode(dax_data$start_geohash))[, c(1,2)]) #convert geohash to lat and lon and cbind to df

# Generate Map Background
bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

# Open PDF Device
pdf("Dax_Idle_MAY.pdf", onefile = TRUE)
for (i in 0:23) {
  
  dax_ <- subset(dax_data_may, hour_of_day == i) #subset data for the day
  
  # Plot Map
  g <- ggmap(map, extent = "panel") +
    geom_point(aes(x = lng, y = lat, alpha = dax_$idle_minutes, size = dax_$distinct_idle_dax_count),
               data = dax_,
               colour = "yellow",
               stroke = 0,
               shape = 15) +
    scale_alpha_continuous(breaks = c(0, 500, 1000),
                           name = "Idle Minutes",
                           range = c(0.001, 0.5)) +
    scale_size(limits = c(0, 1000), range = c(2,7)) +
    labs(size = "No. of\nIdle Dax") +
    ggtitle(paste0("MAY Hour ", i, ": Idle Dax"))
  
  g <- g + theme(axis.text = element_blank(),
                 axis.line = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 legend.position = "bottom") +
    guides(colour = guide_legend(nrow=2))
  
  print(g)
  
}
dev.off()





######################################################################################################
############################# UNBOOKED, BOOKED RIDES BY GEOHASH ######################################
######################################################################################################

################################
##### MAP FOR WHOLE OF MAY #####
################################
# Subset

# Generate Map Background
bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

# Open PDF Device
pdf("LATEST_Unbooked_Rides_by_Geohash_MAY.pdf", onefile = TRUE)
for (i in 0:23) {
  
  data_by_geohash_ <- subset(data_by_geohash, hour == i) #subset data for the day
  
  # Plot Map
  g <- ggmap(map, extent = "panel") +
    geom_point(aes(x = lng, y = lat, alpha = data_by_geohash_$potentialDemand, size = data_by_geohash_$avg_unbooked),
               data = data_by_geohash_,
               colour = "red",
               stroke = 0,
               shape = 15) +
    scale_alpha(limits = c(0.16, 0.97)) +
    labs(alpha = "Potential\nDemand\n(% of\nUnbooked\nRides)") +
    scale_size(range = c(0, 5), limits = c(0.03,7.38)) +
    labs(size = "Avg No. of\nUnbooked\nRides") +
    ggtitle(paste0("MAY Hour ", i, ": Potential Demand (Unbooked Demand / Total Demand)"))
  
  g <- g + theme(axis.text = element_blank(),
                 axis.line = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 legend.position = "bottom") +
    guides(alpha = guide_legend(order = 1), 
           size = guide_legend(order = 2))
  
  print(g)
  
}
dev.off()


### ORIGINAL
# Open PDF Device
pdf("Unbooked_Rides_by_Geohash_MAY.pdf", onefile = TRUE)
for (i in 0:23) {
  
  data_by_geohash_ <- subset(data_by_geohash, hour == i) #subset data for the day
  
  # Plot Map
  g <- ggmap(map, extent = "panel") +
    geom_point(aes(x = lng, y = lat, alpha = data_by_geohash_$potentialDemand, size = data_by_geohash_$avg_unbooked),
               data = data_by_geohash_,
               colour = "red",
               stroke = 0,
               shape = 15) +
    scale_alpha(limits = c(0.90, 0.98)) +
    labs(alpha = "Potential\nDemand\n(% of\nUnbooked\nRides)") +
    scale_size(range = c(0, 5), limits = c(20,300)) +
    labs(size = "No. of\nUnbooked\nRides") +
    ggtitle(paste0("MAY Hour ", i, ": Potential Demand (Unbooked Demand / Total Demand)"))
  
  g <- g + theme(axis.text = element_blank(),
                 axis.line = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 legend.position = "bottom") +
    guides(alpha = guide_legend(order = 1), 
           size = guide_legend(order = 2))
  
  print(g)
  
}
dev.off()

####################################
##### DAX MAP FOR WHOLE OF MAY #####
####################################

# Generate Map Background
bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

# Open PDF Device
pdf("LATEST_Dax_Idle_by_Geohash_MAY.pdf", onefile = TRUE)
for (i in 0:23) {
  
  dax_data_by_geohash_ <- subset(dax_data_by_geohash, hour_of_day == i) #subset data for the day
  
  # Plot Map
  g <- ggmap(map, extent = "panel") +
    geom_point(aes(x = lng, y = lat, 
                   alpha = dax_data_by_geohash_$avg_total_idle_minutes, 
                   size = dax_data_by_geohash_$avg_no_idle_dax),
               data = dax_data_by_geohash_,
               colour = "yellow",
               stroke = 0,
               shape = 15) +
    scale_alpha(limits = c(0, 1100)) +
    labs(alpha = "Total\nIdle Minutes") +
    scale_size(range = c(0, 5), limits = c(0, 700)) +
    labs(size = "No. of\nIdle Dax") +
    ggtitle(paste0("MAY Hour ", i, ": Aggregated Idle Dax"))
  
  g <- g + theme(axis.text = element_blank(),
                 axis.line = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 legend.position = "bottom") +
    guides(alpha = guide_legend(order = 1), 
           size = guide_legend(order = 2))
  
  print(g)
  
}
dev.off()
