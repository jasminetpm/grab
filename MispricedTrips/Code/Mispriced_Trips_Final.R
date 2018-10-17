### PROJECT 2: WHAT ARE MISPRICED TRIPS CURRENTLY?
### Define trips that are mispriced. Find them.
library(ggplot2)
library(ggmap)
library(scales)
library(dplyr)
library(geohash)

# Import the data
data <- read.csv("~/Documents/grab_data_may2018.csv", stringsAsFactors=FALSE)
dax_data <- read.csv("~/Documents/dax_data.csv", stringsAsFactors=FALSE)

# Clean the data
# data <- subset(data, distance < 100) #clean off data with weird distance (eg. 19000km) Sg is 50km from east to west
# data <- subset(data, duration < 5400) #clean off data with weird duration (eg. 12000s) 5400s = 1.5hr
# data <- subset(data, finalfare > 0) #clean off data with finalfare == 0 (115024 occurences of this)

# Extract day and time of data
data$datetime <- substring(data$streamtime, 1, 19)
pb.date <- as.POSIXct(data$datetime, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
data$datetime <- format(pb.date, tz="Singapore", usetz=TRUE)
data$time <- substring(data$datetime, 12, 19)
data$hour <- as.numeric(substring(data$time, 1, 2))
data$surge <- data$finalfare / data$nsnsfare
data$surge_scaled <- rescale(data$surge, 
                             to = c(0.001, 0.1), 
                             from = range(data$surge, na.rm = TRUE, finite = TRUE))

# # Weekend Data
# data$day <- weekdays(as.Date(data$datetime, "%Y-%m-%d"), abbr = TRUE)
# # Subset Weekend Data
# weekend <- subset(data, day == "Sat" | day == "Sun")
# # Subset Weekend Booked Rides and Unbooked Rides
# weekend_booked_rides <- subset(weekend, bookings != 0)
# weekend_unbooked_rides <- subset(weekend, bookings == 0)

# Subset Booked Rides and Unbooked rides
booked_rides <- subset(data, bookings != 0) #didn't subset bookings==1 because bookings range from 1 to 27 lol
unbooked_rides <- subset(data, bookings == 0)


#################################
#### 6pm Weekday
#################################

data18 <- subset(data, hour == 18)
data_18 <- subset(booked_rides, hour == 18)
data_18_ <- subset(unbooked_rides, hour == 18)

bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

g <- ggmap(map, extent = "panel") +
  geom_point(aes(x = pickuplongitude, y = pickuplatitude),
             data = data_18_,
             colour = "red",
             alpha = 0.4,
             size = 0.7,
             stroke = 0) +
  geom_point(aes(x = pickuplongitude, y = pickuplatitude),
             data = data_18,
             colour = "green",
             alpha = 0.4,
             size = 0.7,
             stroke = 0) +
  ggtitle("Unbooked and Booked Rides")

g <- g + theme(axis.text = element_blank(),
               axis.line = element_blank(),
               axis.ticks = element_blank(),
               axis.title = element_blank())

g



################################
##### For R Markdown
################################


### 12 am Pickups

data0 <- subset(data, hour == 0)
data_0 <- subset(booked_rides, hour == 0)
data_0_ <- subset(unbooked_rides, hour ==0)

bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

g <- ggmap(map, extent = "panel") +
  geom_point(aes(x = pickuplongitude, y = pickuplatitude),
             data = data_0_,
             colour = "red",
             alpha = 0.4,
             size = 0.7) +
  geom_point(aes(x = pickuplongitude, y = pickuplatitude),
             data = data_0,
             colour = "green",
             alpha = 0.4,
             size = 0.7) +
  ggtitle("Unbooked and Booked Rides")

g <- g + theme(axis.text = element_blank(),
               axis.line = element_blank(),
               axis.ticks = element_blank(),
               axis.title = element_blank())

g


g_with_surge <- ggmap(map, extent = "panel") +
  geom_point(aes(x = pickuplongitude, y = pickuplatitude),
             data = data_0_,
             colour = "red",
             alpha = 0.4,
             size = 0.7,
             stroke = 0) +
  geom_point(aes(x = pickuplongitude, y = pickuplatitude),
             data = data_0,
             colour = "green",
             alpha = 0.4,
             size = 0.7,
             stroke = 0) +
  geom_point(aes(x= pickuplongitude, y = pickuplatitude, alpha = data0$surge_scaled),
             data = data0,
             colour = "deepskyblue",
             size = 0.7,
             stroke = 0) +
  scale_alpha(range = c(0.001, 0.1),
              name = "Surge",
              labels = NULL) +
  ggtitle("Unbooked and Booked Rides with Surge Pricing")


g_with_surge <- g_with_surge + 
  theme(axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  legend.position = c(0.95, 0.15))

pdf.name <- "Mispriced_Trips_with_surge_12am_new.pdf"
ggsave(pdf.name, g_with_surge, width = 7, height =7)
system(paste("open -a preview", pdf.name))

# g_with_surge







#########################
### 8 am Pickups
#########################
data8 <- subset(data, hour == 8)
data_8 <- subset(booked_rides, hour == 8)
data_8_ <- subset(unbooked_rides, hour ==8)

bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

g <- ggmap(map, extent = "panel") +
  geom_point(aes(x = pickuplongitude, y = pickuplatitude),
             data = data_8_,
             colour = "red",
             alpha = 0.4,
             size = 0.7,
             stroke = 0) +
  geom_point(aes(x = pickuplongitude, y = pickuplatitude),
             data = data_8,
             colour = "green",
             alpha = 0.4,
             size = 0.7,
             stroke = 0) +
  ggtitle("Unbooked and Booked Rides")

g <- g + theme(axis.text = element_blank(),
               axis.line = element_blank(),
               axis.ticks = element_blank(),
               axis.title = element_blank())

g


g_with_surge <- ggmap(map, extent = "panel") +
  geom_point(aes(x = pickuplongitude, y = pickuplatitude),
             data = data_8_,
             colour = "red",
             alpha = 0.4,
             size = 0.7,
             stroke = 0) +
  geom_point(aes(x = pickuplongitude, y = pickuplatitude),
             data = data_8,
             colour = "green",
             alpha = 0.4,
             size = 0.7,
             stroke = 0) +
  geom_point(aes(x= pickuplongitude, y = pickuplatitude, alpha = data8$surge_scaled),
             data = data8,
             colour = "deepskyblue",
             size = 0.7,
             stroke = 0) +
  scale_alpha(range = c(0.001, 0.1),
              name = "Surge",
              labels = NULL) 
  ggtitle("Unbooked and Booked Rides with Surge Pricing")


g_with_surge <- g_with_surge + 
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")


pdf.name <- "Mispriced_Trips_with_surge_8am_new.pdf"
ggsave(pdf.name, g_with_surge, width = 7, height =7)
system(paste("open -a preview", pdf.name))



##################################
####### CLEANING DAX DATA ########
##################################

dax_data <- read.csv("~/Downloads/may_jg_dax_idle_by_geohash.csv", stringsAsFactors=FALSE)
dax_data <- dax_data[,-2] #remove cityid

# Extract hour of day of dax data
dax_data$hour_of_day <- as.numeric(substring(dax_data$datehourlocal, 12, 13))
dax_data <- dax_data[, c(2, 4, 5, 6)]

# Cleaning data
dax_data <- dax_data[complete.cases(dax_data), ]
dax_data <- subset(dax_data, idle_seconds <= 86400) #remove data where idle time >24 hrs

# new data frame with sum of idle hours per location by hour
dax_idle <- group_by(dax_data, hour_of_day)



#####################################
####### DAX DATA CLEANED !!! ########
#####################################

dax_data$idle_hours <- as.integer(dax_data$idle_hours)
dax_data <- subset(dax_data, idle_hours != 0)
dax0 <- subset(dax_data, hour_of_day == 0)

bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

g <- ggmap(map, extent = "panel") +
  geom_point(aes(x = lng, y = lat, alpha = dax0$idle_hours),
             data = dax0,
             colour = "yellow",
             stroke = 0,
             shape = 15,
             size = 5) +
  scale_alpha(name = "Idle Hours") +
  ggtitle("Idle Dax")

g <- g + theme(axis.text = element_blank(),
               axis.line = element_blank(),
               axis.ticks = element_blank(),
               axis.title = element_blank())

pdf.name <- "Dax_Idle_Hour_0.pdf"
ggsave(pdf.name, g, width = 7, height =7)
system(paste("open -a preview", pdf.name))

# g



###################################################
####### DAX DATA OVERLAY-ED WITH HOUR 0!!! ########
###################################################

data0 <- subset(data, hour == 0)
data_0 <- subset(booked_rides, hour == 0)
data_0_ <- subset(unbooked_rides, hour ==0)

bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

g <- ggmap(map, extent = "panel") +
  geom_point(aes(x = pickuplongitude, y = pickuplatitude),
             data = data_0_,
             colour = "red",
             alpha = 0.4,
             size = 0.7,
             stroke = 0) +
  geom_point(aes(x = pickuplongitude, y = pickuplatitude),
             data = data_0,
             colour = "green",
             alpha = 0.4,
             size = 0.7,
             stroke = 0) +
  geom_point(aes(x = lng, y = lat, alpha = dax0$idle_hours),
             data = dax0,
             colour = "yellow",
             stroke = 0,
             shape = 15,
             size = 5) +
  scale_alpha(name = "Idle Hours") +
  ggtitle("Idle Dax Overlaid With Unbooked and Booked Rides")

g <- g + theme(axis.text = element_blank(),
               axis.line = element_blank(),
               axis.ticks = element_blank(),
               axis.title = element_blank())

pdf.name <- "Dax_Idle_Hour_0.pdf"
ggsave(pdf.name, g, width = 7, height =7)
system(paste("open -a preview", pdf.name))


######## HOUR 0 WITH SURGE ############
data0 <- subset(data, hour == 0)
data_0 <- subset(booked_rides, hour == 0)
data_0_ <- subset(unbooked_rides, hour ==0)

g_with_surge <- ggmap(map, extent = "panel") +
  geom_point(aes(x = pickuplongitude, y = pickuplatitude),
             data = data_0_,
             colour = "red",
             alpha = 0.4,
             size = 0.7,
             stroke = 0) +
  geom_point(aes(x = pickuplongitude, y = pickuplatitude),
             data = data_0,
             colour = "green",
             alpha = 0.4,
             size = 0.7,
             stroke = 0) +
  geom_point(aes(x= pickuplongitude, y = pickuplatitude, alpha = data0$surge_scaled),
             data = data0,
             colour = "deepskyblue",
             size = 0.7,
             stroke = 0) +
  scale_alpha(range = c(0.001, 0.1),
              name = "Surge",
              labels = NULL) +
  ggtitle("Unbooked and Booked Rides with Surge Pricing")

g_with_surge <- g_with_surge + 
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")


pdf.name.surge <- "Dax_Idle_Hour_0_Surge.pdf"
ggsave(pdf.name.surge, g_with_surge, width = 7, height =7)
system(paste("open -a preview", pdf.name))

# g


#####################################
########## NEW DAX DATA !!! #########
#####################################
dax_data <- read.csv("~/Documents/may_jg_dax_idle_by_geohash-2.csv", stringsAsFactors=FALSE)
dax_data <- dax_data[, -c(2, 6, 7)]
dax_data$hour_of_day <- as.numeric(substring(dax_data$datehourlocal, 12, 13))

# Cleaning data
dax_data <- dax_data[complete.cases(dax_data), ]
dax_data <- subset(dax_data, idle_seconds <= 86400) #remove data where idle time >24 hrs
dax_data <- subset(dax_data, idle_seconds != 0)
dax_data$idle_hours_per_dax <- (dax_data$idle_seconds/3600)/dax_data$distinct_dax_count

# Subset
dax0 <- subset(dax_data, hour_of_day == 0)
dax0 <- cbind(dax0, gh_decode(dax0$start_geohash))
dax0 <- dax0[, c(2, 3, 6, 8, 9, 10)]


###### Overlay
bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

g_ <- ggmap(map, extent = "panel") +
  geom_point(aes(x = lng, y = lat, alpha = dax0$idle_hours_per_dax),
             data = dax0,
             colour = "yellow",
             stroke = 0,
             shape = 15,
             size = 5) +
  scale_alpha(range = c(0, 0.3),
                name = "Idle Hours per Dax") +
  ggtitle("Idle Dax")

g_ <- g_ + theme(axis.text = element_blank(),
               axis.line = element_blank(),
               axis.ticks = element_blank(),
               axis.title = element_blank())

pdf.name_new <- "Dax_Idle_Hour_0_new.pdf"
ggsave(pdf.name_new, g_, width = 7, height =7)
system(paste("open -a preview", pdf.name_new))





#########################################################
############## Weekends Only Analysis ###################
#########################################################

################################
##### Weekend Data
################################
# weekend_data9 <- subset(weekend, hour == 9)
# weekend_data_9 <- subset(weekend_booked_rides, hour == 9)
# weekend_data_9_ <- subset(weekend_unbooked_rides, hour ==9)
# 
# bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
# map <- get_map(location = bbox,
#                maptype = "toner-hybrid",
#                source = "stamen")
# 
# g <- ggmap(map, extent = "panel") +
#   geom_point(aes(x = pickuplongitude, y = pickuplatitude),
#              data = weekend_data_9_,
#              colour = "red",
#              alpha = 0.4,
#              size = 0.7,
#              stroke = 0) +
#   geom_point(aes(x = pickuplongitude, y = pickuplatitude),
#              data = weekend_data_9,
#              colour = "green",
#              alpha = 0.4,
#              size = 0.7,
#              stroke = 0) +
#   ggtitle("Unbooked and Booked Rides")
# 
# g <- g + theme(axis.text = element_blank(),
#                axis.line = element_blank(),
#                axis.ticks = element_blank(),
#                axis.title = element_blank())
# 
# #g
# 
# pdf.name <- "Weekend_9am.pdf"
# ggsave(pdf.name, g, width = 7, height =7)
# system(paste("open -a preview", pdf.name))

#####

# g_with_surge <- ggmap(map, extent = "panel") +
#   geom_point(aes(x = pickuplongitude, y = pickuplatitude),
#              data = weekend_data_9_,
#              colour = "red",
#              alpha = 0.4,
#              size = 0.7,
#              stroke = 0) +
#   geom_point(aes(x = pickuplongitude, y = pickuplatitude),
#              data = weekend_data_9,
#              colour = "green",
#              alpha = 0.4,
#              size = 0.7,
#              stroke = 0) +
#   geom_point(aes(x= pickuplongitude, y = pickuplatitude, alpha = weekend_data9$surge_scaled),
#              data = weekend_data9,
#              colour = "deepskyblue",
#              size = 0.7,
#              stroke = 0) +
#   scale_alpha(range = c(0.001, 0.1),
#               name = "Surge",
#               labels = NULL) +
#   ggtitle("Unbooked and Booked Rides with Surge Pricing")
# 
# 
# g_with_surge <- g_with_surge + 
#   theme(axis.text = element_blank(),
#         axis.line = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title = element_blank(),
#         legend.position = "none")
# 
# 
# pdf.name <- "Weekend_with_surge_9am.pdf"
# ggsave(pdf.name, g_with_surge, width = 7, height =7)
# system(paste("open -a preview", pdf.name))

