#################################
##### MAP FOR WHOLE OF JUNE #####
#################################
# Subset

# Generate Map Background
bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

# Open PDF Device
pdf("LATEST_Unbooked_Rides_by_Geohash_JUNE.pdf", onefile = TRUE)
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
    ggtitle(paste0("JUNE Hour ", i, ": Potential Demand (Unbooked Demand / Total Demand)"))
  
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


#final data

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

####################################
##### DAX MAP FOR WHOLE OF MAY #####
####################################

# Load data
dax_data <- read.csv("~/Downloads/LATEST product_analytics_hc_june_sgdax_idle_geohash.csv", stringsAsFactors=FALSE)

# cleaning up 
#extract date and time
dax_data$hour_of_day <- as.numeric(substring(dax_data$datehourlocal, 12, 13))
dax_data$idle_minutes <- dax_data$idle_seconds / 60
dax_data$idle_seconds_divided_by_dax <- dax_data$idle_seconds / dax_data$distinct_idle_dax_count
dax_data <- subset(dax_data, idle_seconds_divided_by_dax>40 & idle_seconds_divided_by_dax<441)

# new
dax_data_by_geohash <- dax_data[, c("start_geohash", "idle_seconds_divided_by_dax", "distinct_idle_dax_count", "hour_of_day")]
dax_data_by_geohash <- dax_data_by_geohash %>%
  group_by(start_geohash, hour_of_day) %>%
  summarise(avg_no_idle_dax = round(sum(distinct_idle_dax_count)/31), #per day
            avg_idle_seconds = sum(idle_seconds_divided_by_dax) / 31) %>% #per day
  filter(avg_no_idle_dax <= 70) %>%
  mutate(lat = gh_decode(start_geohash)[, 1]) %>% #convert geohash to lat
  mutate(lng = gh_decode(start_geohash)[, 2]) %>% #convert geohash to lng
  arrange(desc(avg_no_idle_dax))

# new
# Open PDF Device
pdf("NEW_LATEST_Dax_Idle_by_Geohash_JUNE.pdf", onefile = TRUE)
for (i in 0:23) {
  
  dax_data_by_geohash_ <- subset(dax_data_by_geohash, hour_of_day == i) #subset data for the day
  
  # Plot Map
  g <- ggmap(map, extent = "panel") +
    geom_point(aes(x = lng, y = lat, 
                   alpha = dax_data_by_geohash_$avg_idle_seconds, 
                   size = dax_data_by_geohash_$avg_no_idle_dax),
               data = dax_data_by_geohash_,
               colour = "yellow",
               stroke = 0,
               shape = 15) +
    scale_alpha(range = c(0, 1), limits = c(1.2, 292)) +
    labs(alpha = "Avg\nIdle Seconds") +
    scale_size(range = c(0, 4), limits = c(0, 70)) +
    labs(size = "Avg No. of\nIdle Dax") +
    ggtitle(paste0("JUNE Hour ", i, ": Aggregated Idle Dax"))
  
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




# old
dax_data_by_geohash <- dax_data[, c("start_geohash", "idle_minutes", "distinct_idle_dax_count", "hour_of_day")]
dax_data_by_geohash <- dax_data_by_geohash %>%
  group_by(start_geohash, hour_of_day) %>%
  summarise(avg_no_idle_dax = round(sum(distinct_idle_dax_count)/31), #per day
            avg_total_idle_minutes = (sum(idle_minutes) / sum(distinct_idle_dax_count) / 31)) %>% #per day
  filter(avg_no_idle_dax <= 70) %>%
  filter(avg_total_idle_minutes <= 100) %>%
  mutate(lat = gh_decode(start_geohash)[, 1]) %>% #convert geohash to lat
  mutate(lng = gh_decode(start_geohash)[, 2]) %>% #convert geohash to lng
  arrange(desc(avg_no_idle_dax))

dax_data$idle_seconds_divided_by_dax <- dax_data$idle_seconds / dax_data$distinct_idle_dax_count


# Generate Map Background
bbox <- c(103.6, 1.22, 104.05, 1.48) #bounding box of sg map
map <- get_map(location = bbox,
               maptype = "toner-hybrid",
               source = "stamen")

# Open PDF Device
pdf("NEW_LATEST_Dax_Idle_by_Geohash_JUNE.pdf", onefile = TRUE)
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
    scale_alpha(range = c(0.2, 1), limits = c(0, 2)) +
    labs(alpha = "Avg\nIdle Minutes") +
    scale_size(range = c(0, 4), limits = c(0, 70)) +
    labs(size = "Avg No. of\nIdle Dax") +
    ggtitle(paste0("JUNE Hour ", i, ": Aggregated Idle Dax"))
  
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

