##### BTR OF GRAB TRIPS VERSUS PUBLIC TRANSPORT TRIPS PROJECT
##### SUBSETTING DATA
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

BTR_data <- read.csv("~/Documents/grab_data_may2018.csv", row.names=1, stringsAsFactors=FALSE)
dummyoutput <- read.csv("~/Downloads/dummyoutput.csv", stringsAsFactors=FALSE)

# subset JustGrab rides
BTR_data <- subset(BTR_data, vehicletypeid==302)

# extract relevant data (e.g time)
BTR_data$bookings <- ifelse(BTR_data$bookings == 0, 0, 1) #if bookings==0 then 0, else 1
#extract date and time
BTR_data$datetime <- substring(BTR_data$streamtime, 1, 19)
pb.date <- as.POSIXct(BTR_data$datetime, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
BTR_data$datetime <- format(pb.date, tz="Singapore", usetz=TRUE)
BTR_data$time <- substring(BTR_data$datetime, 12, 19)
BTR_data$hour <- as.numeric(substring(BTR_data$time, 1, 2))
BTR_data$surge <- BTR_data$finalfare / BTR_data$nsnsfare

# subset relevant columns
BTR_data <- BTR_data[, c("uuid", "pickuplatitude", "pickuplongitude", 
                         "dropofflatitude", "dropofflongitude", 
                         "surge", "hour", "bookings", "duration", "distance")]

# write to csv
write.csv(BTR_data, "BTR_data.csv", row.names = FALSE)

################################################
################################################

#select first 100 rows, keeping relevant columns
dummydata <- BTR_data[1:1000,]
# write to csv
write.csv(dummydata, "dummydata.csv", row.names = FALSE)
# load dummyoutput
dummyoutput <- read.csv("./dummyoutput.csv", stringsAsFactors=FALSE)
# merge data with google api query results
dummymerged <- merge(dummydata, dummyoutput, by="uuid")
# filtering
dummymerged <- dummymerged[complete.cases(dummymerged),] #remove NAs
dummymerged <- subset(dummymerged, pt_duration > 0)

################################################
########## SUBSETTING DATA BY HOUR #############
################################################

# Hour 0 (DONE)
BTR_data_0 <- subset(BTR_data, hour == 0)
write.csv(BTR_data_0, "BTR_data_0.csv", row.names = FALSE)

# Hour 1 (DONE)
BTR_data_1 <- subset(BTR_data, hour == 1)
write.csv(BTR_data_1, "BTR_data_1.csv", row.names = FALSE)

# Hour 2 (DONE)
BTR_data_2 <- subset(BTR_data, hour == 2)
write.csv(BTR_data_2, "BTR_data_2.csv", row.names = FALSE)

# Hour 3 (DONE)
BTR_data_3 <- subset(BTR_data, hour == 3)
write.csv(BTR_data_3, "BTR_data_3.csv", row.names = FALSE)

# Hour 4 (DONE)
BTR_data_4 <- subset(BTR_data, hour == 4)
write.csv(BTR_data_4, "BTR_data_4.csv", row.names = FALSE)

# Hour 5 (DONE)
BTR_data_5 <- subset(BTR_data, hour == 5)
write.csv(BTR_data_5, "BTR_data_5.csv", row.names = FALSE)

# Hour 6 (DONE)
BTR_data_6 <- subset(BTR_data, hour == 6)
write.csv(BTR_data_6, "BTR_data_6.csv", row.names = FALSE)

# Hour 7 (DONE)
BTR_data_7 <- subset(BTR_data, hour == 7)
write.csv(BTR_data_7, "BTR_data_7.csv", row.names = FALSE)

# Hour 8 (DONE)
BTR_data_8 <- subset(BTR_data, hour == 8)
write.csv(BTR_data_8, "BTR_data_8.csv", row.names = FALSE)

# Hour 9 (DONE)
BTR_data_9 <- subset(BTR_data, hour == 9)
write.csv(BTR_data_9, "BTR_data_9.csv", row.names = FALSE)

# Hour 10 (DONE)
BTR_data_10 <- subset(BTR_data, hour == 10)
write.csv(BTR_data_10, "BTR_data_10.csv", row.names = FALSE)

# Hour 11 (DONE)
BTR_data_11 <- subset(BTR_data, hour == 11)
write.csv(BTR_data_11, "BTR_data_11.csv", row.names = FALSE)

# Hour 12 (DONE)
BTR_data_12 <- subset(BTR_data, hour == 12)
write.csv(BTR_data_12, "BTR_data_12.csv", row.names = FALSE)

# Hour 13 (DONE)
BTR_data_13 <- subset(BTR_data, hour == 13)
write.csv(BTR_data_13, "BTR_data_13.csv", row.names = FALSE)

# Hour 14 (DONE)
BTR_data_14 <- subset(BTR_data, hour == 14)
write.csv(BTR_data_14, "BTR_data_14.csv", row.names = FALSE)

# Hour 15 (DONE)
BTR_data_15 <- subset(BTR_data, hour == 15)
write.csv(BTR_data_15, "BTR_data_15.csv", row.names = FALSE)

# Hour 16 (DONE)
BTR_data_16 <- subset(BTR_data, hour == 16)
write.csv(BTR_data_16, "BTR_data_16.csv", row.names = FALSE)

# Hour 17 (DONE)
BTR_data_17 <- subset(BTR_data, hour == 17)
write.csv(BTR_data_17, "BTR_data_17.csv", row.names = FALSE)

# Hour 18 (DONE)
BTR_data_18 <- subset(BTR_data, hour == 18)
write.csv(BTR_data_18, "BTR_data_18.csv", row.names = FALSE)

# Hour 19 (DONE)
BTR_data_19 <- subset(BTR_data, hour == 19)
write.csv(BTR_data_19, "BTR_data_19.csv", row.names = FALSE)

# Hour 20 (DONE)
BTR_data_20 <- subset(BTR_data, hour == 20)
write.csv(BTR_data_20, "BTR_data_20.csv", row.names = FALSE)

# Hour 21 (DONE)
BTR_data_21 <- subset(BTR_data, hour == 21)
write.csv(BTR_data_21, "BTR_data_21.csv", row.names = FALSE)

# Hour 22 (DONE)
BTR_data_22 <- subset(BTR_data, hour == 22)
write.csv(BTR_data_22, "BTR_data_22.csv", row.names = FALSE)

# Hour 23 (DONE)
BTR_data_23 <- subset(BTR_data, hour == 23)
write.csv(BTR_data_23, "BTR_data_23.csv", row.names = FALSE)


#########################################################################
######################## ANALYSIS OF RESULTS ############################
#########################################################################


############################## BTR/ HOUR-BY-HOUR ################################

pdf("test_BTR_Hour-by-hour.pdf", onefile = TRUE)

for (i in 0:23) {
  
  # make strings for filenames
  BTR_data_filename <- paste("~/Downloads/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  # clean data
  BTR <- BTR[complete.cases(BTR),] #remove NAs
  BTR <- subset(BTR, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
  # find the 99 percentile of pt_duration and duration respectively
  ninetyninthpercentile_pt <- quantile(BTR$pt_duration, 0.99)
  ninetyninthpercentile_grab <- quantile(BTR$duration, 0.99)
  BTR <- subset(BTR, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
  BTR <- subset(BTR, duration < pt_duration) # Grab trips that take less time than public transport
  BTR$bookings <- ifelse(BTR$bookings == 0, 0, 1)
  BTR$grab_to_pt_duration <- BTR$duration / BTR$pt_duration
  
  df_total <- transform(BTR,
                           group = cut(BTR$grab_to_pt_duration,
                                       breaks = seq(0, 1, by=0.1)))
  
  df_total <- df_total %>%
    group_by(group) %>%
    summarise(count_total = n())
  
  BTR_booked <- subset(BTR, bookings == 1)
  df_booked <- transform(BTR_booked, 
                            group = cut(BTR_booked$grab_to_pt_duration,
                                        breaks = seq(0, 1, by=0.1)))
  
  df_booked <- df_booked %>%
    group_by(group) %>%
    summarise(count_booked=n())
  
  # merge the two table's counts
  df <- merge(df_total, df_booked, by = "group")
  df$BTR <- df$count_booked / df$count_total
  firstquartile_count_total <- quantile(df$count_total, 0.30)
  df <- subset(df, count_total >= firstquartile_count_total) #remove all counts lower than first quartile
  
  ### PLOT FOR BTR ### 
  # basic bar plot
  p <- ggplot(data=df, aes(x=rev(group), y=BTR)) +
    geom_bar(stat="identity", fill="aquamarine3") +
    theme_minimal() +
    labs(x="Time Saved", y="BTR") +
    ggtitle(paste0("Hour ", i)) +
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
          axis.text.x = element_blank()) +
    geom_text(aes(label=df$count_total), vjust=-0.25)
  print(p)
  
}

dev.off()


######################## BTR/ HOUR-BY-HOUR WITH SURGE BUCKETS ############################

pdf("BTR_Hour-by-hour_with_Surge_buckets.pdf", onefile = TRUE, width = 11, height = 9)

for (i in 0:23) {
  
  # make strings for filenames
  BTR_data_filename <- paste("~/Downloads/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  # clean data
  BTR <- BTR[complete.cases(BTR),] #remove NAs
  BTR <- subset(BTR, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
  # find the 99 percentile of pt_duration and duration respectively
  ninetyninthpercentile_pt <- quantile(BTR$pt_duration, 0.99)
  ninetyninthpercentile_grab <- quantile(BTR$duration, 0.99)
  BTR <- subset(BTR, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
  BTR <- subset(BTR, duration < pt_duration) # Grab trips that take less time than public transport
  BTR$bookings <- ifelse(BTR$bookings == 0, 0, 1)
  BTR$grab_to_pt_duration <- BTR$duration / BTR$pt_duration
  
  df_total <- transform(BTR,
                        group = cut(BTR$grab_to_pt_duration,
                                    breaks = seq(0, 1, by=0.1)))
  
  df_total <- df_total %>%
    group_by(group) %>%
    summarise(count_total = n())
  
  BTR_booked <- subset(BTR, bookings == 1)
  df_booked <- transform(BTR_booked, 
                         group = cut(BTR_booked$grab_to_pt_duration,
                                     breaks = seq(0, 1, by=0.1)))
  
  df_booked <- df_booked %>%
    group_by(group) %>%
    summarise(count_booked=n())
  
  # merge the two table's counts
  df <- merge(df_total, df_booked, by = "group")
  df$BTR <- df$count_booked / df$count_total
  firstquartile_count_total <- quantile(df$count_total, 0.30)
  df <- subset(df, count_total >= firstquartile_count_total) #remove all counts lower than first quartile
  
  # basic bar plot
  p1 <- ggplot(data=df, aes(x=group, y=BTR)) +
    geom_bar(stat="identity", fill="aquamarine3") +
    theme_minimal() +
    labs(x="Grab to Public Transport Duration", y="BTR") +
    ggtitle(paste0("Hour ", i)) +
    theme(plot.title = element_text(face="bold")) +
    geom_text(aes(label=df$count_total), vjust=-0.25)
  # plot(p)
  
  ### SUBSET BY SURGE ###
  
  # Trying to split buckets based on surge
  BTR_surge_bucket1 <- subset(BTR, surge <=1)
  BTR_surge_bucket2 <- subset(BTR, surge >1 & surge <1.5)
  BTR_surge_bucket3 <- subset(BTR, surge >=1.5)
  
  ### BUCKET 1
  df_total_surge_bucket1 <- transform(BTR_surge_bucket1,
                                group = cut(BTR_surge_bucket1$grab_to_pt_duration,
                                            breaks = seq(0,1, by=0.05)))
  df_total_surge_bucket1 <- df_total_surge_bucket1 %>%
    group_by(group) %>%
    summarise(count_total = n())
  BTR_booked_surge_bucket1 <- subset(BTR_surge_bucket1, bookings >= 1)
  df_booked_surge_bucket1 <- transform(BTR_booked_surge_bucket1, 
                                 group = cut(BTR_booked_surge_bucket1$grab_to_pt_duration,
                                             breaks = seq(0,1, by=0.05)))
  df_booked_surge_bucket1 <- df_booked_surge_bucket1 %>%
    group_by(group) %>%
    summarise(count_booked=n())
  # merge the two table's counts
  df_surge_bucket1 <- merge(df_total_surge_bucket1, df_booked_surge_bucket1, by = "group")
  df_surge_bucket1$BTR <- df_surge_bucket1$count_booked / df_surge_bucket1$count_total
  firstquartile_count_total <- quantile(df_surge_bucket1$count_total, 0.35)
  df_surge_bucket1 <- subset(df_surge_bucket1, count_total > firstquartile_count_total)
  
  # basic bar plot
  p2 <- ggplot(data=df_surge_bucket1, aes(x=group, y=BTR)) +
    geom_bar(stat="identity", fill="darksalmon") +
    theme_minimal() +
    labs(x="Grab to Public Transport Duration", y="BTR") +
    ggtitle(paste0("Surge <= 1")) +
    geom_text(aes(label=df_surge_bucket1$count_total), vjust=-0.25) +
    theme(axis.text.x = element_text(angle = 50, hjust = 1))
  # plot(p)
  
  ### BUCKET 2
  df_total_surge_bucket2 <- transform(BTR_surge_bucket2,
                                group = cut(BTR_surge_bucket2$grab_to_pt_duration,
                                            breaks = seq(0,1, by=0.05)))
  df_total_surge_bucket2 <- df_total_surge_bucket2 %>%
    group_by(group) %>%
    summarise(count_total = n())
  BTR_booked_surge_bucket2 <- subset(BTR_surge_bucket2, bookings >= 1)
  df_booked_surge_bucket2 <- transform(BTR_booked_surge_bucket2, 
                                 group = cut(BTR_booked_surge_bucket2$grab_to_pt_duration,
                                             breaks = seq(0,1, by=0.05)))
  df_booked_surge_bucket2 <- df_booked_surge_bucket2 %>%
    group_by(group) %>%
    summarise(count_booked=n())
  # merge the two table's counts
  df_surge_bucket2 <- merge(df_total_surge_bucket2, df_booked_surge_bucket2, by = "group")
  df_surge_bucket2$BTR <- df_surge_bucket2$count_booked / df_surge_bucket2$count_total
  firstquartile_count_total <- quantile(df_surge_bucket2$count_total, 0.35)
  df_surge_bucket2 <- subset(df_surge_bucket2, count_total > firstquartile_count_total)
  # basic bar plot
  p3 <- ggplot(data=df_surge_bucket2, aes(x=group, y=BTR)) +
    geom_bar(stat="identity", fill="darksalmon") +
    theme_minimal() +
    labs(x="Grab to Public Transport Duration", y="BTR") +
    ggtitle(paste0("1 < Surge < 1.5")) +
    geom_text(aes(label=df_surge_bucket2$count_total), vjust=-0.25) +
    theme(axis.text.x = element_text(angle = 50, hjust = 1))
  #plot(p)
  
  ### BUCKET 3
  df_total_surge_bucket3 <- transform(BTR_surge_bucket3,
                                group = cut(BTR_surge_bucket3$grab_to_pt_duration,
                                            breaks = seq(0,1, by=0.05)))
  df_total_surge_bucket3 <- df_total_surge_bucket3 %>%
    group_by(group) %>%
    summarise(count_total = n())
  BTR_booked_surge_bucket3 <- subset(BTR_surge_bucket3, bookings >= 1)
  df_booked_surge_bucket3 <- transform(BTR_booked_surge_bucket3, 
                                 group = cut(BTR_booked_surge_bucket3$grab_to_pt_duration,
                                             breaks = seq(0,1, by=0.05)))
  df_booked_surge_bucket3 <- df_booked_surge_bucket3 %>%
    group_by(group) %>%
    summarise(count_booked=n())
  # merge the two table's counts
  df_surge_bucket3 <- merge(df_total_surge_bucket3, df_booked_surge_bucket3, by = "group")
  df_surge_bucket3$BTR <- df_surge_bucket3$count_booked / df_surge_bucket3$count_total
  firstquartile_count_total <- quantile(df_surge_bucket3$count_total, 0.35)
  df_surge_bucket3 <- subset(df_surge_bucket3, count_total > firstquartile_count_total)
  # basic bar plot
  p4 <- ggplot(data=df_surge_bucket3, aes(x=group, y=BTR)) +
    geom_bar(stat="identity", fill="darksalmon") +
    theme_minimal() +
    labs(x="Grab to Public Transport Duration", y="BTR") +
    ggtitle(paste0("1.5 <= Surge")) +
    geom_text(aes(label=df_surge_bucket3$count_total), vjust=-0.25) +
    theme(axis.text.x = element_text(angle = 50, hjust = 1))
  #plot(p)
  
  grid.arrange(p1, p2, p3, p4, nrow = 2)
  
}

dev.off()

################################# BTR/ TIME GROUPS #####################################

pdf("BTR_Time_Saved_Time_Groups.pdf", onefile = TRUE, width = 11, height = 9)

### 1. NOCTURNAL (HOUR 0-5) ###

nocturnal <- vector()

for (i in 0:5) {
  # make strings for filenames
  BTR_data_filename <- paste("~/Downloads/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  nocturnal <- rbind(nocturnal, BTR)
}

# clean data
nocturnal <- nocturnal[complete.cases(nocturnal),] #remove NAs
nocturnal <- subset(nocturnal, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(nocturnal$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(nocturnal$duration, 0.99)
nocturnal <- subset(nocturnal, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
nocturnal <- subset(nocturnal, duration < pt_duration) # Grab trips that take less time than public transport
nocturnal$bookings <- ifelse(nocturnal$bookings == 0, 0, 1)
nocturnal$grab_to_pt_duration <- nocturnal$duration / nocturnal$pt_duration

df_total <- transform(nocturnal,
                      group = cut(nocturnal$grab_to_pt_duration,
                                  breaks = seq(0, 1, by=0.1)))

df_total <- df_total %>%
  group_by(group) %>%
  summarise(count_total = n())

BTR_booked <- subset(nocturnal, bookings == 1)
df_booked <- transform(BTR_booked, 
                       group = cut(BTR_booked$grab_to_pt_duration,
                                   breaks = seq(0, 1, by=0.1)))

df_booked <- df_booked %>%
  group_by(group) %>%
  summarise(count_booked=n())

# merge the two table's counts
df <- merge(df_total, df_booked, by = "group")
df$BTR <- df$count_booked / df$count_total
firstquartile_count_booked <- quantile(df$count_booked, 0.25)
df <- subset(df, count_booked >= firstquartile_count_booked) #remove all counts lower than first quartile

labs <- c("30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100")

### PLOT FOR Nocturnal ### 
# basic bar plot
p <- ggplot(data=df, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="aquamarine3") +
  theme_minimal() +
  labs(x="% Time Saved", y="BTR") +
  ggtitle(paste0("Nocturnal (12am-6am)")) +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15)) +
  scale_x_discrete(labels=labs) +
  geom_text(aes(label=df$count_total), vjust=-0.25)
print(p)


### 2. MORNING PEAK (HOUR 6-9) ###

morning_peak <- vector()

for (i in 6:9) {
  # make strings for filenames
  BTR_data_filename <- paste("~/Downloads/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  morning_peak <- rbind(morning_peak, BTR)
}

# clean data
morning_peak <- morning_peak[complete.cases(morning_peak),] #remove NAs
morning_peak <- subset(morning_peak, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(morning_peak$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(morning_peak$duration, 0.99)
morning_peak <- subset(morning_peak, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
morning_peak <- subset(morning_peak, duration < pt_duration) # Grab trips that take less time than public transport
morning_peak$bookings <- ifelse(morning_peak$bookings == 0, 0, 1)
morning_peak$grab_to_pt_duration <- morning_peak$duration / morning_peak$pt_duration

df_total <- transform(morning_peak,
                      group = cut(morning_peak$grab_to_pt_duration,
                                  breaks = seq(0, 1, by=0.1)))

df_total <- df_total %>%
  group_by(group) %>%
  summarise(count_total = n())

BTR_booked <- subset(morning_peak, bookings == 1)
df_booked <- transform(BTR_booked, 
                       group = cut(BTR_booked$grab_to_pt_duration,
                                   breaks = seq(0, 1, by=0.1)))

df_booked <- df_booked %>%
  group_by(group) %>%
  summarise(count_booked=n())

# merge the two table's counts
df <- merge(df_total, df_booked, by = "group")
df$BTR <- df$count_booked / df$count_total
firstquartile_count_booked <- quantile(df$count_booked, 0.25)
df <- subset(df, count_booked >= firstquartile_count_booked) #remove all counts lower than first quartile

labs <- c("20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90")

### PLOT FOR Morning Peak ### 
# basic bar plot
p <- ggplot(data=df, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="aquamarine3") +
  theme_minimal() +
  labs(x="% Time Saved", y="BTR") +
  ggtitle(paste0("Morning Peak (6am-10am)")) +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15)) +
  scale_x_discrete(labels=labs) +
  geom_text(aes(label=df$count_total), vjust=-0.25)
print(p)


### 3. MID-DAY (HOUR 10-16) ###

midday <- vector()

for (i in 10:16) {
  # make strings for filenames
  BTR_data_filename <- paste("~/Downloads/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  midday <- rbind(midday, BTR)
}

# clean data
midday <- midday[complete.cases(midday),] #remove NAs
midday <- subset(midday, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(midday$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(midday$duration, 0.99)
midday <- subset(midday, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
midday <- subset(midday, duration < pt_duration) # Grab trips that take less time than public transport
midday$bookings <- ifelse(midday$bookings == 0, 0, 1)
midday$grab_to_pt_duration <- midday$duration / midday$pt_duration

df_total <- transform(midday,
                      group = cut(midday$grab_to_pt_duration,
                                  breaks = seq(0, 1, by=0.1)))
df_total <- df_total %>%
  group_by(group) %>%
  summarise(count_total = n())

BTR_booked <- subset(midday, bookings == 1)
df_booked <- transform(BTR_booked, 
                       group = cut(BTR_booked$grab_to_pt_duration,
                                   breaks = seq(0, 1, by=0.1)))

df_booked <- df_booked %>%
  group_by(group) %>%
  summarise(count_booked=n())

# merge the two table's counts
df <- merge(df_total, df_booked, by = "group")
df$BTR <- df$count_booked / df$count_total
firstquartile_count_booked <- quantile(df$count_booked, 0.25)
df <- subset(df, count_booked >= firstquartile_count_booked) #remove all counts lower than first quartile

labs <- c("20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90")

### PLOT FOR Mid-day ### 
# basic bar plot
p <- ggplot(data=df, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="aquamarine3") +
  theme_minimal() +
  labs(x="% Time Saved", y="BTR") +
  ggtitle(paste0("Mid-day (10am-4pm)")) +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15)) +
  scale_x_discrete(labels=labs) +
  geom_text(aes(label=df$count_total), vjust=-0.25)
print(p)


### 4. EVENING PEAK (HOUR 17-19) ###

evening_peak <- vector()

for (i in 17:19) {
  # make strings for filenames
  BTR_data_filename <- paste("~/Downloads/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  evening_peak <- rbind(evening_peak, BTR)
}

# clean data
evening_peak <- evening_peak[complete.cases(evening_peak),] #remove NAs
evening_peak <- subset(evening_peak, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(evening_peak$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(evening_peak$duration, 0.99)
evening_peak <- subset(evening_peak, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
evening_peak <- subset(evening_peak, duration < pt_duration) # Grab trips that take less time than public transport
evening_peak$bookings <- ifelse(evening_peak$bookings == 0, 0, 1)
evening_peak$grab_to_pt_duration <- evening_peak$duration / evening_peak$pt_duration

df_total <- transform(evening_peak,
                      group = cut(evening_peak$grab_to_pt_duration,
                                  breaks = seq(0, 1, by=0.1)))

df_total <- df_total %>%
  group_by(group) %>%
  summarise(count_total = n())

BTR_booked <- subset(evening_peak, bookings == 1)
df_booked <- transform(BTR_booked, 
                       group = cut(BTR_booked$grab_to_pt_duration,
                                   breaks = seq(0, 1, by=0.1)))

df_booked <- df_booked %>%
  group_by(group) %>%
  summarise(count_booked=n())

# merge the two table's counts
df <- merge(df_total, df_booked, by = "group")
df$BTR <- df$count_booked / df$count_total
firstquartile_count_booked <- quantile(df$count_booked, 0.25)
df <- subset(df, count_booked >= firstquartile_count_booked) #remove all counts lower than first quartile

labs <- c("10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80")

### PLOT FOR Evening Peak ### 
# basic bar plot
p <- ggplot(data=df, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="aquamarine3") +
  theme_minimal() +
  labs(x="% Time Saved", y="BTR") +
  ggtitle(paste0("Evening Peak (5pm-8pm)")) +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15)) +
  scale_x_discrete(labels=labs) +
  geom_text(aes(label=df$count_total), vjust=-0.25)
print(p)


### 5. NIGHT TIME (HOUR 20-23) ###

night_time <- vector()

for (i in 20:23) {
  # make strings for filenames
  BTR_data_filename <- paste("~/Downloads/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  night_time <- rbind(night_time, BTR)
}

# clean data
night_time <- night_time[complete.cases(night_time),] #remove NAs
night_time <- subset(night_time, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(night_time$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(night_time$duration, 0.99)
night_time <- subset(night_time, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
night_time <- subset(night_time, duration < pt_duration) # Grab trips that take less time than public transport
night_time$bookings <- ifelse(night_time$bookings == 0, 0, 1)
night_time$grab_to_pt_duration <- night_time$duration / night_time$pt_duration

df_total <- transform(night_time,
                      group = cut(night_time$grab_to_pt_duration,
                                  breaks = seq(0, 1, by=0.1)))

df_total <- df_total %>%
  group_by(group) %>%
  summarise(count_total = n())

BTR_booked <- subset(night_time, bookings == 1)
df_booked <- transform(BTR_booked, 
                       group = cut(BTR_booked$grab_to_pt_duration,
                                   breaks = seq(0, 1, by=0.1)))

df_booked <- df_booked %>%
  group_by(group) %>%
  summarise(count_booked=n())

# merge the two table's counts
df <- merge(df_total, df_booked, by = "group")
df$BTR <- df$count_booked / df$count_total
firstquartile_count_booked <- quantile(df$count_booked, 0.25)
df <- subset(df, count_booked >= firstquartile_count_booked) #remove all counts lower than first quartile

labs <- c("20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90")

### PLOT FOR Night Time ### 
# basic bar plot
p <- ggplot(data=df, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="aquamarine3") +
  theme_minimal() +
  labs(x="% Time Saved", y="BTR") +
  ggtitle(paste0("Night Time (8pm-12am)")) +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15)) +
  scale_x_discrete(labels=labs) +
  geom_text(aes(label=df$count_total), vjust=-0.25)
print(p)

dev.off()




############################# BTR/ TIME GROUPS WITH SURGE BUCKETS #################################

pdf("BTR_Time_Saved_Time_Groups_with_Surge_buckets.pdf", onefile = TRUE, width = 11, height = 9)

### 1. NOCTURNAL (HOUR 0-5) ###

nocturnal <- vector()

for (i in 0:5) {
  # make strings for filenames
  BTR_data_filename <- paste("~/Downloads/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  nocturnal <- rbind(nocturnal, BTR)
}

# clean data
nocturnal <- nocturnal[complete.cases(nocturnal),] #remove NAs
nocturnal <- subset(nocturnal, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(nocturnal$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(nocturnal$duration, 0.99)
nocturnal <- subset(nocturnal, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
nocturnal <- subset(nocturnal, duration < pt_duration) # Grab trips that take less time than public transport
nocturnal$bookings <- ifelse(nocturnal$bookings == 0, 0, 1)
nocturnal$grab_to_pt_duration <- nocturnal$duration / nocturnal$pt_duration

df_total <- transform(nocturnal,
                      group = cut(nocturnal$grab_to_pt_duration,
                                  breaks = seq(0, 1, by=0.1)))

df_total <- df_total %>%
  group_by(group) %>%
  summarise(count_total = n())

BTR_booked <- subset(nocturnal, bookings == 1)
df_booked <- transform(BTR_booked, 
                       group = cut(BTR_booked$grab_to_pt_duration,
                                   breaks = seq(0, 1, by=0.1)))

df_booked <- df_booked %>%
  group_by(group) %>%
  summarise(count_booked=n())

# merge the two table's counts
df <- merge(df_total, df_booked, by = "group")
df$BTR <- df$count_booked / df$count_total
firstquartile_count_booked <- quantile(df$count_booked, 0.25)
df <- subset(df, count_booked >= firstquartile_count_booked) #remove all counts lower than first quartile

### PLOT FOR Nocturnal ### 
# basic bar plot
p1 <- ggplot(data=df, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="aquamarine3") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("Nocturnal (12am-6am)")) +
  theme(plot.title = element_text(face="bold"), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label=df$count_total), vjust=-0.25, size=3)

# Trying to split buckets based on surge
BTR_surge_bucket1 <- subset(nocturnal, surge <=1)
BTR_surge_bucket2 <- subset(nocturnal, surge >1 & surge <1.5)
BTR_surge_bucket3 <- subset(nocturnal, surge >=1.5)

### BUCKET 1
df_total_surge_bucket1 <- transform(BTR_surge_bucket1,
                                    group = cut(BTR_surge_bucket1$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_surge_bucket1 <- df_total_surge_bucket1 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_surge_bucket1 <- subset(BTR_surge_bucket1, bookings >= 1)
df_booked_surge_bucket1 <- transform(BTR_booked_surge_bucket1, 
                                     group = cut(BTR_booked_surge_bucket1$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_surge_bucket1 <- df_booked_surge_bucket1 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_surge_bucket1 <- merge(df_total_surge_bucket1, df_booked_surge_bucket1, by = "group")
df_surge_bucket1$BTR <- df_surge_bucket1$count_booked / df_surge_bucket1$count_total
firstquartile_count_total <- quantile(df_surge_bucket1$count_total, 0.25)
df_surge_bucket1 <- subset(df_surge_bucket1, count_total > firstquartile_count_total)

# basic bar plot
p2 <- ggplot(data=df_surge_bucket1, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="darksalmon") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("Surge <= 1")) +
  geom_text(aes(label=df_surge_bucket1$count_total), vjust=-0.25, size=3) +
  theme( 
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 2
df_total_surge_bucket2 <- transform(BTR_surge_bucket2,
                                    group = cut(BTR_surge_bucket2$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_surge_bucket2 <- df_total_surge_bucket2 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_surge_bucket2 <- subset(BTR_surge_bucket2, bookings >= 1)
df_booked_surge_bucket2 <- transform(BTR_booked_surge_bucket2, 
                                     group = cut(BTR_booked_surge_bucket2$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_surge_bucket2 <- df_booked_surge_bucket2 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_surge_bucket2 <- merge(df_total_surge_bucket2, df_booked_surge_bucket2, by = "group")
df_surge_bucket2$BTR <- df_surge_bucket2$count_booked / df_surge_bucket2$count_total
firstquartile_count_total <- quantile(df_surge_bucket2$count_total, 0.25)
df_surge_bucket2 <- subset(df_surge_bucket2, count_total > firstquartile_count_total)
# basic bar plot
p3 <- ggplot(data=df_surge_bucket2, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="darksalmon") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("1 < Surge < 1.5")) +
  geom_text(aes(label=df_surge_bucket2$count_total), vjust=-0.25, size=3) +
  theme(
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 3
df_total_surge_bucket3 <- transform(BTR_surge_bucket3,
                                    group = cut(BTR_surge_bucket3$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_surge_bucket3 <- df_total_surge_bucket3 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_surge_bucket3 <- subset(BTR_surge_bucket3, bookings >= 1)
df_booked_surge_bucket3 <- transform(BTR_booked_surge_bucket3, 
                                     group = cut(BTR_booked_surge_bucket3$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_surge_bucket3 <- df_booked_surge_bucket3 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_surge_bucket3 <- merge(df_total_surge_bucket3, df_booked_surge_bucket3, by = "group")
df_surge_bucket3$BTR <- df_surge_bucket3$count_booked / df_surge_bucket3$count_total
firstquartile_count_total <- quantile(df_surge_bucket3$count_total, 0.25)
df_surge_bucket3 <- subset(df_surge_bucket3, count_total > firstquartile_count_total)
# basic bar plot
p4 <- ggplot(data=df_surge_bucket3, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="darksalmon") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("1.5 <= Surge")) +
  geom_text(aes(label=df_surge_bucket3$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

grid.arrange(p1, p2, p3, p4, nrow = 2)


### 2. MORNING PEAK (HOUR 6-9) ###

morning_peak <- vector()

for (i in 6:9) {
  # make strings for filenames
  BTR_data_filename <- paste("~/Downloads/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  morning_peak <- rbind(morning_peak, BTR)
}

# clean data
morning_peak <- morning_peak[complete.cases(morning_peak),] #remove NAs
morning_peak <- subset(morning_peak, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(morning_peak$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(morning_peak$duration, 0.99)
morning_peak <- subset(morning_peak, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
morning_peak <- subset(morning_peak, duration < pt_duration) # Grab trips that take less time than public transport
morning_peak$bookings <- ifelse(morning_peak$bookings == 0, 0, 1)
morning_peak$grab_to_pt_duration <- morning_peak$duration / morning_peak$pt_duration

df_total <- transform(morning_peak,
                      group = cut(morning_peak$grab_to_pt_duration,
                                  breaks = seq(0, 1, by=0.1)))

df_total <- df_total %>%
  group_by(group) %>%
  summarise(count_total = n())

BTR_booked <- subset(morning_peak, bookings == 1)
df_booked <- transform(BTR_booked, 
                       group = cut(BTR_booked$grab_to_pt_duration,
                                   breaks = seq(0, 1, by=0.1)))

df_booked <- df_booked %>%
  group_by(group) %>%
  summarise(count_booked=n())

# merge the two table's counts
df <- merge(df_total, df_booked, by = "group")
df$BTR <- df$count_booked / df$count_total
firstquartile_count_booked <- quantile(df$count_booked, 0.25)
df <- subset(df, count_booked >= firstquartile_count_booked) #remove all counts lower than first quartile

### PLOT FOR Morning Peak ### 
# basic bar plot
p1 <- ggplot(data=df, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="aquamarine3") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("Morning Peak (6am-10am)")) +
  theme(plot.title = element_text(face="bold"), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label=df$count_total), vjust=-0.25, size=3)

# Trying to split buckets based on surge
BTR_surge_bucket1 <- subset(morning_peak, surge <=1)
BTR_surge_bucket2 <- subset(morning_peak, surge >1 & surge <1.5)
BTR_surge_bucket3 <- subset(morning_peak, surge >=1.5)

### BUCKET 1
df_total_surge_bucket1 <- transform(BTR_surge_bucket1,
                                    group = cut(BTR_surge_bucket1$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_surge_bucket1 <- df_total_surge_bucket1 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_surge_bucket1 <- subset(BTR_surge_bucket1, bookings >= 1)
df_booked_surge_bucket1 <- transform(BTR_booked_surge_bucket1, 
                                     group = cut(BTR_booked_surge_bucket1$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_surge_bucket1 <- df_booked_surge_bucket1 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_surge_bucket1 <- merge(df_total_surge_bucket1, df_booked_surge_bucket1, by = "group")
df_surge_bucket1$BTR <- df_surge_bucket1$count_booked / df_surge_bucket1$count_total
firstquartile_count_total <- quantile(df_surge_bucket1$count_total, 0.25)
df_surge_bucket1 <- subset(df_surge_bucket1, count_total > firstquartile_count_total)

# basic bar plot
p2 <- ggplot(data=df_surge_bucket1, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="darksalmon") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("Surge <= 1")) +
  geom_text(aes(label=df_surge_bucket1$count_total), vjust=-0.25, size=3) +
  theme(
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 2
df_total_surge_bucket2 <- transform(BTR_surge_bucket2,
                                    group = cut(BTR_surge_bucket2$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_surge_bucket2 <- df_total_surge_bucket2 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_surge_bucket2 <- subset(BTR_surge_bucket2, bookings >= 1)
df_booked_surge_bucket2 <- transform(BTR_booked_surge_bucket2, 
                                     group = cut(BTR_booked_surge_bucket2$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_surge_bucket2 <- df_booked_surge_bucket2 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_surge_bucket2 <- merge(df_total_surge_bucket2, df_booked_surge_bucket2, by = "group")
df_surge_bucket2$BTR <- df_surge_bucket2$count_booked / df_surge_bucket2$count_total
firstquartile_count_total <- quantile(df_surge_bucket2$count_total, 0.25)
df_surge_bucket2 <- subset(df_surge_bucket2, count_total > firstquartile_count_total)
# basic bar plot
p3 <- ggplot(data=df_surge_bucket2, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="darksalmon") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("1 < Surge < 1.5")) +
  geom_text(aes(label=df_surge_bucket2$count_total), vjust=-0.25, size=3) +
  theme(
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 3
df_total_surge_bucket3 <- transform(BTR_surge_bucket3,
                                    group = cut(BTR_surge_bucket3$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_surge_bucket3 <- df_total_surge_bucket3 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_surge_bucket3 <- subset(BTR_surge_bucket3, bookings >= 1)
df_booked_surge_bucket3 <- transform(BTR_booked_surge_bucket3, 
                                     group = cut(BTR_booked_surge_bucket3$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_surge_bucket3 <- df_booked_surge_bucket3 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_surge_bucket3 <- merge(df_total_surge_bucket3, df_booked_surge_bucket3, by = "group")
df_surge_bucket3$BTR <- df_surge_bucket3$count_booked / df_surge_bucket3$count_total
firstquartile_count_total <- quantile(df_surge_bucket3$count_total, 0.25)
df_surge_bucket3 <- subset(df_surge_bucket3, count_total > firstquartile_count_total)
# basic bar plot
p4 <- ggplot(data=df_surge_bucket3, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="darksalmon") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("1.5 <= Surge")) +
  geom_text(aes(label=df_surge_bucket3$count_total), vjust=-0.25, size=3) +
  theme(
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

grid.arrange(p1, p2, p3, p4, nrow = 2)

### 3. MID-DAY (HOUR 10-16) ###

midday <- vector()

for (i in 10:16) {
  # make strings for filenames
  BTR_data_filename <- paste("~/Downloads/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  midday <- rbind(midday, BTR)
}

# clean data
midday <- midday[complete.cases(midday),] #remove NAs
midday <- subset(midday, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(midday$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(midday$duration, 0.99)
midday <- subset(midday, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
midday <- subset(midday, duration < pt_duration) # Grab trips that take less time than public transport
midday$bookings <- ifelse(midday$bookings == 0, 0, 1)
midday$grab_to_pt_duration <- midday$duration / midday$pt_duration

df_total <- transform(midday,
                      group = cut(midday$grab_to_pt_duration,
                                  breaks = seq(0, 1, by=0.1)))

df_total <- df_total %>%
  group_by(group) %>%
  summarise(count_total = n())

BTR_booked <- subset(midday, bookings == 1)
df_booked <- transform(BTR_booked, 
                       group = cut(BTR_booked$grab_to_pt_duration,
                                   breaks = seq(0, 1, by=0.1)))

df_booked <- df_booked %>%
  group_by(group) %>%
  summarise(count_booked=n())

# merge the two table's counts
df <- merge(df_total, df_booked, by = "group")
df$BTR <- df$count_booked / df$count_total
firstquartile_count_booked <- quantile(df$count_booked, 0.25)
df <- subset(df, count_booked >= firstquartile_count_booked) #remove all counts lower than first quartile

### PLOT FOR Mid-day ### 
# basic bar plot
p1 <- ggplot(data=df, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="aquamarine3") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("Mid-day (10am-4pm)")) +
  theme(plot.title = element_text(face="bold"), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label=df$count_total), vjust=-0.25, size=3)


# Trying to split buckets based on surge
BTR_surge_bucket1 <- subset(midday, surge <=1)
BTR_surge_bucket2 <- subset(midday, surge >1 & surge <1.5)
BTR_surge_bucket3 <- subset(midday, surge >=1.5)

### BUCKET 1
df_total_surge_bucket1 <- transform(BTR_surge_bucket1,
                                    group = cut(BTR_surge_bucket1$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_surge_bucket1 <- df_total_surge_bucket1 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_surge_bucket1 <- subset(BTR_surge_bucket1, bookings >= 1)
df_booked_surge_bucket1 <- transform(BTR_booked_surge_bucket1, 
                                     group = cut(BTR_booked_surge_bucket1$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_surge_bucket1 <- df_booked_surge_bucket1 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_surge_bucket1 <- merge(df_total_surge_bucket1, df_booked_surge_bucket1, by = "group")
df_surge_bucket1$BTR <- df_surge_bucket1$count_booked / df_surge_bucket1$count_total
firstquartile_count_total <- quantile(df_surge_bucket1$count_total, 0.25)
df_surge_bucket1 <- subset(df_surge_bucket1, count_total > firstquartile_count_total)

# basic bar plot
p2 <- ggplot(data=df_surge_bucket1, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="darksalmon") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("Surge <= 1")) +
  geom_text(aes(label=df_surge_bucket1$count_total), vjust=-0.25, size=3) +
  theme(
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 2
df_total_surge_bucket2 <- transform(BTR_surge_bucket2,
                                    group = cut(BTR_surge_bucket2$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_surge_bucket2 <- df_total_surge_bucket2 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_surge_bucket2 <- subset(BTR_surge_bucket2, bookings >= 1)
df_booked_surge_bucket2 <- transform(BTR_booked_surge_bucket2, 
                                     group = cut(BTR_booked_surge_bucket2$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_surge_bucket2 <- df_booked_surge_bucket2 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_surge_bucket2 <- merge(df_total_surge_bucket2, df_booked_surge_bucket2, by = "group")
df_surge_bucket2$BTR <- df_surge_bucket2$count_booked / df_surge_bucket2$count_total
firstquartile_count_total <- quantile(df_surge_bucket2$count_total, 0.25)
df_surge_bucket2 <- subset(df_surge_bucket2, count_total > firstquartile_count_total)
# basic bar plot
p3 <- ggplot(data=df_surge_bucket2, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="darksalmon") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("1 < Surge < 1.5")) +
  geom_text(aes(label=df_surge_bucket2$count_total), vjust=-0.25, size=3) +
  theme(
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 3
df_total_surge_bucket3 <- transform(BTR_surge_bucket3,
                                    group = cut(BTR_surge_bucket3$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_surge_bucket3 <- df_total_surge_bucket3 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_surge_bucket3 <- subset(BTR_surge_bucket3, bookings >= 1)
df_booked_surge_bucket3 <- transform(BTR_booked_surge_bucket3, 
                                     group = cut(BTR_booked_surge_bucket3$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_surge_bucket3 <- df_booked_surge_bucket3 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_surge_bucket3 <- merge(df_total_surge_bucket3, df_booked_surge_bucket3, by = "group")
df_surge_bucket3$BTR <- df_surge_bucket3$count_booked / df_surge_bucket3$count_total
firstquartile_count_total <- quantile(df_surge_bucket3$count_total, 0.25)
df_surge_bucket3 <- subset(df_surge_bucket3, count_total > firstquartile_count_total)
# basic bar plot
p4 <- ggplot(data=df_surge_bucket3, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="darksalmon") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("1.5 <= Surge")) +
  geom_text(aes(label=df_surge_bucket3$count_total), vjust=-0.25, size=3) +
  theme(
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

grid.arrange(p1, p2, p3, p4, nrow = 2)


### 4. EVENING PEAK (HOUR 17-19) ###

evening_peak <- vector()

for (i in 17:19) {
  # make strings for filenames
  BTR_data_filename <- paste("~/Downloads/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  evening_peak <- rbind(evening_peak, BTR)
}

# clean data
evening_peak <- evening_peak[complete.cases(evening_peak),] #remove NAs
evening_peak <- subset(evening_peak, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(evening_peak$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(evening_peak$duration, 0.99)
evening_peak <- subset(evening_peak, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
evening_peak <- subset(evening_peak, duration < pt_duration) # Grab trips that take less time than public transport
evening_peak$bookings <- ifelse(evening_peak$bookings == 0, 0, 1)
evening_peak$grab_to_pt_duration <- evening_peak$duration / evening_peak$pt_duration

df_total <- transform(evening_peak,
                      group = cut(evening_peak$grab_to_pt_duration,
                                  breaks = seq(0, 1, by=0.1)))

df_total <- df_total %>%
  group_by(group) %>%
  summarise(count_total = n())

BTR_booked <- subset(evening_peak, bookings == 1)
df_booked <- transform(BTR_booked, 
                       group = cut(BTR_booked$grab_to_pt_duration,
                                   breaks = seq(0, 1, by=0.1)))

df_booked <- df_booked %>%
  group_by(group) %>%
  summarise(count_booked=n())

# merge the two table's counts
df <- merge(df_total, df_booked, by = "group")
df$BTR <- df$count_booked / df$count_total
firstquartile_count_booked <- quantile(df$count_booked, 0.25)
df <- subset(df, count_booked >= firstquartile_count_booked) #remove all counts lower than first quartile

### PLOT FOR Evening Peak ### 
# basic bar plot
p1 <- ggplot(data=df, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="aquamarine3") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("Evening Peak (5pm-8pm)")) +
  theme(plot.title = element_text(face="bold"), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label=df$count_total), vjust=-0.25, size=3)


# Trying to split buckets based on surge
BTR_surge_bucket1 <- subset(evening_peak, surge <=1)
BTR_surge_bucket2 <- subset(evening_peak, surge >1 & surge <1.5)
BTR_surge_bucket3 <- subset(evening_peak, surge >=1.5)

### BUCKET 1
df_total_surge_bucket1 <- transform(BTR_surge_bucket1,
                                    group = cut(BTR_surge_bucket1$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_surge_bucket1 <- df_total_surge_bucket1 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_surge_bucket1 <- subset(BTR_surge_bucket1, bookings >= 1)
df_booked_surge_bucket1 <- transform(BTR_booked_surge_bucket1, 
                                     group = cut(BTR_booked_surge_bucket1$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_surge_bucket1 <- df_booked_surge_bucket1 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_surge_bucket1 <- merge(df_total_surge_bucket1, df_booked_surge_bucket1, by = "group")
df_surge_bucket1$BTR <- df_surge_bucket1$count_booked / df_surge_bucket1$count_total
firstquartile_count_total <- quantile(df_surge_bucket1$count_total, 0.25)
df_surge_bucket1 <- subset(df_surge_bucket1, count_total > firstquartile_count_total)

# basic bar plot
p2 <- ggplot(data=df_surge_bucket1, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="darksalmon") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("Surge <= 1")) +
  geom_text(aes(label=df_surge_bucket1$count_total), vjust=-0.25, size=3) +
  theme(
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 2
df_total_surge_bucket2 <- transform(BTR_surge_bucket2,
                                    group = cut(BTR_surge_bucket2$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_surge_bucket2 <- df_total_surge_bucket2 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_surge_bucket2 <- subset(BTR_surge_bucket2, bookings >= 1)
df_booked_surge_bucket2 <- transform(BTR_booked_surge_bucket2, 
                                     group = cut(BTR_booked_surge_bucket2$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_surge_bucket2 <- df_booked_surge_bucket2 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_surge_bucket2 <- merge(df_total_surge_bucket2, df_booked_surge_bucket2, by = "group")
df_surge_bucket2$BTR <- df_surge_bucket2$count_booked / df_surge_bucket2$count_total
firstquartile_count_total <- quantile(df_surge_bucket2$count_total, 0.25)
df_surge_bucket2 <- subset(df_surge_bucket2, count_total > firstquartile_count_total)
# basic bar plot
p3 <- ggplot(data=df_surge_bucket2, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="darksalmon") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("1 < Surge < 1.5")) +
  geom_text(aes(label=df_surge_bucket2$count_total), vjust=-0.25, size=3) +
  theme(
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 3
df_total_surge_bucket3 <- transform(BTR_surge_bucket3,
                                    group = cut(BTR_surge_bucket3$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_surge_bucket3 <- df_total_surge_bucket3 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_surge_bucket3 <- subset(BTR_surge_bucket3, bookings >= 1)
df_booked_surge_bucket3 <- transform(BTR_booked_surge_bucket3, 
                                     group = cut(BTR_booked_surge_bucket3$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_surge_bucket3 <- df_booked_surge_bucket3 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_surge_bucket3 <- merge(df_total_surge_bucket3, df_booked_surge_bucket3, by = "group")
df_surge_bucket3$BTR <- df_surge_bucket3$count_booked / df_surge_bucket3$count_total
firstquartile_count_total <- quantile(df_surge_bucket3$count_total, 0.25)
df_surge_bucket3 <- subset(df_surge_bucket3, count_total > firstquartile_count_total)
# basic bar plot
p4 <- ggplot(data=df_surge_bucket3, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="darksalmon") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("1.5 <= Surge")) +
  geom_text(aes(label=df_surge_bucket3$count_total), vjust=-0.25, size=3) +
  theme(
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

grid.arrange(p1, p2, p3, p4, nrow = 2)



### 5. NIGHT TIME (HOUR 20-23) ###

night_time <- vector()

for (i in 20:23) {
  # make strings for filenames
  BTR_data_filename <- paste("~/Downloads/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  night_time <- rbind(night_time, BTR)
}

# clean data
night_time <- night_time[complete.cases(night_time),] #remove NAs
night_time <- subset(night_time, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(night_time$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(night_time$duration, 0.99)
night_time <- subset(night_time, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
night_time <- subset(night_time, duration < pt_duration) # Grab trips that take less time than public transport
night_time$bookings <- ifelse(night_time$bookings == 0, 0, 1)
night_time$grab_to_pt_duration <- night_time$duration / night_time$pt_duration

df_total <- transform(night_time,
                      group = cut(night_time$grab_to_pt_duration,
                                  breaks = seq(0, 1, by=0.1)))

df_total <- df_total %>%
  group_by(group) %>%
  summarise(count_total = n())

BTR_booked <- subset(night_time, bookings == 1)
df_booked <- transform(BTR_booked, 
                       group = cut(BTR_booked$grab_to_pt_duration,
                                   breaks = seq(0, 1, by=0.1)))

df_booked <- df_booked %>%
  group_by(group) %>%
  summarise(count_booked=n())

# merge the two table's counts
df <- merge(df_total, df_booked, by = "group")
df$BTR <- df$count_booked / df$count_total
firstquartile_count_booked <- quantile(df$count_booked, 0.25)
df <- subset(df, count_booked >= firstquartile_count_booked) #remove all counts lower than first quartile

### PLOT FOR Night Time ### 
# basic bar plot
p1 <- ggplot(data=df, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="aquamarine3") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("Night Time (8pm-12am)")) +
  theme(plot.title = element_text(face="bold"), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label=df$count_total), vjust=-0.25, size=3)

# Trying to split buckets based on surge
BTR_surge_bucket1 <- subset(night_time, surge <=1)
BTR_surge_bucket2 <- subset(night_time, surge >1 & surge <=1.5)
BTR_surge_bucket3 <- subset(night_time, surge >1.5)

### BUCKET 1
df_total_surge_bucket1 <- transform(BTR_surge_bucket1,
                                    group = cut(BTR_surge_bucket1$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_surge_bucket1 <- df_total_surge_bucket1 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_surge_bucket1 <- subset(BTR_surge_bucket1, bookings >= 1)
df_booked_surge_bucket1 <- transform(BTR_booked_surge_bucket1, 
                                     group = cut(BTR_booked_surge_bucket1$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_surge_bucket1 <- df_booked_surge_bucket1 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_surge_bucket1 <- merge(df_total_surge_bucket1, df_booked_surge_bucket1, by = "group")
df_surge_bucket1$BTR <- df_surge_bucket1$count_booked / df_surge_bucket1$count_total
firstquartile_count_total <- quantile(df_surge_bucket1$count_total, 0.25)
df_surge_bucket1 <- subset(df_surge_bucket1, count_total > firstquartile_count_total)

# basic bar plot
p2 <- ggplot(data=df_surge_bucket1, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="darksalmon") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("Surge <= 1")) +
  geom_text(aes(label=df_surge_bucket1$count_total), vjust=-0.25, size=3) +
  theme(
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 2
df_total_surge_bucket2 <- transform(BTR_surge_bucket2,
                                    group = cut(BTR_surge_bucket2$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_surge_bucket2 <- df_total_surge_bucket2 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_surge_bucket2 <- subset(BTR_surge_bucket2, bookings >= 1)
df_booked_surge_bucket2 <- transform(BTR_booked_surge_bucket2, 
                                     group = cut(BTR_booked_surge_bucket2$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_surge_bucket2 <- df_booked_surge_bucket2 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_surge_bucket2 <- merge(df_total_surge_bucket2, df_booked_surge_bucket2, by = "group")
df_surge_bucket2$BTR <- df_surge_bucket2$count_booked / df_surge_bucket2$count_total
firstquartile_count_total <- quantile(df_surge_bucket2$count_total, 0.25)
df_surge_bucket2 <- subset(df_surge_bucket2, count_total > firstquartile_count_total)
# basic bar plot
p3 <- ggplot(data=df_surge_bucket2, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="darksalmon") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("1 < Surge < 1.5")) +
  geom_text(aes(label=df_surge_bucket2$count_total), vjust=-0.25, size=3) +
  theme(
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 3
df_total_surge_bucket3 <- transform(BTR_surge_bucket3,
                                    group = cut(BTR_surge_bucket3$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_surge_bucket3 <- df_total_surge_bucket3 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_surge_bucket3 <- subset(BTR_surge_bucket3, bookings >= 1)
df_booked_surge_bucket3 <- transform(BTR_booked_surge_bucket3, 
                                     group = cut(BTR_booked_surge_bucket3$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_surge_bucket3 <- df_booked_surge_bucket3 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_surge_bucket3 <- merge(df_total_surge_bucket3, df_booked_surge_bucket3, by = "group")
df_surge_bucket3$BTR <- df_surge_bucket3$count_booked / df_surge_bucket3$count_total
firstquartile_count_total <- quantile(df_surge_bucket3$count_total, 0.25)
df_surge_bucket3 <- subset(df_surge_bucket3, count_total > firstquartile_count_total)
# basic bar plot
p4 <- ggplot(data=df_surge_bucket3, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="darksalmon") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("1.5 <= Surge")) +
  geom_text(aes(label=df_surge_bucket3$count_total), vjust=-0.25, size=3) +
  theme(
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

grid.arrange(p1, p2, p3, p4, nrow = 2)

dev.off()





#####################################################################
############################# DISTANCE ##############################
#####################################################################

######################## BTR/ HOUR-BY-HOUR WITH DISTANCE BUCKETS ############################

pdf("BTR_Hour-by-hour_with_Distance_buckets.pdf", onefile = TRUE, width = 11, height = 9)

for (i in 0:23) {
  
  # make strings for filenames
  BTR_data_filename <- paste("~/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  # clean data
  BTR <- BTR[complete.cases(BTR),] #remove NAs
  BTR <- subset(BTR, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
  # find the 99 percentile of pt_duration and duration respectively
  ninetyninthpercentile_pt <- quantile(BTR$pt_duration, 0.99)
  ninetyninthpercentile_grab <- quantile(BTR$duration, 0.99)
  BTR <- subset(BTR, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
  BTR <- subset(BTR, duration < pt_duration) # Grab trips that take less time than public transport
  BTR$bookings <- ifelse(BTR$bookings == 0, 0, 1)
  BTR$grab_to_pt_duration <- BTR$duration / BTR$pt_duration
  
  ### SUBSET BY DISTANCE ###
  
  # Trying to split buckets based on distance
  BTR_dist_bucket1 <- subset(BTR, distance <= 5)
  BTR_dist_bucket2 <- subset(BTR, distance >5 & distance <= 10)
  BTR_dist_bucket3 <- subset(BTR, distance >10 & distance <= 20)
  BTR_dist_bucket4 <- subset(BTR, distance >20)
  
  ### BUCKET 1
  df_total_dist_bucket1 <- transform(BTR_dist_bucket1,
                                      group = cut(BTR_dist_bucket1$grab_to_pt_duration,
                                                  breaks = seq(0,1, by=0.05)))
  df_total_dist_bucket1 <- df_total_dist_bucket1 %>%
    group_by(group) %>%
    summarise(count_total = n())
  BTR_booked_dist_bucket1 <- subset(BTR_dist_bucket1, bookings >= 1)
  df_booked_dist_bucket1 <- transform(BTR_booked_dist_bucket1, 
                                       group = cut(BTR_booked_dist_bucket1$grab_to_pt_duration,
                                                   breaks = seq(0,1, by=0.05)))
  df_booked_dist_bucket1 <- df_booked_dist_bucket1 %>%
    group_by(group) %>%
    summarise(count_booked=n())
  # merge the two table's counts
  df_dist_bucket1 <- merge(df_total_dist_bucket1, df_booked_dist_bucket1, by = "group")
  df_dist_bucket1$BTR <- df_dist_bucket1$count_booked / df_dist_bucket1$count_total
  firstquartile_count_total <- quantile(df_dist_bucket1$count_total, 0.25)
  df_dist_bucket1 <- subset(df_dist_bucket1, count_total > firstquartile_count_total)
  
  # basic bar plot
  p1 <- ggplot(data=df_dist_bucket1, aes(x=group, y=BTR)) +
    geom_bar(stat="identity", fill="deepskyblue2") +
    theme_minimal() +
    labs(x="Grab to Public Transport Duration", y="BTR") +
    ggtitle(paste0("Distance <= 1")) +
    geom_text(aes(label=df_dist_bucket1$count_total), vjust=-0.25) +
    theme(axis.text.x = element_text(angle = 50, hjust = 1))
  
  ### BUCKET 2
  df_total_dist_bucket2 <- transform(BTR_dist_bucket2,
                                      group = cut(BTR_dist_bucket2$grab_to_pt_duration,
                                                  breaks = seq(0,1, by=0.05)))
  df_total_dist_bucket2 <- df_total_dist_bucket2 %>%
    group_by(group) %>%
    summarise(count_total = n())
  BTR_booked_dist_bucket2 <- subset(BTR_dist_bucket2, bookings >= 1)
  df_booked_dist_bucket2 <- transform(BTR_booked_dist_bucket2, 
                                       group = cut(BTR_booked_dist_bucket2$grab_to_pt_duration,
                                                   breaks = seq(0,1, by=0.05)))
  df_booked_dist_bucket2 <- df_booked_dist_bucket2 %>%
    group_by(group) %>%
    summarise(count_booked=n())
  # merge the two table's counts
  df_dist_bucket2 <- merge(df_total_dist_bucket2, df_booked_dist_bucket2, by = "group")
  df_dist_bucket2$BTR <- df_dist_bucket2$count_booked / df_dist_bucket2$count_total
  firstquartile_count_total <- quantile(df_dist_bucket2$count_total, 0.25)
  df_dist_bucket2 <- subset(df_dist_bucket2, count_total > firstquartile_count_total)
  # basic bar plot
  p2 <- ggplot(data=df_dist_bucket2, aes(x=group, y=BTR)) +
    geom_bar(stat="identity", fill="deepskyblue2") +
    theme_minimal() +
    labs(x="Grab to Public Transport Duration", y="BTR") +
    ggtitle(paste0("5 < Distance <= 10")) +
    geom_text(aes(label=df_dist_bucket2$count_total), vjust=-0.25) +
    theme(axis.text.x = element_text(angle = 50, hjust = 1))
  
  ### BUCKET 3
  df_total_dist_bucket3 <- transform(BTR_dist_bucket3,
                                      group = cut(BTR_dist_bucket3$grab_to_pt_duration,
                                                  breaks = seq(0,1, by=0.05)))
  df_total_dist_bucket3 <- df_total_dist_bucket3 %>%
    group_by(group) %>%
    summarise(count_total = n())
  BTR_booked_dist_bucket3 <- subset(BTR_dist_bucket3, bookings >= 1)
  df_booked_dist_bucket3 <- transform(BTR_booked_dist_bucket3, 
                                       group = cut(BTR_booked_dist_bucket3$grab_to_pt_duration,
                                                   breaks = seq(0,1, by=0.05)))
  df_booked_dist_bucket3 <- df_booked_dist_bucket3 %>%
    group_by(group) %>%
    summarise(count_booked=n())
  # merge the two table's counts
  df_dist_bucket3 <- merge(df_total_dist_bucket3, df_booked_dist_bucket3, by = "group")
  df_dist_bucket3$BTR <- df_dist_bucket3$count_booked / df_dist_bucket3$count_total
  firstquartile_count_total <- quantile(df_dist_bucket3$count_total, 0.25)
  df_dist_bucket3 <- subset(df_dist_bucket3, count_total > firstquartile_count_total)
  # basic bar plot
  p3 <- ggplot(data=df_dist_bucket3, aes(x=group, y=BTR)) +
    geom_bar(stat="identity", fill="deepskyblue2") +
    theme_minimal() +
    labs(x="Grab to Public Transport Duration", y="BTR") +
    ggtitle(paste0("10 < Distance <= 20")) +
    geom_text(aes(label=df_dist_bucket3$count_total), vjust=-0.25) +
    theme(axis.text.x = element_text(angle = 50, hjust = 1))
  
  ### BUCKET 4
  df_total_dist_bucket4 <- transform(BTR_dist_bucket4,
                                     group = cut(BTR_dist_bucket4$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
  df_total_dist_bucket4 <- df_total_dist_bucket4 %>%
    group_by(group) %>%
    summarise(count_total = n())
  BTR_booked_dist_bucket4 <- subset(BTR_dist_bucket4, bookings >= 1)
  df_booked_dist_bucket4 <- transform(BTR_booked_dist_bucket4, 
                                      group = cut(BTR_booked_dist_bucket4$grab_to_pt_duration,
                                                  breaks = seq(0,1, by=0.05)))
  df_booked_dist_bucket4 <- df_booked_dist_bucket4 %>%
    group_by(group) %>%
    summarise(count_booked=n())
  # merge the two table's counts
  df_dist_bucket4 <- merge(df_total_dist_bucket4, df_booked_dist_bucket4, by = "group")
  df_dist_bucket4$BTR <- df_dist_bucket4$count_booked / df_dist_bucket4$count_total
  firstquartile_count_total <- quantile(df_dist_bucket4$count_total, 0.25)
  df_dist_bucket4 <- subset(df_dist_bucket4, count_total > firstquartile_count_total)
  # basic bar plot
  p4 <- ggplot(data=df_dist_bucket4, aes(x=group, y=BTR)) +
    geom_bar(stat="identity", fill="deepskyblue2") +
    theme_minimal() +
    labs(x="Grab to Public Transport Duration", y="BTR") +
    ggtitle(paste0("20 < Distance")) +
    geom_text(aes(label=df_dist_bucket4$count_total), vjust=-0.25) +
    theme(axis.text.x = element_text(angle = 50, hjust = 1))
  
  maintitle <- paste0("Hour ", i)
  maintitle = textGrob(maintitle, gp=gpar(fontsize=15, fontface="bold"))
  grid.arrange(p1, p2, p3, p4, nrow = 2, top = maintitle)
  
}

dev.off()







############################# BTR/ TIME GROUPS WITH DISTANCE BUCKETS #################################

pdf("BTR_Time_Groups_with_Distance_buckets.pdf", onefile = TRUE, width = 11, height = 9)

### 1. NOCTURNAL (HOUR 0-5) ###

nocturnal <- vector()

for (i in 0:5) {
  # make strings for filenames
  BTR_data_filename <- paste("~/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  nocturnal <- rbind(nocturnal, BTR)
}

# clean data
nocturnal <- nocturnal[complete.cases(nocturnal),] #remove NAs
nocturnal <- subset(nocturnal, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(nocturnal$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(nocturnal$duration, 0.99)
nocturnal <- subset(nocturnal, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
nocturnal <- subset(nocturnal, duration < pt_duration) # Grab trips that take less time than public transport
nocturnal$bookings <- ifelse(nocturnal$bookings == 0, 0, 1)
nocturnal$grab_to_pt_duration <- nocturnal$duration / nocturnal$pt_duration

# Trying to split buckets based on surge
BTR_dist_bucket1 <- subset(nocturnal, distance <=5)
BTR_dist_bucket2 <- subset(nocturnal, distance >5 & distance <=10)
BTR_dist_bucket3 <- subset(nocturnal, distance >10 & distance <=20)
BTR_dist_bucket4 <- subset(nocturnal, distance >20)

### BUCKET 1
df_total_dist_bucket1 <- transform(BTR_dist_bucket1,
                                    group = cut(BTR_dist_bucket1$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_dist_bucket1 <- df_total_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket1 <- subset(BTR_dist_bucket1, bookings >= 1)
df_booked_dist_bucket1 <- transform(BTR_booked_dist_bucket1, 
                                     group = cut(BTR_booked_dist_bucket1$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket1 <- df_booked_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket1 <- merge(df_total_dist_bucket1, df_booked_dist_bucket1, by = "group")
df_dist_bucket1$BTR <- df_dist_bucket1$count_booked / df_dist_bucket1$count_total
firstquartile_count_total <- quantile(df_dist_bucket1$count_total, 0.25)
df_dist_bucket1 <- subset(df_dist_bucket1, count_total > firstquartile_count_total)

# basic bar plot
p1 <- ggplot(data=df_dist_bucket1, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle("Distance <= 5") +
  geom_text(aes(label=df_dist_bucket1$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

### BUCKET 2
df_total_dist_bucket2 <- transform(BTR_dist_bucket2,
                                    group = cut(BTR_dist_bucket2$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_dist_bucket2 <- df_total_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket2 <- subset(BTR_dist_bucket2, bookings >= 1)
df_booked_dist_bucket2 <- transform(BTR_booked_dist_bucket2, 
                                     group = cut(BTR_booked_dist_bucket2$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket2 <- df_booked_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket2 <- merge(df_total_dist_bucket2, df_booked_dist_bucket2, by = "group")
df_dist_bucket2$BTR <- df_dist_bucket2$count_booked / df_dist_bucket2$count_total
firstquartile_count_total <- quantile(df_dist_bucket2$count_total, 0.25)
df_dist_bucket2 <- subset(df_dist_bucket2, count_total > firstquartile_count_total)
# basic bar plot
p2 <- ggplot(data=df_dist_bucket2, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle("5 < Distance <= 10") +
  geom_text(aes(label=df_dist_bucket2$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

### BUCKET 3
df_total_dist_bucket3 <- transform(BTR_dist_bucket3,
                                    group = cut(BTR_dist_bucket3$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_total_dist_bucket3 <- df_total_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket3 <- subset(BTR_dist_bucket3, bookings >= 1)
df_booked_dist_bucket3 <- transform(BTR_booked_dist_bucket3, 
                                     group = cut(BTR_booked_dist_bucket3$grab_to_pt_duration,
                                                 breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket3 <- df_booked_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket3 <- merge(df_total_dist_bucket3, df_booked_dist_bucket3, by = "group")
df_dist_bucket3$BTR <- df_dist_bucket3$count_booked / df_dist_bucket3$count_total
firstquartile_count_total <- quantile(df_dist_bucket3$count_total, 0.25)
df_dist_bucket3 <- subset(df_dist_bucket3, count_total > firstquartile_count_total)
# basic bar plot
p3 <- ggplot(data=df_dist_bucket3, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("10 < Distance <= 20")) +
  geom_text(aes(label=df_dist_bucket3$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))


### BUCKET 4
df_total_dist_bucket4 <- transform(BTR_dist_bucket4,
                                   group = cut(BTR_dist_bucket4$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket4 <- df_total_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket4 <- subset(BTR_dist_bucket4, bookings >= 1)
df_booked_dist_bucket4 <- transform(BTR_booked_dist_bucket4, 
                                    group = cut(BTR_booked_dist_bucket4$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket4 <- df_booked_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket4 <- merge(df_total_dist_bucket4, df_booked_dist_bucket4, by = "group")
df_dist_bucket4$BTR <- df_dist_bucket4$count_booked / df_dist_bucket4$count_total
firstquartile_count_total <- quantile(df_dist_bucket4$count_total, 0.25)
df_dist_bucket4 <- subset(df_dist_bucket4, count_total > firstquartile_count_total)
# basic bar plot
p4 <- ggplot(data=df_dist_bucket4, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("20 < Distance")) +
  geom_text(aes(label=df_dist_bucket4$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

maintitle = textGrob("Nocturnal (12am-6am)", gp=gpar(fontsize=15, fontface="bold"))
grid.arrange(p1, p2, p3, p4, nrow = 2, top = maintitle)


### 2. MORNING PEAK (HOUR 6-9) ###

morning_peak <- vector()

for (i in 6:9) {
  # make strings for filenames
  BTR_data_filename <- paste("~/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  morning_peak <- rbind(morning_peak, BTR)
}

# clean data
morning_peak <- morning_peak[complete.cases(morning_peak),] #remove NAs
morning_peak <- subset(morning_peak, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(morning_peak$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(morning_peak$duration, 0.99)
morning_peak <- subset(morning_peak, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
morning_peak <- subset(morning_peak, duration < pt_duration) # Grab trips that take less time than public transport
morning_peak$bookings <- ifelse(morning_peak$bookings == 0, 0, 1)
morning_peak$grab_to_pt_duration <- morning_peak$duration / morning_peak$pt_duration

# Trying to split buckets based on surge
BTR_dist_bucket1 <- subset(morning_peak, distance <=5)
BTR_dist_bucket2 <- subset(morning_peak, distance >5 & distance <=10)
BTR_dist_bucket3 <- subset(morning_peak, distance >10 & distance <=20)
BTR_dist_bucket4 <- subset(morning_peak, distance >20)

### BUCKET 1
df_total_dist_bucket1 <- transform(BTR_dist_bucket1,
                                   group = cut(BTR_dist_bucket1$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket1 <- df_total_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket1 <- subset(BTR_dist_bucket1, bookings >= 1)
df_booked_dist_bucket1 <- transform(BTR_booked_dist_bucket1, 
                                    group = cut(BTR_booked_dist_bucket1$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket1 <- df_booked_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket1 <- merge(df_total_dist_bucket1, df_booked_dist_bucket1, by = "group")
df_dist_bucket1$BTR <- df_dist_bucket1$count_booked / df_dist_bucket1$count_total
firstquartile_count_total <- quantile(df_dist_bucket1$count_total, 0.25)
df_dist_bucket1 <- subset(df_dist_bucket1, count_total > firstquartile_count_total)

# basic bar plot
p1 <- ggplot(data=df_dist_bucket1, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("Distance <= 5")) +
  geom_text(aes(label=df_dist_bucket1$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

### BUCKET 2
df_total_dist_bucket2 <- transform(BTR_dist_bucket2,
                                   group = cut(BTR_dist_bucket2$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket2 <- df_total_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket2 <- subset(BTR_dist_bucket2, bookings >= 1)
df_booked_dist_bucket2 <- transform(BTR_booked_dist_bucket2, 
                                    group = cut(BTR_booked_dist_bucket2$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket2 <- df_booked_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket2 <- merge(df_total_dist_bucket2, df_booked_dist_bucket2, by = "group")
df_dist_bucket2$BTR <- df_dist_bucket2$count_booked / df_dist_bucket2$count_total
firstquartile_count_total <- quantile(df_dist_bucket2$count_total, 0.25)
df_dist_bucket2 <- subset(df_dist_bucket2, count_total > firstquartile_count_total)
# basic bar plot
p2 <- ggplot(data=df_dist_bucket2, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("5 < Distance <= 10")) +
  geom_text(aes(label=df_dist_bucket2$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

### BUCKET 3
df_total_dist_bucket3 <- transform(BTR_dist_bucket3,
                                   group = cut(BTR_dist_bucket3$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket3 <- df_total_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket3 <- subset(BTR_dist_bucket3, bookings >= 1)
df_booked_dist_bucket3 <- transform(BTR_booked_dist_bucket3, 
                                    group = cut(BTR_booked_dist_bucket3$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket3 <- df_booked_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket3 <- merge(df_total_dist_bucket3, df_booked_dist_bucket3, by = "group")
df_dist_bucket3$BTR <- df_dist_bucket3$count_booked / df_dist_bucket3$count_total
firstquartile_count_total <- quantile(df_dist_bucket3$count_total, 0.25)
df_dist_bucket3 <- subset(df_dist_bucket3, count_total > firstquartile_count_total)
# basic bar plot
p3 <- ggplot(data=df_dist_bucket3, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("10 < Distance <= 20")) +
  geom_text(aes(label=df_dist_bucket3$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))


### BUCKET 4
df_total_dist_bucket4 <- transform(BTR_dist_bucket4,
                                   group = cut(BTR_dist_bucket4$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket4 <- df_total_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket4 <- subset(BTR_dist_bucket4, bookings >= 1)
df_booked_dist_bucket4 <- transform(BTR_booked_dist_bucket4, 
                                    group = cut(BTR_booked_dist_bucket4$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket4 <- df_booked_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket4 <- merge(df_total_dist_bucket4, df_booked_dist_bucket4, by = "group")
df_dist_bucket4$BTR <- df_dist_bucket4$count_booked / df_dist_bucket4$count_total
firstquartile_count_total <- quantile(df_dist_bucket4$count_total, 0.25)
df_dist_bucket4 <- subset(df_dist_bucket4, count_total > firstquartile_count_total)
# basic bar plot
p4 <- ggplot(data=df_dist_bucket4, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("20 < Distance")) +
  geom_text(aes(label=df_dist_bucket4$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

maintitle = textGrob("Morning Peak (6am-10am)", gp=gpar(fontsize=15, fontface="bold"))
grid.arrange(p1, p2, p3, p4, nrow = 2, top = maintitle)


### 3. MID-DAY (HOUR 10-16) ###

midday <- vector()

for (i in 10:16) {
  # make strings for filenames
  BTR_data_filename <- paste("~/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  midday <- rbind(midday, BTR)
}

# clean data
midday <- midday[complete.cases(midday),] #remove NAs
midday <- subset(midday, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(midday$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(midday$duration, 0.99)
midday <- subset(midday, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
midday <- subset(midday, duration < pt_duration) # Grab trips that take less time than public transport
midday$bookings <- ifelse(midday$bookings == 0, 0, 1)
midday$grab_to_pt_duration <- midday$duration / midday$pt_duration

# Trying to split buckets based on surge
BTR_dist_bucket1 <- subset(midday, distance <=5)
BTR_dist_bucket2 <- subset(midday, distance >5 & distance <=10)
BTR_dist_bucket3 <- subset(midday, distance >10 & distance <=20)
BTR_dist_bucket4 <- subset(midday, distance >20)

### BUCKET 1
df_total_dist_bucket1 <- transform(BTR_dist_bucket1,
                                   group = cut(BTR_dist_bucket1$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket1 <- df_total_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket1 <- subset(BTR_dist_bucket1, bookings >= 1)
df_booked_dist_bucket1 <- transform(BTR_booked_dist_bucket1, 
                                    group = cut(BTR_booked_dist_bucket1$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket1 <- df_booked_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket1 <- merge(df_total_dist_bucket1, df_booked_dist_bucket1, by = "group")
df_dist_bucket1$BTR <- df_dist_bucket1$count_booked / df_dist_bucket1$count_total
firstquartile_count_total <- quantile(df_dist_bucket1$count_total, 0.25)
df_dist_bucket1 <- subset(df_dist_bucket1, count_total > firstquartile_count_total)

# basic bar plot
p1 <- ggplot(data=df_dist_bucket1, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("Distance <= 5")) +
  geom_text(aes(label=df_dist_bucket1$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

### BUCKET 2
df_total_dist_bucket2 <- transform(BTR_dist_bucket2,
                                   group = cut(BTR_dist_bucket2$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket2 <- df_total_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket2 <- subset(BTR_dist_bucket2, bookings >= 1)
df_booked_dist_bucket2 <- transform(BTR_booked_dist_bucket2, 
                                    group = cut(BTR_booked_dist_bucket2$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket2 <- df_booked_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket2 <- merge(df_total_dist_bucket2, df_booked_dist_bucket2, by = "group")
df_dist_bucket2$BTR <- df_dist_bucket2$count_booked / df_dist_bucket2$count_total
firstquartile_count_total <- quantile(df_dist_bucket2$count_total, 0.25)
df_dist_bucket2 <- subset(df_dist_bucket2, count_total > firstquartile_count_total)
# basic bar plot
p2 <- ggplot(data=df_dist_bucket2, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("5 < Distance <= 10")) +
  geom_text(aes(label=df_dist_bucket2$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

### BUCKET 3
df_total_dist_bucket3 <- transform(BTR_dist_bucket3,
                                   group = cut(BTR_dist_bucket3$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket3 <- df_total_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket3 <- subset(BTR_dist_bucket3, bookings >= 1)
df_booked_dist_bucket3 <- transform(BTR_booked_dist_bucket3, 
                                    group = cut(BTR_booked_dist_bucket3$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket3 <- df_booked_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket3 <- merge(df_total_dist_bucket3, df_booked_dist_bucket3, by = "group")
df_dist_bucket3$BTR <- df_dist_bucket3$count_booked / df_dist_bucket3$count_total
firstquartile_count_total <- quantile(df_dist_bucket3$count_total, 0.25)
df_dist_bucket3 <- subset(df_dist_bucket3, count_total > firstquartile_count_total)
# basic bar plot
p3 <- ggplot(data=df_dist_bucket3, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("10 < Distance <= 20")) +
  geom_text(aes(label=df_dist_bucket3$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))


### BUCKET 4
df_total_dist_bucket4 <- transform(BTR_dist_bucket4,
                                   group = cut(BTR_dist_bucket4$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket4 <- df_total_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket4 <- subset(BTR_dist_bucket4, bookings >= 1)
df_booked_dist_bucket4 <- transform(BTR_booked_dist_bucket4, 
                                    group = cut(BTR_booked_dist_bucket4$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket4 <- df_booked_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket4 <- merge(df_total_dist_bucket4, df_booked_dist_bucket4, by = "group")
df_dist_bucket4$BTR <- df_dist_bucket4$count_booked / df_dist_bucket4$count_total
firstquartile_count_total <- quantile(df_dist_bucket4$count_total, 0.25)
df_dist_bucket4 <- subset(df_dist_bucket4, count_total > firstquartile_count_total)
# basic bar plot
p4 <- ggplot(data=df_dist_bucket4, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("20 < Distance")) +
  geom_text(aes(label=df_dist_bucket4$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

maintitle = textGrob("Mid-day (10am-4pm)", gp=gpar(fontsize=15, fontface="bold"))
grid.arrange(p1, p2, p3, p4, nrow = 2, top = maintitle)


### 4. EVENING PEAK (HOUR 17-19) ###

evening_peak <- vector()

for (i in 17:19) {
  # make strings for filenames
  BTR_data_filename <- paste("~/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  evening_peak <- rbind(evening_peak, BTR)
}

# clean data
evening_peak <- evening_peak[complete.cases(evening_peak),] #remove NAs
evening_peak <- subset(evening_peak, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(evening_peak$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(evening_peak$duration, 0.99)
evening_peak <- subset(evening_peak, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
evening_peak <- subset(evening_peak, duration < pt_duration) # Grab trips that take less time than public transport
evening_peak$bookings <- ifelse(evening_peak$bookings == 0, 0, 1)
evening_peak$grab_to_pt_duration <- evening_peak$duration / evening_peak$pt_duration

# Trying to split buckets based on surge
BTR_dist_bucket1 <- subset(evening_peak, distance <=5)
BTR_dist_bucket2 <- subset(evening_peak, distance >5 & distance <=10)
BTR_dist_bucket3 <- subset(evening_peak, distance >10 & distance <=20)
BTR_dist_bucket4 <- subset(evening_peak, distance >20)

### BUCKET 1
df_total_dist_bucket1 <- transform(BTR_dist_bucket1,
                                   group = cut(BTR_dist_bucket1$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket1 <- df_total_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket1 <- subset(BTR_dist_bucket1, bookings >= 1)
df_booked_dist_bucket1 <- transform(BTR_booked_dist_bucket1, 
                                    group = cut(BTR_booked_dist_bucket1$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket1 <- df_booked_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket1 <- merge(df_total_dist_bucket1, df_booked_dist_bucket1, by = "group")
df_dist_bucket1$BTR <- df_dist_bucket1$count_booked / df_dist_bucket1$count_total
firstquartile_count_total <- quantile(df_dist_bucket1$count_total, 0.25)
df_dist_bucket1 <- subset(df_dist_bucket1, count_total > firstquartile_count_total)

# basic bar plot
p1 <- ggplot(data=df_dist_bucket1, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("Distance <= 5")) +
  geom_text(aes(label=df_dist_bucket1$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

### BUCKET 2
df_total_dist_bucket2 <- transform(BTR_dist_bucket2,
                                   group = cut(BTR_dist_bucket2$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket2 <- df_total_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket2 <- subset(BTR_dist_bucket2, bookings >= 1)
df_booked_dist_bucket2 <- transform(BTR_booked_dist_bucket2, 
                                    group = cut(BTR_booked_dist_bucket2$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket2 <- df_booked_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket2 <- merge(df_total_dist_bucket2, df_booked_dist_bucket2, by = "group")
df_dist_bucket2$BTR <- df_dist_bucket2$count_booked / df_dist_bucket2$count_total
firstquartile_count_total <- quantile(df_dist_bucket2$count_total, 0.25)
df_dist_bucket2 <- subset(df_dist_bucket2, count_total > firstquartile_count_total)
# basic bar plot
p2 <- ggplot(data=df_dist_bucket2, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("5 < Distance <= 10")) +
  geom_text(aes(label=df_dist_bucket2$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

### BUCKET 3
df_total_dist_bucket3 <- transform(BTR_dist_bucket3,
                                   group = cut(BTR_dist_bucket3$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket3 <- df_total_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket3 <- subset(BTR_dist_bucket3, bookings >= 1)
df_booked_dist_bucket3 <- transform(BTR_booked_dist_bucket3, 
                                    group = cut(BTR_booked_dist_bucket3$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket3 <- df_booked_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket3 <- merge(df_total_dist_bucket3, df_booked_dist_bucket3, by = "group")
df_dist_bucket3$BTR <- df_dist_bucket3$count_booked / df_dist_bucket3$count_total
firstquartile_count_total <- quantile(df_dist_bucket3$count_total, 0.25)
df_dist_bucket3 <- subset(df_dist_bucket3, count_total > firstquartile_count_total)
# basic bar plot
p3 <- ggplot(data=df_dist_bucket3, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("10 < Distance <= 20")) +
  geom_text(aes(label=df_dist_bucket3$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))


### BUCKET 4
df_total_dist_bucket4 <- transform(BTR_dist_bucket4,
                                   group = cut(BTR_dist_bucket4$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket4 <- df_total_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket4 <- subset(BTR_dist_bucket4, bookings >= 1)
df_booked_dist_bucket4 <- transform(BTR_booked_dist_bucket4, 
                                    group = cut(BTR_booked_dist_bucket4$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket4 <- df_booked_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket4 <- merge(df_total_dist_bucket4, df_booked_dist_bucket4, by = "group")
df_dist_bucket4$BTR <- df_dist_bucket4$count_booked / df_dist_bucket4$count_total
firstquartile_count_total <- quantile(df_dist_bucket4$count_total, 0.25)
df_dist_bucket4 <- subset(df_dist_bucket4, count_total > firstquartile_count_total)
# basic bar plot
p4 <- ggplot(data=df_dist_bucket4, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("20 < Distance")) +
  geom_text(aes(label=df_dist_bucket4$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

maintitle = textGrob("Evening Peak (5pm-8pm)", gp=gpar(fontsize=15, fontface="bold"))
grid.arrange(p1, p2, p3, p4, nrow = 2, top = maintitle)


### 5. NIGHT TIME (HOUR 20-23) ###

night_time <- vector()

for (i in 20:23) {
  # make strings for filenames
  BTR_data_filename <- paste("~/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  night_time <- rbind(night_time, BTR)
}

# clean data
night_time <- night_time[complete.cases(night_time),] #remove NAs
night_time <- subset(night_time, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(night_time$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(night_time$duration, 0.99)
night_time <- subset(night_time, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
night_time <- subset(night_time, duration < pt_duration) # Grab trips that take less time than public transport
night_time$bookings <- ifelse(night_time$bookings == 0, 0, 1)
night_time$grab_to_pt_duration <- night_time$duration / night_time$pt_duration

# Trying to split buckets based on surge
BTR_dist_bucket1 <- subset(night_time, distance <=5)
BTR_dist_bucket2 <- subset(night_time, distance >5 & distance <=10)
BTR_dist_bucket3 <- subset(night_time, distance >10 & distance <=20)
BTR_dist_bucket4 <- subset(night_time, distance >20)

### BUCKET 1
df_total_dist_bucket1 <- transform(BTR_dist_bucket1,
                                   group = cut(BTR_dist_bucket1$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket1 <- df_total_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket1 <- subset(BTR_dist_bucket1, bookings >= 1)
df_booked_dist_bucket1 <- transform(BTR_booked_dist_bucket1, 
                                    group = cut(BTR_booked_dist_bucket1$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket1 <- df_booked_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket1 <- merge(df_total_dist_bucket1, df_booked_dist_bucket1, by = "group")
df_dist_bucket1$BTR <- df_dist_bucket1$count_booked / df_dist_bucket1$count_total
firstquartile_count_total <- quantile(df_dist_bucket1$count_total, 0.25)
df_dist_bucket1 <- subset(df_dist_bucket1, count_total > firstquartile_count_total)

# basic bar plot
p1 <- ggplot(data=df_dist_bucket1, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("Distance <= 5")) +
  geom_text(aes(label=df_dist_bucket1$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

### BUCKET 2
df_total_dist_bucket2 <- transform(BTR_dist_bucket2,
                                   group = cut(BTR_dist_bucket2$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket2 <- df_total_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket2 <- subset(BTR_dist_bucket2, bookings >= 1)
df_booked_dist_bucket2 <- transform(BTR_booked_dist_bucket2, 
                                    group = cut(BTR_booked_dist_bucket2$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket2 <- df_booked_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket2 <- merge(df_total_dist_bucket2, df_booked_dist_bucket2, by = "group")
df_dist_bucket2$BTR <- df_dist_bucket2$count_booked / df_dist_bucket2$count_total
firstquartile_count_total <- quantile(df_dist_bucket2$count_total, 0.25)
df_dist_bucket2 <- subset(df_dist_bucket2, count_total > firstquartile_count_total)
# basic bar plot
p2 <- ggplot(data=df_dist_bucket2, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("5 < Distance <= 10")) +
  geom_text(aes(label=df_dist_bucket2$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

### BUCKET 3
df_total_dist_bucket3 <- transform(BTR_dist_bucket3,
                                   group = cut(BTR_dist_bucket3$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket3 <- df_total_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket3 <- subset(BTR_dist_bucket3, bookings >= 1)
df_booked_dist_bucket3 <- transform(BTR_booked_dist_bucket3, 
                                    group = cut(BTR_booked_dist_bucket3$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket3 <- df_booked_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket3 <- merge(df_total_dist_bucket3, df_booked_dist_bucket3, by = "group")
df_dist_bucket3$BTR <- df_dist_bucket3$count_booked / df_dist_bucket3$count_total
firstquartile_count_total <- quantile(df_dist_bucket3$count_total, 0.25)
df_dist_bucket3 <- subset(df_dist_bucket3, count_total > firstquartile_count_total)
# basic bar plot
p3 <- ggplot(data=df_dist_bucket3, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("10 < Distance <= 20")) +
  geom_text(aes(label=df_dist_bucket3$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))


### BUCKET 4
df_total_dist_bucket4 <- transform(BTR_dist_bucket4,
                                   group = cut(BTR_dist_bucket4$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket4 <- df_total_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket4 <- subset(BTR_dist_bucket4, bookings >= 1)
df_booked_dist_bucket4 <- transform(BTR_booked_dist_bucket4, 
                                    group = cut(BTR_booked_dist_bucket4$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket4 <- df_booked_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket4 <- merge(df_total_dist_bucket4, df_booked_dist_bucket4, by = "group")
df_dist_bucket4$BTR <- df_dist_bucket4$count_booked / df_dist_bucket4$count_total
firstquartile_count_total <- quantile(df_dist_bucket4$count_total, 0.25)
df_dist_bucket4 <- subset(df_dist_bucket4, count_total > firstquartile_count_total)
# basic bar plot
p4 <- ggplot(data=df_dist_bucket4, aes(x=group, y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Grab to Public Transport Duration", y="BTR") +
  ggtitle(paste0("20 < Distance")) +
  geom_text(aes(label=df_dist_bucket4$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

maintitle = textGrob("Night Time (8pm-12am)", gp=gpar(fontsize=15, fontface="bold"))
grid.arrange(p1, p2, p3, p4, nrow = 2, top = maintitle)


dev.off()










############################# BTR/ TIME SAVED (NO X AXIS) TIME GROUPS WITH DISTANCE BUCKETS #################################

pdf("BTR_Time_Saved_Time_Groups_with_Distance_buckets.pdf", onefile = TRUE, width = 11, height = 9)

### 1. NOCTURNAL (HOUR 0-5) ###

nocturnal <- vector()

for (i in 0:5) {
  # make strings for filenames
  BTR_data_filename <- paste("~/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  nocturnal <- rbind(nocturnal, BTR)
}

# clean data
nocturnal <- nocturnal[complete.cases(nocturnal),] #remove NAs
nocturnal <- subset(nocturnal, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(nocturnal$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(nocturnal$duration, 0.99)
nocturnal <- subset(nocturnal, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
nocturnal <- subset(nocturnal, duration < pt_duration) # Grab trips that take less time than public transport
nocturnal$bookings <- ifelse(nocturnal$bookings == 0, 0, 1)
nocturnal$grab_to_pt_duration <- nocturnal$duration / nocturnal$pt_duration

# Trying to split buckets based on surge
BTR_dist_bucket1 <- subset(nocturnal, distance <=5)
BTR_dist_bucket2 <- subset(nocturnal, distance >5 & distance <=10)
BTR_dist_bucket3 <- subset(nocturnal, distance >10 & distance <=20)
BTR_dist_bucket4 <- subset(nocturnal, distance >20)

### BUCKET 1
df_total_dist_bucket1 <- transform(BTR_dist_bucket1,
                                   group = cut(BTR_dist_bucket1$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket1 <- df_total_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket1 <- subset(BTR_dist_bucket1, bookings >= 1)
df_booked_dist_bucket1 <- transform(BTR_booked_dist_bucket1, 
                                    group = cut(BTR_booked_dist_bucket1$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket1 <- df_booked_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket1 <- merge(df_total_dist_bucket1, df_booked_dist_bucket1, by = "group")
df_dist_bucket1$BTR <- df_dist_bucket1$count_booked / df_dist_bucket1$count_total
firstquartile_count_total <- quantile(df_dist_bucket1$count_total, 0.25)
df_dist_bucket1 <- subset(df_dist_bucket1, count_total > firstquartile_count_total)

# basic bar plot
p1 <- ggplot(data=df_dist_bucket1, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle("Distance <= 5") +
  geom_text(aes(label=df_dist_bucket1$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 2
df_total_dist_bucket2 <- transform(BTR_dist_bucket2,
                                   group = cut(BTR_dist_bucket2$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket2 <- df_total_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket2 <- subset(BTR_dist_bucket2, bookings >= 1)
df_booked_dist_bucket2 <- transform(BTR_booked_dist_bucket2, 
                                    group = cut(BTR_booked_dist_bucket2$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket2 <- df_booked_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket2 <- merge(df_total_dist_bucket2, df_booked_dist_bucket2, by = "group")
df_dist_bucket2$BTR <- df_dist_bucket2$count_booked / df_dist_bucket2$count_total
firstquartile_count_total <- quantile(df_dist_bucket2$count_total, 0.25)
df_dist_bucket2 <- subset(df_dist_bucket2, count_total > firstquartile_count_total)
# basic bar plot
p2 <- ggplot(data=df_dist_bucket2, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle("5 < Distance <= 10") +
  geom_text(aes(label=df_dist_bucket2$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 3
df_total_dist_bucket3 <- transform(BTR_dist_bucket3,
                                   group = cut(BTR_dist_bucket3$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket3 <- df_total_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket3 <- subset(BTR_dist_bucket3, bookings >= 1)
df_booked_dist_bucket3 <- transform(BTR_booked_dist_bucket3, 
                                    group = cut(BTR_booked_dist_bucket3$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket3 <- df_booked_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket3 <- merge(df_total_dist_bucket3, df_booked_dist_bucket3, by = "group")
df_dist_bucket3$BTR <- df_dist_bucket3$count_booked / df_dist_bucket3$count_total
firstquartile_count_total <- quantile(df_dist_bucket3$count_total, 0.25)
df_dist_bucket3 <- subset(df_dist_bucket3, count_total > firstquartile_count_total)
# basic bar plot
p3 <- ggplot(data=df_dist_bucket3, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("10 < Distance <= 20")) +
  geom_text(aes(label=df_dist_bucket3$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


### BUCKET 4
df_total_dist_bucket4 <- transform(BTR_dist_bucket4,
                                   group = cut(BTR_dist_bucket4$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket4 <- df_total_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket4 <- subset(BTR_dist_bucket4, bookings >= 1)
df_booked_dist_bucket4 <- transform(BTR_booked_dist_bucket4, 
                                    group = cut(BTR_booked_dist_bucket4$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket4 <- df_booked_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket4 <- merge(df_total_dist_bucket4, df_booked_dist_bucket4, by = "group")
df_dist_bucket4$BTR <- df_dist_bucket4$count_booked / df_dist_bucket4$count_total
firstquartile_count_total <- quantile(df_dist_bucket4$count_total, 0.25)
df_dist_bucket4 <- subset(df_dist_bucket4, count_total > firstquartile_count_total)
# basic bar plot
p4 <- ggplot(data=df_dist_bucket4, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("20 < Distance")) +
  geom_text(aes(label=df_dist_bucket4$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

maintitle = textGrob("Nocturnal (12am-6am)", gp=gpar(fontsize=15, fontface="bold"))
grid.arrange(p1, p2, p3, p4, nrow = 2, top = maintitle)


### 2. MORNING PEAK (HOUR 6-9) ###

morning_peak <- vector()

for (i in 6:9) {
  # make strings for filenames
  BTR_data_filename <- paste("~/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  morning_peak <- rbind(morning_peak, BTR)
}

# clean data
morning_peak <- morning_peak[complete.cases(morning_peak),] #remove NAs
morning_peak <- subset(morning_peak, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(morning_peak$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(morning_peak$duration, 0.99)
morning_peak <- subset(morning_peak, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
morning_peak <- subset(morning_peak, duration < pt_duration) # Grab trips that take less time than public transport
morning_peak$bookings <- ifelse(morning_peak$bookings == 0, 0, 1)
morning_peak$grab_to_pt_duration <- morning_peak$duration / morning_peak$pt_duration

# Trying to split buckets based on surge
BTR_dist_bucket1 <- subset(morning_peak, distance <=5)
BTR_dist_bucket2 <- subset(morning_peak, distance >5 & distance <=10)
BTR_dist_bucket3 <- subset(morning_peak, distance >10 & distance <=20)
BTR_dist_bucket4 <- subset(morning_peak, distance >20)

### BUCKET 1
df_total_dist_bucket1 <- transform(BTR_dist_bucket1,
                                   group = cut(BTR_dist_bucket1$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket1 <- df_total_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket1 <- subset(BTR_dist_bucket1, bookings >= 1)
df_booked_dist_bucket1 <- transform(BTR_booked_dist_bucket1, 
                                    group = cut(BTR_booked_dist_bucket1$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket1 <- df_booked_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket1 <- merge(df_total_dist_bucket1, df_booked_dist_bucket1, by = "group")
df_dist_bucket1$BTR <- df_dist_bucket1$count_booked / df_dist_bucket1$count_total
firstquartile_count_total <- quantile(df_dist_bucket1$count_total, 0.25)
df_dist_bucket1 <- subset(df_dist_bucket1, count_total > firstquartile_count_total)

# basic bar plot
p1 <- ggplot(data=df_dist_bucket1, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("Distance <= 5")) +
  geom_text(aes(label=df_dist_bucket1$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 2
df_total_dist_bucket2 <- transform(BTR_dist_bucket2,
                                   group = cut(BTR_dist_bucket2$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket2 <- df_total_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket2 <- subset(BTR_dist_bucket2, bookings >= 1)
df_booked_dist_bucket2 <- transform(BTR_booked_dist_bucket2, 
                                    group = cut(BTR_booked_dist_bucket2$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket2 <- df_booked_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket2 <- merge(df_total_dist_bucket2, df_booked_dist_bucket2, by = "group")
df_dist_bucket2$BTR <- df_dist_bucket2$count_booked / df_dist_bucket2$count_total
firstquartile_count_total <- quantile(df_dist_bucket2$count_total, 0.25)
df_dist_bucket2 <- subset(df_dist_bucket2, count_total > firstquartile_count_total)
# basic bar plot
p2 <- ggplot(data=df_dist_bucket2, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("5 < Distance <= 10")) +
  geom_text(aes(label=df_dist_bucket2$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 3
df_total_dist_bucket3 <- transform(BTR_dist_bucket3,
                                   group = cut(BTR_dist_bucket3$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket3 <- df_total_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket3 <- subset(BTR_dist_bucket3, bookings >= 1)
df_booked_dist_bucket3 <- transform(BTR_booked_dist_bucket3, 
                                    group = cut(BTR_booked_dist_bucket3$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket3 <- df_booked_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket3 <- merge(df_total_dist_bucket3, df_booked_dist_bucket3, by = "group")
df_dist_bucket3$BTR <- df_dist_bucket3$count_booked / df_dist_bucket3$count_total
firstquartile_count_total <- quantile(df_dist_bucket3$count_total, 0.25)
df_dist_bucket3 <- subset(df_dist_bucket3, count_total > firstquartile_count_total)
# basic bar plot
p3 <- ggplot(data=df_dist_bucket3, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("10 < Distance <= 20")) +
  geom_text(aes(label=df_dist_bucket3$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


### BUCKET 4
df_total_dist_bucket4 <- transform(BTR_dist_bucket4,
                                   group = cut(BTR_dist_bucket4$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket4 <- df_total_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket4 <- subset(BTR_dist_bucket4, bookings >= 1)
df_booked_dist_bucket4 <- transform(BTR_booked_dist_bucket4, 
                                    group = cut(BTR_booked_dist_bucket4$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket4 <- df_booked_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket4 <- merge(df_total_dist_bucket4, df_booked_dist_bucket4, by = "group")
df_dist_bucket4$BTR <- df_dist_bucket4$count_booked / df_dist_bucket4$count_total
firstquartile_count_total <- quantile(df_dist_bucket4$count_total, 0.25)
df_dist_bucket4 <- subset(df_dist_bucket4, count_total > firstquartile_count_total)
# basic bar plot
p4 <- ggplot(data=df_dist_bucket4, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("20 < Distance")) +
  geom_text(aes(label=df_dist_bucket4$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

maintitle = textGrob("Morning Peak (6am-10am)", gp=gpar(fontsize=15, fontface="bold"))
grid.arrange(p1, p2, p3, p4, nrow = 2, top = maintitle)


### 3. MID-DAY (HOUR 10-16) ###

midday <- vector()

for (i in 10:16) {
  # make strings for filenames
  BTR_data_filename <- paste("~/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  midday <- rbind(midday, BTR)
}

# clean data
midday <- midday[complete.cases(midday),] #remove NAs
midday <- subset(midday, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(midday$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(midday$duration, 0.99)
midday <- subset(midday, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
midday <- subset(midday, duration < pt_duration) # Grab trips that take less time than public transport
midday$bookings <- ifelse(midday$bookings == 0, 0, 1)
midday$grab_to_pt_duration <- midday$duration / midday$pt_duration

# Trying to split buckets based on surge
BTR_dist_bucket1 <- subset(midday, distance <=5)
BTR_dist_bucket2 <- subset(midday, distance >5 & distance <=10)
BTR_dist_bucket3 <- subset(midday, distance >10 & distance <=20)
BTR_dist_bucket4 <- subset(midday, distance >20)

### BUCKET 1
df_total_dist_bucket1 <- transform(BTR_dist_bucket1,
                                   group = cut(BTR_dist_bucket1$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket1 <- df_total_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket1 <- subset(BTR_dist_bucket1, bookings >= 1)
df_booked_dist_bucket1 <- transform(BTR_booked_dist_bucket1, 
                                    group = cut(BTR_booked_dist_bucket1$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket1 <- df_booked_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket1 <- merge(df_total_dist_bucket1, df_booked_dist_bucket1, by = "group")
df_dist_bucket1$BTR <- df_dist_bucket1$count_booked / df_dist_bucket1$count_total
firstquartile_count_total <- quantile(df_dist_bucket1$count_total, 0.25)
df_dist_bucket1 <- subset(df_dist_bucket1, count_total > firstquartile_count_total)

# basic bar plot
p1 <- ggplot(data=df_dist_bucket1, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("Distance <= 5")) +
  geom_text(aes(label=df_dist_bucket1$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 2
df_total_dist_bucket2 <- transform(BTR_dist_bucket2,
                                   group = cut(BTR_dist_bucket2$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket2 <- df_total_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket2 <- subset(BTR_dist_bucket2, bookings >= 1)
df_booked_dist_bucket2 <- transform(BTR_booked_dist_bucket2, 
                                    group = cut(BTR_booked_dist_bucket2$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket2 <- df_booked_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket2 <- merge(df_total_dist_bucket2, df_booked_dist_bucket2, by = "group")
df_dist_bucket2$BTR <- df_dist_bucket2$count_booked / df_dist_bucket2$count_total
firstquartile_count_total <- quantile(df_dist_bucket2$count_total, 0.25)
df_dist_bucket2 <- subset(df_dist_bucket2, count_total > firstquartile_count_total)
# basic bar plot
p2 <- ggplot(data=df_dist_bucket2, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("5 < Distance <= 10")) +
  geom_text(aes(label=df_dist_bucket2$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 3
df_total_dist_bucket3 <- transform(BTR_dist_bucket3,
                                   group = cut(BTR_dist_bucket3$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket3 <- df_total_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket3 <- subset(BTR_dist_bucket3, bookings >= 1)
df_booked_dist_bucket3 <- transform(BTR_booked_dist_bucket3, 
                                    group = cut(BTR_booked_dist_bucket3$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket3 <- df_booked_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket3 <- merge(df_total_dist_bucket3, df_booked_dist_bucket3, by = "group")
df_dist_bucket3$BTR <- df_dist_bucket3$count_booked / df_dist_bucket3$count_total
firstquartile_count_total <- quantile(df_dist_bucket3$count_total, 0.25)
df_dist_bucket3 <- subset(df_dist_bucket3, count_total > firstquartile_count_total)
# basic bar plot
p3 <- ggplot(data=df_dist_bucket3, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("10 < Distance <= 20")) +
  geom_text(aes(label=df_dist_bucket3$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


### BUCKET 4
df_total_dist_bucket4 <- transform(BTR_dist_bucket4,
                                   group = cut(BTR_dist_bucket4$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket4 <- df_total_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket4 <- subset(BTR_dist_bucket4, bookings >= 1)
df_booked_dist_bucket4 <- transform(BTR_booked_dist_bucket4, 
                                    group = cut(BTR_booked_dist_bucket4$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket4 <- df_booked_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket4 <- merge(df_total_dist_bucket4, df_booked_dist_bucket4, by = "group")
df_dist_bucket4$BTR <- df_dist_bucket4$count_booked / df_dist_bucket4$count_total
firstquartile_count_total <- quantile(df_dist_bucket4$count_total, 0.25)
df_dist_bucket4 <- subset(df_dist_bucket4, count_total > firstquartile_count_total)
# basic bar plot
p4 <- ggplot(data=df_dist_bucket4, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("20 < Distance")) +
  geom_text(aes(label=df_dist_bucket4$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

maintitle = textGrob("Mid-day (10am-4pm)", gp=gpar(fontsize=15, fontface="bold"))
grid.arrange(p1, p2, p3, p4, nrow = 2, top = maintitle)


### 4. EVENING PEAK (HOUR 17-19) ###

evening_peak <- vector()

for (i in 17:19) {
  # make strings for filenames
  BTR_data_filename <- paste("~/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  evening_peak <- rbind(evening_peak, BTR)
}

# clean data
evening_peak <- evening_peak[complete.cases(evening_peak),] #remove NAs
evening_peak <- subset(evening_peak, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(evening_peak$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(evening_peak$duration, 0.99)
evening_peak <- subset(evening_peak, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
evening_peak <- subset(evening_peak, duration < pt_duration) # Grab trips that take less time than public transport
evening_peak$bookings <- ifelse(evening_peak$bookings == 0, 0, 1)
evening_peak$grab_to_pt_duration <- evening_peak$duration / evening_peak$pt_duration

# Trying to split buckets based on surge
BTR_dist_bucket1 <- subset(evening_peak, distance <=5)
BTR_dist_bucket2 <- subset(evening_peak, distance >5 & distance <=10)
BTR_dist_bucket3 <- subset(evening_peak, distance >10 & distance <=20)
BTR_dist_bucket4 <- subset(evening_peak, distance >20)

### BUCKET 1
df_total_dist_bucket1 <- transform(BTR_dist_bucket1,
                                   group = cut(BTR_dist_bucket1$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket1 <- df_total_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket1 <- subset(BTR_dist_bucket1, bookings >= 1)
df_booked_dist_bucket1 <- transform(BTR_booked_dist_bucket1, 
                                    group = cut(BTR_booked_dist_bucket1$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket1 <- df_booked_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket1 <- merge(df_total_dist_bucket1, df_booked_dist_bucket1, by = "group")
df_dist_bucket1$BTR <- df_dist_bucket1$count_booked / df_dist_bucket1$count_total
firstquartile_count_total <- quantile(df_dist_bucket1$count_total, 0.25)
df_dist_bucket1 <- subset(df_dist_bucket1, count_total > firstquartile_count_total)

# basic bar plot
p1 <- ggplot(data=df_dist_bucket1, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("Distance <= 5")) +
  geom_text(aes(label=df_dist_bucket1$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 2
df_total_dist_bucket2 <- transform(BTR_dist_bucket2,
                                   group = cut(BTR_dist_bucket2$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket2 <- df_total_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket2 <- subset(BTR_dist_bucket2, bookings >= 1)
df_booked_dist_bucket2 <- transform(BTR_booked_dist_bucket2, 
                                    group = cut(BTR_booked_dist_bucket2$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket2 <- df_booked_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket2 <- merge(df_total_dist_bucket2, df_booked_dist_bucket2, by = "group")
df_dist_bucket2$BTR <- df_dist_bucket2$count_booked / df_dist_bucket2$count_total
firstquartile_count_total <- quantile(df_dist_bucket2$count_total, 0.25)
df_dist_bucket2 <- subset(df_dist_bucket2, count_total > firstquartile_count_total)
# basic bar plot
p2 <- ggplot(data=df_dist_bucket2, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("5 < Distance <= 10")) +
  geom_text(aes(label=df_dist_bucket2$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 3
df_total_dist_bucket3 <- transform(BTR_dist_bucket3,
                                   group = cut(BTR_dist_bucket3$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket3 <- df_total_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket3 <- subset(BTR_dist_bucket3, bookings >= 1)
df_booked_dist_bucket3 <- transform(BTR_booked_dist_bucket3, 
                                    group = cut(BTR_booked_dist_bucket3$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket3 <- df_booked_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket3 <- merge(df_total_dist_bucket3, df_booked_dist_bucket3, by = "group")
df_dist_bucket3$BTR <- df_dist_bucket3$count_booked / df_dist_bucket3$count_total
firstquartile_count_total <- quantile(df_dist_bucket3$count_total, 0.25)
df_dist_bucket3 <- subset(df_dist_bucket3, count_total > firstquartile_count_total)
# basic bar plot
p3 <- ggplot(data=df_dist_bucket3, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("10 < Distance <= 20")) +
  geom_text(aes(label=df_dist_bucket3$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


### BUCKET 4
df_total_dist_bucket4 <- transform(BTR_dist_bucket4,
                                   group = cut(BTR_dist_bucket4$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket4 <- df_total_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket4 <- subset(BTR_dist_bucket4, bookings >= 1)
df_booked_dist_bucket4 <- transform(BTR_booked_dist_bucket4, 
                                    group = cut(BTR_booked_dist_bucket4$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket4 <- df_booked_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket4 <- merge(df_total_dist_bucket4, df_booked_dist_bucket4, by = "group")
df_dist_bucket4$BTR <- df_dist_bucket4$count_booked / df_dist_bucket4$count_total
firstquartile_count_total <- quantile(df_dist_bucket4$count_total, 0.25)
df_dist_bucket4 <- subset(df_dist_bucket4, count_total > firstquartile_count_total)
# basic bar plot
p4 <- ggplot(data=df_dist_bucket4, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("20 < Distance")) +
  geom_text(aes(label=df_dist_bucket4$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

maintitle = textGrob("Evening Peak (5pm-8pm)", gp=gpar(fontsize=15, fontface="bold"))
grid.arrange(p1, p2, p3, p4, nrow = 2, top = maintitle)


### 5. NIGHT TIME (HOUR 20-23) ###

night_time <- vector()

for (i in 20:23) {
  # make strings for filenames
  BTR_data_filename <- paste("~/BTR_data_",i,".csv", sep="")
  BTR_data_output_filename <- paste("~/BTR_data_",i,"_output.csv", sep="")
  
  # load data
  BTR_data <- read.csv(BTR_data_filename, stringsAsFactors=FALSE)
  BTR_data_output <- read.csv(BTR_data_output_filename, stringsAsFactors=FALSE)
  BTR <- merge(BTR_data, BTR_data_output, by="uuid")
  
  night_time <- rbind(night_time, BTR)
}

# clean data
night_time <- night_time[complete.cases(night_time),] #remove NAs
night_time <- subset(night_time, pt_duration > 0 & duration > 60) # remove pt_duration = 0 & duration < 60s
# find the 99 percentile of pt_duration and duration respectively
ninetyninthpercentile_pt <- quantile(night_time$pt_duration, 0.99)
ninetyninthpercentile_grab <- quantile(night_time$duration, 0.99)
night_time <- subset(night_time, pt_duration < ninetyninthpercentile_pt & duration < ninetyninthpercentile_grab) #remove outlier data 
night_time <- subset(night_time, duration < pt_duration) # Grab trips that take less time than public transport
night_time$bookings <- ifelse(night_time$bookings == 0, 0, 1)
night_time$grab_to_pt_duration <- night_time$duration / night_time$pt_duration

# Trying to split buckets based on surge
BTR_dist_bucket1 <- subset(night_time, distance <=5)
BTR_dist_bucket2 <- subset(night_time, distance >5 & distance <=10)
BTR_dist_bucket3 <- subset(night_time, distance >10 & distance <=20)
BTR_dist_bucket4 <- subset(night_time, distance >20)

### BUCKET 1
df_total_dist_bucket1 <- transform(BTR_dist_bucket1,
                                   group = cut(BTR_dist_bucket1$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket1 <- df_total_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket1 <- subset(BTR_dist_bucket1, bookings >= 1)
df_booked_dist_bucket1 <- transform(BTR_booked_dist_bucket1, 
                                    group = cut(BTR_booked_dist_bucket1$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket1 <- df_booked_dist_bucket1 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket1 <- merge(df_total_dist_bucket1, df_booked_dist_bucket1, by = "group")
df_dist_bucket1$BTR <- df_dist_bucket1$count_booked / df_dist_bucket1$count_total
firstquartile_count_total <- quantile(df_dist_bucket1$count_total, 0.25)
df_dist_bucket1 <- subset(df_dist_bucket1, count_total > firstquartile_count_total)

# basic bar plot
p1 <- ggplot(data=df_dist_bucket1, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("Distance <= 5")) +
  geom_text(aes(label=df_dist_bucket1$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 2
df_total_dist_bucket2 <- transform(BTR_dist_bucket2,
                                   group = cut(BTR_dist_bucket2$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket2 <- df_total_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket2 <- subset(BTR_dist_bucket2, bookings >= 1)
df_booked_dist_bucket2 <- transform(BTR_booked_dist_bucket2, 
                                    group = cut(BTR_booked_dist_bucket2$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket2 <- df_booked_dist_bucket2 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket2 <- merge(df_total_dist_bucket2, df_booked_dist_bucket2, by = "group")
df_dist_bucket2$BTR <- df_dist_bucket2$count_booked / df_dist_bucket2$count_total
firstquartile_count_total <- quantile(df_dist_bucket2$count_total, 0.25)
df_dist_bucket2 <- subset(df_dist_bucket2, count_total > firstquartile_count_total)
# basic bar plot
p2 <- ggplot(data=df_dist_bucket2, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("5 < Distance <= 10")) +
  geom_text(aes(label=df_dist_bucket2$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

### BUCKET 3
df_total_dist_bucket3 <- transform(BTR_dist_bucket3,
                                   group = cut(BTR_dist_bucket3$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket3 <- df_total_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket3 <- subset(BTR_dist_bucket3, bookings >= 1)
df_booked_dist_bucket3 <- transform(BTR_booked_dist_bucket3, 
                                    group = cut(BTR_booked_dist_bucket3$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket3 <- df_booked_dist_bucket3 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket3 <- merge(df_total_dist_bucket3, df_booked_dist_bucket3, by = "group")
df_dist_bucket3$BTR <- df_dist_bucket3$count_booked / df_dist_bucket3$count_total
firstquartile_count_total <- quantile(df_dist_bucket3$count_total, 0.25)
df_dist_bucket3 <- subset(df_dist_bucket3, count_total > firstquartile_count_total)
# basic bar plot
p3 <- ggplot(data=df_dist_bucket3, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("10 < Distance <= 20")) +
  geom_text(aes(label=df_dist_bucket3$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


### BUCKET 4
df_total_dist_bucket4 <- transform(BTR_dist_bucket4,
                                   group = cut(BTR_dist_bucket4$grab_to_pt_duration,
                                               breaks = seq(0,1, by=0.05)))
df_total_dist_bucket4 <- df_total_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_total = n())
BTR_booked_dist_bucket4 <- subset(BTR_dist_bucket4, bookings >= 1)
df_booked_dist_bucket4 <- transform(BTR_booked_dist_bucket4, 
                                    group = cut(BTR_booked_dist_bucket4$grab_to_pt_duration,
                                                breaks = seq(0,1, by=0.05)))
df_booked_dist_bucket4 <- df_booked_dist_bucket4 %>%
  group_by(group) %>%
  summarise(count_booked=n())
# merge the two table's counts
df_dist_bucket4 <- merge(df_total_dist_bucket4, df_booked_dist_bucket4, by = "group")
df_dist_bucket4$BTR <- df_dist_bucket4$count_booked / df_dist_bucket4$count_total
firstquartile_count_total <- quantile(df_dist_bucket4$count_total, 0.25)
df_dist_bucket4 <- subset(df_dist_bucket4, count_total > firstquartile_count_total)
# basic bar plot
p4 <- ggplot(data=df_dist_bucket4, aes(x=rev(group), y=BTR)) +
  geom_bar(stat="identity", fill="deepskyblue2") +
  theme_minimal() +
  labs(x="Time Saved", y="BTR") +
  ggtitle(paste0("20 < Distance")) +
  geom_text(aes(label=df_dist_bucket4$count_total), vjust=-0.25, size=3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

maintitle = textGrob("Night Time (8pm-12am)", gp=gpar(fontsize=15, fontface="bold"))
grid.arrange(p1, p2, p3, p4, nrow = 2, top = maintitle)


dev.off()