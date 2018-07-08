
master_df16<-read.csv('data/2016Q4-capitalbikeshare-tripdata.csv')
master_df15<-read.csv('data/2015Q4-capitalbikeshare-tripdata.csv')
names(master_df)

master_df <- rbind(master_df16,master_df15)

write.csv(master_df,paste(getwd(),"/data/totaldata.csv",sep=""),col.names=TRUE, na="",sep=",")

master_df<-read.csv('data/totaldata.csv')

# random sample
index = sample(1:nrow(master_df),500000,replace=FALSE)
df<-master_df[index,]

# Factorize variables
df$Start.station.number<- as.factor(df$Start.station.number)
df$End.station.number<- as.factor(df$End.station.number)
df$End.station <- as.factor(df$End.station)
df$Start.station<- as.factor(df$Start.station)
df$Bike.number<- as.factor(df$Bike.number)
df$Member.type <- as.factor(df$Member.type )

# Convert to dates
df$Start.date<- as.POSIXlt(df$Start.date)
df$End.date<- as.POSIXlt(df$End.date)

str(df)

# Count nulls
colSums(sapply(df, is.na))
# Duration           Start.date             End.date Start.station.number 
# 0                    0                    0                    0 
# Start.station   End.station.number          End.station          Bike.number 
# 0                    0                    0                    0 
# Member.type 
# 0 
names(df)

# Convert duration to minutes
df$duration.min = round(df$Duration / 60, digits = 2)

library(dplyr)
# Start.date  / End.date keep day, month, quarter, year, hour

# start date
df$start.hour <- unclass(df$Start.date)$hour
df$start.dayofweek <-  weekdays(df$Start.date)
df$start.dayofmonth <- unclass(df$Start.date)$mday
df$start.month <- months(df$Start.date)
#df$start.quarter <- quarters(df$Start.date)
df$start.dayofyear <- unclass(df$Start.date)$yday
df$start.year <- unclass(df$Start.date)$year+1900  

# End data
df$end.hour <- unclass(df$End.date)$hour
df$end.dayofweek <-  weekdays(df$End.date)
df$end.dayofmonth <- unclass(df$End.date)$mday
df$end.month <- months(df$End.date)
#df$end.quarter <- quarters(df$End.date)
df$end.dayofyear <- unclass(df$End.date)$yday
df$end.year <- unclass(df$End.date)$year+1900 

# Start date day != End date day
df$returnnextday <- ifelse(df$end.dayofyear!=df$start.dayofyear, 1, 0)
df$returnnextday<- as.factor(df$returnnextday)

# Plot distributions
library(ggplot2)
library(corrplot)
library(gridExtra)
library(grid)
library(lattice)
library(e1071)   
plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}


doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

names(df)

plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], Duration = data_in$Duration)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}

# split dataframe into numeric and categorical columns
nums.only <- sapply(df,class)!='factor'
cat.only <- sapply(df,class)=='factor'
nums <- df[ , nums.only]
cat <- df[ , cat.only]

doPlots(cat, fun = plotHist, ii = 6:7, ncol = 2)

library(tidyr)
library(purrr)
df %>%
  keep(is.numeric) %>% 
  gather() %>%
  head()


df %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density() 

df$Start.date<- as.POSIXct(df$Start.date)
df$End.date<- as.POSIXct(df$End.date)
#The first thing we'll do is to run distribution analysis on the main continuous variables in the dataset: total.rides and avg.duration. We will use levels of five categorical variables, i.e. Subscription.Type, weekend_holiday, rushhour, season, and AdverseWeather, as group coloring to generate high level between-group distribution comparison.
names(df)
library(dplyr)
df %>%
  group_by(start.year ,start.month, start.dayofweek, start.hour, Member.type) %>%
  summarise(
    total.rides = n(),
    avg.duration = mean(duration.min)
  ) -> aggdata

names(day_hour_rides)

# save aggregated data 
write.csv(day_hour_rides,paste(getwd(),"/data/aggdata.csv",sep=""),col.names=TRUE, na="",sep=",")


# # avg duration per year per month
avgDuration<-df %>%
  group_by(start.year, start.month) %>%
  summarize(duration.min = mean(duration.min))

# ###############
# 
# ##2
# #calculate % of total rides (number of rides)
# # count rides per year per month
rideNo<-df %>%
  group_by(start.year, start.month) %>%
  tally %>% arrange(desc(n))
# 
# divide (number of month rides)/(number of total rides)
rideNo$percent.rides <- rideNo$n/nrow(df)

ridesyearmonth <- merge(avgDuration, rideNo, by=c("start.year","start.month"))

names(df)
df %>%
  group_by(start.dayofweek, start.hour, Member.type) %>%
  summarise(
    total.rides = n(),
    avg.duration = mean(duration.min)
  ) -> day_hour_rides

# Create a subset just for the time heatmap
day_hour_rides %>%
  ungroup() %>%
  select(start.hour, start.dayofweek, total.rides, avg.duration) %>%
  mutate(total_duration = total.rides * avg.duration, 
         hour = factor(start.hour, levels = (0:23))) %>%
  group_by(start.hour, start.dayofweek) %>%
  summarise(count.rides = sum(total.rides), total.duration = sum(total_duration)) -> df.1


names(df)
library(ggthemes)
library(cowplot)

g1 <- ggplot(data=day_hour_rides)
g1 + geom_histogram(mapping = aes(log(day_hour_rides$avg.duration), fill =  Member.type), bins = 100) -> g2
g1 + geom_histogram(mapping = aes(log(day_hour_rides$total.rides), fill =  Member.type), bins = 100) -> g3

plot_grid(g2)
plot_grid(g3)

df %>%
  group_by(start.month, Member.type) %>%
  summarise(
    total.rides = n(),
    avg.duration = mean(duration.min)
  ) -> month

g4 <- ggplot(data=aggdata)
g4 + geom_histogram(mapping = aes(log(aggdata$total.rides), fill = as.factor(aggdata$start.month)), bins = 100) -> g5
g4 + geom_histogram(mapping = aes(log(aggdata$avg.duration), fill = as.factor(aggdata$start.month)), bins = 100) -> g6
g4 + geom_histogram(mapping = aes(log(aggdata$total.rides), fill = as.factor(aggdata$start.year)), bins = 100) -> g7
g4 + geom_histogram(mapping = aes(log(aggdata$avg.duration), fill = as.factor(aggdata$start.year)), bins = 100) -> g8

plot_grid(g5)
plot_grid(g6)
plot_grid(g7)
plot_grid(g8)

df.1$start.dayofweek  = factor(df.1$start.dayofweek, levels=c("Monday", "Tuesday", "Wednesday","Thursday", "Friday", "Saturday", "Sunday"))

# Create time based heatmaps
g100 <- ggplot(data=df.1, aes(x=start.hour, y=start.dayofweek, fill=count.rides)) +
  geom_tile(color="white", size=0.1)+ coord_equal() +
  labs(x=NULL, y=NULL, title="Count of Rides Per start.dayofweek & start.hour of Day") +
  theme_tufte(base_family="Calibri") + theme(plot.title=element_text(hjust=0.5, size = 10)) +
  theme(axis.ticks=element_blank()) + theme(axis.text=element_text(size=7)) + theme(legend.position="none") +
  scale_fill_gradient(low = "white", high = "steelblue")
g101 <- ggplot(data=df.1, aes(x=start.hour, y=start.dayofweek, fill=total.duration)) +
  geom_tile(color="white", size=0.1)+ coord_equal() +
  labs(x=NULL, y=NULL, title="Total Duration Per start.dayofweek & start.hour of Day") +
  theme_tufte(base_family="Calibri") + theme(plot.title=element_text(hjust=0.5, size = 10)) + theme(legend.position="none") +
  theme(axis.ticks=element_blank()) + theme(axis.text=element_text(size=7)) +
  scale_fill_gradient(low = "white", high = "firebrick")
g102 <- ggplot(data=df.1, aes(x=start.hour, y=start.dayofweek, fill=total.duration/count.rides)) +
  geom_tile(color="white", size=0.1)+ coord_equal() +
  labs(x=NULL, y=NULL, title="Average Duration Per start.dayofweek & start.hour of Day") +
  theme_tufte(base_family="Calibri") + theme(plot.title=element_text(hjust=0.5, size = 10)) + theme(legend.position="none") +
  theme(axis.ticks=element_blank()) + theme(axis.text=element_text(size=7)) +
  scale_fill_gradient(low = "white", high = "springgreen3")
plot_grid(g100, g102, nrow = 2, rel_heights = c(1/2, 1/2))


# plot on map

address<-df
#address<-rbind(master_df16,master_df15)
coord<-read.table("data/Capital_Bike_Share_Locations.csv", header = TRUE, sep = ',', quote = "", stringsAsFactors = FALSE)
coord$LATITUDE<-signif(coord$LATITUDE,digits=10)
coord$LONGITUDE<-signif(coord$LONGITUDE,digits=10)

map_df <- merge(address, coord[ , c("LATITUDE", "LONGITUDE","TERMINAL_NUMBER")], by.x=c("Start.station.number"), by.y=c("TERMINAL_NUMBER"))


str(address)
str(coord)
map_df$duration.min = round(map_df$Duration / 60, digits = 2)
map_df$LATITUDE<-signif(map_df$LATITUDE,digits=10)
map_df$LONGITUDE<-signif(map_df$LONGITUDE,digits=10)
# Create station list with coordinates, total count of rides, and total duration of rides
map.stations <- map_df %>%
  group_by(Start.station) %>%
  summarise(total.rides = n(),
            avg.duration = mean(duration.min),
            subscriber.percentage = mean(Member.type == "Member"),
            lat = first(LATITUDE),
            lon = first(LONGITUDE)
  )
head(map.stations)
map.stations$lat<-signif(map.stations$lat,digits=10)
map.stations$lon<-signif(map.stations$lon,digits=10)

library(ggmap)
library(qmap)

# bubbles map
# download basic map layers for plotting
base.map <- qmap("Wasington DC", zoom = 12, source= "google", maptype="roadmap", color = "bw", crop=FALSE, legend='topleft')
base.map.1 <- qmap("Wasington DC", zoom = 13, source= "google", maptype="roadmap", color = "bw", crop=FALSE, legend='topleft')
base.map.2 <- qmap("Wasington DC", zoom = 14, source= "google", maptype="roadmap", color = "bw", crop=FALSE, legend='topleft')

base.map + geom_point(aes(x = lon, y = lat, size=total.rides, color=avg.duration), data = map.stations,
                      alpha = .5)+ scale_size(range = c(1, 5)) + scale_colour_gradient(low = "purple", high = "red")
base.map.1 + geom_point(aes(x = lon, y = lat, size=total.rides, color=avg.duration), data = map.stations,
                        alpha = .5) + scale_size(range = c(1, 5)) + scale_colour_gradient(low = "purple", high = "red")
base.map.2 + geom_point(aes(x = lon, y = lat, size=total.rides, color=avg.duration), data = map.stations,
                        alpha = .5) + scale_size(range = c(1, 10)) + scale_colour_gradient(low = "purple", high = "red")


# heatmap
# Create ride density map
base.map.2 + geom_density2d(data = map_df[sample(1:nrow(map_df), 10000),], 
                            aes(x = LONGITUDE, y = LATITUDE), size = 0.5) + stat_density2d(data = map_df[sample(1:nrow(map_df), 10000),], 
                                                                                           aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), size = 3, 
                                                                                           bins = 30, geom = "polygon", contour = TRUE) + scale_fill_gradient(low = "springgreen", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)


# Create a subsliced ridership set of 15000 observations
ride_df.sample <- map_df[sample(1:nrow(map_df), 50000),]
names(ride_df.sample)

ride_df.sample$Start.date<- as.POSIXlt(ride_df.sample$Start.date)

ride_df.sample$start.month <- months(ride_df.sample$Start.date)

# Create base layers for faceted mapping
dc.3 <- get_map('washington dc', zoom = 14, source = "google", maptype = "roadmap", crop=FALSE, color="bw")
dc.map.3 <- ggmap(dc.3, base_layer = ggplot(aes(x = LONGITUDE, y = LATITUDE), data = ride_df.sample))
# Ride frequency heatmap by month
dc.map.3 + stat_density2d(aes(x=LONGITUDE, y=LATITUDE, fill=..level.., alpha=..level..),
                          bins=7, geom="polygon", data=ride_df.sample) +
  scale_fill_gradient(low="springgreen", high="tomato") + scale_alpha(range = c(0.1, 0.6), guide = FALSE) + 
  facet_wrap(~start.month, nrow = 1) +
  guides(fill=guide_legend(title="ride\nfrequency")) +
  ggtitle("Ride Distribution by Month") +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.text = element_blank(),
        plot.title = element_text(color="black", size=16, hjust=0)) -> g13
g13

names(df)


# popular day of week
ggplot(data = df) +
  geom_density(aes(x = start.hour, color = start.dayofweek), adjust = 3) 

# popular day of month
ggplot(data = df) +
  geom_density(aes(x = start.dayofyear, color =Member.type), adjust = 3) 

# popular day of month
ggplot(data = df) +
  geom_density(aes(x = start.dayofmonth, color =Member.type), adjust = 3) 

# popular day of month
ggplot(data = df) +
  geom_density(aes(x = start.dayofmonth, color =start.month), adjust = 3) 

# popular duration min
ggplot(data = df) +
  geom_density(aes(x = duration.min, color = Member.type), adjust = 3) + xlim(0, 100)

df$fromto <- paste(df$Start.station,df$End.station, sep="-")

# most common routes
theme_set(theme_bw())  
names(df)

toproutes<-df %>%
  group_by(Start.station) %>%
  tally %>% arrange(desc(n))

toproutes <- head(toproutes,10)
toproutes$Start.station <- factor(toproutes$Start.station, levels = toproutes$Start.station[order(toproutes$n)])
theme_set(theme_bw())

# Draw plot

ggplot(toproutes, aes(x=Start.station, y=n, reorder(Start.station, n))) +
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Most popular stations") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  coord_flip()



theme_set(theme_classic())
names(ride_df)
# Histogram on a Categorical variable

str(df)
yearmonth<- df %>%
  group_by(start.year, start.month, Member.type) %>%
  summarise(
    total.rides = n(),
    avg.duration = mean(duration.min)
  )  


theme_set(theme_classic())

df$start.month  = factor(df$start.month, levels=c("October", "November", "December"))
df$start.year  = factor(df$start.year, levels=c("2015", "2016"))

# Histogram on a Categorical variable
g <- ggplot(df, aes(start.month))
g + geom_bar(aes(fill=Member.type), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Count rides per month") 

# Histogram on a Categorical variable
g <- ggplot(df, aes(start.month))
g + geom_bar(aes(fill=start.year), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Count rides per month") 

# Histogram on a Categorical variable
g <- ggplot(df, aes(start.year))
g + geom_bar(aes(fill=Member.type), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Count rides per year") 


library("plyr") 

# boxplots per year - month
names(df)
count.summary <- ddply(df,.(start.year, start.month,start.dayofweek ), summarise, 
                       total.rides = length(X),
                       avgDuration = mean(duration.min))
names(count.summary)
# boxplot of rental v.s. holiday 
ggplot(count.summary, aes(x = start.year, y = total.rides, fill = factor(start.month))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Year") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_fill_manual(values=c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12"), 
                    name="Month:",
                    breaks=c("October", "November", "December"),
                    labels=c("October", "November", "December")) +
  theme(plot.title = element_text(size = 11, face="bold"))

ggplot(count.summary, aes(x = start.year, y = avgDuration, fill = factor(start.month))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Year") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_fill_manual(values=c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12"), 
                    name="Month:",
                    breaks=c("October", "November", "December"),
                    labels=c("October", "November", "December")) +
  theme(plot.title = element_text(size = 11, face="bold"))


# boxplots per year - day of week

# boxplot of rental v.s. holiday 
ggplot(count.summary, aes(x = start.year, y = total.rides, fill = factor(start.dayofweek))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Year") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_fill_manual(values=c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12", "#68C3D4", "#FFE8D1","#826251"), 
                    name="Weekday:",
                    breaks=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                    labels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  theme(plot.title = element_text(size = 11, face="bold"))

ggplot(count.summary, aes(x = start.year, y = avgDuration, fill = factor(start.dayofweek))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Year") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_fill_manual(values=c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12", "#68C3D4","#FFE8D1", "#826251"), 
                    name="Weekday:",
                    breaks=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                    labels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  theme(plot.title = element_text(size = 11, face="bold"))


