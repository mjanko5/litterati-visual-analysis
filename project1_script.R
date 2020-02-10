litdata <- read.table(file = "https://www.evl.uic.edu/aej/424/litterati challenge-65.csv", sep = ",", header = TRUE)

#library(dplyr)
#tbldata <- tbl_df(litdata)  #create a new table tbldata for dplyr

##filter the locations that are out of bounds
#filtered <- filter(tbldata, lat > 41.84 & lon < -87.80 & lon > -87.83) #updated (check later)

filtered <- subset(litdata, lat > 41.84 & lon < -87.80 & lon > -87.83)


#plot a map using lat and long
library(ggplot2)
map_plot <- ggplot(filtered, aes(x=lon, y=lat)) + geom_point(color="black")


#dates
library(lubridate)
newDate <- ymd_hms(filtered$litterTimestamp)   #ex: 2020-01-07 22:14:20 UTC
date <- date(newDate)                          #only hours
hour <- hour(newDate)
weekday <- weekdays(newDate)

filtered$date <- date       #add new data to table
filtered$hour <- hour       #add new data to table
filtered$weekday <- weekday
filtered$litterTimestamp <- NULL   #remove old data from table


#show plot: time vs lat
ggplot(filtered, aes(x=date, y=lat)) + geom_point(color="blue")

ggplot(filtered, aes(x=date, y=lat-41.9)) + geom_line(color="red") + geom_line(aes(y=lon+87.8, color="blue")) #try something


#install.packages("leaflet")
library(leaflet)
litmap <- leaflet(filtered) %>% 
  addTiles() %>% 
  addCircles(lng = filtered$lon, lat = filtered$lat, color = "blue")
litmap


#interesting: 
#ggplot(filtered, aes(x=newDate, y=challengeId)) + geom_point(color="blue") +  labs(title="my titulor", x="date", y = "challenge IDD") + geom_line()
#ggplot(filtered, aes(x=newDate, y=litterjoinId)) + geom_point(color="blue") +  labs(title="my titulor", x="date", y = "litterjoinId") + geom_line()



#bar charts
#by user:
ggplot(filtered, aes(x=factor(username)))  + geom_bar(stat="count", width=0.7, fill="steelblue")

#goal:
#NOTE: factor use:
  # 1) ?
  # 2) doesn't matter
  # 3) no

#over time: 
ggplot(filtered, aes(x=factor(date))) + labs(title="Amount of Litter Picked Over Time", x="date", y = "amount per day") + geom_bar(stat="count", width=0.7, fill="steelblue")
#weekday:
ggplot(filtered, aes(x=factor(weekday))) + labs(title="Amount of Litter Picked by Day of The Week", x="day", y = "amount per day") + geom_bar(stat="count", width=0.7, fill="steelblue")
#hour
ggplot(filtered, aes(x=hour)) + labs(title="Amount of Litter Picked by Hour of The Day", x="hour", y = "amount per hour") + geom_bar(stat="count", width=0.7, fill="steelblue")



# get top 10 pickers:
occurences <-table(unlist(filtered$username))         #get frequency of each and convert df to table 
top10pickers <- sort(occurences, decreasing=T)[1:10] #sort and get top 10





