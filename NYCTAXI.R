library(ggplot2)
library(reshape2)
data = read.csv("trip_length.csv")
#plot the time series for yellow taxi trip lengthes from Jan.2009 to Dec.2015 by month
datayellow=read.csv("09-15.csv")
ts.plot(ts(datayellow[,-1], start = c(2009,1), frequency = 12), gpars = list(xlab = " ", ylab = " ", lty=1))

#plot the time series for green taxi trip lengthes from Jan.2009 to Dec.2015 by month
datagreen=read.csv("green13-15.csv")
ts.plot(ts(datagreen[,-1], start = c(2013,1), frequency = 12), gpars = list(xlab = " ", ylab = " ", lty=1))

#plot the trip lengths for yellow taxi from 2009 to 2016 by year
yellowData = data[data['color'] == 'yellow', ]
ggplot(data=yellowData, aes(x=year, y=trip_length, group=1)) +
  geom_line()+
  geom_point() + 
  labs(title="Plot of Yellow Trip Length",x="Year", y = "Trip Length")

#plot the trip lengths for green taxi from 2013 to 2016 by year
greenData = data[data['color'] == 'green', ]
ggplot(data=greenData, aes(x=year, y=trip_length, group=1)) +
  geom_line()+
  geom_point() + 
  labs(title="Plot of Green Trip Length",x="Year", y = "Trip Length")
#test if the variance of yellow taxi is different from the variance of green taxi 
var.test(greenData$trip_length, yellowData$trip_length)
t.test(greenData$trip_length, yellowData$trip_length)

#plot the trip lengths for yellow taxi for different areas from 2009 to 2015
trip_area = read.csv("trip_area.csv")
trip_area$Airports = 1000 * trip_area$Airports
trip_area_melt <- melt(trip_area, id="Year") 

ggplot(data=trip_area_melt,
       aes(x=Year, y=value, colour=variable)) +
       geom_line() + geom_point() +
  labs(title="Plot of Yellow Trip Length of different areas",x="Year", y = "Trip Length")
#do t-test to see if the manhattan trip lengths and the airport trip lengths are different
t.test(trip_area$Manhattan,trip_area$Airports)
 
