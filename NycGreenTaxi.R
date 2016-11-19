# Setting the working directory
setwd("/MY_FILE_DIRECTORY")
# Loading the Data
green_tripdata = read.csv("green_tripdata_2015-09.csv")
# Dimensions of the data loaded.
dim(green_tripdata)


hist(green_tripdata$Trip_distance, main = "Histogram of the number of the Trip Distance", xlab = "Distance of Trip", ylab = "Number of Trips",  xlim = c(0,10), ylim = c(0,40000), breaks = "FD", col = "green")
# plotting trips that are less than 10units less in distance as the trips more than 10units are fewer than 100 in frequency.
# STRUCTURE
# The structure seems to be right skewed.
# HYPOTHESES
# It can be noticed that 
# the frequency of the trips is more within 0.5 miles to 5 miles of distance. 
# This can be interpreted as, people tend to take taxis for distance between 0.5 miles and 5 miles. 
# An assumption can be made based on the observation that for distance less than 0.5 miles people prefer to walk 
# whereas for distance more than 5 miles people prefer to take some other mode of transport, maybe subway, bus or personal vehicle as taxi could be expensive.


# Truncating dates from the data and just getting the time. 
hour_of_day <- strftime(green_tripdata$lpep_pickup_datetime, format="%H:%M:%S")
# Calculating Mean.
mean_by_hourofday <- tapply(green_tripdata$Trip_distance, hour_of_day, function(x) mean(x))
# Calculating Median.
median_by_hourofday <- tapply(green_tripdata$Trip_distance, hour_of_day, function(x) median(x))

# NYC Airport Location through Google Maps.
nyc_airport_longitude <- -73.7822222222
nyc_airport_latitude <- 40.6441666667

# FOR PICKUP
# Calculating the distance of pickup point from the airport using pythagoras theorem.
distance_from_airport_pickup <- sqrt((green_tripdata$Pickup_longitude - nyc_airport_longitude)^2 + (green_tripdata$Pickup_latitude - nyc_airport_latitude)^2)
green_tripdata$distance_from_airport_pickup = distance_from_airport_pickup

# FOR DROPOFF
# Calculating the distance of dropoff point from the airport using pythagoras theorem.
distance_from_airport_dropoff <- sqrt((green_tripdata$Dropoff_longitude - nyc_airport_longitude)^2 + (green_tripdata$Dropoff_latitude - nyc_airport_latitude)^2)
green_tripdata$distance_from_airport_dropoff = distance_from_airport_dropoff

# Calculating radius of NYC airport Area.
# Getting the Longitude and Latitude using google maps.
# Using pythagoras theorem for calculationg distance.
# Taking aiport location as centre and the area outside the area from aiport (observed on google maps) as point on the circumference of the area surrounding the airport.
nyc_airport_area_radius <- sqrt((40.651311 - nyc_airport_latitude)^2 + (-73.755243 - nyc_airport_longitude)^2)

trip_near_nyc_airport <- green_tripdata[(green_tripdata$distance_from_airport_pickup < nyc_airport_area_radius) | (green_tripdata$distance_from_airport_dropoff < nyc_airport_area_radius),]

# Number of transactions happening in the area of NYC Airport
nrow(trip_near_nyc_airport)
#14495
mean(trip_near_nyc_airport$Total_amount)
# Average Fare
# 46.23954
summary(trip_near_nyc_airport) # Getting insights of the data.
# By looking at the summary of the above generated dataset it can be observed that the minimum Total_amount is
# -400, which shows that sometimes cab drivers sometimes don't get paid for the service they provide.


# Calculating Tip percent.
tip_percent <- (green_tripdata$Tip_amount/green_tripdata$Total_amount)*100
green_tripdata$Tip_percent <- tip_percent # The tip percent variable to that data set "green_tripdata"
summary(green_tripdata) # Getting insights of data through summary

# Predictive Model
summary(green_tripdata$Tip_amount) # Getting insights of data through summary

# On the basis of the Trip_distance
plot(green_tripdata$Tip_amount, green_tripdata$Trip_distance, xlim = c(0,2), ylim = c(0,400),type = "h", xlab = "Tip Amount", ylab = "Trip Distance", main = "Tip Amount wrt to Trip Distance")
# Removing Outliers, It can be seen that tip amount is the least for farthest distance and remains constant for distance between 0 to 50 miles.

# On the basis of Payment_Type
summary(green_tripdata$Payment_type) # Getting insights of data through summary
plot(green_tripdata$Tip_amount, green_tripdata$Payment_type,type = "h", xlab = "Tip Amount", ylab = "Payment Type", main = "Tip Amount wrt to Payment Type")
# It shows that people using payment type 1 amd 2 tend to pay tips whereas people with payment type 3,4 and 5 don't.

# On the basis of Trip_type
summary(green_tripdata$Trip_type) # Getting insights of data through summary
plot(green_tripdata$Tip_amount, green_tripdata$Trip_type,type = "h", xlab = "Tip Amount", ylab = "Trip Type", main = "Tip Amount wrt to Trip Type")
# No relation between these two entities can be established.


# While plotting the graphs above for the data of Tip Amount and Total Fare Amount, it was seen that there were some
# negative values like -50 for distance 0. This makes me think what can be the case where the cab driver had to pay the 
# customer. Also, it was seen that tip amount was almost constant no matter how long the distance was.
# CONCLUSION
# People in NYC don't tend to pay tips to cab drivers of green taxi and they don't tend to take taxis for farther distances.

