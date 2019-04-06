
predictTemp <- function(date,time,h_distance,h_date,h_time,point){
  set.seed(1234567890)
  library(geosphere)
  library(ggplot2)
  library(reshape2)
  stations <- read.csv("stations.csv", fileEncoding = "latin1")
  temps <- read.csv("temps50k.csv", fileEncoding = "latin1")
  st <- merge(stations,temps,by="station_number")
  
  h_distance <- h_distance # These three values are up to the students
  h_date <- h_date
  h_time <- h_time
  #a <- 58.4274 # The point to predict (up to the students)
  #b <- 14.826
  date <- date # The date to predict (up to the students)
  
  times <- time
  
  temp <- vector(length=length(times))
  
  # Students’ code here
  
  interestLoc <- point
  stationLoc <- st[,c("longitude","latitude")]
  stationDates <- st$date
  
  interestTime <- as.POSIXct(times, format="%H:%M:%S") ## because difftime needs date-time obect
  stationTimes <- as.POSIXct(st$time, format="%H:%M:%S")
  
  distKernel <- function(stationLoc,interestLoc){
    
    distance <- distHaversine(stationLoc, interestLoc) 
    return (exp(-(distance / h_distance)^2))
  }
  
  dateKernel <- function(stationDate,interestDate){
    dateDiff <- as.numeric( difftime(interestDate,stationDate,units = "days") )
    return (exp(-(dateDiff / h_date)^2))
  }
  
  timeKernel <- function( interestHour){
    timeDiff <-as.numeric( difftime(stationTimes,interestHour,units = "hours") )
    return (exp(-(timeDiff / h_time)^2))
  }
  
  
  
  distDiff<- distKernel(stationLoc, interestLoc)
  dateDiff <- dateKernel(stationDates,date)
  timeDiff <- lapply(interestTime,FUN = timeKernel) #12 index with 50000 diff each
  
  additive <- function(timeDiff,distDiff,dateDiff){
    cum <- distDiff+dateDiff+timeDiff
    # sum (distDiff,dateDiff,timeDiff) does nto work y?
    temp <- sum((st$air_temperature * cum)) / sum(cum)
  }
  
  multiplicative <- function(timeDiff,distDiff,dateDiff){
    mul <- distDiff*dateDiff*timeDiff
    temp <- sum((st$air_temperature * mul)) / sum(mul)
  }
  
  additiveTemps <- unlist(lapply(timeDiff,additive,distDiff=distDiff,dateDiff=dateDiff))
  multiplicativeTemps <- unlist(lapply(timeDiff,multiplicative,distDiff=distDiff,dateDiff=dateDiff))
  
  plot(x=interestTime,y=additiveTemps, type="o", xlab = "Time",ylab = "Tempreature",main = "Additive kernel")
  plot(x=interestTime,y=multiplicativeTemps, type="o", xlab = "Time",ylab = "Tempreature",main="Multiplicative kernel")
  
  results <- data.frame(Ind = times,additive=additiveTemps,multi=multiplicativeTemps)
  results <- melt(results,id="Ind")
  ggplot(results)+
    geom_point(aes(x=Ind,y=value,color=variable))
}

a <- 58.4274 # The point to predict (up to the students)
b <- 14.826
h_distance <- 1000000 # These three values are up to the students
h_date <- 12
h_time <- 7
date="2015-05-04"

times <- c("02:00:00","04:00:00", "06:00:00", "08:00:00","10:00:00","12:00:00" ,
           "14:00:00", "16:00:00","18:00:00",
           "20:00:00","22:00:00","24:00:00")
point <- c(a,b)

predictTemp(date,times,h_distance,h_date,h_time, point)
