library(readr)
absentiism <- read_delim("../common/data/absentiism_at_work/Absenteeism_at_work.csv",  ";", escape_double = FALSE, trim_ws = TRUE)

smallDat <- data.frame(ID = absentiism$ID, month = absentiism$`Month of absence`, hours = absentiism$`Absenteeism time in hours`)


# group by subject
smallDat <- smallDat[order(smallDat$ID), ]


# we track patients for three years, create months in advance
ids <- unique(smallDat$ID)

timeSeries <- c()


for(idIndex in 1:length(ids)){
  currentID <- ids[idIndex]
  # add a zero for the very first month (which will then jut add unto the zero)
  timeSeries <- c(timeSeries, 0)
  
  subjectRows <- smallDat[which(smallDat$ID == currentID),]
  # first month doesn't have an offset
  currentMonth <- subjectRows$month[1]
  
  dims <- dim(subjectRows)
  for(r in 1:dims[1]){
    nextRowMonth <- subjectRows$month[r]
    # pad with zeros for ommitted months
    if(nextRowMonth >= currentMonth)
      timeSeries <- c(timeSeries, rep.int(0, max(0, nextRowMonth - currentMonth - 1)))
    
    #if it's the same month, just keep adding up to the prior month
    if(currentMonth == nextRowMonth){
      l <- length(timeSeries)
      timeSeries[l] <- timeSeries[l] + subjectRows$hours[r] 
    }
    #if the months jumped a year
    else if(nextRowMonth < currentMonth){
      noToPad <- 12 - currentMonth + (nextRowMonth - 1)
      timeSeries <- c(timeSeries, rep.int(0, noToPad))
      timeSeries <- c(timeSeries, subjectRows$hours[r])
    }
    else{
      timeSeries <- c(timeSeries, subjectRows$hours[r])
    }
    currentMonth <- nextRowMonth
  }
}


absentiismObs <- data.frame(time = 1:length(timeSeries), obs = timeSeries)
write.csv(absentiismObs, file='../common/data/absentiism_at_work/absentiismObs.csv')


