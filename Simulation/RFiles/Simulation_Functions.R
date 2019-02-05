Agg <- function(rawData) {
  # This function aggregates the data to minute-level
  #
  # Args: 
  #   rawData: Activity dataframe for one user. Dataframe has two columns: timestamp and activity.inference
  #
  # Returns:
  #   Data Aggregated to minute-level
  
  # Convert the unix timestamp to DateTime format
  timeStamp <- as.POSIXct(rawData$timestamp,origin="1970-01-01") 
  activity <- rawData$activity.inference
  data <- data.frame(timeStamp,activity)

  # Convert the time series to intervals of 1 min 
  tsSplit <- cut(data$timeStamp, breaks = "1 min")
  dataTsSplit <- data.frame(tsSplit, data$activity)
  colnames(dataTsSplit) <- c('time', 'activity')
  
  # Aggregate the data to minute level. The most frequent activity in a minute becomes the activity for that minute
  dataAgg <- aggregate(activity~time, data=dataTsSplit, FUN=function(activity){names(which.max(table(activity)))})
  
  # Assign appropiate names to the activities
  dataAgg$activity[dataAgg$activity == 0] <- "S"  # Stationary
  dataAgg$activity[dataAgg$activity == 1] <- "W"  # Running
  dataAgg$activity[dataAgg$activity == 2] <- "R"  # Walking
  dataAgg$activity[dataAgg$activity == 3] <- "M"  # Missing
  return(dataAgg)
}

MinPad <- function(aggData) {
  # This function adds the missing minutes for each hour and assigns NA for the corresponding activities
  #
  # Args:
  #   aggData: Minute level data
  #
  # Returns:
  #   Data padded with missing minutes
  data <- aggData
  data$time <- as.POSIXct(data$time,origin="1970-01-01")
  
  # Provide start date
  startDate <- as.Date(data$time[1], "%m/%d/%Y", tz = "America/New_York")
  startDateTime <- paste(startDate, " ", "00:00:00", sep = "")
  startDateTime <- as.POSIXct(startDateTime)
  
  # Provide end date
  endDate <- as.Date(data$time[nrow(data)], "America/New_York")
  endDateTime <- paste(endDate, " ", "23:59:00", sep = "")
  endDateTime <- as.POSIXct(endDateTime)
  
  # Pad
  paddedTime <- as.factor(seq(startDateTime, endDateTime, by = 60))  # Create a sequence that runs from the start time to the end time at minute-level
  paddedAct <- rep(NA, length(paddedTime)) # Assign NAs for activities
  for(i in 1:length(paddedTime)){
    temp <- data[data$time == as.character(paddedTime[i]), 'activity'] # Get the activity information that available for the minutes in data, rest will be NA
    if(length(temp) != 0){
      paddedAct[i] <- temp
    }
  }  
  paddedData <- data.frame(paddedTime, paddedAct)
  names(paddedData) <- c('time', 'activity')
  return(paddedData)
}

Impute <- function(paddedData) {
  # For each NA value, the function parses the sequence five steps forward and five steps backward
  # If the parsing resturns one element(activity) then that element replaces NA
  # If the parsing returns two elements(one from forward parse and one from backward parse) one of them is picked with a probability of 0.5 and that element replaces the NA
  # If the parsing does not any results then NA is replaced with 'M'
  #
  # Args:
  #   paddedData: Minute-level data (every minute intact from start date to end date)
  #
  # Returns:
  #   Complete data. NAs are replaced with S, W, R (activites) or 'M' for missing. Missing is also considered as an activity
  #   For example, if a user turns of the device everyday during class, then 'Missing' data will have pattern which can make the user identifiable
  data <- paddedData
  
  # A reference series which will be parsed
  refSeries <- as.character(data$activity)
  
  # A new series with replacements for NAs
  newSeries <- as.character(data$activity)
  
  for(k in 1:length(refSeries)){
    if (is.na(refSeries[k])) {
      fwdele <- ForwardParse(refSeries, k)
      bwdele <- BackwardParse(refSeries, k)
    if (is.na(fwdele) & !is.na(bwdele)) {
      newSeries[k] <- bwdele
    } else if (is.na(bwdele) & !is.na(fwdele)) {
      newSeries[k] <- fwdele
    } else if (is.na(bwdele) & is.na(fwdele)) {
      newSeries[k] <- 'M'
    } else {
      randval <- sample(c(fwdele, bwdele), 1, prob = c(0.5,0.5))
      newSeries[k] <- randval
    }
    }
  }
  data$activity <- newSeries
  return(data)
}

ForwardParse <- function(seq, pos) {
  # This function parses the sequence in the forward direction for 5 steps
  #
  # Args:
  #   seq: Sequence to be parsed
  #   pos: Starting position
  # 
  # Returns:
  #   The first element found
  element <- NA
  stop <- 0
  check <- 1
  while (stop != 1) {
    if ((pos + check) <= length(seq)) {
      element <- seq[pos + check]
      if (!is.na(element)) {
        stop <- 1
      }
      if (check == 5) {
        stop <- 1
      }
      } else {
        stop <- 1
      }
      check <- check + 1
    }
  return(element)
}

BackwardParse <- function(seq,pos) {
  # This function parses the sequence in the backward direction for 5 steps
  #
  # Args:
  #   seq: Sequence to be parsed
  #   pos: Starting position
  # 
  # Returns:
  #   The first element found
  element <- NA
  stop <- 0
  check <- 1
  while (stop != 1){
      if (pos - check >= 1) {
        element <- seq[pos - check]
        if (!is.na(element)) {
          stop <- 1
        }
        if (check == 5) {
          stop <- 1
        }
      } else {
        stop <- 1
      }
      check <- check + 1
    }
  return(element)
}
  
SeqGen <- function(impData) {
  # This function splits the sequence of activities into hours
  #
  # Args:
  #   impData: Complete activity data
  #
  # Returns: 
  #   A list of, list of activities for every hour
  listofSeqs <- list()
  lptr <- 1
  d <- impData
  d$time <- as.POSIXct(d$time, origin="1970-01-01")
  split <- cut(d$time, breaks="hour")
  dhourly <- data.frame(split, d$activity)
  names(dhourly) <- c('Time', 'Activity')
  uniqueHour <- as.character(unique(dhourly$Time)) #To get the start and end date time
  for (i in 1:length(uniqueHour)) {
    temp <-as.vector(dhourly[dhourly$Time == uniqueHour[i], 'Activity'])
        listofSeqs[[lptr]] <- temp
        lptr <- lptr + 1
  }
  return(listofSeqs)
}

InitStateGen <- function(seq) {
  # This function returns the initial state for a given sequence based on probabilistic sampling
  #
  # Args:
  #   seq: The sequence from which the initial state needs to be selected
  #
  # Returns:
  #   The initial state
  probVec <- table(seq)/length(seq)
  initState <- sample(names(probVec), 1, prob = probVec)
  return(initState)
}

McSimulate <- function(initState, stm, n){
  # This function simulates n+1 states using the STM (stm)
  #
  # Args:
  #   initState: Initial state to start from
  #   stm: State transition matrix
  #   n: number of states to be simulated
  #
  # Returns:
  #   A sequence of states
  stateToBeFound <- initState
  generatedSeq <- c(initState)
  gptr <- 2
  for (j in 1:n){
    transProb <- stm[stateToBeFound,]
    names(transProb) <- colnames(stm)
    
    # Finding the next state
    x1 <- transProb
    x2 <- sort(x1)
    x3 <- append(x2, 0, after=0) 
    
    # Generate a random number, check which interval it falls in, and obtain the corresponding state
    u<- runif(1)
    for (i in 1:length(x3)) {
      if (i != length(x3)) {
        if ((u > x3[i]) & (u <= x3[i + 1])) {
          y <- names(x3[i + 1])
        }
      }
      if (i == length(x3)) {
        if (u > x3[i]){
          y <- names(x3[i])
        }
      }
    }
    generatedSeq[gptr] <- y
    gptr <- gptr + 1
    stateToBeFound <- y
  }
  return(generatedSeq)
}