# Initialization
# List of STMs for each user. Number of State Transition Matrices (STMs) for each user is equal to the total number of hours present for that user
userList <- list() 
uptr <- 1

# List of initial states for each user. Each list consists of as many initial states as the number of hours present for that user
initStates <- list() 

for (user in startUserId:endUserId) {
  stmList <- list()
  initStateTemp <- list() 
  print(paste("User Id  ", user, sep=" "))
  
  # To read the input data file When file names are 01,02.., append user with 0
  if (user < 10) {
    file <- paste("Data/activity/activity_u0", user, ".csv", sep="")
  } else {
    file <- paste("Data/activity/activity_u", user, ".csv", sep="")
  }
  
  # Read the data
  if (file.exists(file)) {
    rawData <- read.csv(file)
    
    # Aggregate the data to minute-level
    aggData <- Agg(rawData)
    
    # Add missing minutes
    paddedData <- MinPad(aggData)
    
    # Fill in the missing activities 
    impData <- Impute(paddedData)
    
    # Split the data into hours. For each hour, get the sequence of activities.
    seqList <- SeqGen(impData) 
    
    # Using the sequence for each hour generate STMs for each hour. Also generate an initial state for each hour.
    for (hr in 1:length(seqList)) {
      mcFit <- markovchainFit(data = seqList[[hr]])
      stm <- mcFit$estimate
      stm <- as(stm, "matrix")
      stmList[[hr]] <- stm
      initStateTemp[[hr]] <- InitStateGen(seqList[[hr]])
    }
    
    # Each user now has a list of STMs, as many STMs as the number of hours present in their data 
    # Each user also has a list of initial states, as many initial states as the number of hours
    userList[[uptr]] <- stmList
    initStates[[uptr]] <- initStateTemp
    uptr <- uptr+1
  }
}
