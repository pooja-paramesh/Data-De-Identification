# Initialization
# List of simulated users. Each list consists of a sequence of states
simList <- list() 
sptr <- 1

# Using each user (also referred to as original user) as a seed, simulate n new users (also referred to as simulated users)
for (optr in 1:length(userList)) { 
  origuser <- userList[[optr]]
  origuserInitStates <- initStates[[optr]]
  for (susrptr in 1:n) {
    simuser <- vector()
    noise <- sample(0:1, hours, prob=c((1 - noiseperc), noiseperc), replace = T)  # Create vector which samples 0s and 1s using noiceperc as the probability
    for (hptr in 1:hours) {
      # For hour hptr, if noise is 0, use the origuser's current hour's STM to simulate one hour of data
      if (noise[hptr] == 0) {  
        tempVec <- McSimulate(origuserInitStates[[hptr]], origuser[[hptr]], 59)
        simuser <- c(simuser, tempVec)}
      
      # For hour hptr, if noise is 1, jump to different user randomly and use that user's current hour's STM to simulate one hour of data
      if (noise[hptr] == 1) {  
        randomUser <- sample(c(1:length(userList)), 1)
        tempVec <- McSimulate(initStates[[randomUser]][[hptr]], userList[[randomUser]][[hptr]], 59)
        simuser <- c(simuser, tempVec)
      }
    }
    write.csv(simuser,paste("SimUsers/user_",optr,"_",susrptr,".csv",sep = ""),row.names = F)
    simList[[sptr]] <- simuser
    sptr <- sptr + 1
  }
}
