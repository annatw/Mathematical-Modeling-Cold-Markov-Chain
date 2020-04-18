#This file contains the code for the Markov chain function and the state objects.

#We will first create a new class that will contain the current state in the MC
setClass("chainState", slots = list(infected = "numeric", recovered = "numeric"))

#We will need functions to allow us to access the infected and recovered counts.
setGeneric("getInfected", function(object) standardGeneric("getInfected"))
setGeneric("getRecovered", function(object) standardGeneric("getRecovered"))

setMethod("getInfected", signature(object = "chainState"), function(object){
  return(object@infected)
})

setMethod("getRecovered", signature(object = "chainState"), function(object){
  return(object@recovered)
})

#The Markov Chain accepts one parameter, a value for p in [0,1]
coldChain <- function(p){
  #We will first code defensively
  if(p > 1){
    return(-1)
  } else if (p < 0) {
    return(-1)
  }
  
  #Now, we will simulate the chain of length 5
  #To do this, we will first create the object 
  #that will hold the states at each point in the chain.
  chainList <- c()
  
  #We will always start the chain at (1,0)
  chainList <- c(chainList, new("chainState", infected=1, recovered = 0))
  
  #Now, we will use a for-loop to generate the five step chain. 
  for(f in 2:5){
    #We'll pull out the current state for easier reference.
    if(length(chainList) == 1){
      current_state <<- chainList[[1]]
    } else{
      current_state <- chainList[[f-1]] #Recall that R starts it's vectors at 1.
    }
    #We'll now have to figure out what state's transitions we're interested in.
    #This is trivial if infected = 0 as it returns to the state with probability 1.
    if(getInfected(current_state) == 0){
      newInfected <- 0 #Nothing from nothing gets nothing.
    } else if (getInfected(current_state) == 1){
      #This is a similarly pretty basic case, as it takes on values from Binomial distribution.
      newInfected <- rbinom(n=1, size = 5 - getInfected(current_state) - getRecovered(current_state),
                            prob = p)
    } else if(getInfected(current_state) == 2){
      #There can be 1, 2, or 3 recovered if 2 are infected. Each has a different
      #Transition possibility.
      if(getRecovered(current_state) == 1){
        #This can go to 0, 1, or 2
        #We will first generate the two cutoffs for it going to 0 and 1.
        cutOff0 <- (1-p)^4
        cutOff1 <- (1-p)^4 + 4 * p * (1-p)^3 + 2 *p^2 *(1-p)^2
        
        #Now, we will generate the uniform random variable.
        decider <- runif(1, min = 0, max = 1)
        
        if(decider < cutOff0){
          newInfected <- 0
        } else if (decider > cutOff0){
          if(decider < cutOff1){
            newInfected <- 1
          } else{
            newInfected <- 2
          }
        }
      } else if (getRecovered(current_state) == 2){
        #This can go to either 0 or 1.
        #We will first generate the probability cutoff for 0 infections.
        cutoff <- (1-p)^2
        #We will now generate a uniform random variable from 0 to 1
        variableTest <- runif(1, min = 0, max = 1)
        if(variableTest < cutoff){
          newInfected <- 0
        } else {
          newInfected <- 1
        }        
      } else if (getRecovered(current_state) == 3){
        #This case has no more members of the family left to infect.
        newInfected <- 0
      } 
    } else if(getInfected(current_state) == 3){
       #This state may occur if 1 or 2 have recovered.
      if(getRecovered(current_state) == 1){
        #We can either go to 0 or 1 infections here.
        #We will first generate the probability cutoff for 0 infection.
        cutoff <- 1 - 3 * p + 3 * p^2 - p^3
        #We will now generate a uniform random variable from 0 to 1
        variableTest <- runif(1, min = 0, max = 1)
        if(variableTest < cutoff){
          newInfected <- 0
        } else {
          newInfected <- 1
        }
      } else if(getRecovered(current_state) == 2){
        #Everyone is used up, so no more infections occur.
        newInfected <- 0
      }
    } else if(getInfected(current_state) == 4){
      #This case is only accessible if X2 = (4,1).
      #It always goes to 0,5 as a result.
      newInfected <- 0
    }
    #Now, we will set the new recovered number.
    newRecovered <- getInfected(current_state) + getRecovered(current_state)
    #We will make a new link of the chain.
    newLink <- new("chainState", infected= newInfected, recovered = newRecovered)
    
    chainList <- c(chainList, newLink)
  } 
  
  return(chainList)
}

