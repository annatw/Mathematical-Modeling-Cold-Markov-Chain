source("coldChain.R")

#This function returns the ith prime up to the thirtieth. 
#It accepts an integer i
getIthPrime <- function(i){
  if(i == 0){
    return(-1)
  } else{
    thirtyPrimes <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37,
                      41, 43, 47, 53,
                      59, 61, 67, 71, 73, 79, 83, 89, 97, 
                      101, 103, 107, 109, 113)
    return(thirtyPrimes[i])
  }
}

#This returns the numerical "score" for the chain.
chainScore <- function(chainList){
  if(length(chainList) == 5){
    score <- 1 #We will be multiplying this by unique primes for each possible spot value.
    for(i in 2:5){
      current_link <- chainList[[i]]
      prime_value <- 1
      if(i == 2){
        #B value is always 1 when i = 2
        current_inf <- getInfected(current_link)
        prime_value <- getIthPrime(i+1)
        score <- score * prime_value
      } else if (i == 3){
        #Here, we have 11 possible options.
        placement <- 0
        current_inf <- getInfected(current_link)
        current_rec <- getRecovered(current_link)
        
        if(current_rec == 1){
          placement <- 1
        } else if(current_rec == 2){
          placement <- 2 + current_inf
        } else if(current_rec == 3){
          placement <- 6 + current_inf
        } else if(current_rec == 4){
          placement <- 9 + current_inf
        } else {
          placement <- 11
        }
        
        prime_value <- getIthPrime(placement + 5)
        score <- score * prime_value
      } else if (i == 4){
        #Here, we have 8 possible options.
        placement <- 0
        current_inf <- getInfected(current_link)
        current_rec <- getRecovered(current_link)
        
        if(current_rec == 1){
          placement <- 1
        } else if (current_rec == 2){
          placement <- 2
        } else if (current_rec == 3){
          placement <- 3 + current_inf
        } else if (current_rec == 4){
          placement <- 6 + current_inf
        } else {
          placement <- 8
        }
        
        prime_value <- getIthPrime(placement + 16)
        score <- score * prime_value
      } else if (i == 5){
        #Here, we have 8 possible options.
        placement <- 0
        current_inf <- getInfected(current_link)
        current_rec <- getRecovered(current_link)
        
        if(current_rec == 1){
          placement <- 1
        } else if (current_rec == 2){
          placement <- 2
        } else if (current_rec == 3){
          placement <- 3
        } else if (current_rec == 4){
          placement <- 4 + current_inf
        } else {
          placement <- 6
        }
        
        prime_value <- getIthPrime(placement + 24)
        score <- score * prime_value
      }
    }
    
    return(score)
  } else{
    return(-1)
  }
}