remove(list = ls())

source("coldChain.R")
source("chainScore.R")
library(data.table)

#Trial is a single generation of a coldChain
#All we care about, as always, is the score.
trial <- function(p){
  chain <- coldChain(p)
  
  score <- chainScore(chain)
  
  return(score)
}

#This will handle a run of the Monte Carlo Experiment for a given
#n and p. n is an integer > 0 and p \in (0,1)
#It returns a chi_square statistic.
monteCarlo <- function(n, p){
  score_vector <- c()
  for(i in 1:n){
    score_vector <- c(score_vector, trial(p))
  }
  
  #Now, we wish to find the Pearson's Test Statistic for this Run.
  exp_data <- read.csv("obsData.csv")
  #We will now create a data table with our "observed" data.
  obs_data <- data.table(trial = 1:n, Score = score_vector)
  
  #Now, we will overwrite this to be a character instead of numeric.
  obs_data$Score <- as.character(obs_data$Score)
  exp_data$Score <- as.character(exp_data$Score)
  
  #Now, we will use the .N functionality with the data table package to
  #Find the number of trials for each score.
  obs_score_freq <- obs_data[,.N, by = "Score"]
  
  #Now, we need to create a data frame with both the "observed" and actual N
  combined_frame <- merge(obs_score_freq, exp_data, by = "Score")
  
  #And we need to cover for any missing scores.
  combined_frame[is.na(combined_frame)] <- 0
  
  #Also - chi-square acts terribly if some expected values are 0. To correct,
  #We will add one to both the obs_nums and exp_nums
  combined_frame$N <- combined_frame$N + 1
  combined_frame$Number <- combined_frame$Number + 1
  
  #We will now find the Chi-Square Statistic
  chi_sq_stat <- 0
  for(i in 1:nrow(combined_frame)){
    top <- combined_frame[i]$N - combined_frame[i]$Number
    top <- top * top
    bottom <- combined_frame[i]$Number
    chi_sq_stat <- top / bottom + chi_sq_stat
  }
  
  return(chi_sq_stat)
}

#Now, we will create a handler function to evaluate p.
p_find <- function(){
  #Initial Vector
  #It won't possibly be > 0.9.
  vecStart <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                0.6, 0.7, 0.8, 0.9)
  
  #This stores the current probabilities of interest.
  current_probabilities <- vecStart
  #We'll get p to 7 places.
  for(a in 1:3){
    mean_chi_squares <- c()
    for(i in 1:length(current_probabilities)){
      chi_squares <- c()
      for(j in 1:250){ 
        chi_squares <- c(chi_squares, monteCarlo(n = 654, 
                                                 p = current_probabilities[i]))
      }
      
      mean_chi_squares <- c(mean_chi_squares, mean(chi_squares))
    }
    #Now, we will figure out the placement of the lowest mcs
    #To simplify this, we will assume that the mcs function over
    #p is roughly parabolic. So we just need to find when it rises again.
    cutoffValve <- 0
    for(check in 2:length(mean_chi_squares)){
      if(mean_chi_squares[check] >= mean_chi_squares[check-1]){
        cutoffValve <- check - 1
        break()
      }
    }
    
    #Now, we redo the currentProbabilities.
    current_probabilities <- current_probabilities[cutoffValve] + current_probabilities * 0.1
  }
  return(current_probabilities)
}

p_find()
