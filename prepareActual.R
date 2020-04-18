source("coldChain.R")
source("chainScore.R")

originalData <- read.csv("originalData.csv")

#We need to iterate over the above data and generate a score for each row.
#We will need to procedurally generate chainLinks to accomplish this.
originalData$Score <- -1

for(i in 1:nrow(originalData)){
  row_Chain <- c()
  past_rec <- 0
  for(j in 2:6){
    if(j == 2){ #Case corresponds to Gen 1.
      new_link <- new("chainState", infected = originalData[i,j], recovered = 0)
      past_rec <- past_rec + originalData[i,j]
    } else {
      #Now, we record the number of infectives.
      curInf <- originalData[i,j]
      
      #And we generate a new link.
      new_link <- new("chainState", infected = curInf, recovered = past_rec)
      
      past_rec <- past_rec + curInf
    }
    
    row_Chain <- c(row_Chain, new_link)
  }
  
  #We now score the chain
  rowScore <- chainScore(row_Chain)
  #And save the score
  originalData[i,8] <- rowScore
}

#Now, we save the original data with the new code column.
write.csv(originalData, "obsData.csv")
