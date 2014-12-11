
setwd("C:/R/Assnmnt3")


#hospital_stat_set <- read.csv("Assnmnt3/outcome-of-care-measures.csv", colClasses = "character")
hospital_stat_set <- read.csv("outcome-of-care-measures.csv", colClasses = "character")



rankall <- function( outcome, rank_val = "best") {

  ##  2 - Name, 7 - State
  ##  11. Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists the risk adjusted rate (percentage) for each hospital.
  ##  17. Hospital 30-Day Death (Mortality) Rates from Heart Failure: Lists the risk adjusted rate (percentage) for each hospital.
  ##  23. Hospital 30-Day Death (Mortality) Rates from Pneumonia: Lists the risk adjusted rate (percentage) for each hospital.
  
  hss<- hospital_stat_set[,c(2,7,11,17,23)]
  states1 <- factor(hss[,c(2)])
  state <- levels(states1)
  #print(states)
  outcomes_lst <- c("heart attack","heart failure", "pneumonia")

  outcomes_val <- list()
  outcomes_val[["heart attack"]] <- 3
  outcomes_val[["heart failure"]] <- 4
  outcomes_val[["pneumonia"]] <- 5
  
    
  if (!(outcome %in% outcomes_lst)) {
    stop(" invalid outcome") 
  }
  
  req_colmn <- outcomes_val[[outcome]]
  hss[, req_colmn] <- as.numeric(hss[, req_colmn])
  
  hss <-  hss[!is.na(hss[req_colmn]), ]
  #соритруем
  hss <-  hss[ order(hss[,2], hss[,req_colmn], hss[,1]), ]
  
  
  
  counter <- rank_val
  if (rank_val == "best" ){
    counter <- 1    
   }


  hospital <- vector()
  
  for (cur_state in state) {
    
    
    subset2 <-hss[hss$State== cur_state,] 

    if (rank_val == "worst" ){
       counter <- nrow(subset2)    
    }
    
        
    if (cur_state == "WI")  {
       print(subset2)
       print(nrow(subset2))
       print(counter)
       print(subset2[counter,][[1]])
     } 
    
    
    #if (counter <= nrow(subset2)){
      hospital <- c(hospital, subset2[counter,][[1]])
    #} else hospital <- c(hospital, NA)
    
    
  }
  
  res = data.frame(hospital, state)
  #print(res)
  
  
  return(res)
     
}


#rankall( "heart attack", "worst")

