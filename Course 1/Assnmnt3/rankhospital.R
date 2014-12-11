
#setwd("C:/R")


#hospital_stat_set <- read.csv("Assnmnt3/outcome-of-care-measures.csv", colClasses = "character")
hospital_stat_set <- read.csv("outcome-of-care-measures.csv", colClasses = "character")



rankhospital <- function(state, outcome, rank_val) {

  hss<- hospital_stat_set   
  
  outcomes_lst <- c("heart attack","heart failure", "pneumonia")
  outcomes_val <- list()
  outcomes_val[["heart attack"]] <- 11
  outcomes_val[["heart failure"]] <- 17
  outcomes_val[["pneumonia"]] <- 23
  
  
  if (!(state %in% hss$State)) {
    stop("invalid state") 
  }
  
  if (!(outcome %in% outcomes_lst)) {
    stop(" invalid outcome") 
  }
  
  
  hss <- subset(hss, State == state)
  
##  11. Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists the risk adjusted rate (percentage) for each hospital.
##  17. Hospital 30-Day Death (Mortality) Rates from Heart Failure: Lists the risk adjusted rate (percentage) for each hospital.
##  23. Hospital 30-Day Death (Mortality) Rates from Pneumonia: Lists the risk adjusted rate (percentage) for each hospital.
  
  req_colmn <- outcomes_val[[outcome]]
  hss[, req_colmn] <- as.numeric(hss[, req_colmn])
  
  #отделяем только нуные колонки
  subset1 <- hss[,c(req_colmn,2)]
  #Убираем НА
  subset2 <-  subset1[!is.na(subset1[1]), ]
  #соритруем
  subset2 <-  subset2[ order(subset2[,1], subset2[,2]), ]


  if (rank_val == "best" ){
    rank_val <- 1    
   }

  if (rank_val == "worst" ){
    rank_val <- nrow(subset2)    
  }

print(subset2)
print(rank_val)

  if (rank_val <= nrow(subset2)){
    res <- vector()
    res <-subset2[rank_val,][[2]]
    print(class(subset2[rank_val,][[2]]))
    print(res)
    return(res)
  }
  else  return(NA)    
     
}


rankhospital("NC", "heart attack", "worst")

