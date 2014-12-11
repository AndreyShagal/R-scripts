
# setwd("C:/R")
# outcome <- read.csv("Assnmnt3/outcome-of-care-measures.csv", colClasses = "character")
# head(outcome)
# ncol(outcome)
# nrow(outcome)
# names(outcome)

# outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
# hist(outcome[, 11])


#hospital_stat_set <- read.csv("Assnmnt3/outcome-of-care-measures.csv", colClasses = "character")
#hospital_stat_set <- read.csv("outcome-of-care-measures.csv", colClasses = "character")



best <- function(state, outcome) {
  hss<- hospital_stat_set   
  
  outcomes_lst <- c("heart attack","heart failure", "pneumonia")
  outcomes_val <- list()
  outcomes_val[["heart attack"]] <- 11
  outcomes_val[["heart failure"]] <- 17
  outcomes_val[["pneumonia"]] <- 23
  
  ##subse <-  subset(hss, State == state)  
  
  if (!(state %in% hss$State)) {
    stop("invalid state") 
  }
  
  if (!(outcome %in% outcomes_lst)) {
    stop(" invalid outcome") 
  }
  #class(hss)
  #print(names(hss))
  
  
  hss <- subset(hss, State == state)
  
##  11. Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists the risk adjusted rate (percentage) for each hospital.
##  17. Hospital 30-Day Death (Mortality) Rates from Heart Failure: Lists the risk adjusted rate (percentage) for each hospital.
##  23. Hospital 30-Day Death (Mortality) Rates from Pneumonia: Lists the risk adjusted rate (percentage) for each hospital.
  
  req_colmn <- outcomes_val[[outcome]]
  hss[, req_colmn] <- as.numeric(hss[, req_colmn])
  #hss[["Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]]
  subset1 <- subset(hss, hss[req_colmn] == min(hss[req_colmn],na.rm=TRUE ) ) 
  
  print(sort(subset1[["Hospital.Name"]]))
  
  ##print (hss[["Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]])
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate

}

#best("AA","BB")  
best("TX","heart attack")  

