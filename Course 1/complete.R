complete <- function(directory = "specdata",  id = 1:332) {
  print (id)
  
  elm <- vector()
  for(i in id){
  if(i<10){
    elm <-c(elm, paste0("00", as.character(i))  )   
  }
  else if(i>=10 & i < 100){
    elm <-c(elm, paste0("0", as.character(i))  )   
  }
  else{
    elm <- c(elm, as.character(i)       )
  }

  }
  
  
  filelst <- paste0(directory,"/", elm, ".csv")    
  
  ids <- vector()
  nobs <- vector ()  
  
  
  for( i in seq_along(filelst) )  {
    
    data <- read.csv( filelst[i])
    #ss <- subset(data, is.na(sulfate) == FALSE, is.na(nitrate) == FALSE)
    dat1 <- subset(data, is.na(sulfate) == FALSE)
    dat2 <- subset(dat1, is.na(nitrate) == FALSE)
    
    # почему-то дает неверный результат :()
    ss <- length( subset(data, is.na(sulfate) == FALSE, is.na(nitrate) == FALSE)[[1]]      )
      
    #print (id)
    #print (ss) 
    #print (length(dat2[[1]]) )
      
    ids <- c(ids,  id[i]  )
    nobs <-c(nobs,  length(dat2[[1]])  )        
    
  }
  
  
  res <- data.frame(id = ids, nobs )
  return(res)
  #source("complete.R")
  
}


complete("specdata", 322:1)
