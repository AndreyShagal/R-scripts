corr <- function(directory = "specdata",  threshold  = 150) {
  print (threshold)
  id <- 1:332
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
  cors <- vector()
  for( i in seq_along(filelst) )  {
    sulfate <- vector()
    nitrate <- vector()    
    
    data <- read.csv( filelst[i])
    #print (filelst[i])
    dat1 <- subset(data, is.na(sulfate) == FALSE)
    dat2 <- subset(dat1, is.na(nitrate) == FALSE)
    #print( length(dat2[[2]])  )    
    #print(dat2)  
    if (length(dat2[[2]] ) > threshold ) {        
      
      sulfate <-c(sulfate,  dat2[[2]]  ) 
      nitrate <-c(nitrate,  dat2[[3]]  )        
      #print (cor(sulfate,nitrate))
      cors <- c( cors, round( cor(sulfate,nitrate), 5 ) )          

      #print(sulfate)      
    }
    
  }
  #print(sulfate)
  if(length(cors) > 0) {
    return(cors)
  } else   return(c(0))
  #source("complete.R")
  
}


cr <- corr("specdata", 150)
head(cr)
summary(cr)