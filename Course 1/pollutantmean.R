pollutantmean <- function(directory = "specdata", pollutant = "sulfate", id = 1:332) {
  
    elm <- vector()
    
    if(length(id[id<10]) > 0) {
    elm <- paste0("00",id[id<10])  
    }
    
    #print( length( id[id >=10 & id < 100] ) )
    
    if( length( id[id >=10 & id < 100] ) > 0) {
      elm <- c(elm, paste0("0",id[id >=10 & id < 100] ))
    }
    
    elm <-c( elm, as.character(id[id >=100] )  )
     
  meanvect <- vector()
  for( my_filename in paste0(directory,"/", elm, ".csv"))  {
    
    data <- read.csv( my_filename)
    
    meanvect <- c(meanvect, data[[pollutant]][!is.na(data[[pollutant]])] )
    
    #meanvect <- subset(data[[pollutant]], is.na(data[[pollutant]] == TRUE ))
  }
  print(mean (meanvect))
  print(my_filename)
  return(mean (meanvect))  
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  #data
}

#pollutantmean(id = 13:233)
pollutantmean("specdata","nitrate",23)
pollutantmean("specdata", "nitrate", 70:72)
