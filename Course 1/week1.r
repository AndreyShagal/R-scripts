x<-4
class(x)
x <-c(4,"a", TRUE)
class(x)
x
x<-c(1,3,5)
y<-c(3,2,10)
cbind(x,y)
x <- list(2, "a", "b", TRUE)
x[[1]]
class(x[[1]])
class(x[1])
x<-1:4
y<-2
x+y
 x <- c(17, 14, 4, 5, 13, 12, 10)
x[x>10]<-4
x
 x <- c(17, 14, 4, 5, 13, 12, 10)
x
x[x > 10]<-4
x
dir
ls
con <- gzfile("rprog-data-quiz1_data1.zip")
data <- read.csv(con)
data
data <- read.csv("hw1_data.csv")
data
count(data)
data$ozone
data$Ozone
data$Ozone is.na()
is.na(data$Ozone)
is.na(data$Ozone)[TRUE]
d<-is.na(data$Ozone)
d[d==TRUE]
data[!is.na(data$Ozone)]
data[1,2]
data[1]
data[ is.na(data$Ozone)]
data[, is.na(data$Ozone) ]
subset(data, is.na(Ozone) == FALSE)
ss1 <- subset(data, is.na(Ozone) == FALSE)
mean (ss1[1])
mean(ss1[1])
ss1[1]
ss1[[1]]
mean(ss1[[1]])
ss2 <- subset(data, Ozone > 31, Temp > 90)
ss2
ss2[1]
ss2[[1]]
ss2 <- subset(data, is.na(Ozone) == False, is.na( Temp) == FALSE)
ss2 <- subset(data, is.na(Ozone) == FALSE, is.na( Temp) == FALSE)
ss2
ss1ss2 <- subset(ss1, Ozone > 31, Temp > 90)
ss1ss2
ss1ss2 <- subset(ss1, Ozone > 31)
ss1ss2
ss2ss2 <- subset(ss1ss2, Temp > 90)
ss2ss2
mean(ss2ss2$Solar.R)
ss2 <-subset(data, Month == 6)
ss2
mean(ss2$Temp)
ss4 <- subset(data, Month == 5)
max(ss4[Ozone])
max(ss4[[Ozone]])
max(ss4$Ozone)
ss4
max( subset(ss4$Ozone, is.na(ss4$Ozone) == FALSE ) )
complete.cases
help("complete")
