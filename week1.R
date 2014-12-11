setwd("c:/R")
getwd()
if(!file.exists("myder")) {
  dir.create("myder")
}
list.files("myder")

my_url <-"https://wiki.artezio.com/display/PMCARD/ART-QMS"
download.file(my_url, destfile = "./myder/proj.html", method="auto" )

list.files("myder")




install.packages("RCurl")
install.packages("xlsx")
install.packages("XML")
install.packages("jsonlite")
install.packages("httr")

install.packages("data.table")

library(RCurl)
library(xlsx)

curl = getCurlHandle()
curlSetOpt(cookiejar = 'cookies.txt', followlocation = TRUE, autoreferer = TRUE, curl = curl)
html <- getURL('https://wiki.artezio.com/login.action?os_destination=%2Findex.action',  ssl.verifypeer = FALSE , curl = curl)


params <- list(
  'os_username'    = 'ashagalov',
  'os_password'    = 'pUnx1982'
)

sss <- postForm('https://wiki.artezio.com/https://wiki.artezio.com/dologin.action', params = params, curl = curl, style='POST')

html <- getURL('https://wiki.artezio.com/display/PMCARD/ART-QMS', curl = curl)



#grepl('Logout', html)


############################
#  XML HTML
############################



library(XML)
fileUrl <- "http://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse(fileUrl,useInternal=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)

rootNode
names(rootNode)
rootNode[[1]]
rootNode[[1]][[2]]
xmlSApply(rootNode,xmlValue)
xpathSApply(rootNode,"//name",xmlValue)
xpathSApply(rootNode,"//price",xmlValue)


doc <- htmlTreeParse(html,useInternal=TRUE)
scores <- xpathSApply(doc,"//td",xmlValue)
#teams <- xpathSApply(doc,"//li[@class='team-name']",xmlValue)


############################
# JSON
############################


library(jsonlite)
jsonData <- fromJSON("https://api.github.com/users/jtleek/repos")
names(jsonData)
names(jsonData$owner)
names(jsonData$owner$login)
jsonData$owner
jsonData$owner$login

myjson <- toJSON(iris, pretty=TRUE)
cat(myjson)

iris2 <- fromJSON(myjson)
head(iris2)




library(data.table)
DT = data.table(x=rnorm(9),y=rep(c("a","b","c"),each=3),z=rnorm(9))
head(DT,3)
tables()

DT[2,]
[DT$y=="a",]
DT[c(2,3)]
  
DT[,table(y)]

DT

DT2 <- DT
DT[,zz:='Ooo!']
DT2

tmp <- 10
DT[,m1:= {tmp <<- tmp+1; tmp }]

DT[,a:=x>0]

#Считает среднее по всем тру, и по всем фолс отдельно
DT[,b:= mean(x+w),by=a]

DT

#Посчитать количество элементов -> .N
set.seed(123);
DT <- data.table(x=sample(letters[1:3], 1E5, TRUE))
DT[, .N, by=x]

#Ключ
DT <- data.table(x=rep(c("a","b","c"),each=100), y=rnorm(300))
setkey(DT, x)
DT['ab']


# MERGE!!!
DT1 <- data.table(x=c('a', 'a', 'b', 'dt1'), y=1:4)
DT2 <- data.table(x=c('a', 'b', 'dt2'), z=5:7)

DT1
DT2
setkey(DT1, x); setkey(DT2, x)
merge(DT1, DT2)

big_df <- data.frame(x=rnorm(1E6), y=rnorm(1E6))
file <- tempfile()
write.table(big_df, file=file, row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE)
system.time(fread(file))

system.time(read.table(file, header=TRUE, sep="\t"))


