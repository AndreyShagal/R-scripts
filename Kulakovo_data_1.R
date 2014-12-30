#Kulakovo_data_1.R
setwd("c:/R")
Sys.setlocale("LC_CTYPE", "russian")
install.packages("data.table")
install.packages("xlsx")
install.packages("lattice")
install.packages("sqldf")

library(xlsx)
library(data.table)
library(lattice)
library(sqldf)


datafile = "SRR292770_1.fastq"
read.csv.sql(datafile,"select count(*) from file")


#e?ei	Iaeiaeu	Iaeiaeu_max	OEI	OY_1	OY_2	AA?AIAIIINOU	Noaaey	Eeioieaiay eioeeuo?aoey	Neea?ic no?iiu	iee?ininoau ni neaa?-oaiiiaiii	Iaee?ea ieaciioeoia	PR HISTO Score ?aeac %	PR HISTO Score  no?iiu %	ER HISTO Score ?aeac %	ER HISTO Score  no?iiu %	ieiiiiaee	PR/ER	LIF yieoaeee	LIF no?iia	ESR1	PGR	MSX-1/HOX-7	HOXA-10	HOXA-11	VEGF iau	IL1b	IL2	LIF	LIFR	IL8	IL15	IL18	TNFa	CD56	Cox-2	MMP11	HB-EGF	IGFBP1	IGF1	VEGF 189	CD68	IL6	IL10	TGFb	OsM	MMP-2	MMP-7	MMP9	AREG	IGF2	BCL2	BAX	CD45	PTEN	MMP8	IGFBP2	IGFBP4	IL12A

pregn <- read.xlsx2("Kulakovo/in_data.xlsx", "Data1", encoding="UTF-8", colClasses="character",  stringsAsFactors=FALSE)


#Ia?aaiaei ia ec oaenoa a NA
pregn[pregn == '#NULL!'] <-NA

#Ec oaenoa a eio   ii?ao eo?oa au a oaeoi??
pregn[pregn == 'ii?ia'] <- 1
pregn[pregn == 'Ii?ia'] <- 1
pregn[pregn == 'OY'] <- -1
y <- pregn[["OY_1"]]
y[ y == ''] <- 0
pregn[["OY_1"]] <- y
#
#levels(pregn[["OY_1"]] )
# via?a?ei Ii?i e ii?i
#levels(pregn[["OY_1"]] )[1] <- 1
#levels(pregn[["OY_1"]] )[2] <- 1
#levels(pregn[["OY_1"]] )[2] <- -1


#Oae?aai no?iee aaa  aa?aiaiiinou NA
pregn <- pregn[complete.cases(pregn[7]),]

#pregn <- as.numeric(pregn)


#Oae?aai ionoo? eieiieo OEI  e IGF1  a iae oieuei iaii cia?aiea
pregn <- subset(pregn, select = - c(OEI,IGF1) )

#Ioai? cia?aiee, aac i?iioneia - a iineaaieo 19 eieiieao i?aiu iiiai  NA
pregn_cut <- pregn[1:(ncol(pregn)-19)]
table(complete.cases(pregn_cut))

#head(pregnT[1:20])

for( i in 1:ncol(pregn) )  {
pregn[[i]] <-  as.numeric(pregn[[i]])
print(colnames(pregn)[i])
}



#aiaaaei oaeoi?
pregn[["AA?AIAIIINOU"]] <- factor( pregn[["AA?AIAIIINOU"]],labels = c("IAa?", "Aa?"))


for( i in  1:(ncol(pregn) - 1) )  {
print(i)

h1<-histogram(pregn[[i]] ~ pregn[[i]]  | pregn[[6]] ,   data=pregn,  equal.widths=TRUE, main=colnames(pregn)[i] ) 
png(filename = paste(as.character(i), colnames(pregn)[i] , ".png") )
print(h1)

#readline()
dev.off()


}






pregnT <- subset(pregn, pregn["AA?AIAIIINOU"]==1 )
pregnF <- subset(pregn, pregn["AA?AIAIIINOU"]==0 )


matrixT <- vector(mode="numeric", length=0)
for( i in 1:(ncol(pregnT)-1) )  {
C <- pregnT[[i]] 
meanval <- mean(C[is.na(C)!= TRUE])
matrixT <- c(matrixT, meanval)
}




matrixF <- vector(mode="numeric", length=0)
for( i in 1:(ncol(pregnF)-1) )  {
C <- pregnF[[i]] 
meanval <- mean(C[is.na(C)!= TRUE])
matrixF <- c(matrixF, meanval)
}

matrixM <- vector(mode="numeric", length=0)
for( i in 1:(ncol(pregn)-1) )  {
C <- pregn[[i]] 
meanval <- mean(C[is.na(C)!= TRUE])
matrixM <- c(matrixM, meanval)
}



png(filename ="mean11.png") 
c0 <- plot(1:11,matrixF[1:11], col="red")
c1 <- lines(1:11,matrixF[1:11], col="red")
c2 <- lines(1:11,matrixT[1:11], col="blue")
c3 <- lines(1:11,matrixM[1:11], col="green")
print(c0)
print(c1)
print(c2)
print(c3)
dev.off()


png(filename ="mean22.png") 
c0 <- plot(20:57,matrixF[20:57], col="red")
c1 <- lines(20:57,matrixF[20:57], col="red")
c2 <- lines(20:57,matrixT[20:57], col="blue")
c3 <- lines(20:57,matrixM[20:57], col="green")
print(c0)
print(c1)
print(c2)
print(c3)
dev.off()


png(filename ="mean_whole.png") 
c0 <- plot(1:57,matrixF[1:57], col="red")
c1 <- lines(1:57,matrixF[1:57], col="red")
c2 <- lines(1:57,matrixT[1:57], col="blue")
c3 <- lines(1:57,matrixM[1:57], col="green")
print(c0)
print(c1)
print(c2)
print(c3)
dev.off()


png(filename ="mean_whole2.png") 
c0 <- plot(1:57,matrixF[1:57], col="red")
c1 <- lines(1:57,matrixF[1:57], col="red")
c2 <- lines(1:57,matrixT[1:57], col="blue")
c3 <- lines(1:57,matrixM[1:57], col="green")
print(c0)
print(c1)
print(c2)
print(c3)
dev.off()


for( i in 1:(ncol(pregn)-1) )  {

SD <- sd( pregn[[i]][ is.na(pregn[[i]] ) != TRUE  ])
print (colnames(pregn)[i] )
print(SD)
}

points(matrixT , 1:57, col = "blue")
points(matrixF , 1:57, col = "red")

xyplot(matrixT ~ 1:57, layout = c(2, 1)) 
xyplot(matrixF ~ 1:57, layout = c(2, 1)) 

#####################################################33


fn = as.character(i)

#na.rm=TRUE
#pregnT[ pregnT == '#NULL!'] <- NA
#pregnF[ pregnF == '#NULL!'] <- NA
#pregnT <- na.omit(pregnT)

#colnames(pregnT)

	#which(colnames(pregnT)=="Eiy eieiiee") 
    #print(pregnT$i)
    #CurrCollTrue <- as.numeric(pregnT[[i]])
	#CurrCollFalse<- as.numeric(pregnF[[i]])
 #for( i in 1:ncol(pregnT) )  {
 
 # for( i in 10:15 )  
#  for( i in 50:ncol(pregnT) )  {
   for( i in 49:49 )  {
    print( as.numeric(pregnT[[i]]) )
    print( as.numeric(pregnF[[i]]) )	  
	print( colnames(pregnT)[i] )

	
	   
	par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
	
	pregnT[[i]] <- as.numeric( pregnT[[i]] )
	    ss <-   subset(pregnT[[i]], is.na(pregnT[[i]]) != TRUE   )  
	    print(ss) 
	if(length(ss)>0)	{
        hist( ss  , main= colnames(pregnT)[i], col="blue",  breaks = c(0, 0.002, 0.003, 0.004, 0.005, 0.006, 0.008)  )
	} else {
	   hist( c(-1,-1))
	}
	
	pregnF[[i]] <- as.numeric( pregnF[[i]] )
	
	ss2 <-  subset(pregnF[[i]], is.na(pregnF[[i]]) != TRUE   ) 
	
	ss2 <-  subset(ss2, ss2 <= 0.008   ) 
	
	if(length(ss2)>0) {
        hist( ss2 , main= colnames(pregnF)[i], col="blue",    breaks = c(0, 0.002, 0.003, 0.004, 0.005, 0.006, 0.008)  )	
	}else {
	   hist(  c(-1,-1)  )
	}

	readline()
 	}