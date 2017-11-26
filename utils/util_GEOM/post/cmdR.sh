data  <- read.table("2008_A7NI.txt",header=TRUE,na.strings="-999")
dataA7 <- na.omit(data)
summary(dataA7)
dataRous <- read.table("2008_ROUSSILLON.txt",header=TRUE,na.strings="-999")
summary(dataRous)
dataSab <- read.table("2008_SABLONS.txt",header=TRUE,na.strings="-999")
summary(dataSab)

dataNoxA7 <- (46/30)*dataA7$NO+dataA7$NO2
summary(dataNoxA7)

dataNoxRous <- (46/30)*dataRous$NO+dataRous$NO2
summary(dataNoxRous)
dataNoxSab <- (46/30)*dataSab$NO+dataSab$NO2
summary(dataNoxSab)


input <- function(F)
{
data <- read.table(F,header=TRUE,na.strings="-999")
dataF <- na.omit(data)
summary(dataF)
}
calcnox <- function(F)
{
data <- read.table(F,header=TRUE,na.strings="-999")
dataF <- na.omit(data)
dataNoxF <- (46/30)*dataF$NO+dataF$NO2
summary(dataNoxF)
}


fond1 <- read.table("fond_2008.dat" ,header=FALSE,na.strings="-999")
fond <- na.omit(fond1)
summary(fond)
Noxfond <- (46/30)*fond$V3+fond$V4
summary(Noxfond)


input("2008_A7NI.txt")
calcnox("2008_A7NI.txt")
input("2008_ROUSSILLON.txt")
calcnox("2008_ROUSSILLON.txt")
input("2008_SABLONS.txt")
calcnox("2008_SABLONS.txt")

