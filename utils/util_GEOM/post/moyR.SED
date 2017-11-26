input <- function(F)
{
data <- read.table(F,header=TRUE,na.strings="-999")
dataF <- na.omit(data)
totomean <- summary(dataF)
write.table(totomean,file="mean.out")
}
calcnox <- function(F)
{
data <- read.table(F,header=TRUE,na.strings="-999")
dataF <- na.omit(data)
dataNoxF <- (46/30)*dataF$NO+dataF$NO2
totomean <- summary(dataNoxF)
write.table(totomean,file="meanNOx.out")
}

input("_FIC_")
calcnox("_FIC_")


