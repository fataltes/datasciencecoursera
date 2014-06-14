corr<-function(directory, threshold=0){
  result<-vector(mode="numeric", length=0)
  for (i in 1:332) {
    ## to read the file, I decided to concat two zeros at first and the
    ## just read the last three characters for file name
    ## in this way I will have names such as 00321 or 0045 at the first stage
    ## but I will read files 321 and 045 at last
    twoZeros<-paste("0", "0", i, sep="")
    charLength<-nchar(twoZeros)
    fileName<-substr(twoZeros, charLength-2, charLength)
    data<-read.csv(paste(directory, "/", fileName, ".csv", sep=""))
    ## getting all data except NAs
    availableData<-data[complete.cases(data[,c("sulfate", "nitrate")]), 
                        c("sulfate", "nitrate")]
    ## data[!is.na(data[,"nitrate"]) & 
    ##                      !is.na(data[,"sulfate"]), c("sulfate", "nitrate")]
    ## computing the correlation between sulfate and nitrate
    if(length(availableData[,"sulfate"])>threshold){
      result<-c(result,cor(availableData[,1], availableData[,2]))
    }
  }
  result
}