complete<-function(directory, id=1:332) {
  result <- data.frame(id=numeric(length(id)), nobs=numeric(length(id)))
  for (i in 1:length(id)) {
    ## to read the file, I decided to concat two zeros at first and the
    ## just read the last three characters for file name
    ## in this way I will have names such as 00321 or 0045 at the first stage
    ## but I will read files 321 and 045 at last
    twoZeros<-paste("0", "0", id[i], sep="")
    charLength<-nchar(twoZeros)
    fileName<-substr(twoZeros, charLength-2, charLength)
    data<-read.csv(paste(directory, "/", fileName, ".csv", sep=""))
    ## counting the available records of both pollutants
    result[i,1]<-id[i]
    result[i,2]<-length(data[!is.na(data[,"nitrate"]) & 
                               !is.na(data[,"sulfate"]), "nitrate"])
  }
  result  
}