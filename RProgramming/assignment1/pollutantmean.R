pollutantmean<-function(directory, pollutant, id=1:332) {
  result <- matrix(0, nrow=length(id), ncol=2)
  for (i in 1:length(id)) {
    ## to read the file, I decided to concat two zeros at first and the
    ## just read the last three characters for file name
    ## in this way I will have names such as 00321 or 0045 at the first stage
    ## but I will read files 321 and 045 at last
    twoZeros<-paste("0", "0", id[i], sep="")
    charLength<-nchar(twoZeros)
    fileName<-substr(twoZeros, charLength-2, charLength)
    data<-read.csv(paste(directory, "/", fileName, ".csv", sep=""))
    ## sum of values of that pollutant
    result[i,1]<-sum(data[!is.na(data[,pollutant]), pollutant])
    ## number of valid values of that pollutant
    result[i,2]<-length(data[!is.na(data[,pollutant]), pollutant])
  }
  ## computing mean value of that pollutant
  pollutantMean<-sum(result[, 1])/sum(result[, 2])
  round(pollutantMean, 3)
}