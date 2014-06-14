fileurl<-"http://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
if (!file.exists("data")) {
  dir.create("data")
}
## For https we have to use method "curl"
## But here it didn't work with download.file with error download had nonzero exit status
## I found the method below without using https and it worked 
x<-read.csv(fileurl)
write.csv(x, "data/cameras.csv")
dateDownloaded<-date()
