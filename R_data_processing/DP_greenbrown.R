# Data import from GEE
#c <- read.csv("C:/Users/Danilchyk/Documents/UK/_DP/Scripts/DATA_GEE/2_Belec/borovice_NDMI.csv", encoding="UTF-8", header=FALSE)
#c <- read.csv("C:/Users/Danilchyk/Documents/UK/_DP/Scripts/DATA_GEE/5_Medenec/buk_NDMI.csv", encoding="UTF-8", header=FALSE)
#c <- read.csv("C:/Users/Danilchyk/Documents/UK/_DP/Scripts/DATA_GEE/4_Frydlant/habr_NDVI.csv", encoding="UTF-8", header=FALSE)
#c <- read.csv("C:/Users/Danilchyk/Documents/UK/_DP/Scripts/DATA_GEE/6_Vranovice/jasan_NDMI.csv", encoding="UTF-8", header=FALSE)
#c <- read.csv("C:/Users/Danilchyk/Documents/UK/_DP/Scripts/DATA_GEE/3_Modrava/smrk_NDRE.csv", encoding="UTF-8", header=FALSE)
#c <- read.csv("C:/Users/Danilchyk/Documents/UK/_DP/Scripts/DATA_GEE/1_Chribska/dub_NDVI.csv", encoding="UTF-8", header=FALSE)

library(splitstackshape)


# Split of import into 2 columns
split <- strsplit((c[,]),";")
u <- unlist(split)

date <- u[seq(1, length(u), 2)]
date <- date[-1]

ndvi <- u[seq(2, length(u), 2)]
ndvi <- ndvi[-1]
ndvi <- as.numeric(ndvi)


dt <- list()
for(i in date){
  dd <- substr(i,1:7,8)
  #return(dd)
  for(n in 1:length(dd)){
    dt <- c(dt,dd[n])
  }
}
dt 
dt <- unlist(dt)
xd <- as.Date(dt,tryFormats = c("%Y%m%d"))
str(xd)
xd

# Seperate data by years
year17 <- grep("^2017",xd,value=TRUE)
year17 <- as.Date(year17)
year18 <- grep("^2018",xd,value=TRUE)
year18 <- as.Date(year18)
year19 <- grep("^2019",xd,value=TRUE)
year19 <- as.Date(year19)
year20 <- grep("^2020",xd,value=TRUE)
year20 <- as.Date(year20)

## borovice. 2017 - 1:7, 2018 - 8:34, 2019 - 35:57, 2020 - 58:73 
## buk. 2017 - 1:5, 2018 - 6:42, 2019 - 43:80, 2020 - 81:115 
## habr. 2017 - 1:14, 2018 - 15:61, 2019 - 62:99, 2020 - 100:135  
## jasan_dub_ld. 2017 - 1:18, 2018 - 19:62, 2019 - 63:104, 2020 - 105:149 
## jasan_dub_lz. 2017 - 1:21, 2018 - 22:70, 2019 - 71:119, 2020 - 120:171 
## jasan_vr. 2017 - 1:16, 2018 - 17:55, 2019 - 56:93, 2020 - 94:137
## smrk. 2017 - 1:6, 2018 - 7:22, 2019 - 23:41, 2020 - 42:58
## dub1. 2017 - 1:10, 2018 - 11:52, 2019 - 53:89, 2020 - 90:120 
ndvi17 <- ndvi[1:14]
ndvi18 <- ndvi[15:61]
ndvi19 <- ndvi[62:99]
ndvi20 <- ndvi[100:135]
coll17 <- data.frame(date=year17,ndvi=ndvi17)
coll18 <- data.frame(date=year18,ndvi=ndvi18)
coll19 <- data.frame(date=year19,ndvi=ndvi19)
coll20 <- data.frame(date=year20,ndvi=ndvi20)

# Data preparation
coll <- data.frame(date=xd,ndvi=ndvi)


# Plot
plot(coll$date,coll$ndvi,type="b", col="black",
     yaxt="n", lty=3, xlab="", ylab="")
title(main="NDVI Time Series", col.main="red", col.sub="blue",
      xlab="Dates of observation", ylab="NDVI values",
      col.lab="black", cex.lab=0.75)


##################### Test methods


#### Package Greenbrown
library(greenbrown)
#potential outliers removal
coll18 <- coll18[-c(1:3),]
coll19 <- coll19[-c(1:4),]
coll20 <- coll20[-c(3,5),]

ts <- coll18[,2]

#fitting DL function to the VI time series (ts)
fitlb <- FitDoubleLogBeck(ts(ts), 1:length(ts(ts)),1:length(ts(ts)),weighting = TRUE,hessian=FALSE, plot=TRUE)
#fitle <- FitDoubleLogElmore(ts(ts), 1:length(ts(ts)),1:length(ts(ts)),weighting = TRUE,hessian=FALSE, plot=TRUE)

#Retrieving SOS and EOS breakpoints using derivatives method
PhenoDeriv(fitlb$predicted, plot=TRUE)
#PhenoDeriv(fitle$predicted, plot=TRUE)

coll18[c(8,36),]
coll19[c(13,36),]
coll20[c(14,35),]

#Retrieving SOS and EOS using derivatives on raw data
PhenoDeriv(ts, plot=TRUE)
coll18[c(4,36),]
coll19[c(12,38),]
coll20[c(17,42),]







