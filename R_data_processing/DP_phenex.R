# Data import from GEE
#c <- read.csv("C:/Users/Danilchyk/Documents/UK/_DP/Scripts/DATA_GEE/2_Belec/borovice_NDRE.csv", encoding="UTF-8", header=FALSE)
#c <- read.csv("C:/Users/Danilchyk/Documents/UK/_DP/Scripts/DATA_GEE/5_Medenec/buk_NDVI.csv", encoding="UTF-8", header=FALSE)
#c <- read.csv("C:/Users/Danilchyk/Documents/UK/_DP/Scripts/DATA_GEE/4_Frydlant/habr_NDMI.csv", encoding="UTF-8", header=FALSE)
#c <- read.csv("C:/Users/Danilchyk/Documents/UK/_DP/Scripts/DATA_GEE/6_Vranovice/jasan_NDVI.csv", encoding="UTF-8", header=FALSE)
#c <- read.csv("C:/Users/Danilchyk/Documents/UK/_DP/Scripts/DATA_GEE/3_Modrava/smrk_NDMI.csv", encoding="UTF-8", header=FALSE)
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
ndvi17 <- ndvi[1:16]
ndvi18 <- ndvi[17:55]
ndvi19 <- ndvi[56:93]
ndvi20 <- ndvi[94:137]
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

## Phenex (method = DLogistic / LinIP / Spline / DSig / Gauss / GaussMix / Growth / FFT / SavGol)
library(phenex)

#########  2018  #######
coll18 <- data.frame(date=year18,ndvi=ndvi18)
coll18 <- coll18[-c(1),] #potential outliers removing
max(coll18[,2])-min(coll18[,2])

#non-smoothed data
nocorr <- new("NDVI",values=coll18[,2],year=as.integer(2018))
res <- modelValues(nocorr,multipleSeasons = FALSE,method = "gauss",
                   asym=TRUE,MARGIN=2, doParallel=FALSE, silent=TRUE)
plot(res)

greenup <- phenoPhase(res, phase="greenup", method="local", 
                      threshold=0.32, n=1000)
greenup
coll18[6,]  #the result obtained after calling variable greenup
senesc.ndvi <- phenoPhase(res,phase="senescence",
                          method="local",threshold=0.32, n=1000)
senesc.ndvi 
coll18[29,] #the result obtained after calling variable senesc.ndvi

#smoothed data (Running average)
model18 <- modelNDVI(ndvi.values = coll18[,2],year.int = 2018,
                     multipleSeasons = FALSE,correction = 'ravg', window=5,
                     method = "gauss",asym=TRUE,MARGIN=2, doParallel=FALSE, 
                     silent=TRUE)
for (ndvi.ob in model18){ plot(ndvi.ob) }
title("NDMI TS Gaussian Asymmetric function for 2018")
legend("topright",legend=c("Original values","Modelled values","Fitted model"),
       pch=c(1,1,NA),col=c('black','red','blue'),lty=c(NA,NA,1),cex=0.6,
       inset=0.085)
greenup <- phenoPhase(model18[[1]], phase="greenup", method="local", 
                      threshold=0.32, n=1000)
greenup
coll18[8,]  #
senesc.ndvi <- phenoPhase(model18[[1]],phase="senescence",method="local",
                          threshold=0.32, n=1000)
senesc.ndvi
coll18[28,]

##########  2019  ########
#smrk 2019 snih !
coll19 <- data.frame(date=year19,ndvi=ndvi19)
coll19 <- coll19[-c(1:7),] 
max(coll19[,2])-min(coll19[,2])

#non-smoothed data
nocorr <- new("NDVI",values=coll19[,2],year=as.integer(2019))
res <- modelValues(nocorr,multipleSeasons = FALSE,method = "gauss",asym=TRUE,MARGIN=2, doParallel=FALSE, silent=TRUE)
plot(res)

greenup <- phenoPhase(res, phase="greenup", method="local", threshold=0.35, n=1000)
greenup
coll19[11,]
senesc.ndvi <- phenoPhase(res,phase="senescence",method="local",threshold=0.35, n=1000)
senesc.ndvi
coll19[25,]

#smoothed data (Running average)
model19 <- modelNDVI(ndvi.values = coll19[,2],year.int = 2019,
                     multipleSeasons = FALSE,correction = 'ravg', window=5,
                     method = "Gauss",asym=TRUE,MARGIN=2, doParallel=FALSE, silent=TRUE)
for (ndvi.ob in model19){ plot(ndvi.ob) }
title("NDMI TS Gaussian Asymmetric function for 2019")
legend("topleft",legend=c("Original values","Modelled values","Fitted model"),pch=c(1,1,NA),col=c('black','red','blue'),lty=c(NA,NA,1),cex=0.5,inset=0.07)
greenup <- phenoPhase(model19[[1]], phase="greenup", method="local", threshold=0.35, n=1000)
greenup
coll19[2,]
senesc.ndvi <- phenoPhase(model19[[1]],phase="senescence",method="local",threshold=0.35, n=1000)
senesc.ndvi
coll19[7,]


########## 2020 ############
coll20 <- data.frame(date=year20,ndvi=ndvi20)
coll20 <- coll20[-c(1:3),] 
max(coll20[,2])-min(coll20[,2])

#non-smoothed data
nocorr <- new("NDVI",values=coll20[,2],year=as.integer(2020))
res <- modelValues(nocorr,multipleSeasons = FALSE,method = "gauss",asym=TRUE,MARGIN=2, doParallel=FALSE, silent=TRUE)
plot(res)

greenup <- phenoPhase(res, phase="greenup", method="local", threshold=0.32, n=1000)
greenup
coll20[15,]
senesc.ndvi <- phenoPhase(res,phase="senescence",method="local",threshold=0.32, n=1000)
senesc.ndvi
coll20[37,]

#smoothed data (Running average)
model20 <- modelNDVI(ndvi.values = coll20[,2],year.int = 2020,
                     multipleSeasons = FALSE,correction = 'ravg', window=5,
                     method = "Gauss",asym=TRUE,MARGIN=2, doParallel=FALSE, silent=TRUE)
for (ndvi.ob in model20){ plot(ndvi.ob) }
title("NDMI TS Gaussian Asymmetric function for 2020")
legend("topleft",legend=c("Original values","Modelled values","Fitted model"),pch=c(1,1,NA),col=c('black','red','blue'),lty=c(NA,NA,1),cex=0.55,inset=0.07)
greenup <- phenoPhase(model20[[1]], phase="greenup", method="local", threshold=0.32, n=1000)
greenup
coll20[11,]
senesc.ndvi <- phenoPhase(model20[[1]],phase="senescence",method="local",threshold=0.32, n=1000)
senesc.ndvi
coll20[42,]

