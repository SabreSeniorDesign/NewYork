#install packages
install.packages("data.table")
install.packages("mlogit")


### Prevent the console from being frozen, ensures that the display of output in the console is current
flush.console()


### Check and load packages
library(data.table)
library(mlogit)


col.names = 
  c("propID",
    "sessID",
    "transID",
    "checkinDate",
    "AP",
    "LOS",
    "cityCd",
    "shopDate",
    "rating",
    "prefType",
    "long",
    "lat",
    "booked",
    "spotlight",
    "num_prop_display",
    "total_prop_display",
    "minRate",
    "wifi",
    "pool",
    "shuttle",
    "fitness",
    "breakfast",
    "restaurant",
    "parking")

col.classes =
  c("character", #propID
    "character", #sessID
    "character", #transID
    "character",#checkindate
    "numeric", #advpurchase
    "numeric", #lengthofstay
    "factor", #CityCd (only 8)
    "character", #shopDate
    "character", #rating
    "prefType", #character, **i think this should be deleted
    "numeric",   #Longitude
    "numeric",   #Latitude
    "numeric",   #Booked
    "numeric",   #SpotLight
    "numeric", #num_prop_display
    "numeric", #total_prop_display
    "numeric", #minRate
    "integer",   #wifi
    "integer",   #pool
    "integer",   #shuttle
    "integer",   #fitness
    "integer",   #breakfast
    "integer",   #restaurant
    "integer"    #parking
  )


### Read in hotel shopping data
#shelby
data = fread ("/users/shelb/Documents/Hotel_shop_amenities_full.csv", header = TRUE, nrows = -1, stringsAsFactors = F,colClasses = col.classes)
setnames(data, col.names[col.classes != "NULL"])

#olivia
#data = fread ("Desktop/Hotel CCM/data/Hotel_shop_amenities_full1.csv", header = TRUE, nrows = -1, stringsAsFactors = F,colClasses = col.classes)
#setnames(data, col.names[col.classes != "NULL"])

#before we do this lets make sure to remove the columns we dont need 
#removing these columns: num_prop_display, total_prop_display, pref_type
data$prefType <- NULL
data$num_prop_display <- NULL
data$total_prop_display <- NULL


### Clean data
sum(complete.cases(data))
#if we do this, we only retain 47% of data... what is solution

#1. assign NAs in spotlight to 0, assume if its not 1 it's 0 
data$spotlight[is.na(data$spotlight)] <- 0 
summary(data$spotlight)
#4966/7278 ... now at 68%

#2. assign NA hotel rates to average for that star
#need to delete outliers (the highest cost, all are in NY)

#this method is definitely not the most efficient

onestar <- data[which(data$rating == "1 CROWN"),]
summary(onestar)
#mean room is 80.60
data$minRate[is.na(data$minRate) & data$rating == "1 CROWN"] <- 80.60


twostar <- data[which(data$rating == "2 CROWN"),]
summary(twostar)
#mean room is 140.94
#300 NAs
data$minRate[is.na(data$minRate) & data$rating == "2 CROWN"] <- 140.94


threestar <- data[which(data$rating == "3 CROWN"),]
summary(threestar)
#mean room 168.4
#780 NAs
data$minRate[is.na(data$minRate) & data$rating == "3 CROWN"] <- 168.4


fourstar <- data[which(data$rating == "4 CROWN"),]
summary(fourstar)
#mean room 248.45
#850 NAs
data$minRate[is.na(data$minRate) & data$rating == "4 CROWN"] <- 248.45


fivestar <- data[which(data$rating == "5 CROWN" & data$minRate < 1000),]
summary(fivestar)
#mean room 435.5 
#166 NAs
data$minRate[is.na(data$minRate) & data$rating == "5 CROWN"] <- 435.5

data$rating <- as.factor(data$rating)
summary(data$rating)
addNA(data$rating)

#210 hotels do not have rating info 
#need to check if any of these were booked
data$rating[is.na(data$rating)] <- "unknown"

#olivia only 
install.packages("forcats")
library(forcats)
data$rating <- fct_explicit_na(data$rating)


#how many are we losing?
complete.cases(data)
sum(complete.cases(data))
#72 lost because the session did not have the hotel rating on it
#7206/7278=  99% data is retained... good to go
data = data[complete.cases(data)]

#trying to ad numID
data$NumericID <- c(as.factor(data$sessID))

### Get search data
data = data[, shopDate := as.Date(shopDate, format = "%m/%d/%y")]

### not necessary 
### so we can fit Cox proportional hazards model as CCM
#data[, time:=ifelse(booked==1,1,2)]
#data[, status:=as.numeric(time==1)] #0:censored, 1:recurrence

### Number of amenities
#data[, amenities:=wifi+pool+shuttle+fitness+breakfast+restaurant+parking]
#data[, stars:=ifelse(rating==".", 0, as.numeric(rating))]


### Sort
setkey(data, shopDate, NumericID, propID)

### Unique dates
#what does this mean, do we anticipate duplicates
#dates = sort(data[, unique(shopDate)])

#average price of each session
#use NumericID
avgPrice_perSession <- aggregate (minRate ~ NumericID,
                                  data = data,
                                  mean)

# Now have average price of ALL properties looked at in a session
names (avgPrice_perSession) <- c ("NumericID", "avgPrice_allProps")

#merge with data set
data <- merge (data, avgPrice_perSession,
                   by = "NumericID")

duplicated(data)
data <- unique(data, by= c("NumericID", "propID"))
data[ , N:= length(propID), by="NumericID"]
data = data[N > 1]
data[,sumbooked:= sum(booked), by="NumericID" ]
data = data[sumbooked ==1]
table(data$booked, data$NumericID)

####################################### I. FIT MLOGIT I  ###################################################

setkey(data, NumericID, propID)
f1 = mFormula(booked ~ -1 + spotlight + 
                wifi + pool + shuttle + fitness + breakfast + restaurant + parking)

data1 = mlogit(f1,
              data = data[,list(NumericID, propID, booked, spotlight, 
              wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
              shape = "long", alt.var = "propID", chid.var = "NumericID")
#whole dataset - data1
summary(ccm1)


########################### Creating Clusters 

##### NYC
dataNYC <- data[which(data$cityCd == "NYC"),]
dataNYC$cityCd <- NULL
#need to calculate average price of a hotel in NYC
NYCprice <- mean(dataNYC$minRate)

#what norbert and ross helped us write
#duplicated(dataNYC)
#dataNYC <- unique(dataNYC, by= c("NumericID", "propID"))
#dataNYC[ , N:= length(propID), by="NumericID"]
#dataNYC = dataNYC[N > 1]
#dataNYC[,sumbooked:= sum(booked), by="NumericID" ]
#dataNYC = dataNYC[sumbooked ==1]

#not enough data, singularity
#fix pool =2
#table(dataNYC$pool, dataNYC$wifi)
#table(dataNYC$booked, dataNYC$NumericID)


#combine SNA LGB BUR - 767
#atl, lax area, chicago, nyc 
#reconsider segmentation, wider bands - 3 or 4 
#difference between segments and cities

#####this is the mlogit that works######
setkey(dataNYC, NumericID, propID)
dataNYC1 = mlogit(f1,
              data = dataNYC[,list(NumericID, propID, booked, spotlight, 
                                wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
              shape = "long", alt.var = "propID", chid.var = "NumericID")
#saved at dataNYC1

#cluster1
NYC1 <- dataNYC[which (dataNYC$AP < 10 & dataNYC$avgPrice_allProps < NYCprice),]
setkey(NYC1, NumericID, propID)

dataNYCc1 = mlogit(f1,
                  data = NYC1[,list(NumericID, propID, booked, spotlight, 
                                       wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
                  shape = "long", alt.var = "propID", chid.var = "NumericID")
#saved as dataNYCc1


#cluster2
NYC2 <- dataNYC[which (dataNYC$AP < 10 & dataNYC$avgPrice_allProps > NYCprice),]

setkey(NYC2, NumericID, propID)

dataNYCc2 = mlogit(f1,
                   data = NYC2[,list(NumericID, propID, booked, spotlight, 
                                     wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
                   shape = "long", alt.var = "propID", chid.var = "NumericID")
#saved as dataNYCc2

#cluster3
NYC3 <- dataNYC[which (dataNYC$AP >= 10),]

setkey(NYC3, NumericID, propID)

dataNYCc3 = mlogit(f1,
                   data = NYC3[,list(NumericID, propID, booked, spotlight, 
                                     wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
                   shape = "long", alt.var = "propID", chid.var = "NumericID")
#saved as dataNYCc3

##ATL
dataATL <- data[which(data$cityCd == "ATL"),]
dataATL$cityCd <- NULL
#need to calculate average price of a hotel in ATL
ATLprice <- mean(dataATL$minRate)
setkey(dataATL, NumericID, propID)

dataATL1 = mlogit(f1,
                   data = dataATL[,list(NumericID, propID, booked, spotlight, 
                                     wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
                   shape = "long", alt.var = "propID", chid.var = "NumericID")

#cluster1
ATL1 <- dataATL[which (dataATL$AP < 10 & dataATL$avgPrice_allProps < ATLprice),]

setkey(ATL1, NumericID, propID)

dataATLc1 = mlogit(f1,
                  data = ATL1[,list(NumericID, propID, booked, spotlight, 
                                       wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
                  shape = "long", alt.var = "propID", chid.var = "NumericID")


#cluster2
ATL2 <- dataATL[which (dataATL$AP < 10 & dataATL$avgPrice_allProps > ATLprice),]
setkey(ATL2, NumericID, propID)

dataATLc2 = mlogit(f1,
                   data = ATL2[,list(NumericID, propID, booked, spotlight, 
                                     wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
                   shape = "long", alt.var = "propID", chid.var = "NumericID")


#cluster3
ATL3 <- dataATL[which (dataATL$AP >= 10),]

setkey(ATL3, NumericID, propID)

dataATLc3 = mlogit(f1,
                   data = ATL3[,list(NumericID, propID, booked, spotlight, 
                                     wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
                   shape = "long", alt.var = "propID", chid.var = "NumericID")



##LAX AREA
dataLAX <- data[which(data$cityCd == "BUR" | data$cityCd == "LGB" | data$cityCd == "SNA" | data$cityCd == "LAX"),]
dataLAX$cityCd <- NULL
#need to calculate average price of a hotel in BUR
LAXprice <- mean(dataLAX$minRate)

setkey(dataLAX, NumericID, propID)

dataLAX1 = mlogit(f1,
                  data = dataLAX[,list(NumericID, propID, booked, spotlight, 
                                       wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
                  shape = "long", alt.var = "propID", chid.var = "NumericID")


#cluster1
LAX1 <- dataLAX[which (dataLAX$AP < 10 & dataLAX$avgPrice_allProps < LAXprice),]

setkey(LAX1, NumericID, propID)

dataLAXc1 = mlogit(f1,
                   data = LAX1[,list(NumericID, propID, booked, spotlight, 
                                     wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
                   shape = "long", alt.var = "propID", chid.var = "NumericID")

#cluster2
LAX2 <- dataLAX[which (dataLAX$AP < 10 & dataLAX$avgPrice_allProps > LAXprice),]
setkey(LAX2, NumericID, propID)

dataLAXc2 = mlogit(f1,
                   data = LAX2[,list(NumericID, propID, booked, spotlight, 
                                     wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
                   shape = "long", alt.var = "propID", chid.var = "NumericID")

#cluster3
LAX3 <- dataLAX[which (dataLAX$AP >= 10),]
setkey(LAX3, NumericID, propID)

dataLAXc3 = mlogit(f1,
                   data = LAX3[,list(NumericID, propID, booked, spotlight, 
                                     wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
                   shape = "long", alt.var = "propID", chid.var = "NumericID")

##CHI
dataCHI <- data[which(data$cityCd == "CHI"),]
dataCHI$cityCd <- NULL
#need to calculate average price of a hotel in CHI
CHIprice <- mean(dataCHI$minRate)

setkey(dataCHI, NumericID, propID)

dataCHI1 = mlogit(f1,
                  data = dataCHI[,list(NumericID, propID, booked, spotlight, 
                                       wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
                  shape = "long", alt.var = "propID", chid.var = "NumericID")


#cluster1
CHI1 <- dataCHI[which (dataCHI$AP < 10 & dataCHI$avgPrice_allProps < CHIprice),]
setkey(CHI1, NumericID, propID)

dataCHIc1 = mlogit(f1,
                   data = CHI1[,list(NumericID, propID, booked, spotlight, 
                                     wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
                   shape = "long", alt.var = "propID", chid.var = "NumericID")
#cluster2
CHI2 <- dataCHI[which (dataCHI$AP < 10 & dataCHI$avgPrice_allProps > CHIprice),]
setkey(CHI2, NumericID, propID)

dataCHIc2 = mlogit(f1,
                   data = CHI2[,list(NumericID, propID, booked, spotlight, 
                                     wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
                   shape = "long", alt.var = "propID", chid.var = "NumericID")

#cluster3
CHI3 <- dataCHI[which (dataCHI$AP >= 10),]
setkey(CHI3, NumericID, propID)

dataCHIc3 = mlogit(f1,
                   data = CHI3[,list(NumericID, propID, booked, spotlight, 
                                     wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
                   shape = "long", alt.var = "propID", chid.var = "NumericID")


summary(dataATL1)
summary(dataATLc1)
summary(dataATLc2)
summary(dataATLc3)
summary(dataCHI1)
summary(dataCHIc1)
summary(dataCHIc2)
summary(dataCHIc3)
summary(dataLAX1)
summary(dataLAXc1)
summary(dataLAXc2)
summary(dataLAXc3)
summary(dataNYC1)
summary(dataNYCc1)
summary(dataNYCc2)
summary(dataNYCc3)

#########old clusters!!!!!!!!!!!!!!!########
#cluster1
#CHI1 <- dataCHI[which (dataCHI$AP >15 & dataCHI$LOS >3 & dataCHI$avgPrice_allProps < CHIprice),]


#cluster2
#CHI2 <- dataCHI[which (dataCHI$AP >15 & dataCHI$LOS >3 &  dataCHI$avgPrice_allProps > CHIprice),]

#cluster3
#CHI3 <- dataCHI[which (dataCHI$AP > 10 & dataCHI$LOS < 3 &  dataCHI$avgPrice_allProps < CHIprice),]


#cluster4
#CHI4 <- dataCHI[which (dataCHI$AP > 10 & dataCHI$LOS < 3 &  dataCHI$avgPrice_allProps > CHIprice),]


#cluster5
#CHI5 <- dataCHI[which (dataCHI$AP > 5 & dataCHI$LOS < 5),]


#cluster6
#CHI6 <- dataCHI[which (dataCHI$AP < 5 & dataCHI$LOS < 3),]
