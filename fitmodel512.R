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
data = fread ("/users/shelb/Documents/GitHub/Hotel_shop_amenities_full.csv", header = TRUE, nrows = -1, stringsAsFactors = F,colClasses = col.classes)
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
                wifi + pool + shuttle + fitness + breakfast + restaurant + parking +
                rating*fitness + rating*breakfast + rating*restaurant)

ccm3 = mlogit(f1,
              data = NYC1[,list(NumericID, propID, booked, spotlight, 
              wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
              shape = "long", alt.var = "propID", chid.var = "NumericID")

ccm2 = mlogit(f1, data=data_whole[,list(AP, LOS, minRate)], shape = "long", alt.var = "propID", chid.var = "NumericID")

summary(ccm1)



#logodds ratio --> look up. log of the odds ratio.
#predict(mlogit) and give it the model ccm1... 



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


setkey(dataNYC, NumericID, propID)
f1 = mFormula(booked ~ -1 + spotlight + wifi + pool + shuttle + breakfast + restaurant + parking)
ccm3 = mlogit(f1,
              data = dataNYC[,list(NumericID, propID, booked, spotlight, 
                                wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
              shape = "long", alt.var = "propID", chid.var = "NumericID")


#cluster1
NYC1 <- dataNYC[which (dataNYC$AP < 10 & dataNYC$avgPrice_allProps < NYCprice),]

#cluster2
NYC2 <- dataNYC[which (dataNYC$AP < 10 & dataNYC$avgPrice_allProps > NYCprice),]

#cluster3
NYC3 <- dataNYC[which (dataNYC$AP >= 10),]



##ATL
dataATL <- data[which(data$cityCd == "ATL"),]
#need to calculate average price of a hotel in ATL
ATLprice <- mean(dataATL$minRate)


#cluster1
ATL1 <- dataATL[which (dataATL$AP >15 & dataATL$LOS >3 & dataATL$avgPrice_allProps < ATLprice),]


#cluster2
ATL2 <- dataATL[which (dataATL$AP >15 & dataATL$LOS >3 &  dataATL$avgPrice_allProps > ATLprice),]

#cluster3
ATL3 <- dataATL[which (dataATL$AP > 10 & dataATL$LOS < 3 &  dataATL$avgPrice_allProps < ATLprice),]


#cluster4
ATL4 <- dataATL[which (dataATL$AP > 10 & dataATL$LOS < 3 &  dataATL$avgPrice_allProps > ATLprice),]


#cluster5
ATL5 <- dataATL[which (dataATL$AP > 5 & dataATL$LOS < 5),]


#cluster6
ATL6 <- dataATL[which (dataATL$AP < 5 & dataATL$LOS < 3),]

##LAX AREA
dataLAX <- data[which(data$cityCd == "BUR" | data$cityCd == "LGB" | data$cityCd == "SNA" | data$cityCd == "LAX"),]
#need to calculate average price of a hotel in BUR
LAXprice <- mean(dataLAX$minRate)

#cluster1
LAX1 <- dataLAX[which (dataLAX$AP >15 & dataLAX$LOS >3 & dataLAX$avgPrice_allProps < LAXprice),]


#cluster2
LAX2 <- dataLAX[which (dataLAX$AP >15 & dataLAX$LOS >3 &  dataLAX$avgPrice_allProps > LAXprice),]

#cluster3
LAX3 <- dataLAX[which (dataLAX$AP > 10 & dataLAX$LOS < 3 &  dataLAX$avgPrice_allProps < LAXprice),]


#cluster4
LAX4 <- dataLAX[which (dataLAX$AP > 10 & dataLAX$LOS < 3 &  dataLAX$avgPrice_allProps > LAXprice),]


#cluster5
LAX5 <- dataLAX[which (dataLAX$AP > 5 & dataLAX$LOS < 5),]


#cluster6
LAX6 <- dataLAX[which (dataLAX$AP < 5 & dataLAX$LOS < 3),]



##CHI
dataCHI <- data[which(data$cityCd == "CHI"),]
#need to calculate average price of a hotel in CHI
CHIprice <- mean(dataCHI$minRate)


#cluster1
CHI1 <- dataCHI[which (dataCHI$AP >15 & dataCHI$LOS >3 & dataCHI$avgPrice_allProps < CHIprice),]


#cluster2
CHI2 <- dataCHI[which (dataCHI$AP >15 & dataCHI$LOS >3 &  dataCHI$avgPrice_allProps > CHIprice),]

#cluster3
CHI3 <- dataCHI[which (dataCHI$AP > 10 & dataCHI$LOS < 3 &  dataCHI$avgPrice_allProps < CHIprice),]


#cluster4
CHI4 <- dataCHI[which (dataCHI$AP > 10 & dataCHI$LOS < 3 &  dataCHI$avgPrice_allProps > CHIprice),]


#cluster5
CHI5 <- dataCHI[which (dataCHI$AP > 5 & dataCHI$LOS < 5),]


#cluster6
CHI6 <- dataCHI[which (dataCHI$AP < 5 & dataCHI$LOS < 3),]
