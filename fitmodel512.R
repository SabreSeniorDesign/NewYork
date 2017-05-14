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
data = fread ("Desktop/Hotel CCM/data/Hotel_shop_amenities_full1.csv", header = TRUE, nrows = -1, stringsAsFactors = F,colClasses = col.classes)
setnames(data, col.names[col.classes != "NULL"])

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
#install.packages("forcats")
#library(forcats)
#data$rating <- fct_explicit_na(data$rating)


#how many are we losing?
complete.cases(data)
sum(complete.cases(data))
#72 lost because the session did not have the hotel rating on it
#7206/7278=  99% data is retained... good to go
data = data[complete.cases(data)]

#trying to ad numID
#$id <- as.numeric(as.factor(data$sessID))
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
avgPrice_perSession <- aggregate (minRate ~ transID,
                                  data = cluster2,
                                  mean)

# Now have average price of ALL properties looked at in a session
names (avgPrice_perSession) <- c ("NumericID", "avgPrice_allProps")

#merge with data set
data <- merge (data, avgPrice_perSession,
                   by = "transID")



####################################### I. FIT MLOGIT I  ###################################################
cat("\nFitting model MLOGIT1... \n")
t.ptm <- proc.time()
#proc.time determines cpu time to run a model 

setkey(data_train, NumericID, propID)
f1 = mFormula(booked ~ -1 + spotlight + 
                wifi + pool + shuttle + fitness + breakfast + restaurant + parking +
                rating*fitness + 
                rating*breakfast + rating*restaurant)

ccm1 = mlogit(f1,
              data = data_train[list (NumericID, propID, booked, spotlight, 
                                       wifi, pool, shuttle, fitness, breakfast, restaurant, parking, rating)], 
              shape = "long", alt.var = "propID", chid.var = "NumericID")

summary(ccm1)



#dont use both diffprice and ratio2
#logodds ratio --> look up. log of the odds ratio.
#predict(mlogit) and give it the model ccm1... use what norbert did

