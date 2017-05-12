


#install packages
install.packages("data.table")
install.packages("mlogit")


### Prevent the console from being frozen, ensures that the display of output in the console is current
flush.console()

## Directory settings
# i dont think i am going to use this - olivia  
#cur.dir       = file.path("Users", "oliviabuerkle", "Documents", "Hotel CCM", 
sep = .Platform$file.sep
#data.dir      = "data"
#output.dir    = "output"

### Check and load packages
library(data.table)
library(mlogit)
#library(glmnet)
#library(survival)

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
    "lon",
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
    "character",
    "character",
    "character",#checkinhdate
    "numeric", #advpurchase
    )

######################stopped here##############
col.classes = 
  c("character", #01.Rating
    "character", #02.SessionId
    "character", #03.TransactionId
    "character", #04.City_Cd
    "character", #05.Hotel_Chain
    "character", #06.SpotLight_Member
    "character", #07.SearchDate
    "numeric",   #08.AdvPurch
    "numeric",   #09.LengthStay
    "character", #10.Prop_Code
    "character", #11.BookedProp
    "character", #12.CheckinDate
    "numeric",   #13.PropCnt
    "numeric",   #14.Property_Latitude
    "numeric",   #15.Property_Longitude
    "numeric",   #16.Booked
    "numeric",   #17.Price
    "numeric",   #18.NumProps_onScreen
    "numeric",   #19.Line_Num
    "numeric",   #20.SpotLight
    "character", #21.AdvPurchCat
    "character", #22.LengthStayCat
    "numeric",   #23.Rating_Num
    "numeric",   #24.Highest_LineNum_this_shop
    "character", #25.MaxLineNum",
    "numeric",   #26.Ratio2
    "numeric",   #27.Num_Spot_Member
    "numeric",   #28.NumSpot_Interaction
    "numeric",   #29.Ratio
    "numeric",   #30.NumericId
    "numeric",   #31.DiffPrice
    "character", #32.NumProps_Cat
    "integer",   #33.Wifi
    "integer",   #34.Pool
    "integer",   #35.Shuttle
    "integer",   #36.Fitness
    "integer",   #37.Breakfast
    "integer",   #38.Restaurant
    "integer"    #39.Parking
  )

### Read in hotel shopping data
data = fread ("/users/shelb/Documents/Hotel_shop_amenities_full.csv", header = TRUE, nrows = -1, stringsAsFactors = F,colClasses = col.classes)
setnames(data, col.names[col.classes != "NULL"])


# i didnt use this method
#hotelData <- read.csv("Desktop/Hotel CCM/data/Hotel_shop_amenities.csv",
#                      +                       header = TRUE, nrows = -1)


### Clean data
data = data[complete.cases(data)]

### Get seach data
data = data[, SearchDate := as.Date(SearchDate, format = "%m/%d/%y")]

### not necessary 
#Add time and status (censoring) variable 
### so we can fit Cox proportional hazards model as CCM
data[, time:=ifelse(Booked==1,1,2)]
data[, status:=as.numeric(time==1)] #0:censored, 1:recurrence

### Number of amenities
data[, amenities:=Wifi+Pool+Shuttle+Fitness+Breakfast+Restaurant+Parking]
data[, stars:=ifelse(Rating_Num==".", 0, as.numeric(Rating_Num))]

data[, amenities2:=8-amenities]
data[, stars2:=6-stars]
data[, spotlight2:=1-SpotLight]

### Sort
setkey(data, SearchDate, NumericId, Prop_Code)

### Unique dates
dates = sort(data[, unique(SearchDate)])

### Select test and validation data
insample_rate = 0.7
data_train = data[SearchDate <= dates[round(insample_rate*length(dates))]]
data_valid = data[SearchDate > dates[round(insample_rate*length(dates))]]
#rm(data)
gc()





####################################### I. FIT MLOGIT I  ###################################################
cat("\nFitting model MLOGIT1... \n")
t.ptm <- proc.time()
#proc.time determines cpu time to run a model 

setkey(data_train, NumericId, Prop_Code)
f1 = mFormula(Booked ~ -1 + Ratio2 + SpotLight + Line_Num + 
                Wifi + Pool + Shuttle + Fitness + Breakfast + Restaurant + Parking +
                Rating*Fitness + 
                Rating*Breakfast + Rating*Restaurant + 
                NumSpot_Interaction)
ccm1 = mlogit(f1,
              data = data_train[, list(NumericId, Prop_Code, Booked, Ratio2, SpotLight, Line_Num, 
                                       Wifi, Pool, Shuttle, Fitness, Breakfast, Restaurant, Parking, 
                                       NumSpot_Interaction, Rating)], 
              shape = "long", alt.var = "Prop_Code", chid.var = "NumericId")

summary(ccm1)



#dont use both diffprice and ratio2
#logodds ratio --> look up. log of the odds ratio.
#predict(mlogit) and give it the model ccm1... use what norbert did

