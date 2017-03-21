# Another file (read in later) is a big file with look up
# data on each hotel property (not just NYC) with what
# attributes (e.g. swimming pool, restaurant) are available
# at each property.  This data will be merged with (joined
# to) the main shop/book data later

directory <- "/Users/Shelb/Documents/"
filename  <- "ChoiceDat1.txt"
fullFile  <- paste (directory, filename, sep = "/")

# Data looks like:
# RequestStartDate|AdvancePurchase|SessionId|TransactionID|Length_Of_Stay|shoppedproperty|Hotel_Property_Id|City_Cd|Booking_Dt|Service_Start_Dt|/|Property_Latitude|Property_Longitude|bookedind|HotelChainCode|lowestroomprice|Rating|Total_display_props|DisplayLineNbr|PrefSubscriptionType
# 5/19/2015|1|CEF84998BB831F27EB38B1A000000000|EB38B1A0|1|164,800|34,610|NYC|5/19/2015|5/20/2015|9|40.743840000000000|-73.992960000000000|0|HX|329.0000|2CROWN|21|4|?
# 4/29/2015|5|CEDED0ACB054AF2EB3C6CEE000000000|B3C6CEE0|1|12,075|5,261|NYC|4/29/2015|5/4/2015|3|40.754954000000000|-73.973224000000000|0|DT|369.0000|3CROWN|278|17|?
# 5/11/2015|24|CEEE6CEBA2A654A5681271C000000000|681271C0|1|1,329|25,015|NYC|5/11/2015|6/4/2015|3|40.709734000000000|-74.014737000000000|0|MC|549.0000|4CROWN|19|2|SPOT

# So can read dates directly

setClass ("myDate")
setAs ("character",
       "myDate",
       function (from) as.Date (from, format = "%m/%d/%Y"))

varNames <- c ("shopDate",           # RequestStartDate, which is same as Booking_Dt
               "advPurch",           # AdvancePurchase = checkin_date minus shopping_date
               "sessionId",
               "transactionId",      # a shopping session is given by a unique combination
               # of sessionId, transactionId, and checkin date
               "lengthStay",         # Length_Of_Stay
               "shoppedproperty",    # not using this
               "propId",             # Hotel_Property_Id
               "city",               # City_Cd
               "Booking_Dt",         # same as shopDate
               "checkin",            # Service_Start_Dt
               "propCount_wrong",    # not sure what this is
               "lat",                # Property_Latitude, not read in for choice model
               # used for graphing on map
               "long",               # Property_Longitude
               "booked",             # booked indicator 0/1
               "chain",              # HotelChainCode
               "price_char",         # lowestroomprice = price of room, sometimes a ?
               "crowns",             # number of crowns property rating, e.g. 2CROWNS
               "propCount_char",     # Total_display_props, number of properties displayed, sometimes a ?
               "lineNum_char",       # DisplayLineNbr, line number this property, sometimes this is a ?
               "spotlight_member")   # PrefSubscriptionType, is this a Spotlight property
# either a ? or the word SPOT

#myDate below is a user defined class (defined above)

varFormats <- c ("myDate",    # shopDate
                 "numeric",   # advPurch
                 "character", # sessionId
                 "character", # TransactionId
                 "numeric",   # lengthStay
                 "NULL",      # shoppedProperty
                 "character", # propId
                 "character", # city
                 "NULL",      # Booking_Dt
                 "myDate",    # checkin
                 "NULL",      # propCount_wrong
                 "numeric",   # lat
                 "numeric",   # long
                 "numeric",   # booked
                 "character", # chain
                 "character", # price_char
                 "character", # stars
                 "character", # propCount_char
                 "character", # lineNum_char, sometimes this is a ?
                 "character") # spotlight_member

# Received "embedded nulls" error message weirdness, so fileEncoding
# See http://stackoverflow.com/questions/4806823/how-to-detect-the-right-encoding-for-read-csv
# and http://stackoverflow.com/questions/24734911/warning-message-line-appears-to-contain-embedded-nulls

mainData <- read.csv (fullFile,
                      stringsAsFactors = FALSE,
                      nrow = -1,
                      sep = "|",
                      fileEncoding = "UCS-2LE",
                      col.names = varNames,
                      colClasses = varFormats,
                      na.strings = "NA")

mainData <- within (mainData,   {
  lineNum      <- ifelse (lineNum_char == "?", NA,
                          lineNum_char)
  lineNum      <- as.numeric (lineNum)
  lineNum_char <- NULL
  
  price      <- ifelse (price_char == "?", NA,
                        price_char)
  # Fine, except some prices have commas
  #took away commas below 
  price      <- as.numeric (gsub (",", "", price))
  price_char <- NULL
  
  propcount      <- ifelse (propCount_char == "?", NA,
                            propCount_char)
  propcount      <- as.numeric (gsub (",", "", propcount))
  propCount_char <- NULL
  
  spotlight        <- ifelse (spotlight_member == "SPOT", 1, 0)
  spotlight_member <- NULL
  
  # make advance purchase into a factor (categorical) variable, by days 
  APcat <- cut (advPurch, breaks = c (-1, 3, 7, 14, Inf),
                labels = c ("0 to 3",
                            "4 to 7",
                            "8 to 14",
                            "15+"))
  
  LOScat <- cut (lengthStay, breaks = c (-1, 3, 7, Inf),
                 labels = c ("0 to 3",
                             "4 to 7",
                             "8+"))
  
  crowns <- ifelse (crowns == "?", NA, crowns)
  
  # I don't know how property ID got a comma in it
  propId <- gsub (",", "", propId)
  
  # Bharadwaj also created a Manhattan (the good part)
  # indicator called Box.  I am just re-using his
  # lat and long numbers
  box  <- ifelse (lat  >=  40.74 & lat  <=  40.78 &
                    long >= -74.01 & long <= -73.96,    1, 0)
  lat  <- NULL
  long <- NULL
  
  # booked = dependent var in choice model must be 0 or 1
  booked <- ifelse (booked %in% c (0, 1), booked, NA)
})

# For choice models, each shopping session = unique combination of
# session ID, transaction ID, and checkin date must have exactly
# one 1 (booking) and all others 0

checkBooked <- aggregate (booked ~ sessionId +
                            transactionId +
                            checkin,
                          data = mainData,
                          sum)

goodSession <- subset (checkBooked,
                       booked == 1,  # this is really sum of booked column
                       # for this shopping session.  Want
                       # this to be exactly 1
                       select = c (sessionId,
                                   transactionId,
                                   checkin))

mainData <- merge (mainData, goodSession,
                   by = c ("sessionId",
                           "transactionId",
                           "checkin"))

# OK, now start doing some segmentation, so we can fit choice models
# by segment

# To do this, only want one observation per shopping session, to
# look at attributes such as advance purchase and length of stay
# which are repeated for each row of a shopping session

# Maybe look to see of patterns in day-of-week of shop, checkin, checkout

forSegment    <- subset (mainData,
                         booked == 1,
                         select = c (shopDate,
                                     checkin,
                                     advPurch,
                                     lengthStay))

forSegment <- within (forSegment,   {
  DOW_shop     <- weekdays (shopDate,             abbreviate = TRUE)
  DOW_checkin  <- weekdays (checkin,              abbreviate = TRUE)
  DOW_checkout <- weekdays (checkin + lengthStay, abbreviate = TRUE)
})

praxSeg    <- subset (mainData,
                         booked == 0 & lengthStay < 69,
                         select = c (price,
                                     advPurch,
                                     lengthStay))

# For now, just some quick looking at the data = simple tables and
# graphs.  Maybe later try fancy statistical clustering techniques, or
# not, depending on what simple approach is able to find

# Also, need to merge (join) in property attribute data
# keeping just top 7 amenities Bharwadaj used
