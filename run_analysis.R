#!/usr/bin/R --verbose --quiet

#
# SUBSET TO VALID EVENT TYPES
#

## load valid event types as per section 2.1.1 "Storm Data Event Table" table 1 on page 6.
## National Weather Service Storm Data Documentation
## See also http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype
## and NWS Directive 10-1605.
eventtypes <- read.csv("eventtypes.csv", stringsAsFactors = FALSE)
eventtypes <- transform(eventtypes, eventtype = toupper(str_trim(eventtype)))

#
# LOAD RAW DATA
#

## download archive into local directory
require(utils)
require(stringr)
require(dplyr)

archiveName <- file.path("stormdata.csv.bz2")
if (!file.exists(archiveName)) {
    archiveUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(archiveUrl, destfile = archiveName, method = "curl", mode = "wb")
    print(paste(Sys.time(), "archive downloaded"))
}

## load into data frame and convert date column to date
### REBUILD FROM SCRATCH
if(is.null(stormdata)) {
    stormdata <- read.csv(archiveName, stringsAsFactors = FALSE)
}

################################################################################

## PRELIMINARY
# What data to include
# histogram of storm activity measurements
data <- stormdata[, c("EVTYPE", "STATE", "BGN_DATE", "END_DATE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
names(data) <- tolower(names(data))
names(data) <- gsub("_", "", names(data))
data <- transform(data, bgndate = as.Date(bgndate, format= "%m/%d/%Y 0:00:00", tz = "C"))
data <- transform(data, enddate = as.Date(enddate, format= "%m/%d/%Y 0:00:00", tz = "C"))
data <- transform(data, year = strtoi(format(data$bgndate, "%Y")))
data <- transform(data, evtype = toupper(str_trim(evtype)))

# before histogram
histEvents <- hist(data$year)

## Use data from 1996 onwards as this is when standardised event types were defined:
## from http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype
## we can argue that really should look at dates after 1996 ...
data <- data %>% filter(year >= 1996)

# after histogram
histEvents <- hist(data$year)

#
# DAMAGE
#

## what are the damage exponents?
unique(data$propdmgexp)
unique(data$cropdmgexp)

## convert damage exponents to associated numerics
## see section 2.7 Damage, http://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf
data$propdmgexp <- strtoi(chartr("KMB", "369", data$propdmgexp))
data$cropdmgexp <- strtoi(chartr("KMB", "369", data$cropdmgexp))

## update damage using exponents
options(scipen = 20)
data$propdmg <- data$propdmg * 10^data$propdmgexp
data$cropdmg <- data$cropdmg * 10^data$cropdmgexp

## drop old exponents
data$propdmgexp <- NULL
data$cropdmgexp <- NULL

## (temp) what is total damage
data <- data %>% mutate(damage = propdmg + cropdmg)

#
# EVENT TYPES
#
## See section 2.1 Permitted Storm Data Events.
## load valid event types as per section 2.1.1 "Storm Data Event Table" table 1 on page 6.
## National Weather Service Storm Data Documentation
## See also http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype
## and NWS Directive 10-1605.
## See also FAQ "How accurate is the data?"
#
# data <- data[toupper(str_trim(data$evtype)) %in% eventtypes$eventtype, ]
data <- data %>% filter(evtype %in% eventtypes$eventtype)
histEvents <- hist(data$year)

## pretty printing of event types
data <- transform(data, evtype = str_to_title(evtype))

## dropped events
dropped <- data %>% filter(!(evtype %in% eventtypes$eventtype))
## ignore summaries
dropped <- dropped[!(grepl("summary", dropped$evtype, ignore.case = T)), ]
histEvents <- hist(dropped$year)


## what evtypes can be corrected?
#fixtypes <- data[!(toupper(data$evtype) %in% eventtypes$eventtype), "evtype"]
#unique(fixtypes)
#
# drop summaries
#data <- data[!(grepl("summary", data$evtype, ignore.case = T)), ]
#
# correct some obvious event types
#data <- transform(data, evtype = gsub("TSTM WIND/HAIL", "HAIL", evtype))
#data <- transform(data, evtype = gsub("TSTM", "THUNDERSTORM", evtype))


unique(data$year)

# Casualties per year and average per year
# is there a change in which is the top evtype each year?

## summary by evtype per year
health <- data %>%
    group_by(year, evtype) %>%
    summarise(casualty = sum(fatalities + injuries)) %>%
    arrange(year, desc(casualty))

# this has all the data I want to plot for casualities
# try with full set of event types and with just offical list
for (y in unique(health$year)) {
    x <- subset(health, year == y, c(year, evtype, casualty))
    print(x[1:5,])
}

## average evtype per year
health <- health %>%
    group_by(evtype) %>%
    summarise(annual = mean(casualty))

health[order(health$annual, decreasing = T)[1:5],]
#              evtype    annual
# 1           Tornado 1386.1250
# 2    Excessive Heat  511.7500
# 3             Flood  448.2500
# 4         Lightning  299.5000
# 5 Thunderstorm Wind  218.5714

#


################################################################################

## get valid data
data <- stormdata[stormdata$END_DATE != "", c("EVTYPE", "STATE", "BGN_DATE", "END_DATE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

## filter on valid event types as above
data <- data %>% filter(toupper(str_trim(EVTYPE)) %in% eventtypes$eventtype)

## tidy names
names(data) <- tolower(names(data))
names(data) <- gsub("_", "", names(data))

## pretty printing of event types
data <- transform(data, evtype = str_to_title(evtype))

## names(stormdata)[names(stormdata) == "state.1"] <- "statecode"
## convert begin and end dates (dates only as time not required for analysis)
data <- transform(data, bgndate = as.Date(bgndate, format= "%m/%d/%Y 0:00:00", tz = "C"))
data <- transform(data, enddate = as.Date(enddate, format= "%m/%d/%Y 0:00:00", tz = "C"))

## convert damage exponents to associated numerics
data$propdmgexp <- strtoi(chartr("KMB+-", "36900", data$propdmgexp))
data$cropdmgexp <- strtoi(chartr("KMB", "369", data$cropdmgexp))

## update damage using exponents
options(scipen = 20)
data$propdmg <- data$propdmg * 10^data$propdmgexp
data$cropdmg <- data$cropdmg * 10^data$cropdmgexp

## can now drop exponents
data$propdmgexp <- NULL
data$cropdmgexp <- NULL


## factors
# stormdata <- transform(stormdata, evtype = factor(evtype))
# stormdata <- transform(stormdata, statecode = factor(statecode))
# stormdata <- transform(stormdata, timezone = toupper(timezone))

#
# SAVE
#
save.image(compress = "bzip2")

## tidy dates
## old date format
## bgndate + bgntime + timezone
## 4/18/1950 0:00:00 + 0130 + CST
## There is one record with bgntime = "000" (3 characters not 4 or 11)
##
## new date format
## bgndate + bgntime + timezone
## 1/6/1996 0:00:00 + 08:00:00 PM + CST
#    convertDateTime <- function(date, time, zone) {
#        ifelse(nchar(time) == 11,
#           strptime(x = paste(date,time), tz = zone, format= "%m/%d/%Y 0:00:00 %X %p"),
#           strptime(x = paste(date,time), tz = zone, format= "%m/%d/%Y 0:00:00 %H%M"))
#    }
#    begindate <- with(stormdata, convertDateTime(bgndate, bgntime, timezone))

## damage exponent types:
## can convert with chartr(old, new, vector)

# unique(data$propdmgexp)
# [1] "B" ""  "K" "M" "0" "5" "4" "+" "7" "-"

# odd entrie in property exponent
# data[data$propdmgexp %in% c("+", "-", "0"), ]
#       evtype       state    bgndate    enddate fatalities injuries propdmg propdmgexp cropdmg cropdmgexp
# 169     HEAVY SNOW    CA 1994-01-27 1994-01-27          0        0       0          0     0.0
# 288     HEAVY SNOW    CO 1994-10-14 1994-10-17          0        5       0          0     0.0
# 1371     ICE STORM    MO 1994-12-06 1994-12-07          0        0      50          0     3.5          K
# 1526         FLOOD    NE 1995-05-16 1995-05-17          0        0       0          0     0.0
# 1580       TORNADO    NV 1995-06-05 1995-06-05          0        0      60          +     0.0
# 2394     HIGH WIND    OR 1995-12-12 1995-12-12          2        0      15          -     0.0
# 2422   FLASH FLOOD    PA 1995-06-24 1995-06-24          0        0       7          0     0.0
# 3077   FLASH FLOOD    VA 1995-06-28 1995-06-28          0        0      47          0   160.0          K
# 494166 FLASH FLOOD    MI 2011-07-27 2011-07-27          0        0       0          0     0.0          K

# unique(data$cropdmgexp)
# [1] ""  "M" "K" "B"

# odd entrie in property exponent
# data[data$propdmgexp %in% c("+", "-", "0"), ]
#       evtype       state    bgndate    enddate fatalities injuries propdmg propdmgexp cropdmg cropdmgexp
# 169     HEAVY SNOW    CA 1994-01-27 1994-01-27          0        0       0          0     0.0
# 288     HEAVY SNOW    CO 1994-10-14 1994-10-17          0        5       0          0     0.0
# 1371     ICE STORM    MO 1994-12-06 1994-12-07          0        0      50          0     3.5          K
# 1526         FLOOD    NE 1995-05-16 1995-05-17          0        0       0          0     0.0
# 1580       TORNADO    NV 1995-06-05 1995-06-05          0        0      60          +     0.0
# 2394     HIGH WIND    OR 1995-12-12 1995-12-12          2        0      15          -     0.0
# 2422   FLASH FLOOD    PA 1995-06-24 1995-06-24          0        0       7          0     0.0
# 3077   FLASH FLOOD    VA 1995-06-28 1995-06-28          0        0      47          0   160.0          K
# 494166 FLASH FLOOD    MI 2011-07-27 2011-07-27          0        0       0          0     0.0          K


#
# PLOTS
#

## fatalities + injuries by event type
people <- data %>%
            select(evtype, fatalities, injuries) %>%
            mutate(fatinj = fatalities + injuries) %>%
            group_by(evtype) %>%
            summarise(affected = sum(fatinj)) %>%
            select(evtype, affected)

with(people[people$affected > 0], plot(people$evtype, people$affected, type = "h", col = "red"))

## damage
## prop = property
## crop = crop
