#!/usr/bin/R --verbose --quiet

#
# INITIAL
#
require(utils, quietly = TRUE)
require(stringr, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(reshape2, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(scales, quietly = TRUE)

################################################################################

#
# LOAD RAW DATA
#

## download archive into local directory
archiveName <- file.path("stormdata.csv.bz2")
if (!file.exists(archiveName)) {
    archiveUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(archiveUrl, destfile = archiveName, method = "curl", mode = "wb")
    print(paste(Sys.time(), "archive downloaded"))
}

## load into data frame and convert date column to date
if(!exists("stormdata")) {
    stormdata <- read.csv(archiveName, stringsAsFactors = FALSE)
}

################################################################################

#
# TIDY COLUMNS
#

## only need a subset of fields from storm data for this analysis
data <- stormdata[, c("EVTYPE", "BGN_DATE", "FATALITIES", "INJURIES",
                      "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
names(data) <- tolower(names(data))
names(data) <- gsub("_", "", names(data))
data <- transform(data, bgndate = as.Date(bgndate, format= "%m/%d/%Y 0:00:00", tz = "C"))

#
# USE DATA AFTER 1996
#

## prior 1996 contains only Tornado, Thunderstorm Wind and Hail events
## NWS Directive 10-1605 introduced 48 standardised event types
# data <- transform(data, year = strtoi(format(data$bgndate, "%Y")))
data <- data %>% filter(strtoi(format(data$bgndate, "%Y")) >= 1996)

fixevtype <- NULL
# correct signiticant event types
if (exists("fixevtype") && fixevtype == TRUE) {
    data <- transform(data, evtype = toupper(evtype))
    data <- transform(data, evtype = gsub("TSTM", "THUNDERSTORM", evtype))
    data <- transform(data, evtype = gsub(" \\(G\\d+\\)", "", evtype), perl = TRUE)
    data <- transform(data, evtype = gsub("THUNDERSTORM WIND/HAIL", "THUNDERSTORM WIND", evtype))
    data <- transform(data, evtype = gsub("HURRICANE.*", "HURRICANE/TYPHOON", evtype))
    data <- transform(data, evtype = gsub("RIP CURRENTS", "RIP CURRENT", evtype))
    #data <- transform(data, evtype = gsub("AVALANCE", "AVALANCHE", evtype))
    #data <- transform(data, evtype = gsub("COASTALSTORM", "COASTAL STORM", evtype))
    #data <- transform(data, evtype = gsub("WINTER WEATHER.MIX", "WINTER WEATHER", evtype))
    #data <- transform(data, evtype = gsub("COASTAL FLOODING.*", "COASTAL FLOOD", evtype))
    #data <- transform(data, evtype = gsub("URBAN/SML STREAM FLD", "FLASH FLOOD", evtype))
    #data <- transform(data, evtype = gsub("CSTL", "COASTAL", evtype))
}

data <- transform(data, evtype = str_to_title(str_trim(evtype)))

#
# CASUALTIES
#

# summarise by event type, total used to order plot
casualty <- data %>%
    mutate(total = fatalities + injuries) %>%
    filter(total > 0) %>%
    select(evtype, total, fatalities, injuries) %>%
    group_by(evtype) %>%
    summarise(total = sum(total), fatalities = sum(fatalities), injuries = sum(injuries))

# melt into long format to help with plotting
casualty <- melt(casualty, id.vars = c("evtype", "total"), variable.name = "casualties")
casualty <- transform(casualty, casualties = factor(casualties))

# use top 10 event types
casualty.worst <- arrange(casualty, desc(total))

# plot casualties in descending order (first 2*10 rows since long format)
casualty.worst[1:(2*10),] %>%
    ggplot(aes(x = reorder(evtype, -total), y = value, fill = casualties)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_light(base_family = "Avenir", base_size = 11) +
    scale_fill_brewer(name = "Emission Source Type", palette = "Set2") +
    scale_x_discrete(name = "Event Type") +
    scale_y_discrete(name = "Casualties", breaks = pretty_breaks(n = 10)) +
    ggtitle("United States: Casualties from Severe Storm Weather")


#
# DAMAGE
#

# convert damage exponents to associated numerics
# where strtoi() will convert "" to 0
data$propdmgexp <- strtoi(chartr("KMB", "369", data$propdmgexp))
data$cropdmgexp <- strtoi(chartr("KMB", "369", data$cropdmgexp))

# update damage using translated exponents
options(scipen = 20)
data$propdmg <- data$propdmg * 10^data$propdmgexp
data$cropdmg <- data$cropdmg * 10^data$cropdmgexp

# drop old exponent columns
data$propdmgexp <- NULL
data$cropdmgexp <- NULL

# order by descending damages
data <- arrange(desc(propdmg + cropdmg))

# summarise by event type, total used to order plot
damage <- data %>%
    mutate(total = propdmg + cropdmg) %>%
    filter(total > 0) %>%
    select(evtype, total, propdmg, cropdmg) %>%
    group_by(evtype) %>%
    summarise(total = sum(total), property = sum(propdmg), crop = sum(cropdmg))

# melt into long format to help with plotting
damage <- melt(damage, id.vars = c("evtype", "total"), variable.name = "damages")
damage <- transform(damage, damages = factor(damages))

## use top 10 event types
damage.worst <- arrange(damage, desc(total))

## plot damages in descending order (first 2*10 rows since long format)
damage.worst[1:(2*10),] %>%
    ggplot(aes(x = reorder(evtype, -total), y = value/10^9, fill = damages)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_light(base_family = "Avenir", base_size = 11) +
    theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_brewer(name = "Damage", palette = "Set2") +
    scale_x_discrete(name = "Weather Event") +
    scale_y_continuous(name = "Damages (estimate in USD Billions)", breaks = pretty_breaks(n = 10)) +
    ggtitle("United States: Economic Damages from Severe Storm Weather")
################################################################################

#
# SAVE
#
save.image(compress = "bzip2")

################################################################################

#
# EVENT TYPES
#

## load valid event types as per section 2.1.1 "Storm Data Event Table" table 1 on page 6.
## National Weather Service Storm Data Documentation
## See also http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype
## and NWS Directive 10-1605.
eventtypes <- read.csv("eventtypes.csv", stringsAsFactors = FALSE)
eventtypes <- transform(eventtypes, eventtype = toupper(str_trim(eventtype)))

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


################################################################################

#
# HISTOGRAM OF EVENT TYPES
#

## Use data from 1996 onwards as this is when broader range event types were
## recored. See http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype
## show effect this has
## show effect this has
par(mfrow = c(1, 2), mar = c(3, 4, 1, 1), oma = c(2, 1, 1, 0))
hist(data$year, main = "", xlab = "")
## limit to events after 1996
data <- data %>% filter(year >= 1996)
hist(data$year, main = "", ylab = "", xlab = "")
title(main = "Histogram: Event Frequencies by Year", outer = TRUE)
mtext("Year", side = 1, outer = TRUE)

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
