#!/usr/bin/R --verbose --quiet

rm(list=setdiff(ls(), c("stormdata", "eventtypes")))

#
# INITIAL
#
require(utils, quietly = TRUE)
require(stringr, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(reshape2, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(scales, quietly = TRUE)

# download archive into local directory
archiveName <- file.path("stormdata.csv.bz2")
if (!file.exists(archiveName)) {
    archiveUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(archiveUrl, destfile = archiveName, method = "curl", mode = "wb")
    print(paste(Sys.time(), "archive downloaded"))
}

if(!exists("stormdata")) {
    stormdata <- read.csv(archiveName, stringsAsFactors = FALSE)
}

# only need a subset of fields from storm data for this analysis
data <- stormdata[, c("EVTYPE", "BGN_DATE", "FATALITIES", "INJURIES",
                      "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

# cleanup and lowercase column names
names(data) <- tolower(names(data))
names(data) <- gsub("_", "", names(data))

# as date
data <- transform(data, bgndate = as.Date(bgndate, format= "%m/%d/%Y 0:00:00", tz = "C"))

# from 1996
data <- data %>% filter(strtoi(format(data$bgndate, "%Y")) >= 1996)

# remove records with no casualties or damages
data <- data %>% filter(!(fatalities == 0 & injuries == 0 & propdmg == 0 & cropdmg == 0))

# title-case event types for pretty display
data <- transform(data, evtype = str_to_title(str_trim(evtype)))

# summarise by event type, and total is used to order results
casualty <- data %>%
    mutate(total = fatalities + injuries) %>%
    filter(total > 0) %>%
    select(evtype, total, fatalities, injuries) %>%
    group_by(evtype) %>%
    summarise(total = sum(total), fatalities = sum(fatalities), injuries = sum(injuries))

# melt into long format to help with plotting
casualty <- melt(casualty, id.vars = c("evtype", "total"), variable.name = "casualties")
casualty <- transform(casualty, casualties = factor(casualties))

# convert damage exponents to associated numerics
# where strtoi() will convert "" to 0
data$propdmgexp <- strtoi(chartr("KMB", "369", data$propdmgexp))
data$cropdmgexp <- strtoi(chartr("KMB", "369", data$cropdmgexp))

# update damage using translated exponents
options(scipen = 20)
data$propdmg <- data$propdmg * 10^data$propdmgexp
data$cropdmg <- data$cropdmg * 10^data$cropdmgexp

# summarise by event type, and total is used to order results
damage <- data %>%
    mutate(total = propdmg + cropdmg) %>%
    filter(total > 0) %>%
    select(evtype, total, propdmg, cropdmg) %>%
    group_by(evtype) %>%
    summarise(total = sum(total), property = sum(propdmg), crop = sum(cropdmg))

# melt into long format to help with plotting
damage <- melt(damage, id.vars = c("evtype", "total"), variable.name = "damages")
damage <- transform(damage, damages = factor(damages))

# use top 10 event types
casualty.worst <- arrange(casualty, desc(total))

# plot casualties in descending order (first 2*10 rows since long format)
casualty.worst[1:(2*10),] %>%
    ggplot(aes(x = reorder(evtype, -total), y = value/1000, fill = casualties)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_light(base_family = "Avenir", base_size = 11) +
    theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_brewer(name = "Casualty", palette = "Set2") +
    scale_x_discrete(name = "Weather Event") +
    scale_y_discrete(name = "Casualties (thousands)", breaks = pretty_breaks(n = 10)) +
    ggtitle("United States: Casualties from Severe Storm Weather (1996-2011)")

# use top 10 event types
damage.worst <- arrange(damage, desc(total))

# plot damages in descending order (first 2*10 rows since long format)
damage.worst[1:(2*10),] %>%
    ggplot(aes(x = reorder(evtype, -total), y = value/10^9, fill = damages)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_light(base_family = "Avenir", base_size = 11) +
    theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_brewer(name = "Damage", palette = "Set2") +
    scale_x_discrete(name = "Weather Event") +
    scale_y_continuous(name = "Damages (estimate in USD Billions)", breaks = pretty_breaks(n = 10)) +
    ggtitle("United States: Economic Damages from Severe Storm Weather (1996-2011)")

#
# REPORTS
#

# http://www.ncdc.noaa.gov/billions/docs/smith-and-katz-2013.pdf

#
# 48 VALID EVENTS
#

# The documentation states:
#
# Some information appearing in Storm Data may be provided by or gathered from
# sources outside the National Weather Service (NWS), such as the media, law
# enforcement and/or other government agencies, private companies, individuals,
# etc. An effort is made to use the best available information, but because of
# time and resource constraints, information from these sources may be unverified
# by the NWS. Accordingly, the NWS does not guarantee the accuracy or validity of
# the information.
#
# Source: Section 1 "Storm Data Disclaimer", [National Weather Service Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)

# Table 1, Section 2.1.1 "Storm Data Event Table" National Weather Service Storm Data Documentation
# NWS Directive 10-1605:  http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype
eventtypes <- read.csv("eventtypes.csv", stringsAsFactors = FALSE)
eventtypes <- transform(eventtypes, eventtype = toupper(str_trim(eventtype)))

# unique event types by year
aggregate(year ~ evtype, data, FUN = function(x) length(unique(x)))

#
# WAYS TO CORRECT EVTYPE
#

# correct signiticant event types
if (exists("fixevtype")) {
    # trim and uppercase to standardise what we are looking at
    data <- transform(data, evtype = toupper(str_trim(evtype, side = "both")))
    # what do we have?
    unique(data$evtype)
    # how many do we have
    evcount <- length(unique(data$evtype))
    # map to a real event type
    data <- transform(data, evtype = gsub("TSTM", "THUNDERSTORM", evtype))
    data <- transform(data, evtype = gsub("^THUNDERSTORM.*", "THUNDERSTORM WIND", evtype))
    data <- transform(data, evtype = gsub("HURRICANE.*", "HURRICANE (TYPHOON)", evtype))
    data <- transform(data, evtype = gsub("^TYPHOON", "HURRICANE (TYPHOON)", evtype))
    data <- transform(data, evtype = gsub("STORM SURGE.*", "STORM SURGE/TIDE", evtype))
    data <- transform(data, evtype = gsub("CSTL", "COASTAL", evtype))
    data <- transform(data, evtype = gsub("EXTREME COLD.*", "EXTREME COLD/WIND CHILL", evtype))
    data <- transform(data, evtype = gsub("TIDAL FLOODING", "COASTAL FLOOD", evtype))
    data <- transform(data, evtype = gsub(".*COASTAL FLOOD.*", "COASTAL FLOOD", evtype))
    data <- transform(data, evtype = gsub("COASTALSTORM", "COASTAL STORM", evtype))
    data <- transform(data, evtype = gsub("RIVER FLOOD.*", "FLOOD", evtype))
    data <- transform(data, evtype = gsub("ICE JAM FLOOD.*", "FLOOD", evtype))
    data <- transform(data, evtype = gsub(".*FLASH.FLOOD.*", "FLASH FLOOD", evtype))
    data <- transform(data, evtype = gsub("GUSTY WINDS", "STRONG WIND", evtype))
    data <- transform(data, evtype = gsub("NON.THUNDERSTORM WIND", "STRONG WIND", evtype))
    data <- transform(data, evtype = gsub("HIGH WIND.*", "HIGH WIND", evtype))
    data <- transform(data, evtype = gsub("WINDS", "WIND", evtype))
    data <- transform(data, evtype = gsub("^FOG", "DENSE FOG", evtype))
    data <- transform(data, evtype = gsub("AVALANCE", "AVALANCHE", evtype))
    data <- transform(data, evtype = gsub("WILD.* FIRE", "WILDFIRE", evtype))
    data <- transform(data, evtype = gsub("RIP CURRENTS", "RIP CURRENT", evtype))
    data <- transform(data, evtype = gsub("URBAN/SML STREAM FLD", "FLASH FLOOD", evtype))
    data <- transform(data, evtype = gsub("WINTRY MIX*", "WINTER WEATHER", evtype))
    data <- transform(data, evtype = gsub("WINTER WEATHER.*", "WINTER WEATHER", evtype))
    # show impact of updates
    totals <- data %>%
        mutate(totcas = fatalities + injuries) %>%
        mutate(totdmg = propdmg + cropdmg) %>%
        select(evtype, totcas, totdmg) %>%
        group_by(evtype) %>%
        summarise(totcas = sum(totcas), totdmg = sum(totdmg))
    # what are the major events
    head(totals %>% arrange(desc(totcas)) %>% select(evtype, totcas), 20)
    head(totals %>% arrange(desc(totdmg)) %>% select(evtype, totdmg), 20)
    # what do we have now?
    unique(data$evtype)
}

#
# CHECK STATE
#

# One way to do that elimination is use the R builtin datasets state.abb and
# state.name to make that elimination easier:
# https://class.coursera.org/repdata-015/forum/thread?thread_id=71#post-319
# Annoyingly, those only have exactly the 50 states, and I think I might want
# to include District of Columbia.

# starting with post 1996 non-zero data we have
data <- stormdata[, c("EVTYPE", "BGN_DATE", "FATALITIES", "INJURIES",
                      "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
names(data) <- tolower(names(data))
names(data) <- gsub("_", "", names(data))
data <- transform(data, bgndate = as.Date(bgndate, format= "%m/%d/%Y 0:00:00", tz = "C"))
data <- data %>% filter(!(fatalities == 0 & injuries == 0 & propdmg == 0 & cropdmg == 0))
data <- transform(data, year = strtoi(format(data$bgndate, "%Y")))
data <- data %>% filter(year >= 1996)
data <- transform(data, evtype = str_to_title(str_trim(evtype)))

# so if we exclude bad events, how does this change the results?

# show change in event frequency if we remove all invalid event types
par(mfrow = c(1, 2), mar = c(3, 4, 1, 1), oma = c(2, 1, 1, 0))
# histogram of all events post 1996
hist(data$year, main = "", xlab = "")
# histogram of only valid events post 1996
eventtypes <- read.csv("eventtypes.csv", stringsAsFactors = FALSE)
eventtypes <- transform(eventtypes, eventtype = str_to_title(str_trim(eventtype)))
# exclude bad events
validevents <- data %>% filter(evtype %in% eventtypes$eventtype)
hist(validevents$year, main = "", ylab = "", xlab = "")
title(main = "Histogram: Event Frequencies by Year", outer = TRUE)
mtext("Year", side = 1, outer = TRUE)

# show how this effects the order of event severity

# casualties?

# summarise by event type, and total is used to order results
casualty <- data %>%
    mutate(total = fatalities + injuries) %>%
    filter(total > 0) %>%
    select(evtype, total, fatalities, injuries) %>%
    group_by(evtype) %>%
    summarise(total = sum(total), fatalities = sum(fatalities), injuries = sum(injuries))
# melt into long format to help with plotting
casualty <- melt(casualty, id.vars = c("evtype", "total"), variable.name = "casualties")
casualty <- transform(casualty, casualties = factor(casualties))
# original list
casualty <- arrange(casualty, desc(total))
# now drop bad events
validevents <- casualty %>% filter(evtype %in% eventtypes$eventtype)
# top 20 of each
cbind(casualty[1:20,], validevents[1:20,])

# what about damages?

# convert damage exponents to associated numerics
# where strtoi() will convert "" to 0
data$propdmgexp <- strtoi(chartr("KMB", "369", data$propdmgexp))
data$cropdmgexp <- strtoi(chartr("KMB", "369", data$cropdmgexp))
# update damage using translated exponents, where 10^0 = 1 (i.e. no change)
options(scipen = 20)
data$propdmg <- data$propdmg * 10^data$propdmgexp
data$cropdmg <- data$cropdmg * 10^data$cropdmgexp
# summarise by event type, and total is used to order results
damage <- data %>%
    mutate(total = propdmg + cropdmg) %>%
    filter(total > 0) %>%
    select(evtype, total, propdmg, cropdmg) %>%
    group_by(evtype) %>%
    summarise(total = sum(total), property = sum(propdmg), crop = sum(cropdmg))
# melt into long format to help with plotting
damage <- melt(damage, id.vars = c("evtype", "total"), variable.name = "damages")
damage <- transform(damage, damages = factor(damages))
# order by most severe events
damage <- arrange(damage, desc(total))
# now drop bad events
validevents <- damage %>% filter(evtype %in% eventtypes$eventtype)
# top 20 of each
cbind(damage[1:20,], validevents[1:20,])


#
# CALCULATE MINIMUM DISTANCE BETWEEN EVTYPE
#
require(utils, quietly = TRUE)
require(stringr, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(reshape2, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(scales, quietly = TRUE)
require(stringdist, quietly = TRUE)

eventtypes <- read.csv("eventtypes.csv", stringsAsFactors = FALSE)
eventtypes <- transform(eventtypes, eventtype = toupper(str_trim(eventtype)))
et.master <- eventtypes$eventtype

data <- stormdata[, c("EVTYPE", "BGN_DATE", "FATALITIES", "INJURIES",
                      "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
names(data) <- tolower(names(data))
names(data) <- gsub("_", "", names(data))
# convert string to date
data <- transform(data, bgndate = as.Date(bgndate, format= "%m/%d/%Y 0:00:00", tz = "C"))
# remove records where no casualties or damages have been recorded
data <- data %>% filter(!(fatalities == 0 & injuries == 0 & propdmg == 0 & cropdmg == 0))
# uppercase event type
data <- transform(data, evtype = toupper(str_trim(evtype, side = "both")))
# use data from 1996
data <- transform(data, year = strtoi(format(data$bgndate, "%Y")))
data <- data %>% filter(year >= 1996)
# get unqiue event types
et.data <- unique(data$evtype)
length(evtypes)

# map to a real event type
et.data = gsub("TSTM", "THUNDERSTORM", et.data)
et.data = gsub("CSTL", "COASTAL", et.data)

bestmatch <- function(x) {
    m <- stringdist(x, et.master)
    et.master[which.min(m)]
}

sapply(et.data, FUN = function(x) et.master[which.min(stringdist(x, et.master))])

