#!/usr/bin/R --verbose --quiet

rm(list=setdiff(ls(), c("stormdata")))

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

# Table 1, Section 2.1.1 "Storm Data Event Table" National Weather Service Storm Data Documentation
# NWS Directive 10-1605:  http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype
eventtypes <- read.csv("eventtypes.csv", stringsAsFactors = FALSE)
eventtypes <- transform(eventtypes, eventtype = toupper(str_trim(eventtype)))

#
# WAYS TO CORRECT EVTYPE
#

# correct signiticant event types
if (exists("fixevtype")) {
    # se to uppercase only to make this easier
    data <- transform(data, evtype = toupper(evtype))
    # change according to order of significance as measured by causalities, damages
    data <- transform(data, evtype = gsub("TSTM", "THUNDERSTORM", evtype))
    data <- transform(data, evtype = gsub("^THUNDERSTORM.*", "THUNDERSTORM WIND", evtype))
    data <- transform(data, evtype = gsub("HURRICANE.*", "HURRICANE (TYPHOON)", evtype))
    data <- transform(data, evtype = gsub("^TYPHOON", "HURRICANE (TYPHOON)", evtype))
    data <- transform(data, evtype = gsub("EXTREME COLD.*", "EXTREME COLD/WIND CHILL", evtype))
    data <- transform(data, evtype = gsub("TIDAL FLOODING", "COASTAL FLOOD", evtype))
    data <- transform(data, evtype = gsub("CSTL", "COASTAL", evtype))
    data <- transform(data, evtype = gsub(".*COASTAL FLOOD.*", "COASTAL FLOOD", evtype))
    data <- transform(data, evtype = gsub("RIVER FLOOD.*", "FLOOD", evtype))
    data <- transform(data, evtype = gsub("ICE JAM FLOOD.*", "FLOOD", evtype))
    data <- transform(data, evtype = gsub(".*FLASH.FLOOD.*", "FLASH FLOOD", evtype))


    data <- transform(data, evtype = gsub("AVALANCE", "AVALANCHE", evtype))
    data <- transform(data, evtype = gsub("COASTALSTORM", "COASTAL STORM", evtype))
    data <- transform(data, evtype = gsub("RIP CURRENTS", "RIP CURRENT", evtype))
    data <- transform(data, evtype = gsub("URBAN/SML STREAM FLD", "FLASH FLOOD", evtype))
    data <- transform(data, evtype = gsub("WINTER WEATHER.*", "WINTER WEATHER", evtype))
    data <- transform(data, evtype = gsub(".* FIRE", "WILDFIRE", evtype))

    data <- transform(data, evtype = gsub("^WIND", "STRONG WIND", evtype))
    data <- transform(data, evtype = gsub("GUSTY WINDS.*", "STRONG WIND", evtype))

}

#
# CHECK STATE
#

# One way to do that elimination is use the R builtin datasets state.abb and
# state.name to make that elimination easier:
# https://class.coursera.org/repdata-015/forum/thread?thread_id=71#post-319
# Annoyingly, those only have exactly the 50 states, and I think I might want
# to include District of Columbia.

