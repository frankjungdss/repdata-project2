--- 
title: 'Impact of Severe Storm Events' 
author: "Frank Jung" 
date: "12 June 2015" 
output: 
  html_document: 
    keep_md: yes
    toc: yes
---

## Synopsis

In this report we explore the NOAA Storm Database for the effects that severe 
storm weather has had to both people and property. The data comes from the 
United States [National Weather Service](http://www.weather.gov/) and covers the
period from 1950 to 2011.  The data includes [many](#events) types of weather
events. However, we selected only events after 1996 as that was when the largest
range of weather events were first being measured. From this data we found that
the greatest number of casualties (as measured by injury or fatality) occurred 
during [tornados](#what-events-had-the-most-casualties). We also found that the 
greatest economic cost occurred as a consequence of 
[floods](#what-events-had-the-greatest-economic-cost).

## Data Processing

Read [Storm Data](#data) to obtain casualty and economic damage measurements.
Storm data begins in 1950 and ends in November 2011.


```r
# set our runtime environment and global defaults.
require(knitr, quietly = TRUE)
require(utils, quietly = TRUE)
require(stringr, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(reshape2, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(scales, quietly = TRUE)
opts_chunk$set(echo = TRUE, cache = TRUE, cache.path = "cache/", fig.width = 10, fig.height = 7, fig.path = "figure/")
```

### Download Storm Data

Download and load [Storm Data](#data) which is a `bzip2` archive containing a
storm data `CSV` records.


```r
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
```

### Subset Storm Data and Tidy

Subset data for the columns required for analysis of casualties and damages. Not
all fields contained in the dataset are relevant for this exploration.

The column names were set to lowercase and cast to appropriate classes.


```r
# only need a subset of fields from storm data for this analysis
data <- stormdata[, c("EVTYPE", "BGN_DATE", "FATALITIES", "INJURIES",
                      "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
# cleanup and lowercase column names
names(data) <- tolower(names(data))
names(data) <- gsub("_", "", names(data))
data <- transform(data, bgndate = as.Date(bgndate, format= "%m/%d/%Y 0:00:00", tz = "C"))
data <- transform(data, evtype = str_to_title(str_trim(evtype)))
```

From 1950 to 1995 only Tornado, Thunderstorm Wind and Hail weather events were
being recorded. Then in 1996 the [NWS Directive 
10-1605](http://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf) introduced 48 
standardised event types. So this analysis will use data from 1996 to 2011.


```r
data <- data %>% filter(strtoi(format(data$bgndate, "%Y")) >= 1996)
```

More detail can be found at [Storm Events Database: Event
Types](http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype)

### Casualties

Prepare a data frame of total casualties, (composed of fatalties and injuries).
The report will list weather events in descending order of total casualties.


```r
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
```

### Economic Damages

The property and crop damage exponents are documented in Section 2.7 "Damage",
[National Weather Service Storm Data
Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)

They are: **K** for thousands ($10^3$), **M** for millions ($10^6$), **B** for
billions ($10^9$).

The following will map `propdmgexp` and `cropdmgexp` from a character to the 
associated numeric value. We will then update the property (`propdmg`) and crop 
(`cropdmg`) damage estimates using these converted exponents.


```r
# convert damage exponents to associated numerics
# where strtoi() will convert "" to 0
data$propdmgexp <- strtoi(chartr("KMB", "369", data$propdmgexp))
data$cropdmgexp <- strtoi(chartr("KMB", "369", data$cropdmgexp))

# update damage using translated exponents
options(scipen = 20)
data$propdmg <- data$propdmg * 10^data$propdmgexp
data$cropdmg <- data$cropdmg * 10^data$cropdmgexp
```

Prepare a data frame of total damages, (composed of crop and property damage
estimates in USD). The report will list event types in descending order of total
damages.


```r
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
```

### Events

There are several issues with this Storm Data. This analysis will not address 
these issues, other than to highlight them and note that they should be
considered when evaluating these [resuls](#results):

* provenance - multiple sources that have not been guarenteed by the NWS
* event types - there are more [event
  types](http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype) than
  the offical list

The documentation states:

_Some information appearing in Storm Data may be provided by or gathered from 
sources outside the National Weather Service (NWS), such as the media, law 
enforcement and/or other government agencies, private companies, individuals, 
etc. An effort is made to use the best available information, but because of 
time and resource constraints, information from these sources may be unverified 
by the NWS. Accordingly, the NWS does not guarantee the accuracy or validity of 
the information._

Source: Section 1 "Storm Data Disclaimer", [National Weather Service Storm Data 
Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)

As per NWS Directive 10-1605 there are only 48 valid event types
as listed in [Table 1, Section 2.1.1 "Storm Data Event Table", National Weather 
Service Storm Data Documentation](#documentation). See also [Event Types 
Available](http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype)

## Results

### What events had the most casualties?

The total number of casualties (fatalities and injuries) were counted from 1996 
to 2011. Total casualties were then sorted in descending order. The top ten 
events that cause the greatest loss of life or injury are:


```r
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
```

![plot of chunk showcasualties](figure/showcasualties-1.png) 

### What events had the greatest economic cost? 

All Crop and property damage estimates were summed from 1996 to 2011. Total 
costs were then sorted in descending order. The top ten events that incur the 
greatest economic cost are:


```r
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
```

![plot of chunk showdamages](figure/showdamages-1.png) 

## Appendices

### Data

* [Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)
[47Mb] 

### Code

* all code and associated documentation is available on GitHub from [here](https://github.com/frankhjung/repdata-project2).

### Documentation

* National Weather Service [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)

* National Climatic Data Center Storm Events
[FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)

### Session Information

This document was produced in RStudio. The session information detailing
packages used is:


```r
sessionInfo()
```

```
## R version 3.2.0 (2015-04-16)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Linux Mint LMDE
## 
## locale:
##  [1] LC_CTYPE=en_AU.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_AU.UTF-8        LC_COLLATE=en_AU.UTF-8    
##  [5] LC_MONETARY=en_AU.UTF-8    LC_MESSAGES=en_AU.UTF-8   
##  [7] LC_PAPER=en_AU.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_AU.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] scales_0.2.4   ggplot2_1.0.1  reshape2_1.4.1 dplyr_0.4.1   
## [5] stringr_1.0.0  knitr_1.10.5  
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.11.6        codetools_0.2-11   digest_0.6.8      
##  [4] assertthat_0.1     MASS_7.3-40        grid_3.2.0        
##  [7] plyr_1.8.2         gtable_0.1.2       DBI_0.3.1         
## [10] formatR_1.2        magrittr_1.5       evaluate_0.7      
## [13] stringi_0.4-1      lazyeval_0.1.10    proto_0.3-10      
## [16] RColorBrewer_1.1-2 tools_3.2.0        munsell_0.4.2     
## [19] parallel_3.2.0     colorspace_1.2-6
```
