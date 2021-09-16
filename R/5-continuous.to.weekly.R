# packages ----

packages <- c("ISOweek","lubridate","abind", "tidyverse")
install.packages(setdiff(packages, rownames(installed.packages())))

require(ISOweek)
require(lubridate)
require(abind)
require(tidyverse)

#source("Definitions.r") #settings already runs definitions
source("Settings.r")
source("R/Functions.r")


#load("data/individual.sows2.RData")
#load("data/animal.RData")
load("data/indicators.RData")
# index.dates.week
# indicators.data
# indicators.labels
# indicators.type
# indicators.sys
# indicators.limits
# indicators.categories


#Indicators continuous to weekly for dashboard ----


## RUN in the following order:

indicators.continuous.to.weekly <- list_along(1:length(indicators.data))
names(indicators.continuous.to.weekly) <- indicators.labels


# weekly range ----

range.weekly <- max(1,(dim(index.dates.week)[1]-weekly.window+1)):dim(index.dates.week)[1]
#the range should be applied when doing DETECTION - which weeks are relevant for training/detection
#to actually restrict the historical data to less than all date farm,
#please set a start date on Settings.


## structure continuous indicators with SyS to weekly ----

for (i in intersect(which(indicators.sys==TRUE), which(indicators.type=="C"))) {
  
  indicators.continuous.to.weekly[[i]] <- continuous.to.weekly(df.indicator=indicators.time.series[[i]],
                                                               limits=indicators.limits[[i]]
                                                               )
}

