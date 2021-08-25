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
#index.dates.week
# indicators.data
# indicators.labels
# indicators.type
#indicators.sys
#indicators.limits


## RUN in the following order:

indicators.time.series <- list_along(1:length(indicators.data))
names(indicators.time.series) <- indicators.labels


# weekly range ----

range.weekly <- max(1,(dim(index.dates.week)[1]-weekly.window+1)):dim(index.dates.week)[1]
  #the range should be applied when doing DETECTION - which weeks are relevant for training/detection
  #to actually restrict the historical data to less than all date farm,
  #please set a start date on Settings.


# structure weekly indicators with SyS ----

for (i in intersect(which(indicators.sys==TRUE), which(indicators.type=="W"))) {

  indicators.time.series[[i]] <- weekly.indicators(indicator=indicators.data[[i]],
                                                   range.weekly=range.weekly)
}


# structure continuous indicators with SyS ----

for (i in intersect(which(indicators.to.keep.excel$sys==TRUE), which(indicators.to.keep.excel$type=="C"))) {

  indicators.time.series[[i]] <- continuous.indicators(indicator=indicators.data[[i]],
                                                       continuous.window=continuous.window)
}


# structure non-sys indicators ----

for (i in which(indicators.to.keep.excel$sys==FALSE)) {

  indicators.time.series[[i]] <- non.sys.indicators(indicator=indicators.data[[i]],
                                                    range.weekly=indicators.time.series[[i]],
                                                    continuous.window=continuous.window)
}


# clean baseline ----

for (i in which(indicators.to.keep.excel$sys==TRUE)) {

  if (indicators.to.keep.excel$limits[i]=="limit.upp") {

  indicators.time.series[[i]] <- clean_baseline_perc(df.indicator=indicators.time.series[[i]],
                                                     limit.upp=limit.upp,
                                                     limit.lw=NULL,
                                                     run.window.weekly=run.window.weekly,
                                                     nr.production.cycles=nr.production.cycles)
  }

  if (indicators.to.keep.excel$limits[i]=="limit.lw") {

    indicators.time.series[[i]] <- clean_baseline_perc(df.indicator=indicators.time.series[[i]],
                                                       limit.upp=NULL,
                                                       limit.lw=limit.lw,
                                                       run.window.weekly=run.window.weekly,
                                                       nr.production.cycles=nr.production.cycles)
  }

  if (indicators.to.keep.excel$limits[i]=="both") {

    indicators.time.series[[i]] <- clean_baseline_perc(df.indicator=indicators.time.series[[i]],
                                                       limit.upp=limit.upp,
                                                       limit.lw=limit.lw,
                                                       run.window.weekly=run.window.weekly,
                                                       nr.production.cycles=nr.production.cycles)
  }
}


# apply EWMA ----

for (i in which(indicators.to.keep.excel$sys==TRUE)) {

  indicators.time.series[[i]] <- apply_ewma(df.indicator=indicators.time.series[[i]],
                                            evaluate.weekly.window=evaluate.weekly.window,
                                            baseline.weekly.window=baseline.weekly.window,
                                            lambda=lambda,
                                            limit.sd=limit.sd,
                                            guard.band.weekly=guard.band.weekly,
                                            correct.baseline.UCL.ewma=correct.baseline.UCL.ewma,
                                            correct.baseline.LCL.ewma=correct.baseline.LCL.ewma,
                                            UCL.ewma=UCL.ewma,
                                            LCL.ewma=LCL.ewma)
}


# apply Shewhart ----

for (i in which(indicators.to.keep.excel$sys==TRUE)) {

  indicators.time.series[[i]] <- shew_apply(df.indicator=indicators.time.series[[i]],
                                            evaluate.weekly.window=evaluate.weekly.window,
                                            baseline.weekly.window=baseline.weekly.window,
                                            limit.sd=limit.sd,
                                            guard.band.weekly=guard.band.weekly,
                                            correct.baseline.UCL.shew=correct.baseline.UCL.shew,
                                            correct.baseline.LCL.shew=correct.baseline.LCL.shew,
                                            UCL.shew=UCL.shew,
                                            LCL.shew=LCL.shew)
}
