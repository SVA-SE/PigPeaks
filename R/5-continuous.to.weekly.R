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
  
  indicators.continuous.to.weekly[[i]] <- continuous.to.weekly(indicator=indicators.data[[i]]
                                                               )
}


# clean baseline ----

for (i in intersect(which(indicators.sys==TRUE), which(indicators.type=="C"))) {

    range.indicator <- range.weekly
    
  if (indicators.limits[i]=="limit.upp") {
    
    indicators.continuous.to.weekly[[i]] <- clean_baseline_perc(df.indicator=indicators.continuous.to.weekly[[i]],
                                                       limit.upp=limit.upp,
                                                       limit.lw=NULL,
                                                       run.window.weekly=run.window.weekly,
                                                       median.days.production.cycles=median.days.production.cycles,
                                                       nr.production.cycles=nr.production.cycles,
                                                       range=range.indicator,
                                                       indicator.type=indicators.type[i])
  }
  
  if (indicators.limits[i]=="limit.lw") {
    
    indicators.continuous.to.weekly[[i]] <- clean_baseline_perc(df.indicator=indicators.continuous.to.weekly[[i]],
                                                       limit.upp=NULL,
                                                       limit.lw=limit.lw,
                                                       run.window.weekly=run.window.weekly,
                                                       median.days.production.cycles=median.days.production.cycles,
                                                       nr.production.cycles=nr.production.cycles,
                                                       range=range.indicator,
                                                       indicator.type=indicators.type[i])
  }
  
  if (indicators.limits[i]=="both") {
    
    indicators.continuous.to.weekly[[i]] <- clean_baseline_perc(df.indicator=indicators.continuous.to.weekly[[i]],
                                                       limit.upp=limit.upp,
                                                       limit.lw=limit.lw,
                                                       run.window.weekly=run.window.weekly,
                                                       median.days.production.cycles=median.days.production.cycles,
                                                       nr.production.cycles=nr.production.cycles,
                                                       range=range.indicator,
                                                       indicator.type=indicators.type[i])
  }
}


# apply EWMA ----

for (i in intersect(which(indicators.sys==TRUE), which(indicators.type=="C"))) {
  
  if (indicators.limits[i]=="limit.upp"){
    
    correct.baseline.UCL.ewma = TRUE
    correct.baseline.LCL.ewma = FALSE
  }
  
  if (indicators.limits[i]=="limit.lw") {
    
    correct.baseline.UCL.ewma = FALSE
    correct.baseline.LCL.ewma = TRUE
  }
  
  if (indicators.limits[i]=="both") {
    
    correct.baseline.UCL.ewma = TRUE
    correct.baseline.LCL.ewma = TRUE
  }
  
  indicators.continuous.to.weekly[[i]] <- apply_ewma(df.indicator=indicators.continuous.to.weekly[[i]],
                                            evaluate.weekly.window=evaluate.weekly.window,
                                            baseline.weekly.window=baseline.weekly.window,
                                            continuous.window=continuous.window,
                                            lambda=lambda,
                                            limit.sd=limit.sd,
                                            guard.band.weekly=guard.band.weekly,
                                            correct.baseline.UCL.ewma=correct.baseline.UCL.ewma,
                                            correct.baseline.LCL.ewma=correct.baseline.LCL.ewma,
                                            UCL.ewma=UCL.ewma,
                                            LCL.ewma=LCL.ewma,
                                            indicator.type=indicators.type[i])
}


# apply Shewhart ----

for (i in intersect(which(indicators.sys==TRUE), which(indicators.type=="C"))) {
  
  if (indicators.limits[i]=="limit.upp"){
    
    correct.baseline.UCL.shew = TRUE
    correct.baseline.LCL.shew = FALSE
  }
  
  if (indicators.limits[i]=="limit.lw") {
    
    correct.baseline.UCL.shew = FALSE
    correct.baseline.LCL.shew = TRUE
  }
  
  if (indicators.limits[i]=="both") {
    
    correct.baseline.UCL.shew = TRUE
    correct.baseline.LCL.shew = TRUE
  }
  
  indicators.continuous.to.weekly[[i]] <- shew_apply(df.indicator=indicators.continuous.to.weekly[[i]],
                                            evaluate.weekly.window=evaluate.weekly.window,
                                            baseline.weekly.window=baseline.weekly.window,
                                            continuous.window=continuous.window,
                                            limit.sd=limit.sd,
                                            guard.band.weekly=guard.band.weekly,
                                            correct.baseline.UCL.shew=correct.baseline.UCL.shew,
                                            correct.baseline.LCL.shew=correct.baseline.LCL.shew,
                                            UCL.shew=UCL.shew,
                                            LCL.shew=LCL.shew,
                                            indicator.type=indicators.type[i])
}



# 
# TS.barplot.pg.continuous (df.indicator = indicators.continuous.to.weekly$`piglets per farrowing`,
#                indicator.label="indicator",
#                show.window = 52,
#                index.dates = index.dates.week,
#                ylabel = 'Number of sows',
#                xlabel = 'Week',
#                target = NULL,
#                target.unit = NULL,                 #c("value","vector"), defaults to vector
#                shading.matrix = NULL,
#                limits = NULL,
#                UCL.EWMA = FALSE,                       #Added
#                LCL.EWMA = FALSE,                      #Added
#                UCL.SHEW = FALSE,                       #Added
#                LCL.SHEW = FALSE,                      #Added
#                alarms.EWMA.UPP = FALSE,                #Added
#                alarms.EWMA.LW = FALSE,                #Added
#                alarms.SHEW.UPP = FALSE,                #Added
#                alarms.SHEW.LW = FALSE,                #Added
#                argument.list = FALSE,
#                group.labels=c('gilts','young','prime','mature'))
