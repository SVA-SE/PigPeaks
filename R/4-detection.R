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

## indicators.sys

indicators.sys <- c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE,
                    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,
                    TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE,
                    FALSE)

## indicators.limits

indicators.limits <- c("non-sys", "limit.upp", "limit.upp", "both", "limit.upp", 
                       "limit.upp", "non-sys", "non-sys", "limit.upp", "non-sys", 
                       "both", "both", "limit.lw", "both", "limit.lw",
                       "limit.upp", "limit.upp", "limit.upp", "non-sys", "limit.lw",
                       "limit.upp", "non-sys", "limit.lw", "limit.lw", "limit.upp",
                       "limit.upp", "limit.upp", "non-sys", "both", "non-sys",
                       "non-sys")


## % dead born per farrowing *100
# run once
#indicators.data$perc.dead.born.litter[, "indicator"] <- round(indicators.data$perc.dead.born.litter[, "indicator"]*100, 2)


## RUN in the following order:

indicators.time.series <- list_along(1:length(indicators.data))


# weekly range ----

for (i in which(indicators.type=="W")) {
  
  indicators.time.series[[i]] <- range.weekly(indicator=indicators.data[[i]],
                                              weekly.window=weekly.window)
}

names(indicators.time.series) <- indicators.labels
 

# structure weekly indicators with SyS ----

for (i in intersect(which(indicators.sys==TRUE), which(indicators.type=="W"))) {
  
  indicators.time.series[[i]] <- weekly.indicators(indicator=indicators.data[[i]],
                                                   range.weekly=indicators.time.series[[i]])
}


# structure continuous indicators with SyS ----

for (i in intersect(which(indicators.sys==TRUE), which(indicators.type=="C"))) {
  
  indicators.time.series[[i]] <- continuous.indicators(indicator=indicators.data[[i]],
                                                       continuous.window=continuous.window)
}


# structure non-sys indicators ----

for (i in which(indicators.sys==FALSE)) {
  
  indicators.time.series[[i]] <- non.sys.indicators(indicator=indicators.data[[i]],
                                                    range.weekly=indicators.time.series[[i]],
                                                    continuous.window=continuous.window)
}


# clean baseline ----

for (i in which(indicators.sys==TRUE)) {
  
  if (indicators.limits[i]=="limit.upp") {

  indicators.time.series[[i]] <- clean_baseline_perc(df.indicator=indicators.time.series[[i]],
                                                     limit.upp=limit.upp,
                                                     limit.lw=NULL,
                                                     run.window.weekly=run.window.weekly,
                                                     nr.production.cycles=nr.production.cycles)
  }
  
  if (indicators.limits[i]=="limit.lw") {
    
    indicators.time.series[[i]] <- clean_baseline_perc(df.indicator=indicators.time.series[[i]],
                                                       limit.upp=NULL,
                                                       limit.lw=limit.lw,
                                                       run.window.weekly=run.window.weekly,
                                                       nr.production.cycles=nr.production.cycles)
  }
  
  if (indicators.limits[i]=="both") {
    
    indicators.time.series[[i]] <- clean_baseline_perc(df.indicator=indicators.time.series[[i]],
                                                       limit.upp=limit.upp,
                                                       limit.lw=limit.lw,
                                                       run.window.weekly=run.window.weekly,
                                                       nr.production.cycles=nr.production.cycles)
  }
}


# apply EWMA ----

for (i in which(indicators.sys==TRUE)) {
  
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

for (i in which(indicators.sys==TRUE)) {
  
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
