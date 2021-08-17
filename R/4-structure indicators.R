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
                       "limit.lw", "non-sys", "limit.lw", "limit.lw", "limit.upp",
                       "limit.upp", "limit.upp", "non-sys", "both", "non-sys",
                       "non-sys")

#for (i in intersect(which(indicators.sys==TRUE), which(indicators.type=="W"))) {}

head(indicators.data[["perc.failure"]][,,"numerator"])


indicators.time.series <- list_along(1:length(indicators.data))


# weekly range ----

for (i in intersect(which(indicators.sys==TRUE), which(indicators.type=="W"))) {
  
  indicators.time.series[[i]] <- range.weekly(indicator=indicators.data[[i]],
                                              weekly.window=weekly.window)
}

names(indicators.time.series) <- indicators.labels
 

# structure weekly indicators ----

for (i in intersect(which(indicators.sys==TRUE), which(indicators.type=="W"))) {
  
  indicators.time.series[[i]] <- weekly.indicators(indicator=indicators.data[[i]],
                                                   range=indicators.time.series[[i]])
}


# structure continuous indicators ----

for (i in intersect(which(indicators.sys==TRUE), which(indicators.type=="C"))) {
  
  indicators.time.series[[i]] <- continuous.indicators(indicator=indicators.data[[i]],
                                                       continuous.window=continuous.window)
}


# clean baseline ----

for (i in which(indicators.sys==TRUE)) {
  
  if (indicators.limits[i]=="limit.upp") {

  indicators.time.series[[i]] <- clean_baseline_perc(df.indicator=indicators.time.series[[i]],
                                                     limit.upp=limit.upp,
                                                     limit.lw=NULL,
                                                     run.window.weekly=run.window.weekly,
                                                     run.window.continuous=run.window.continuous)
  }
  
  if (indicators.limits[i]=="limit.lw") {
    
    indicators.time.series[[i]] <- clean_baseline_perc(df.indicator=indicators.time.series[[i]],
                                                       limit.upp=NULL,
                                                       limit.lw=limit.lw,
                                                       run.window.weekly=run.window.weekly,
                                                       run.window.continuous=run.window.continuous)
  }
  
  if (indicators.limits[i]=="both") {
    
    indicators.time.series[[i]] <- clean_baseline_perc(df.indicator=indicators.time.series[[i]],
                                                       limit.upp=limit.upp,
                                                       limit.lw=limit.lw,
                                                       run.window.weekly=run.window.weekly,
                                                       run.window.continuous=run.window.continuous)
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
  
# Services ----

## Reservices per week

range_weekly <- range.weekly(indicator=indicators.data$reservices.week,    #indicators.data$reservices.week
                             weekly.window = 271)

df.reservices.week <- weekly.indicators(indicator=indicators.data$reservices.week,
                                        range=range_weekly)

df.reservices.week <- clean_baseline_perc(df.indicator=df.reservices.week,
                                          limit.upp=0.95,
                                          limit.lw=NULL,
                                          run.window.weekly=104,
                                          run.window.continuous=NULL)

df.reservices.week <- apply_ewma(df.indicator=df.reservices.week,
                                 evaluate.weekly.window=165,
                                 baseline.weekly.window=104,
                                 lambda=0.2,
                                 limit.sd=c(2.5,3,3.5),
                                 guard.band.weekly=2,
                                 correct.baseline.UCL=TRUE,
                                 correct.baseline.LCL=TRUE,
                                 UCL.ewma=2,
                                 LCL.ewma=2)

df.reservices.week <- shew_apply(df.indicator=df.reservices.week,
                                 evaluate.weekly.window=165,
                                 baseline.weekly.window=104,
                                 limit.sd=c(2.5,3,3.5),
                                 guard.band.weekly=2,
                                 correct.baseline.UCL=FALSE,
                                 correct.baseline.LCL=FALSE,
                                 UCL.shew=2,
                                 LCL.shew=2)


# Farrowings ----

## Days between farrowings

df.days.between.farrowings <- continuous.indicators(indicator=indicators.data$days.between.farrowings,
                                                    continuous.window=5500)

df.days.between.farrowings <- clean_baseline_perc(df.indicator=df.days.between.farrowings,
                                                  limit.upp=0.95,
                                                  limit.lw=0.05,
                                                  run.window.weekly=NULL,
                                                  run.window.continuous=1300)

df.days.between.farrowings <- apply_ewma(df.indicator=df.days.between.farrowings,
                                         evaluate.weekly.window=NULL,
                                         baseline.weekly.window=NULL,
                                         lambda=0.2,
                                         limit.sd=c(2.5,3,3.5),
                                         guard.band.weekly=NULL,
                                         correct.baseline.UCL.ewma=TRUE,
                                         correct.baseline.LCL.ewma=TRUE,
                                         UCL.ewma=2,
                                         LCL.ewma=2)

df.days.between.farrowings <- shew_apply(df.indicator=df.days.between.farrowings,
                                         evaluate.weekly.window=NULL,
                                         baseline.weekly.window=NULL,
                                         limit.sd=c(2.5,3,3.5),
                                         guard.band.weekly=NULL,
                                         correct.baseline.UCL.shew=FALSE,
                                         correct.baseline.LCL.shew=FALSE,
                                         UCL.shew=2,
                                         LCL.shew=2)

# Post-Weaning ----

## Deaths in weaners per week

range_weekly <- range.weekly(indicator=piglets.deaths.week,
                             weekly.window = 271)

df.piglets.deaths.week <- weekly.indicators(indicator=piglets.deaths.week)

df.piglets.deaths.week <- clean_baseline_perc(df.indicator=df.piglets.deaths.week,
                                              limit.upp=0.95,
                                              limit.lw=NULL,
                                              run.window.weekly=104,
                                              run.window.continuous=NULL)

df.piglets.deaths.week <- apply_ewma(df.indicator=df.piglets.deaths.week,
                                     evaluate.weekly.window=165,
                                     baseline.weekly.window=104,
                                     lambda=0.2,
                                     limit.sd=c(2.5,3,3.5),
                                     guard.band.weekly=2,
                                     correct.baseline.UCL=TRUE,
                                     correct.baseline.LCL=TRUE,
                                     UCL=2,
                                     LCL=2)

df.piglets.deaths.week <- shew_apply(df.indicator=df.piglets.deaths.week,
                                     evaluate.weekly.window=165,
                                     baseline.weekly.window=104,
                                     limit.sd=c(2.5,3,3.5),
                                     guard.band.weekly=2,
                                     correct.baseline.UCL=FALSE,
                                     correct.baseline.LCL=FALSE,
                                     UCL=FALSE,
                                     LCL=FALSE)


# Exit ----

## Dead sows per week

range_weekly <- range.weekly(indicator=number.deaths.week,
                             weekly.window = 271)

df.number.deaths.week <- weekly.indicators(indicator=number.deaths.week)

df.number.deaths.week <- clean_baseline_perc(df.indicator=df.number.deaths.week,
                                             limit.upp=0.95,
                                             limit.lw=NULL,
                                             run.window.weekly=104,
                                             run.window.continuous=NULL)

df.number.deaths.week <- apply_ewma(df.indicator=df.number.deaths.week,
                                    evaluate.weekly.window=165,
                                    baseline.weekly.window=104,
                                    lambda=0.2,
                                    limit.sd=c(2.5,3,3.5),
                                    guard.band.weekly=2,
                                    correct.baseline.UCL=TRUE,
                                    correct.baseline.LCL=TRUE,
                                    UCL=2,
                                    LCL=2)

df.number.deaths.week <- shew_apply(df.indicator=df.number.deaths.week,
                                    evaluate.weekly.window=165,
                                    baseline.weekly.window=104,
                                    limit.sd=c(2.5,3,3.5),
                                    guard.band.weekly=2,
                                    correct.baseline.UCL=FALSE,
                                    correct.baseline.LCL=FALSE,
                                    UCL=FALSE,
                                    LCL=FALSE)
