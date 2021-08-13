# packages ----

packages <- c("ISOweek","lubridate","abind")
install.packages(setdiff(packages, rownames(installed.packages())))

require(ISOweek)
require(lubridate)
require(abind)

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

indicators.time.series <- list()

for (i in 1:length(indicators.to.keep.numerical)){
  indicators.time.series[[i]] <- get(indicators.all[indicators.to.keep.numerical[i]])
}
names(indicators.data)<-indicators.all[indicators.to.keep.numerical]


indicators.time.series <- list()

for (i in which(which(indicators.sys==TRUE) && which(indicators.type=="W"))) {
  
  indicators.time.series[[i]] <- range.weekly(indicator=indicators.data[[i]],
                                              weekly.window=weekly.window)
}


which(which(indicators.sys==TRUE) %in% which(indicators.type=="W"))


names(indicators.time.series) <- indicators.labels

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

df.days.between.farrowings <- continuous.indicators(indicator=days.between.farrowings,
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
                                         correct.baseline.UCL=TRUE,
                                         correct.baseline.LCL=TRUE,
                                         UCL=2,
                                         LCL=2)

df.days.between.farrowings <- shew_apply(df.indicator=df.indicator,
                                         evaluate.weekly.window=NULL,
                                         baseline.weekly.window=NULL,
                                         limit.sd=c(2.5,3,3.5),
                                         guard.band.weekly=NULL,
                                         correct.baseline.UCL=FALSE,
                                         correct.baseline.LCL=FALSE,
                                         UCL=FALSE,
                                         LCL=FALSE)

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
