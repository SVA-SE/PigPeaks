# packages ----

packages <- c("ISOweek","lubridate","abind")
install.packages(setdiff(packages, rownames(installed.packages())))

require(ISOweek)
require(lubridate)
require(abind)

source("Definitions.r")
source("Settings.r")
source("R/Functions.r")


load("data/individual.sows.RData")
load("data/animal.RData")
load("data/indicators.RData")



# Gilts ----

## Age at first service

## Age at first farrowing


# Empty sows ----

## Sows empty longer than 4 days


# Services ----

## Reservices per week

matrix1 <- weekly.indicators(indicator1)
matrix2 <- weekly.indicators(indicator2)


weekly.indicators(indicators.data=list(reservices.week=reservices.week,
                                                   number.deaths.week=number.deaths.week,
                                                   piglets.deaths.week=piglets.deaths.week),
                              weekly.window=weekly.window)

weekly.indicators(weekly.window = 300)


weekly.indicators.parity(weekly.window=271)
weekly.indicators.parity(weekly.window=271)[[1]]


# Farrowings ----

## Days between farrowings

continuous.indicators(continuous.window=5500)


# Post-Weaning ----

## Deaths in weaners per week

weekly.indicators.nonparity(weekly.window=271)


# Exit ----

## Dead sows per week

weekly.indicators.parity(weekly.window=271)
weekly.indicators.parity(weekly.window=271)[[2]]
