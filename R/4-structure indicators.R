# packages ----

packages <- c("ISOweek","lubridate","abind")
install.packages(setdiff(packages, rownames(installed.packages())))

require(ISOweek)
require(lubridate)
require(abind)

source("Definitions.r")
source("Settings.r")
source("R/Functions.r")


load("individual.sows.RData")
load("animal.RData")
load("indicators.RData")


# Gilts ----

## Age at first service

## Age at first farrowing


# Empty sows ----

## Sows empty longer than 4 days


# Services ----

## Reservices per week

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
