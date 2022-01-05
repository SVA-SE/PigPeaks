rm(list=ls())
packages <- c("RODBC")
install.packages(setdiff(packages, rownames(installed.packages()))) 

library(RODBC)

source("Settings.r")
source("R/Functions.r")

server="xxxx" #private information referring to the local computer, see documentation
database="xxxx" #name of the WinPig database on your computer, see documentation #"WP_Test2" #"WP_farm03"
farm.name="xxxx" #name will be used to save base files


load("data/individual.sows.RData")
load("data/animal.RData")
load("data/indicators.RData")
load("data/indicators.results.RData")


today.is <- Sys.Date()
#today.is <- as.Date("2018-03-07",format="%Y-%m-%d")



last.update.was <- max(index.dates.week$start)


if(is.null(median.days.production.cycles)){
  fetchCDB.start.date <- last.update.was - (2*median(indicators.data[[which(indicators.labels=="days between farrowings")]][,1]))
}else{
  fetchCDB.start.date <- last.update.was - (2*median.days.production.cycles)
}



update.start.date <- last.update.was - (evaluate.weekly.window*7)
#starting evaluate.window weeks ago, from the last update, not from today



# read data in ----

date_filter <- fetchCDB.start.date
date_logical <- ">= "


source("R/1-extract data from database.r")



# create raw data arrays ----
start.date   <- lastmon(update.start.date) #use lastmon function just to make sure it is a monday

end.date <- today.is

source("R/2-UPDATE raw data arrays.r")

source("R/3-UPDATE indicators.r")

source("R/4-UPDATE detection.r")


rmarkdown::render("R/5-dashboard.Rmd",
                  output_file = "../data/index.html")

