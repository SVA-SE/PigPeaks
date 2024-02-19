rm(list=ls())
packages <- c("RODBC")
uninstalled <- setdiff(packages, rownames(installed.packages()))
if (length(uninstalled))
        install.packages(uninstalled)

invisible(lapply(packages, library, character.only = TRUE))



    ###make sure to set farm name, and if needed database connections here
source("Settings.r")


source("R/Functions.r")



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


# source("R/1-extract data from database.r")



# create raw data arrays ----
start.date   <- lastmon(update.start.date) #use lastmon function just to make sure it is a monday

end.date <- today.is

source("R/2-UPDATE raw data arrays.r")

source("R/3-UPDATE indicators.r")

source("R/4-UPDATE detection.r")


rmarkdown::render("R/5-dashboard.Rmd",
                  output_file = "../data/index.html")

