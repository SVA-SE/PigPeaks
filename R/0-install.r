packages <- c("RODBC")
install.packages(setdiff(packages, rownames(installed.packages()))) 

library(RODBC)

source("Settings.r")

server="xxxx" #private information referring to the local computer, see documentation
database="xxxx1" #name of the WinPig database on your computer, see documentation #"WP_Test2" #"WP_farm03"
farm.name="xxxx" #name will be used to save base files

install.date <- Sys.Date()  
#install.date <- as.Date("2015-01-01")

start.date <- NULL #leave NULL to start from the latest possible date
#set to a date in the format "2010-12-31" to start from a specific date
#


# read data in ----

date_filter <- install.date
date_logical <- "<= "

source("R/1-extract data from database.r")



# create raw data arrays ----
end.date <- install.date

source("R/2-create raw data arrays.r")

source("R/3-tabulate indicators.r")

source("R/4-detection.r")


rmarkdown::render("R/5-dashboard.Rmd",
                  output_file = "../data/index.html")

