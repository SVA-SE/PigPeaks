packages <- c("RODBC")
uninstalled <- setdiff(packages, rownames(installed.packages()))
if (length(uninstalled))
        install.packages(uninstalled)

invisible(lapply(packages, library, character.only = TRUE))


    ###make sure to set farm name, and if needed database connections here
source("Settings.r")



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

