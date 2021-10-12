##make sure you are in agreement with
##Definitions.r

##chose the indicators
##Table_Indicators_CSV.csv

##input farm settings in
##Settings.r

### RETROSPECTIVE SETUP: -----

## if direct access to database:
source("R/1-extract data from database.r")

##only need to do once and then indicators will be saved for all the retrospective phase:
source("R/2-create raw data arrays.r")
source("R/3-tabulate indicators.r")

##apply the temporal aberration detection algorithms
source("R/4-detection.r")

##create the dashboard
source("R/5-dashboard.Rmd")
