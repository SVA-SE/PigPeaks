# packages ----

packages <- c("RODBC")
uninstalled <- setdiff(packages, rownames(installed.packages()))
if (length(uninstalled))
        install.packages(uninstalled)

invisible(lapply(packages, library, character.only = TRUE))

source("Settings.r")


# read data in ----
conn <- odbcDriverConnect(paste0('driver={SQL Server};server=',server,';database=',database,';trusted_connection=true'))

if(!exists("date_filter")){
  date_filter <- Sys.Date()
}

if(!exists("date_logical")){
date_logical <- "<= "
}


animal              <- sqlQuery(conn, "SELECT ID,AnimalNumber,BirthDate,Sex,AnimalType,EntryDate,ExitDate,ExitType,ExitCause1Id,ExitCause2Id  FROM animal;")
service             <- sqlQuery(conn, paste0("SELECT * FROM service WHERE EventDate ",date_logical,"'",date_filter,"' ;"))
service <- service[service$LinkedToEvent!=1,]
pregnancyTest      <- sqlQuery(conn, paste0("SELECT * FROM pregnancyTest WHERE EventDate ",date_logical,"'",date_filter,"' ;"))
abortion            <- sqlQuery(conn, paste0("SELECT * FROM abortion WHERE EventDate ",date_logical,"'",date_filter,"' ;"))
farrowing           <- sqlQuery(conn, paste0("SELECT * FROM farrowing WHERE EventDate ",date_logical,"'",date_filter,"' ;"))
weaning             <- sqlQuery(conn, paste0("SELECT * FROM weaning WHERE EventDate ",date_logical,"'",date_filter,"' ;"))

dead.piglet         <- sqlQuery(conn, paste0("SELECT * FROM deadPiglet WHERE EventDate ",date_logical,"'",date_filter,"' ;"))
dead.piglet.group   <- sqlQuery(conn, paste0("SELECT * FROM deadPigletGroup WHERE EventDate ",date_logical,"'",date_filter,"' ;"))

progeny.count       <- sqlQuery(conn, paste0("SELECT * FROM progenyCount WHERE EventDate ",date_logical,"'",date_filter,"' ;"))
progeny.dead        <- sqlQuery(conn, paste0("SELECT * FROM progenyDead WHERE EventDate ",date_logical,"'",date_filter,"' ;"))
progeny.entry       <- sqlQuery(conn, paste0("SELECT * FROM progenyEntry WHERE EventDate ",date_logical,"'",date_filter,"' ;"))
progeny.exit        <- sqlQuery(conn, paste0("SELECT * FROM progenyExit WHERE EventDate ",date_logical,"'",date_filter,"' ;"))
progeny.transfer    <- sqlQuery(conn, paste0("SELECT * FROM progenyTransfer WHERE EventDate ",date_logical,"'",date_filter,"' ;"))
cause               <- sqlQuery(conn, "SELECT * FROM Cause_NameTranslated;")

rm(conn,database,packages,server)

save.image(file=paste0("data/",farm.name,".RData"))

