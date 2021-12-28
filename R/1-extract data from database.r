# packages ----

packages <- c("RODBC")
install.packages(setdiff(packages, rownames(installed.packages()))) 

library(RODBC)

source("Settings.r")


# read data in ----
conn <- odbcDriverConnect(paste0('driver={SQL Server};server=',server,';database=',database,';trusted_connection=true'))

animal              <- sqlQuery(conn, "SELECT ID,BirthDate,Sex,AnimalType,EntryDate,ExitDate,ExitType,ExitCause1Id,ExitCause2Id  FROM animal;")
service             <- sqlQuery(conn, "SELECT * FROM service;")
  service <- service[service$LinkedToEvent!=1,]
pregnancyTest      <- sqlQuery(conn, "SELECT * FROM pregnancyTest;")
abortion            <- sqlQuery(conn, "SELECT * FROM abortion;")
farrowing           <- sqlQuery(conn, "SELECT * FROM farrowing;")
weaning             <- sqlQuery(conn, "SELECT * FROM weaning;")

dead.piglet         <- sqlQuery(conn, "SELECT * FROM deadPiglet;")
dead.piglet.group   <- sqlQuery(conn, "SELECT * FROM deadPigletGroup;")

progeny.count       <- sqlQuery(conn, "SELECT * FROM progenyCount;")
progeny.dead        <- sqlQuery(conn, "SELECT * FROM progenyDead;")
progeny.entry       <- sqlQuery(conn, "SELECT * FROM progenyEntry;")
progeny.exit        <- sqlQuery(conn, "SELECT * FROM progenyExit;")
progeny.transfer    <- sqlQuery(conn, "SELECT * FROM progenyTransfer;")
cause               <- sqlQuery(conn, "SELECT * FROM Cause_NameTranslated;")

rm(conn,database,packages,server)

save.image(file=paste0("data/",farm.name,".RData"))

