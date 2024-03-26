# packages ----

packages <- c("ISOweek","lubridate","abind")
uninstalled <- setdiff(packages, rownames(installed.packages()))
if (length(uninstalled))
        install.packages(uninstalled)

invisible(lapply(packages, library, character.only = TRUE))

#source("Definitions.r") #settings already runs definitions
source("Settings.r")
source("R/Functions.r")


load(paste0("data/",farm.name,".RData"))



# date columns ----

animal$BirthDate<-as_date(animal$BirthDate)
animal$EntryDate<-as_date(animal$EntryDate)
animal$ExitDate<-as_date(animal$ExitDate)
service$EventDate<-as_date(service$EventDate)
abortion$EventDate<-as_date(abortion$EventDate)
farrowing$EventDate<-as_date(farrowing$EventDate)
weaning$EventDate<-as_date(weaning$EventDate)
#pregnancyTest$EventDate<-as_date(pregnancyTest$EventDate)
progeny.dead$EventDate<-as_date(progeny.dead$EventDate)



# create BASE arrays for the first time ----

#active.sows <- which(animal$AnimalType==0&animal$Sex==1)
active.sows <- which(animal$Sex==1)
active.sows.ID <- animal$ID[active.sows]
active.sows.displayID <- data.frame(codesID=as.numeric(as.character(animal$ID[active.sows])),
                                    displayID=as.numeric(as.character(animal$AnimalNumber[active.sows])))
LAST.ANIMAL <- tail(animal$ID,1)

birth <-data.frame(AnimalId=animal$ID[active.sows],
                   EventDate=animal$BirthDate[active.sows])


exit <-data.frame(AnimalId=animal$ID[active.sows],
                  EventDate=animal$ExitDate[active.sows],
                  ExitType=animal$ExitType[active.sows],
                  ExitCause=animal$ExitCause2Id[active.sows],
                  AnimalType=animal[match(animal$ID[active.sows],animal$ID),"AnimalType"] )

exit <- exit[exit$AnimalType==0|exit$AnimalType==3,]

death <- exit[exit$ExitType==3|exit$ExitType==4,]


# dates.index ----
#start.date for that farm
#is the MONDAY before the date when the first sow was born

if(is.null(start.date)){
start.date   <- lastmon(min(animal$BirthDate[active.sows],na.rm=T))
}else{start.date   <- lastmon(as.Date(start.date))}


index.dates.week<-data.frame(ISOweek=date2ISOweek(start.date),
                             year=as.numeric(substr(as.character(date2ISOweek(start.date)),1,4)),
                             week=as.numeric(substr(as.character(date2ISOweek(start.date)),7,8)),
                             start=start.date,
                             end=start.date+6
)

index.dates.days <-dates_df(start.date, start.date,
                            date.format="%Y-%m-%d"
)
index.dates.days$ISOweek=date2ISOweek(index.dates.days$dates)




# data structure - all events for individual sows ----
# record individually all events that happen with each sow each day
# and the day when they change status, etc


individual.sows.info <- c(sow.events, sow.info, events.info)
individual.sows <- list()

empty.matrix <- matrix(NA,
                       nrow=dim(index.dates.days)[1],
                       ncol=length(active.sows))
colnames(empty.matrix)<- active.sows.ID

for (i in 1:length(individual.sows.info)){
  individual.sows[[i]]<-empty.matrix
}

names(individual.sows)<- c(sow.events,sow.info, events.info)




# Week1 ----
for (d in 1:7){

  date.today <- start.date + d - 1
  date.row=d

  index.dates.days[date.row,]<- dates_df(date.today, date.today,
                                         date.format="%Y-%m-%d")
  index.dates.days$ISOweek[date.row]=date2ISOweek(index.dates.days$dates[date.row])


  for (event in 1:length(individual.sows)){
    if(dim(individual.sows[[event]])[1]>=date.row){
      individual.sows[[event]][date.row,]<-NA  #assumes any event after this date will be RE-RUN (re-written)
    }else{
      add.array <- array(NA,
                         c(date.row-dim(individual.sows[[event]])[1],
                           dim(individual.sows[[event]])[2])
      )
      individual.sows[[event]] <- rbind(individual.sows[[event]],add.array)
    }
  }


  day.events <- list()

  for(event in sow.events){
    event.dataset <- get(event)
    event.rows <- which(event.dataset$EventDate==date.today)

    if(length(event.rows)>0){
      day.events[[event]]<- event.dataset[event.rows,]
    }
  }


  #births
  if(!is.null(dim(day.events$birth))){
    for (r in 1:dim(day.events$birth)[1]){
      sowID <- day.events$birth[r,"AnimalId"]
      sow.dim <- which(colnames(individual.sows[[1]])==sowID)
      individual.sows$birth[date.row,sow.dim]<-1
      individual.sows$status[date.row,sow.dim]<-0
      individual.sows$parity[date.row,sow.dim]<-0
    }
  }

  #services
  if(!is.null(dim(day.events$service))){
    for (r in 1:dim(day.events$service)[1]){
      sowID <- day.events$service[r,"AnimalId"]
      sow.dim <- which(colnames(individual.sows[[1]])==sowID)

      #making sure this is not a reinsemination from yesterday --> doesn't work for day=1
      if(date.row>1){
        if(is.na(individual.sows$service[date.row-1,sow.dim])){

          #service happened
          individual.sows$service[date.row,sow.dim]<-1

          #but it was a reservice
          if(!all(is.na(individual.sows$service[((max(1,(date.row-reservice.threshold))):(date.row-1)),sow.dim]))){
            individual.sows$service[date.row,sow.dim]<-2
          }

          #either way
          individual.sows$status[date.row,sow.dim]<-2

          #only for true service
          if(individual.sows$service[date.row,sow.dim]==1){
            if(all(is.na(individual.sows$parity[1:date.row,sow.dim]))){
              last.know.parity <-NA
            }else{
              last.know.parity <- max(which(!is.na(individual.sows$parity[1:(date.row-1),sow.dim])))
            }

            if(is.na(last.know.parity)){
              individual.sows$parity[date.row,sow.dim]<-1
            }else{
              individual.sows$parity[date.row,sow.dim]<-individual.sows$parity[last.know.parity,sow.dim]+1
            }
          }

          #reservice
          if(individual.sows$service[date.row,sow.dim]==2){
            individual.sows$parity[date.row,sow.dim]<-individual.sows$parity[max(which(!is.na(individual.sows$parity[1:(date.row-1),sow.dim]))),sow.dim]
          }

        }}
    }
  }
  #}

  # #pregnancyTest
  # if(!is.null(dim(day.events$pregnancyTest))){
  #   for (r in 1:dim(day.events$pregnancyTest)[1]){
  # 
  #     sowID <- day.events$pregnancyTest[r,"AnimalId"]
  #     sow.dim <- which(colnames(individual.sows[[1]])==sowID)
  # 
  #     individual.sows$pregnancyTest[date.row,sow.dim]<-day.events$pregnancyTest[r,"TestResult"]
  #     individual.sows$status[date.row,sow.dim]<-3
  #   }
  # }

  #abortion
  if(!is.null(dim(day.events$abortion))){
    for (r in 1:dim(day.events$abortion)[1]){
      sowID <- day.events$abortion[r,"AnimalId"]
      sow.dim <- which(colnames(individual.sows[[1]])==sowID)
      individual.sows$abortion[date.row,sow.dim]<-1
      individual.sows$abortion[date.row,sow.dim]<-1
    }
  }


  #farrowing #"NrBornAlive","NrBornDead"
  if(!is.null(dim(day.events$farrowing))){
    for (r in 1:dim(day.events$farrowing)[1]){
      sowID <- day.events$farrowing[r,"AnimalId"]
      sow.dim <- which(colnames(individual.sows[[1]])==sowID)

      individual.sows$farrowing[date.row,sow.dim]<-1
      individual.sows$status[date.row,sow.dim]<-4

      individual.sows$NrBornAlive[date.row,sow.dim]<-day.events$farrowing[r,"LiveBorn"]
      individual.sows$NrBornDead[date.row,sow.dim]<-day.events$farrowing[r,"StillBorn"]
      individual.sows$NrSmallStillBorn[date.row,sow.dim]<-day.events$farrowing[r,"SmallStillBorn"]
      individual.sows$NrWeakBorn[date.row,sow.dim]<-day.events$farrowing[r,"WeakBorn"]
      individual.sows$NrMummified[date.row,sow.dim]<-day.events$farrowing[r,"Mummified"]
      individual.sows$NrMoved[date.row,sow.dim]<-day.events$farrowing[r,"TransferredPiglets"]
    }
  }

  #weaning ,"NrWeaned"
  if(!is.null(dim(day.events$weaning))){
    for (r in 1:dim(day.events$weaning)[1]){
      sowID <- day.events$weaning[r,"AnimalId"]
      sow.dim <- which(colnames(individual.sows[[1]])==sowID)

      individual.sows$weaning[date.row,sow.dim]<-1
      individual.sows$status[date.row,sow.dim]<-1

      individual.sows$NrWeaned[date.row,sow.dim]<-day.events$weaning[r,"NumOfWeaned"]
      individual.sows$WeanedTotalWeight[date.row,sow.dim]<-day.events$weaning[r,"TotalWeight"]

    }
  }

  #exit #"ExitReason"
  if(!is.null(dim(day.events$exit))){
    for (r in 1:dim(day.events$exit)[1]){
      sowID <- day.events$exit[r,"AnimalId"]
      sow.dim <- which(colnames(individual.sows[[1]])==sowID)

      individual.sows$exit[date.row,sow.dim]<-1
      individual.sows$status[date.row,sow.dim]<-5

      individual.sows$ExitReason[date.row,sow.dim]<-day.events$exit[r,"ExitCause"]
      individual.sows$ExitType[date.row,sow.dim]<-day.events$exit[r,"ExitType"]

      if(day.events$exit[r,"ExitType"]==3|day.events$exit[r,"ExitType"]==4){
        individual.sows$death[date.row,sow.dim]<-1
      }

    }
  }



}


# rest of retrospectively available data ----
all.dates<- c(
  animal$BirthDate,
  animal$ExitDate,
  service$EventDate,
  abortion$EventDate,
  farrowing$EventDate,
  weaning$EventDate)

if(!exists("end.date")){
  end.date <- max(all.dates,na.rm=T)
}

total.date.rows <- as.integer(end.date-start.date)+1

#options(error = browser(), warn = 2)

for (d in 8:total.date.rows){

  date.today <- start.date + d - 1
  date.row=d #which(index.dates.days$dates==date.today)

  index.dates.days[date.row,]<- dates_df(date.today, date.today,
                                         date.format="%Y-%m-%d")
  index.dates.days$ISOweek[date.row]=date2ISOweek(index.dates.days$dates[date.row])

  for (event in 1:length(individual.sows)){

    if(dim(individual.sows[[event]])[1]>=date.row){
      individual.sows[[event]][date.row,]<-NA  #assumes any event after this date will be RE-RUN (re-written)
    }else{
      add.array <- array(NA,
                         c(date.row-dim(individual.sows[[event]])[1],
                           dim(individual.sows[[event]])[2])
      )
      individual.sows[[event]] <- rbind(individual.sows[[event]],add.array)
    }
  }


  day.events <- list()

  for(event in sow.events){
    event.dataset <- get(event)
    event.rows <- which(event.dataset$EventDate==date.today)

    if(length(event.rows)>0){
      day.events[[event]]<- event.dataset[event.rows,]
    }
  }


  #births
  if(!is.null(dim(day.events$birth))){
    for (r in 1:dim(day.events$birth)[1]){
      sowID <- day.events$birth[r,"AnimalId"]
      sow.dim <- which(colnames(individual.sows[[1]])==sowID)
      individual.sows$birth[date.row,sow.dim]<-1
      individual.sows$status[date.row,sow.dim]<-0
      individual.sows$parity[date.row,sow.dim]<-0
    }
  }

  #services
  if(!is.null(dim(day.events$service))){
    for (r in 1:dim(day.events$service)[1]){
      sowID <- day.events$service[r,"AnimalId"]
      sow.dim <- which(colnames(individual.sows[[1]])==sowID)

      #making sure this is not a reinsemination from yesterday
      if(is.na(individual.sows$service[date.row-1,sow.dim])){

        #service happened
        individual.sows$service[date.row,sow.dim]<-1

        #but it was a reservice
        if(!all(is.na(individual.sows$service[((max(1,(date.row-reservice.threshold))):(date.row-1)),sow.dim]))){
          individual.sows$service[date.row,sow.dim]<-2
        }

        #either way
        individual.sows$status[date.row,sow.dim]<-2

        #only for true service
        if(individual.sows$service[date.row,sow.dim]==1){
          if(all(is.na(individual.sows$parity[1:date.row,sow.dim]))){
            last.know.parity <-NA
          }else{
            last.know.parity <- max(which(!is.na(individual.sows$parity[1:(date.row-1),sow.dim])))
          }

          if(is.na(last.know.parity)){
            individual.sows$parity[date.row,sow.dim]<-1
          }else{
            individual.sows$parity[date.row,sow.dim]<-individual.sows$parity[last.know.parity,sow.dim]+1
          }
        }

        #reservice
        if(individual.sows$service[date.row,sow.dim]==2){
          individual.sows$parity[date.row,sow.dim]<-individual.sows$parity[max(which(!is.na(individual.sows$parity[1:(date.row-1),sow.dim]))),sow.dim]
        }


      }#if(is.na(individual.sows[date.row-1,"service",sow.dim]))
    }# for (r in 1:dim(day.events$service)[1])
  }#if(!is.null(dim(day.events$service))){


  # #pregnancyTest
  # if(!is.null(dim(day.events$pregnancyTest))){
  #   for (r in 1:dim(day.events$pregnancyTest)[1]){
  # 
  #     sowID <- day.events$pregnancyTest[r,"AnimalId"]
  #     sow.dim <- which(colnames(individual.sows[[1]])==sowID)
  # 
  #     individual.sows$pregnancyTest[date.row,sow.dim]<-day.events$pregnancyTest[r,"TestResult"]
  #     individual.sows$status[date.row,sow.dim]<-3
  #   }
  # }

  #abortion
  if(!is.null(dim(day.events$abortion))){
    for (r in 1:dim(day.events$abortion)[1]){
      sowID <- day.events$abortion[r,"AnimalId"]
      sow.dim <- which(colnames(individual.sows[[1]])==sowID)
      individual.sows$abortion[date.row,sow.dim]<-1
      individual.sows$status[date.row,sow.dim]<-1
    }
  }


  #farrowing #"NrBornAlive","NrBornDead"
  if(!is.null(dim(day.events$farrowing))){
    for (r in 1:dim(day.events$farrowing)[1]){
      sowID <- day.events$farrowing[r,"AnimalId"]
      sow.dim <- which(colnames(individual.sows[[1]])==sowID)


      individual.sows$farrowing[date.row,sow.dim]<-1
      individual.sows$status[date.row,sow.dim]<-4

      individual.sows$NrBornAlive[date.row,sow.dim]<-day.events$farrowing[r,"LiveBorn"]
      individual.sows$NrBornDead[date.row,sow.dim]<-day.events$farrowing[r,"StillBorn"]
      individual.sows$NrSmallStillBorn[date.row,sow.dim]<-day.events$farrowing[r,"SmallStillBorn"]
      individual.sows$NrWeakBorn[date.row,sow.dim]<-day.events$farrowing[r,"WeakBorn"]
      individual.sows$NrMummified[date.row,sow.dim]<-day.events$farrowing[r,"Mummified"]
      individual.sows$NrMoved[date.row,sow.dim]<-day.events$farrowing[r,"TransferredPiglets"]


    }
  }

  #weaning ,"NrWeaned"
  if(!is.null(dim(day.events$weaning))){
    for (r in 1:dim(day.events$weaning)[1]){
      sowID <- day.events$weaning[r,"AnimalId"]
      sow.dim <- which(colnames(individual.sows[[1]])==sowID)

      individual.sows$weaning[date.row,sow.dim]<-1
      individual.sows$status[date.row,sow.dim]<-1

      individual.sows$NrWeaned[date.row,sow.dim]<-day.events$weaning[r,"NumOfWeaned"]
      individual.sows$WeanedTotalWeight[date.row,sow.dim]<-day.events$weaning[r,"TotalWeight"]

    }
  }

  #exit #"ExitReason"
  if(!is.null(dim(day.events$exit))){
    for (r in 1:dim(day.events$exit)[1]){
      sowID <- day.events$exit[r,"AnimalId"]
      sow.dim <- which(colnames(individual.sows[[1]])==sowID)


      individual.sows$exit[date.row,sow.dim]<-1
      individual.sows$status[date.row,sow.dim]<-5

      individual.sows$ExitReason[date.row,sow.dim]<-day.events$exit[r,"ExitCause"]
      individual.sows$ExitType[date.row,sow.dim]<-day.events$exit[r,"ExitType"]

      if(day.events$exit[r,"ExitType"]==3|day.events$exit[r,"ExitType"]==4){
        individual.sows$death[date.row,sow.dim]<-1
      }


    }
  }

}



for (s in 1:dim(individual.sows[[1]])[2]){

  if(!all(is.na(individual.sows$parity[,s]))){

    first.parity <- min(which(!is.na(individual.sows$parity[,s])))
    exit.day <- dim(individual.sows[[1]])[1]
    if(length(which(individual.sows$exit[,s]==1))>0){
      exit.day <-min(which(individual.sows$exit[,s]==1))
    }

    for (r in (first.parity+1):exit.day){
      if(is.na(individual.sows$parity[r,s])){
        individual.sows$parity[r,s] <- individual.sows$parity[(r-1),s]
      }
    }}
}



save(individual.sows,active.sows.displayID,index.dates.days,file="data/individual.sows.RData")
save(animal,exit,progeny.dead,file="data/animal.RData")




