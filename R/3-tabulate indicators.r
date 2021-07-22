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


index.dates.days$ISOweekYear <- as.numeric(substr(as.character(index.dates.days[,"ISOweek"]),1,4))
index.dates.week <- index.dates.days[!duplicated(index.dates.days[,c("ISOweekYear","week")]),
                                     c("ISOweekYear","week","dates")]
colnames(index.dates.week)<- c("ISOweekYear","week","start")



# number of services ----

total.services <- individual.sows$service


services <- total.services
for (s in 1:dim(total.services)[2]){
  services[which(services[,s]==2),s]<-NA
}

reservices<- total.services
for (s in 1:dim(total.services)[2]){
  reservices[which(reservices[,s]==1),s]<-NA
  reservices[which(reservices[,s]==2),s]<-1
}


services.days <- create.counts.days(data.matrix=services)
services.week <- create.counts.week(data.matrix=services)


# number of REservices ---

reservices.days <- create.counts.days(data.matrix=reservices)
reservices.week <- create.counts.week(data.matrix=reservices)
#sum(apply(reservices.days,MARGIN=2,FUN=sum,na.rm=TRUE))
#sum(apply(reservices.week,MARGIN=2,FUN=sum,na.rm=TRUE))



# days between farrowings ----

days.between.farrowings <- create.nonTS.timeto(event1.matrix=individual.sows$farrowing,
                                               event1.value=1,
                                               event2.matrix=individual.sows$farrowing,
                                               event2.value=1)
days.between.farrowings <- days.between.farrowings[days.between.farrowings[,"indicator"]!=0,]




# mortality weaned pigs ----


piglets.deaths.days <- rep(0,dim(index.dates.days)[1])
if(dim(progeny.dead)[1]>1){
  for (d in 1:dim(index.dates.days)[1]){
    piglets.deaths.days[d] <- sum(progeny.dead$NumOfPigs[(which(progeny.dead$EventDate==index.dates.days$dates[d]))])
  }
}

t <- data.frame(year=index.dates.days$year,
                week=index.dates.days$week,
                counts=piglets.deaths.days)
v <- aggregate(t,by=list(t$year,t$week),FUN=sum)
piglets.deaths.week <- v$counts





# deaths ----
number.deaths.days <- create.counts.days(data.matrix=individual.sows$death)
number.deaths.week <- create.counts.week(data.matrix=individual.sows$death)



#mortality break down by parity groups gilts and parity 1
#getting directly from exit

death.gilts <- exit[(exit$ExitType==3|exit$ExitType==4)&(
  exit$AnimalType==3),]

gilts.deaths.days <- rep(0,dim(index.dates.days)[1])
if(dim(death.gilts)[1]>1){
  for (d in 1:dim(index.dates.days)[1]){
    gilts.deaths.days[d] <- length(which(exit$EventDate==index.dates.days$dates[d]))
  }
}

t <- data.frame(year=index.dates.days$year,
                week=index.dates.days$week,
                counts=gilts.deaths.days)
v <- aggregate(t,by=list(t$year,t$week),FUN=sum)
gilts.deaths.week <- v$counts





## ALL INDICATORS ----


save(individual.sows,
     index.dates.days,  index.dates.week,

     services.days,     services.week,
     reservices.days,   reservices.week,

     piglets.deaths.days, piglets.deaths.week,

     number.deaths.days,
     number.deaths.week,

     days.between.farrowings,

     active.sows.displayID,

     file="indicators.RData")

#load("indicators.RData")
