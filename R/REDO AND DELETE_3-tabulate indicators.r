packages <- c("ISOweek", "lubridate", "abind")
uninstalled <- setdiff(packages, rownames(installed.packages()))
if (length(uninstalled))
        install.packages(uninstalled)

invisible(lapply(packages, library, character.only = TRUE))

#source('1-RETRO-data.R', echo=TRUE)
#setwd(paste0(getwd(),"/"))


#source("Definitions.r")
#source("Functions.r")
#load("individual.sows.RData")
#load("animal.RData")
#load("1RETRO-data.RData")


#individual.sows
#index.dates.days
#index.dates.week
#dimnames(individual.sows[,,1])

#head(index.dates.days)

index.dates.days$ISOweekYear <- as.numeric(substr(as.character(index.dates.days[,"ISOweek"]),1,4))
index.dates.week <- index.dates.days[!duplicated(index.dates.days[,c("ISOweekYear","week")]),
                                     c("ISOweekYear","week","dates")]
colnames(index.dates.week)<- c("ISOweekYear","week","start")


#dim(index.dates.week)
#head(index.dates.week)


# number of services ----

total.services <- individual.sows[,c("service","parity"),]

services <- total.services
for (s in 1:dim(total.services)[3]){
  services[which(services[,"service",s]==2),"service",s]<-NA
}

reservices<- total.services
for (s in 1:dim(total.services)[3]){
  reservices[which(reservices[,"service",s]==1),"service",s]<-NA
  reservices[which(reservices[,"service",s]==2),"service",s]<-1
}


services.days <- create.counts.days(data=services,
                                    count.column="service")
services.week <- create.counts.week(data=services,
                                    count.column="service")

empty.sows <- create.counts.week(data=services,
                                 count.column="service")
#sum(apply(services.days,MARGIN=2,FUN=sum,na.rm=TRUE))
#sum(apply(services.week,MARGIN=2,FUN=sum,na.rm=TRUE))



# number of REservices ---

reservices.days <- create.counts.days(data=reservices,
                                      count.column="service")
reservices.week <- create.counts.week(data=reservices,
                                      count.column="service")
#sum(apply(reservices.days,MARGIN=2,FUN=sum,na.rm=TRUE))
#sum(apply(reservices.week,MARGIN=2,FUN=sum,na.rm=TRUE))



# number of empty sows per day ----

# status sows ----
status.sows <- individual.sows[,c("status","parity"),]
#table(individual.sows[,"status",],individual.sows[,"parity",],useNA="always")

for (s in 1:dim(status.sows)[3]){
  
  if(all(is.na(status.sows[,"status",s]))) next
  
  first.status <- min(which(!is.na(status.sows[,"status",s])))
  
  for (r in (first.status+1):dim(status.sows)[1]){
    
    if(is.na(status.sows[r,"status",s])){
      status.sows[r,"status",s] <- status.sows[r-1,"status",s]
    }
    
  }
}


empty.sows <- matrix(NA,nrow=length(index.dates.days[,1]),ncol=length(parity)+1)
colnames(empty.sows)<- c(0,parity)


for (r in 1:dim(empty.sows)[1]){
  for (c in 1:dim(empty.sows)[2]){
    parity.sows <- which(status.sows[r,"parity",]==c)
    empty.sows[r,c] <- length(which(status.sows[r,"status",parity.sows]==0|status.sows[r,"status",parity.sows]==1))
  }}

#test <- strptime (as.character(service$EventDate), format = "%Y-%m-%d")
#table(test$wday)



# pregnancy sucess percentages ----

#assumed pregnant at 4 weeks after service
#is a sow which 4 weeks after service did not get reserviced
#nor did she exit
#but to make sure that a sow is not counted multiple times, 
#ONLY AT THE EXACT X WEEKS AFTER SERVICE:
#1 is pregnant
#2 is reserviced
#3 is exited
#everything else is NA

assumed.pregnant <- array(NA,
                          dim=c(
                            dim(individual.sows)[1],
                            dim(individual.sows)[3],
                            length(parity)
                          ))


for (r in (reservice.perc.window*7+1):dim(individual.sows)[1]){
  for (s in 1:dim(individual.sows)[3]){
    if (!is.na(individual.sows[(r-(7*reservice.perc.window)+1),"service",s])&
        individual.sows[(r-(7*reservice.perc.window)+1),"service",s]==1){
      
      assumed.pregnant[r,s,individual.sows[(r-(7*reservice.perc.window)+1),"parity",s]]<-1
      
      if (length(which(individual.sows[((r-(7*reservice.perc.window)+2):(r)),"service",s]==2))>0){
        assumed.pregnant[r,s,individual.sows[(r-(7*reservice.perc.window)+1),"parity",s]]<-2
      }
      
      if (length(which(!is.na(individual.sows[((r-(7*reservice.perc.window)+2):(r)),"exit",s])))>0){
        assumed.pregnant[r,s,individual.sows[(r-(7*reservice.perc.window)+1),"parity",s]]<-3
      }
    }
  }
}
#length(which(assumed.pregnant==0))
#length(which(assumed.pregnant==1))

perc.pregnancy <-create.perc.week.1(data=assumed.pregnant,
                                    value.to.count=1,
                                    test="equal")
perc.failure <-create.perc.week.1(data=assumed.pregnant,
                                  value.to.count=1,
                                  test="greater")
perc.reservice <-create.perc.week.1(data=assumed.pregnant,
                                    value.to.count=2,
                                    test="equal")




#sum(pregnancy.perc[,,1])
#sum(failure.perc[,,1])
#sum(reservice.perc[,,1])
# sum(pregnancy.perc[,,2])
# sum(failure.perc[,,2])
# sum(reservice.perc[,,2])


# time to reservice, when there is a reservice ----

time.to.reservice <- create.nonTS.timeto(event1.col="service",
                                         event1.value=1,
                                         event2.col="service",
                                         event2.value=2)
#dim(time.to.reservice)
#converting back to date: as.Date(time.to.reservice$date,origin="1970-01-01")


rereservices <- create.nonTS.timeto(event1.col="service",
                                    event1.value=2,
                                    event2.col="service",
                                    event2.value=2)

rereservices <- rereservices[rereservices[,1]!=0&rereservices[,1]<114,]





# time.to.first.service ----

time.to.first.service <- create.nonTS.timeto(event1.col="birth",
                                             event1.value=1,
                                             event2.col="parity",
                                             event2.value=1,
                                             condition="first")
#dim(time.to.first.service)
#converting back to date: as.Date(time.to.reservice$date,origin="1970-01-01")

# length(time.to.first.service[,2])
# length(unique(time.to.first.service[,2]))
# check <- sort(unique(time.to.first.service[,2]))
# check2 <- which((1:dim(individual.sows)[3])%in%check==FALSE)
# #many sows exit without any service, which is why the dimension of this is not as large as dim(individual.sows)


# time.to.first.farrowing ----

time.to.first.farrowing <- create.nonTS.timeto(event1.col="birth",
                                               event1.value=1,
                                               event2.col="farrowing",
                                               event2.value=1,
                                               condition="first")
#dim(time.to.first.service)
#converting back to date: as.Date(time.to.reservice$date,origin="1970-01-01")

# length(time.to.first.service[,2])
# length(unique(time.to.first.service[,2]))
# check <- sort(unique(time.to.first.service[,2]))
# check2 <- which((1:dim(individual.sows)[3])%in%check==FALSE)
# #many sows exit without any service, which is why the dimension of this is not as large as dim(individual.sows)



# abortion ----

abortions.days <- create.counts.days(count.column="abortion")
abortions.week <- create.counts.week(count.column="abortion")
#sum(apply(abortions.days,MARGIN=2,FUN=sum,na.rm=TRUE))
#sum(apply(abortions.week,MARGIN=2,FUN=sum,na.rm=TRUE))


time.to.abortion <- create.nonTS.timeto(event1.col="service",
                                        event1.value=c(1,2),
                                        event2.col="abortion",
                                        event2.value=1)
#dim(time.to.abortion)
#tail(time.to.abortion)
#converting back to date: as.Date(time.to.reservice$date,origin="1970-01-01")



# farrowing counts ----

farrowings.days <- create.counts.days(count.column="farrowing")
farrowings.week <- create.counts.week(count.column="farrowing")
#sum(apply(farrowings.days,MARGIN=2,FUN=sum,na.rm=TRUE))

total.born.days <- create.counts.days(count.column=c("NrBornAlive","NrBornDead"))
total.born.week <- create.counts.week(count.column=c("NrBornAlive","NrBornDead"))
#sum(apply(total.born.days,MARGIN=2,FUN=sum,na.rm=TRUE))

live.born.days <- create.counts.days(count.column="NrBornAlive")
live.born.week <- create.counts.week(count.column="NrBornAlive")
#sum(apply(live.born.days,MARGIN=2,FUN=sum,na.rm=TRUE))

dead.born.days <- create.counts.days(count.column="NrBornDead")
dead.born.week <- create.counts.week(count.column="NrBornDead")
#sum(apply(dead.born.days,MARGIN=2,FUN=sum,na.rm=TRUE))

small.born.days <- create.counts.days(count.column="NrSmallStillBorn")
small.born.week <- create.counts.week(count.column="NrSmallStillBorn")
#sum(apply(small.born.days,MARGIN=2,FUN=sum,na.rm=TRUE))

weak.born.days <- create.counts.days(count.column="NrWeakBorn")
weak.born.week <- create.counts.week(count.column="NrWeakBorn")
#sum(apply(weak.born.days,MARGIN=2,FUN=sum,na.rm=TRUE))

mummi.born.days <- create.counts.days(count.column="NrMummified")
mummi.born.week <- create.counts.week(count.column="NrMummified")
#sum(apply(mummi.born.days,MARGIN=2,FUN=sum,na.rm=TRUE))


# farrowing percentages ----

perc.dead.born.week <- abind(dead.born.week,total.born.week, along=3)
dimnames(perc.dead.born.week)[[3]]<-c("numerator","denominator")
#sum(apply(perc.dead.born.week[,,1],MARGIN=2,FUN=sum,na.rm=TRUE))
#sum(apply(perc.dead.born.week[,,2],MARGIN=2,FUN=sum,na.rm=TRUE))

perc.small.born.week <- abind(small.born.week,total.born.week, along=3)
dimnames(perc.small.born.week)[[3]]<-c("numerator","denominator")

perc.weak.born.week <- abind(weak.born.week,total.born.week, along=3)
dimnames(perc.weak.born.week)[[3]]<-c("numerator","denominator")

perc.mummi.born.week <- abind(mummi.born.week,total.born.week, along=3)
dimnames(perc.mummi.born.week)[[3]]<-c("numerator","denominator")


# farrowing no-time series ----
pregnancy.length <- create.nonTS.timeto(event1.col="service",
                                        event1.value=c(1,2),
                                        event2.col="farrowing",
                                        event2.value=1)
#head(pregnancy.length)
#summary(pregnancy.length[,1])

services.to.farrow <- create.nonTS.eventsto(event1.col="service",
                                            event1.value=c(1,2),
                                            event2.col="farrowing",
                                            event2.value=1)
#head(services.to.farrow)
#summary(services.to.farrow[,1])


total.born.litter <- create.nonTS.counts(event.col="farrowing",
                                         event.value=1,
                                         count.column=c("NrBornAlive","NrBornDead"),
                                         denominator.col=NULL)
#head(total.born.litter)
#summary(total.born.litter[,1])

live.born.litter <- create.nonTS.counts(event.col="farrowing",
                                        event.value=1,
                                        count.column="NrBornAlive",
                                        denominator.col=NULL)

dead.born.litter <- create.nonTS.counts(event.col="farrowing",
                                        event.value=1,
                                        count.column="NrBornDead",
                                        denominator.col=NULL)

perc.dead.born.litter <- create.nonTS.counts(event.col="farrowing",
                                             event.value=1,
                                             count.column="NrBornDead",
                                             denominator.col=c("NrBornAlive","NrBornDead"))

small.born.litter <- create.nonTS.counts(event.col="farrowing",
                                         event.value=1,
                                         count.column="NrSmallStillBorn",
                                         denominator.col=NULL)

perc.small.born.litter <- create.nonTS.counts(event.col="farrowing",
                                              event.value=1,
                                              count.column="NrSmallStillBorn",
                                              denominator.col=c("NrBornAlive","NrBornDead"))

weak.born.litter <- create.nonTS.counts(event.col="farrowing",
                                        event.value=1,
                                        count.column="NrWeakBorn",
                                        denominator.col=NULL)

perc.weak.born.litter <- create.nonTS.counts(event.col="farrowing",
                                             event.value=1,
                                             count.column="NrWeakBorn",
                                             denominator.col=c("NrBornAlive","NrBornDead"))

mummi.born.litter <- create.nonTS.counts(event.col="farrowing",
                                         event.value=1,
                                         count.column="NrMummified",
                                         denominator.col=NULL)

perc.mummi.born.litter <- create.nonTS.counts(event.col="farrowing",
                                              event.value=1,
                                              count.column="NrMummified",
                                              denominator.col=c("NrBornAlive","NrBornDead"))

# weaning indicators ----

weanings.days <- create.counts.days(count.column="weaning")
weanings.week <- create.counts.week(count.column="weaning")
#sum(apply(weanings.days,MARGIN=2,FUN=sum,na.rm=TRUE))

total.wean.days <- create.counts.days(count.column="NrWeaned")
total.wean.week <- create.counts.week(count.column="NrWeaned")
#sum(apply(total.wean.days,MARGIN=2,FUN=sum,na.rm=TRUE))

##dead.wean.days <- create.counts.days(count.column="XXXX")
##dead.wean.week <- create.counts.week(count.column="XXXX")

#how to get there?
DiffWeaned <- array(NA,
                    dim=c(
                      dim(individual.sows)[1],
                      2,
                      dim(individual.sows)[3]
                    ))
colnames(DiffWeaned)<- c("DiffWeaned","NegDiffWeaned")
individual.sows <- abind(individual.sows,DiffWeaned,along=2)
#options(error=browser, warn = 2)
for (is in 1:dim(individual.sows)[3]){
  for (r in 1:dim(individual.sows)[1]){
    if(!is.na(individual.sows[r,"NrWeaned",is])){
      #browser()
      #last born row
      r0 <- max(which(!is.na(individual.sows[1:(r),"NrBornAlive",is])))
      
      #last weaned row
      if(r==min(which(!is.na(individual.sows[,"NrWeaned",is])))){
        r1<-0
      }else{
        r1 <- max(which(!is.na(individual.sows[1:(r-1),"NrWeaned",is])))}
      
      #if there wasn't already a weaning event after the last registered birth
      if(r0>r1){
        DiffWeaned <- (individual.sows[r,"NrWeaned",is]-
                         individual.sows[r0,"NrBornAlive",is] +
                         individual.sows[r0,"NrMoved",is])
        
        individual.sows[r,"DiffWeaned",is]<- -DiffWeaned
        if(DiffWeaned>=0){
          individual.sows[r,"NegDiffWeaned",is]<- 0
        }else{
          individual.sows[r,"NegDiffWeaned",is]<- abs(DiffWeaned)
        }
        
      }
    }
  }
}



diff.wean.days <- create.counts.days(count.column="DiffWeaned")
diff.wean.week <- create.counts.week(count.column="DiffWeaned")

negdiff.wean.days <- create.counts.days(count.column="NegDiffWeaned")
negdiff.wean.week <- create.counts.week(count.column="NegDiffWeaned")


##perc.dead.wean.week <- abind(dead.wean.week,total.wean.week, along=3)
##dimnames(perc.dead.wean.week)[[3]]<-c("numerator","denominator")


weaning.length <- create.nonTS.timeto(event1.col="farrowing",
                                      event1.value=1,
                                      event2.col="weaning",
                                      event2.value=1)

total.wean.litter <- create.nonTS.counts(event.col="weaning",
                                         event.value=1,
                                         count.column="NrWeaned",
                                         denominator.col=NULL)

negdiff.weaned.litter <- create.nonTS.counts(event.col="weaning",
                                             event.value=1,
                                             count.column="NegDiffWeaned",
                                             denominator.col=NULL)

perc.negdiff.weaned.litter <- create.nonTS.counts(event.col="weaning",
                                                  event.value=1,
                                                  count.column="NegDiffWeaned",
                                                  denominator.col="NrWeaned")

weight.wean.litter <- create.nonTS.counts(event.col="weaning",
                                          event.value=1,
                                          count.column="WeanedTotalWeight",
                                          denominator.col=NULL)

# exit indicators ----
number.exits.days <- create.counts.days(count.column="exit")
number.exits.week <- create.counts.week(count.column="exit")


# deaths ----
number.deaths.days <- create.counts.days(count.column="death")
number.deaths.week <- create.counts.week(count.column="death")

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


# exit reason ----

exit.reasons <- cause[(cause$ID2==swedish),"ID1"]
exit.reason.days <- matrix(NA,
                           nrow=dim(index.dates.days)[1],
                           ncol=length(exit.reasons)
)

colnames(exit.reason.days)<-as.character(cause[(cause$ID2==swedish),3])

for (r in 1:dim(index.dates.days)[1]){
  for (e in 1:length(exit.reasons)){
    exit.reason.days[r,e]<- length(which(individual.sows[r,"ExitReason",]==exit.reasons[e]))
  }
}


exit.reason.week <- matrix(NA,nrow=dim(index.dates.week)[1],ncol=dim(exit.reason.days)[2])
colnames(exit.reason.week)<- colnames(exit.reason.days)

for (r in 1:dim(index.dates.week)[1]){
  for (c in 1:dim(exit.reason.days)[2]){
    exit.reason.week[r,c] <- sum(exit.reason.days[((((r-1)*7)+1):min(c((r*7),dim(index.dates.days)[1]))),c],na.rm=TRUE)
  }}


# exit type ----


exit.type.days <- matrix(NA,
                           nrow=dim(index.dates.days)[1],
                           ncol=ncol(exit.type)
)

colnames(exit.type.days)<-colnames(exit.type)

for (r in 1:dim(index.dates.days)[1]){
  for (e in 1:ncol(exit.type)){
    exit.type.days[r,e]<- length(which(individual.sows[r,"ExitType",]==exit.type[1,e]))
  }
}


exit.type.week <- matrix(NA,nrow=dim(index.dates.week)[1],ncol=dim(exit.type.days)[2])
colnames(exit.type.week)<- colnames(exit.type.days)

for (r in 1:dim(index.dates.week)[1]){
  for (c in 1:dim(exit.type.days)[2]){
    exit.type.week[r,c] <- sum(exit.type.days[((((r-1)*7)+1):min(c((r*7),dim(index.dates.days)[1]))),c],na.rm=TRUE)
  }}





# exit after event ----

ExitAfter <- array(NA,
                   dim=c(
                     dim(individual.sows)[1],
                     1,
                     dim(individual.sows)[3]
                   ))
colnames(ExitAfter)<- c("ExitAfter")
individual.sows <- abind(individual.sows,ExitAfter,along=2)

#sow.events.before.exit <- c("birth","service","reservice","abortion","farrowing","weaning")


for (is in 1:dim(individual.sows)[3]){
  for (r in 1:dim(individual.sows)[1]){
    if(!is.na(individual.sows[r,"exit",is])){
      
      birth <- ifelse(length(which(!is.na(individual.sows[1:r,"birth",is])))==0,
                      0,max(which(!is.na(individual.sows[1:r,"birth",is])),na.rm=T))
      service  <- ifelse(length(which(!is.na(individual.sows[1:r,"service",is])))==0,
                         0,max(which(!is.na(individual.sows[1:r,"service",is])),na.rm=T))      
      reservice <- 0
      if(service>0){
        if(individual.sows[service,"service",is]==2){
          reservice<-service
          service <-0
        }}
      
      abortion  <- ifelse(length(which(!is.na(individual.sows[1:r,"abortion",is])))==0,
                          0,max(which(!is.na(individual.sows[1:r,"abortion",is])),na.rm=T))
      farrowing <- ifelse(length(which(!is.na(individual.sows[1:r,"farrowing",is])))==0,
                          0,max(which(!is.na(individual.sows[1:r,"farrowing",is])),na.rm=T))
      weaning   <- ifelse(length(which(!is.na(individual.sows[1:r,"weaning",is])))==0,
                          0,max(which(!is.na(individual.sows[1:r,"weaning",is])),na.rm=T))
      
      individual.sows[r,"ExitAfter",is]<- order(c(birth,service,reservice,abortion,farrowing,weaning))[6]
      
    }}}



#sow.events.before.exit <- c("birth","service","reservice","abortion","farrowing","weaning")

exit.after.event.days <- matrix(NA,
                                nrow=dim(index.dates.days)[1],
                                ncol=length(sow.events.before.exit)
)

colnames(exit.after.event.days)<-sow.events.before.exit

for (r in 1:dim(index.dates.days)[1]){
  for (e in 1:length(sow.events.before.exit)){
    exit.after.event.days[r,e]<- length(which(individual.sows[r,"ExitAfter",]==e))
  }
}
#apply(exit.after.event.days,2,sum)

exit.after.event.week <- matrix(NA,nrow=dim(index.dates.week)[1],ncol=dim(exit.after.event.days)[2])
colnames(exit.after.event.week)<- colnames(exit.after.event.days)

for (r in 1:dim(index.dates.week)[1]){
  for (c in 1:dim(exit.after.event.days)[2]){
    exit.after.event.week[r,c] <- sum(exit.after.event.days[((((r-1)*7)+1):min(c((r*7),dim(index.dates.days)[1]))),c],na.rm=TRUE)
  }}



# DEATH after event ----

DeathAfter <- array(NA,
                   dim=c(
                     dim(individual.sows)[1],
                     1,
                     dim(individual.sows)[3]
                   ))
colnames(DeathAfter)<- c("DeathAfter")
individual.sows <- abind(individual.sows,DeathAfter,along=2)


for (is in 1:dim(individual.sows)[3]){
  for (r in 1:dim(individual.sows)[1]){
    if(!is.na(individual.sows[r,"death",is])){
      
      birth <- ifelse(length(which(!is.na(individual.sows[1:r,"birth",is])))==0,
                      0,max(which(!is.na(individual.sows[1:r,"birth",is])),na.rm=T))
      service  <- ifelse(length(which(!is.na(individual.sows[1:r,"service",is])))==0,
                         0,max(which(!is.na(individual.sows[1:r,"service",is])),na.rm=T))      
      reservice <- 0
      if(service>0){
        if(individual.sows[service,"service",is]==2){
          reservice<-service
          service <-0
        }}
      
      abortion  <- ifelse(length(which(!is.na(individual.sows[1:r,"abortion",is])))==0,
                          0,max(which(!is.na(individual.sows[1:r,"abortion",is])),na.rm=T))
      farrowing <- ifelse(length(which(!is.na(individual.sows[1:r,"farrowing",is])))==0,
                          0,max(which(!is.na(individual.sows[1:r,"farrowing",is])),na.rm=T))
      weaning   <- ifelse(length(which(!is.na(individual.sows[1:r,"weaning",is])))==0,
                          0,max(which(!is.na(individual.sows[1:r,"weaning",is])),na.rm=T))
      
      individual.sows[r,"DeathAfter",is]<- order(c(birth,service,reservice,abortion,farrowing,weaning))[6]
      
    }}}



#sow.events.before.exit <- c("birth","service","reservice","abortion","farrowing","weaning")

death.after.event.days <- matrix(NA,
                                nrow=dim(index.dates.days)[1],
                                ncol=length(sow.events.before.exit)
)

colnames(death.after.event.days)<-sow.events.before.exit

for (r in 1:dim(index.dates.days)[1]){
  for (e in 1:length(sow.events.before.exit)){
    death.after.event.days[r,e]<- length(which(individual.sows[r,"DeathAfter",]==e))
  }
}
#apply(exit.after.event.days,2,sum)

death.after.event.week <- matrix(NA,nrow=dim(index.dates.week)[1],ncol=dim(death.after.event.days)[2])
colnames(death.after.event.week)<- colnames(death.after.event.days)

for (r in 1:dim(index.dates.week)[1]){
  for (c in 1:dim(death.after.event.days)[2]){
    death.after.event.week[r,c] <- sum(death.after.event.days[((((r-1)*7)+1):min(c((r*7),dim(index.dates.days)[1]))),c],na.rm=TRUE)
  }}





# empty days ----
empty.days <- create.nonTS.timeto(event1.col="weaning",
                                  event1.value=1,
                                  event2.col="service",
                                  event2.value=1)

# empty more than 4 days every week ----
#empty.long.week

empty.long.week <- matrix(NA,
                          nrow=dim(index.dates.week)[1],
                          ncol=length(parity))
colnames(empty.long.week) <- parity

for (r in 1:dim(index.dates.week)[1]){
  for (c in parity){
    
    year=index.dates.week$ISOweekYear[r]
    week=index.dates.week$week[r]
    week.rows <- which(index.dates.days$ISOweekYear==year&index.dates.days$week==week)
    week.dates <- as.numeric(index.dates.days[week.rows,1])
    
    empty.long.week[r,c] <- length(which(empty.days[,3]%in%week.dates&
                                           empty.days[,2]==c&
                                           empty.days[,1]>empty.days.target))
    
  }
}



# days between farrowings ----

days.between.farrowings <- create.nonTS.timeto(event1.col="farrowing",
                                               event1.value=1,
                                               event2.col="farrowing",
                                               event2.value=1)
days.between.farrowings <- days.between.farrowings[days.between.farrowings[,"indicator"]!=0,]


#status at the beginning of each week:----
index.mondays <- which(index.dates.days$dow==1)

status.every.monday <- array(NA,dim=c(length(index.mondays),
                                      6,
                                      16))
dimnames(status.every.monday)[[3]]<-0:15
dimnames(status.every.monday)[[2]]<-0:5

for (w in 1:dim(status.every.monday)[1]){
  for (p in 1:dim(status.every.monday)[3]){
    for (s in 1:dim(status.every.monday)[2]){
      
      status.every.monday[w,s,p]<- length(which(status.sows[index.mondays[w],"status",]==
                                                  as.numeric(dimnames(status.every.monday)[[2]][s])&
                                                  status.sows[index.mondays[w],"parity",]==
                                                  as.numeric(dimnames(status.every.monday)[[3]][p])
      ))
    }
  }
}


## ALL INDICATORS ----


save(individual.sows,
     index.dates.days,  index.dates.week,
     
     status.sows, empty.sows,
     empty.long.week,
     
     services.days,     services.week,
     reservices.days,   reservices.week,
     
     abortions.days,    abortions.week,
     
     farrowings.days,   farrowings.week,
     total.born.days,   total.born.week,
     live.born.days,    live.born.week,
     dead.born.days,    dead.born.week,
     small.born.days,   small.born.week,
     weak.born.days,    weak.born.week,
     mummi.born.days,   mummi.born.week,
     
     weanings.days,     weanings.week,
     total.wean.days,   total.wean.week,
     
     perc.reservice,    perc.failure,       perc.pregnancy,
     perc.dead.born.week, perc.small.born.week,
     perc.weak.born.week, perc.mummi.born.week,
     
     time.to.reservice, time.to.first.service,
     time.to.abortion,
     pregnancy.length,
     services.to.farrow,
     total.born.litter, live.born.litter,
     dead.born.litter, perc.dead.born.litter,
     weak.born.litter, perc.weak.born.litter,
     small.born.litter, perc.small.born.litter,
     mummi.born.litter, perc.mummi.born.litter,
     
     weaning.length,
     total.wean.litter,
     weight.wean.litter,
     weanings.days, weanings.week,
     total.wean.days, total.wean.week,
     diff.wean.days, diff.wean.week,
     negdiff.wean.days, negdiff.wean.week,
     negdiff.weaned.litter, perc.negdiff.weaned.litter,
     
     number.exits.days, number.exits.week,
     
     exit.reason.days, exit.reason.week,
     exit.type.days, exit.type.week,
     number.deaths.days,number.deaths.week,
     gilts.deaths.days, gilts.deaths.week,
     piglets.deaths.days,piglets.deaths.week,
     exit.after.event.days, exit.after.event.week,
     death.after.event.days, death.after.event.week,
     
     empty.days,
     
     days.between.farrowings,
     
     time.to.first.farrowing,
     rereservices,
     
     status.every.monday,
     
     
     active.sows.displayID,
     
     file="indicators.RData")

#load("indicators.RData")
