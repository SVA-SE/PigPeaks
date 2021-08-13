# packages ----

packages <- c("ISOweek","lubridate","abind")
install.packages(setdiff(packages, rownames(installed.packages())))

require(ISOweek)
require(lubridate)
require(abind)

#source("Definitions.r") #settings already runs definitions
source("Settings.r")
source("R/Functions.r")


load("data/individual.sows.RData")
load("data/animal.RData")


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


services.week <- create.counts.week(rows.index.days=index.dates.days[,1],
                                    rows.index.week=index.dates.week[,1],
                                    cols.index=parity,
                                    data.matrix=services, #<--
                                    group.matrix=individual.sows$parity)


# number of REservices ---

reservices.week <- create.counts.week(data.matrix=reservices)
#sum(apply(reservices.week,MARGIN=2,FUN=sum,na.rm=TRUE))




# pregnancy success percentages ----

#assumed pregnant at 4 weeks after service (reservice.perc.window)
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
                            dim(individual.sows$service)[1],
                            dim(individual.sows$service)[2],
                            length(parity)
                          ))


for (r in (reservice.perc.window*7+1):dim(individual.sows$service)[1]){
  for (s in 1:dim(individual.sows$service)[2]){
    if (!is.na(individual.sows$service[(r-(7*reservice.perc.window)+1),s])&
        individual.sows$service[(r-(7*reservice.perc.window)+1),s]==1){

      assumed.pregnant[r,s,individual.sows$parity[(r-(7*reservice.perc.window)+1),s]]<-1

      if (length(which(individual.sows$service[((r-(7*reservice.perc.window)+2):(r)),s]==2))>0){
        assumed.pregnant[r,s,individual.sows$parity[(r-(7*reservice.perc.window)+1),s]]<-2
      }

      if (length(which(!is.na(individual.sows$exit[((r-(7*reservice.perc.window)+2):(r)),s])))>0){
        assumed.pregnant[r,s,individual.sows$parity[(r-(7*reservice.perc.window)+1),s]]<-3
      }
    }
  }
}
#length(which(assumed.pregnant==2))
#length(which(assumed.pregnant==1))
#length(which(assumed.pregnant==3))

perc.pregnancy <-create.perc.week.1(rows.index.week=index.dates.week[,1],
                                    cols.index=parity,
                                    data=assumed.pregnant,
                                    value.to.count=1,
                                    test="equal")  #c("equal","greater")

perc.failure <-create.perc.week.1(data=assumed.pregnant,
                                  value.to.count=1,
                                  test="greater")
perc.reservice <-create.perc.week.1(data=assumed.pregnant,
                                    value.to.count=2,
                                    test="equal")


# dim(perc.pregnancy)
# sum(perc.pregnancy[,,1])
# sum(perc.failure[,,1])
# sum(perc.reservice[,,1])



# time to reservice, when there is a reservice ----

time.to.reservice <- create.nonTS.timeto(group.matrix=individual.sows$parity,
                                         index.dates=index.dates.days[,1],
                                         col1="indicator",
                                         col2="parity",
                                         col3="date",
                                         event1.matrix=individual.sows$service,
                                         event1.value=1,
                                         event2.matrix=individual.sows$service,
                                         event2.value=2,
                                         condition="first")
#dim(time.to.reservice)
#tail(time.to.reservice)
#converting back to date: as.Date(time.to.reservice$date,origin="1970-01-01")



rereservices <- create.nonTS.timeto(event1.matrix=individual.sows$service,
                                    event1.value=2,
                                    event2.matrix=individual.sows$service,
                                    event2.value=2,
                                    condition="first")
#dim(rereservices)
#tail(rereservices)
#rereservices[,1]


rereservices <- rereservices[rereservices[,1]!=0&rereservices[,1]<min.pregnancy.length,]





# time.to.first.service ----

time.to.first.service <- create.nonTS.timeto(event1.matrix=individual.sows$birth,
                                             event1.value=1,
                                             event2.matrix=individual.sows$service,
                                             event2.value=1,
                                             condition="first")

#dim(time.to.first.service)
#converting back to date: as.Date(time.to.reservice$date,origin="1970-01-01")
#tail(time.to.first.service)


# time.to.first.farrowing ----

time.to.first.farrowing <- create.nonTS.timeto(event1.matrix=individual.sows$birth,
                                               event1.value=1,
                                               event2.matrix=individual.sows$farrowing,
                                               event2.value=1,
                                               condition="first")
#dim(time.to.first.farrowing)
#tail(time.to.first.farrowing)



# abortion ----

abortions.week <- create.counts.week(data.matrix=individual.sows$abortion)
#sum(apply(abortions.week,MARGIN=2,FUN=sum,na.rm=TRUE))


time.to.abortion <- create.nonTS.timeto(event1.matrix=individual.sows$service,
                                        event1.value=c(1,2),
                                        event2.matrix=individual.sows$abortion,
                                        event2.value=1,
                                        condition=NULL)
#dim(time.to.abortion)
#tail(time.to.abortion)
#converting back to date: as.Date(time.to.reservice$date,origin="1970-01-01")


# days between farrowings ----

days.between.farrowings <- create.nonTS.timeto(event1.matrix=individual.sows$farrowing,
                                               event1.value=1,
                                               event2.matrix=individual.sows$farrowing,
                                               event2.value=1)
days.between.farrowings <- days.between.farrowings[days.between.farrowings[,"indicator"]!=0,]
#dim(days.between.farrowings)
#tail(days.between.farrowings)


# farrowing counts ----

farrowings.week <- create.counts.week(data.matrix=individual.sows$farrowing)
#sum(apply(farrowings.week,MARGIN=2,FUN=sum,na.rm=TRUE))
#dim(farrowings.week)


live.born.week <- create.counts.week(data.matrix=individual.sows$NrBornAlive)
dead.born.week <- create.counts.week(data.matrix=individual.sows$NrBornDead)
total.born.week = live.born.week + dead.born.week
#tail(total.born.week)
#dim(total.born.week)


small.born.week <- create.counts.week(data.matrix=individual.sows$NrSmallStillBorn)


weak.born.week <- create.counts.week(data.matrix=individual.sows$NrWeakBorn)


mummi.born.week <- create.counts.week(data.matrix=individual.sows$NrMummified)



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



# farrowing continuous ----
pregnancy.length <- create.nonTS.timeto(event1.matrix=individual.sows$service,
                                        event1.value=c(1,2),
                                        event2.matrix=individual.sows$farrowing,
                                        event2.value=1,
                                        condition=NULL)
#tail(pregnancy.length)
#summary(pregnancy.length[,1])
#dim(pregnancy.length)

services.to.farrow <- create.nonTS.eventsto(event1.matrix=individual.sows$service,
                                            event1.value=c(1,2),
                                            event2.matrix=individual.sows$farrowing,
                                            event2.value=1)
# tail(services.to.farrow)
# summary(services.to.farrow[,1])
# dim(services.to.farrow)


total.born.litter <- create.nonTS.counts(group.matrix=individual.sows$parity,
                                         index.dates=index.dates.days[,1],
                                         col1="indicator",
                                         col2="parity",
                                         col3="date",
                                         event.matrix=individual.sows$farrowing,
                                         event.value=1,
                                         count.matrix=list(individual.sows$NrBornAlive,
                                                           individual.sows$NrBornDead),
                                         denominator.matrix=NULL)

#tail(total.born.litter)
#summary(total.born.litter[,1])
#dim(total.born.litter)

live.born.litter <- create.nonTS.counts(event.matrix=individual.sows$farrowing,
                                        event.value=1,
                                        count.matrix=list(individual.sows$NrBornAlive),
                                        denominator.matrix=NULL)

dead.born.litter <- create.nonTS.counts(event.matrix=individual.sows$farrowing,
                                        event.value=1,
                                        count.matrix=list(individual.sows$NrBornDead),
                                        denominator.matrix=NULL)

perc.dead.born.litter <- create.nonTS.counts(event.matrix=individual.sows$farrowing,
                                             event.value=1,
                                             count.matrix=list(individual.sows$NrBornDead),
                                             denominator.matrix=list(individual.sows$NrBornAlive,
                                                                     individual.sows$NrBornDead))

small.born.litter <- create.nonTS.counts(event.matrix=individual.sows$farrowing,
                                         event.value=1,
                                         count.matrix=list(individual.sows$NrSmallStillBorn),
                                         denominator.matrix=NULL)

perc.small.born.litter <- create.nonTS.counts(event.matrix=individual.sows$farrowing,
                                              event.value=1,
                                              count.matrix=list(individual.sows$NrSmallStillBorn),
                                              denominator.matrix=list(individual.sows$NrBornAlive,
                                                                      individual.sows$NrBornDead))

weak.born.litter <- create.nonTS.counts(event.matrix=individual.sows$farrowing,
                                        event.value=1,
                                        count.matrix=list(individual.sows$NrWeakBorn),
                                        denominator.matrix=NULL)

perc.weak.born.litter <- create.nonTS.counts(event.matrix=individual.sows$farrowing,
                                              event.value=1,
                                              count.matrix=list(individual.sows$NrWeakBorn),
                                              denominator.matrix=list(individual.sows$NrBornAlive,
                                                                      individual.sows$NrBornDead))

mummi.born.litter <- create.nonTS.counts(event.matrix=individual.sows$farrowing,
                                         event.value=1,
                                         count.matrix=list(individual.sows$NrMummified),
                                         denominator.matrix=NULL)

perc.mummi.born.litter <- create.nonTS.counts(event.matrix=individual.sows$farrowing,
                                              event.value=1,
                                              count.matrix=list(individual.sows$NrMummified),
                                              denominator.matrix=list(individual.sows$NrBornAlive,
                                                                      individual.sows$NrBornDead))

# weaning indicators ----

weanings.week <- create.counts.week(data.matrix=individual.sows$weaning)
#sum(apply(weanings.week,MARGIN=2,FUN=sum,na.rm=TRUE))
#dim(weanings.week)
#tail(weanings.week)

total.wean.week <- create.counts.week(data.matrix=individual.sows$NrWeaned)
#dim(total.wean.week)
#tail(total.wean.week)


##dead.wean.week <- c
#how to get there?
individual.sows$DiffWeaned <- array(NA,
                    dim=c(
                      dim(individual.sows[[1]])[1],
                      dim(individual.sows[[1]])[2]
                    ))
individual.sows$NegDiffWeaned <- array(NA,
                    dim=c(
                      dim(individual.sows[[1]])[1],
                      dim(individual.sows[[1]])[2]
                    ))


for (is in 1:dim(individual.sows[[1]])[2]){
  for (r in 1:dim(individual.sows[[1]])[1]){
    if(!is.na(individual.sows$NrWeaned[r,is])){

      #last born row
      r0 <- max(which(!is.na(individual.sows$NrBornAlive[1:(r),is])))

      #last weaned row
      if(r==min(which(!is.na(individual.sows$NrWeaned[,is])))){
        r1<-0
      }else{
        r1 <- max(which(!is.na(individual.sows$NrWeaned[1:(r-1),is])))}

      #if there wasn't already a weaning event after the last registered birth
      if(r0>r1){
        DiffWeaned <- (individual.sows$NrWeaned[r,is]-
                                       individual.sows$NrBornAlive[r0,is] +
                                       individual.sows$NrMoved[r0,is])

        individual.sows$DiffWeaned[r,is]<- -DiffWeaned

        if(DiffWeaned>=0){
          individual.sows$NegDiffWeaned[r,is]<- 0
        }else{
          individual.sows$NegDiffWeaned[r,is]<- abs(DiffWeaned)
        }

      }
    }
  }
}




diff.wean.week <- create.counts.week(data.matrix=individual.sows$DiffWeaned)
# dim(diff.wean.week)
# tail(diff.wean.week)

negdiff.wean.week <- create.counts.week(data.matrix=individual.sows$NegDiffWeaned)
# dim(negdiff.wean.week)
# tail(negdiff.wean.week)



weaning.length <- create.nonTS.timeto(event1.matrix=individual.sows$farrowing,
                                       event1.value=1,
                                       event2.matrix=individual.sows$weaning,
                                       event2.value=1)

total.wean.litter <- create.nonTS.counts(event.matrix=individual.sows$weaning,
                                         event.value=1,
                                         count.matrix=list(individual.sows$NrWeaned),
                                         denominator.matrix=NULL)

negdiff.weaned.litter <- create.nonTS.counts(event.matrix=individual.sows$weaning,
                                             event.value=1,
                                             count.matrix=list(individual.sows$NegDiffWeaned),
                                             denominator.matrix=NULL)


perc.negdiff.weaned.litter <- create.nonTS.counts(event.matrix=individual.sows$weaning,
                                                  event.value=1,
                                                  count.matrix=list(individual.sows$NegDiffWeaned),
                                                  denominator.matrix=list(individual.sows$NrWeaned))


weight.wean.litter <- create.nonTS.counts(event.matrix=individual.sows$weaning,
                                          event.value=1,
                                          count.matrix=list(individual.sows$WeanedTotalWeight),
                                          denominator.matrix=NULL)


# exit indicators ----
number.exits.week <- create.counts.week(data.matrix=individual.sows$exit)
# dim(number.exits.week)
# tail(number.exits.week)

# deaths ----
number.deaths.week <- create.counts.week(data.matrix=individual.sows$death)
# dim(number.deaths.week)
# tail(number.deaths.week)



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


gilts.deaths.week <- rep(NA,dim(index.dates.week)[1])
for (r in 1:dim(index.dates.week)[1]){
  gilts.deaths.week[r] <- sum(gilts.deaths.days[((((r-1)*7)+1):min(c((r*7),dim(index.dates.days)[1])))],na.rm=TRUE)
  }


# mortality weaned pigs ----


piglets.deaths.days <- rep(0,dim(index.dates.days)[1])
if(dim(progeny.dead)[1]>1){
  for (d in 1:dim(index.dates.days)[1]){
    piglets.deaths.days[d] <- sum(progeny.dead$NumOfPigs[(which(progeny.dead$EventDate==index.dates.days$dates[d]))])
  }
}

piglets.deaths.week <- rep(NA,dim(index.dates.week)[1])
for (r in 1:dim(index.dates.week)[1]){
  piglets.deaths.week[r] <- sum(piglets.deaths.days[((((r-1)*7)+1):min(c((r*7),dim(index.dates.days)[1])))],na.rm=TRUE)
}






# exit reason ----

exit.reasons <- cause[(cause$ID2==language),"ID1"]
exit.reason.days <- matrix(NA,
                           nrow=dim(index.dates.days)[1],
                           ncol=length(exit.reasons)
)

colnames(exit.reason.days)<-as.character(cause[(cause$ID2==language),3])


for (r in 1:dim(index.dates.days)[1]){
  for (e in 1:length(exit.reasons)){
    exit.reason.days[r,e]<- length(which(individual.sows$ExitReason[r,]==exit.reasons[e]))
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
    exit.type.days[r,e]<- length(which(individual.sows$ExitType[r,]==exit.type[1,e]))
  }
}


exit.type.week <- matrix(NA,nrow=dim(index.dates.week)[1],ncol=dim(exit.type.days)[2])
colnames(exit.type.week)<- colnames(exit.type.days)

for (r in 1:dim(index.dates.week)[1]){
  for (c in 1:dim(exit.type.days)[2]){
    exit.type.week[r,c] <- sum(exit.type.days[((((r-1)*7)+1):min(c((r*7),dim(index.dates.days)[1]))),c],na.rm=TRUE)
  }}





# exit after event ----

individual.sows$ExitAfter <- array(NA,
                   dim=c(
                     dim(individual.sows[[1]])[1],
                     dim(individual.sows[[1]])[2]
                   ))
#sow.events.before.exit <- c("birth","service","reservice","abortion","farrowing","weaning")


for (is in 1:dim(individual.sows[[1]])[2]){
  for (r in 1:dim(individual.sows[[1]])[1]){
    if(!is.na(individual.sows$exit[r,is])){

      birth <- ifelse(length(which(!is.na(individual.sows$birth[1:r,is])))==0,
                      0,max(which(!is.na(individual.sows$birth[1:r,is])),na.rm=T))
      service  <- ifelse(length(which(!is.na(individual.sows$service[1:r,is])))==0,
                         0,max(which(!is.na(individual.sows$service[1:r,is])),na.rm=T))
      reservice <- 0
      if(service>0){
        if(individual.sows$service[service,is]==2){
          reservice<-service
          service <-0
        }}

      abortion  <- ifelse(length(which(!is.na(individual.sows$abortion[1:r,is])))==0,
                          0,max(which(!is.na(individual.sows$abortion[1:r,is])),na.rm=T))
      farrowing <- ifelse(length(which(!is.na(individual.sows$farrowing[1:r,is])))==0,
                          0,max(which(!is.na(individual.sows$farrowing[1:r,is])),na.rm=T))
      weaning   <- ifelse(length(which(!is.na(individual.sows$weaning[1:r,is])))==0,
                          0,max(which(!is.na(individual.sows$weaning[1:r,is])),na.rm=T))

      individual.sows$ExitAfter[r,is]<- order(c(birth,service,reservice,abortion,farrowing,weaning))[6]

    }}}



#sow.events.before.exit <- c("birth","service","reservice","abortion","farrowing","weaning")

exit.after.event.days <- matrix(NA,
                                nrow=dim(index.dates.days)[1],
                                ncol=length(sow.events.before.exit)
)

colnames(exit.after.event.days)<-sow.events.before.exit

for (r in 1:dim(index.dates.days)[1]){
  for (e in 1:length(sow.events.before.exit)){
    exit.after.event.days[r,e]<- length(which(individual.sows$ExitAfter[r,]==e))
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

individual.sows$DeathAfter <- array(NA,
                    dim=c(
                      dim(individual.sows[[1]])[1],
                      dim(individual.sows[[1]])[2]
                    ))


for (is in 1:dim(individual.sows[[1]])[2]){
  for (r in 1:dim(individual.sows[[1]])[1]){
    if(!is.na(individual.sows$death[r,is])){

      birth <- ifelse(length(which(!is.na(individual.sows$birth[1:r,is])))==0,
                      0,max(which(!is.na(individual.sows$birth[1:r,is])),na.rm=T))
      service  <- ifelse(length(which(!is.na(individual.sows$service[1:r,is])))==0,
                         0,max(which(!is.na(individual.sows$service[1:r,is])),na.rm=T))
      reservice <- 0
      if(service>0){
        if(individual.sows$service[service,is]==2){
          reservice<-service
          service <-0
        }}

      abortion  <- ifelse(length(which(!is.na(individual.sows$abortion[1:r,is])))==0,
                          0,max(which(!is.na(individual.sows$abortion[1:r,is])),na.rm=T))
      farrowing <- ifelse(length(which(!is.na(individual.sows$farrowing[1:r,is])))==0,
                          0,max(which(!is.na(individual.sows$farrowing[1:r,is])),na.rm=T))
      weaning   <- ifelse(length(which(!is.na(individual.sows$weaning[1:r,is])))==0,
                          0,max(which(!is.na(individual.sows$weaning[1:r,is])),na.rm=T))

      individual.sows$DeathAfter[r,is]<- order(c(birth,service,reservice,abortion,farrowing,weaning))[6]

    }}}



#sow.events.before.exit <- c("birth","service","reservice","abortion","farrowing","weaning")

death.after.event.days <- matrix(NA,
                                 nrow=dim(index.dates.days)[1],
                                 ncol=length(sow.events.before.exit)
)

colnames(death.after.event.days)<-sow.events.before.exit

for (r in 1:dim(index.dates.days)[1]){
  for (e in 1:length(sow.events.before.exit)){
    death.after.event.days[r,e]<- length(which(individual.sows$DeathAfter[r,]==e))
  }
}
#apply(exit.after.event.days,2,sum)

death.after.event.week <- matrix(NA,nrow=dim(index.dates.week)[1],ncol=dim(death.after.event.days)[2])
colnames(death.after.event.week)<- colnames(death.after.event.days)

for (r in 1:dim(index.dates.week)[1]){
  for (c in 1:dim(death.after.event.days)[2]){
    death.after.event.week[r,c] <- sum(death.after.event.days[((((r-1)*7)+1):min(c((r*7),dim(index.dates.days)[1]))),c],na.rm=TRUE)
  }}






# number of empty sows per day ----

# status sows ----
status.sows <- individual.sows$status

for (s in 1:dim(status.sows)[2]){

  if(all(is.na(status.sows[,s]))) next

  first.status <- min(which(!is.na(status.sows[,s])))

  for (r in (first.status+1):dim(status.sows)[1]){

    if(is.na(status.sows[r,s])){
      status.sows[r,s] <- status.sows[r-1,s]
    }

  }
}


empty.sows <- matrix(NA,
                     nrow=length(index.dates.days[,1]),
                     ncol=length(parity)+1)
colnames(empty.sows)<- c(0,parity)


for (r in 1:dim(empty.sows)[1]){
  for (c in 1:dim(empty.sows)[2]){
    parity.sows <- which(individual.sows$parity[r,]==colnames(empty.sows)[c])
    empty.sows[r,c] <- length(which(status.sows[r,parity.sows]==0|status.sows[r,parity.sows]==1))
  }}




# empty days ----
empty.days <- create.nonTS.timeto(event1.matrix=individual.sows$weaning,
                                  event1.value=1,
                                  event2.matrix=individual.sows$service,
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




## ALL INDICATORS ----

save(individual.sows,file="data/individual.sows2.RData")

save(index.dates.days,  index.dates.week,

     services.week              ,#   NumberOfWeeks x   ParityFrom1
     reservices.week	          ,#	 NumberOfWeeks x   ParityFrom1
     perc.pregnancy	            ,#	 NumberOfWeeks x   ParityFrom1   x 2
     perc.failure			          ,#   NumberOfWeeks x   ParityFrom1   x 2
     perc.reservice		          ,#	 NumberOfWeeks x   ParityFrom1   x 2
     time.to.reservice          ,#	 579   4
     rereservices		            ,#   0 4
     time.to.first.service  	  ,#	 2353    4
     time.to.first.farrowing	  ,#	 2172    4
     abortions.week		      	  ,#	 NumberOfWeeks x   ParityFrom1
     time.to.abortion	      	  ,#	 34  4
     days.between.farrowings	  ,#	 6934    4
     farrowings.week		        ,#	 NumberOfWeeks x   ParityFrom1
     total.born.week		    	  ,#	 NumberOfWeeks x   ParityFrom1
     live.born.week			        ,#	 NumberOfWeeks x   ParityFrom1
     dead.born.week		      	  ,#	 NumberOfWeeks x   ParityFrom1
     small.born.week	    		  ,#	 NumberOfWeeks x   ParityFrom1
     weak.born.week		      	  ,#	 NumberOfWeeks x   ParityFrom1
     mummi.born.week	    		  ,#	 NumberOfWeeks x   ParityFrom1
     perc.dead.born.week   		  ,#	 NumberOfWeeks x   ParityFrom1   2
     perc.small.born.week	  	  ,#	 NumberOfWeeks x   ParityFrom1   2
     perc.weak.born.week	  	  ,#	 NumberOfWeeks x   ParityFrom1   2
     perc.mummi.born.week	  	  ,#	 NumberOfWeeks x   ParityFrom1   2
     pregnancy.length		        ,#	 9106    4
     services.to.farrow		      ,#	 9106    4
     total.born.litter		      ,#	 9106    4
     live.born.litter		        ,#	 9106    4
     dead.born.litter		        ,#	 9106    4
     perc.dead.born.litter		  ,#	 9106    4
     small.born.litter	    	  ,#	 9106    4
     perc.small.born.litter		  ,#	 9106    4
     weak.born.litter		        ,#	 9106    4
     perc.weak.born.litter		  ,#	 9106    4
     mummi.born.litter	    	  ,#	 9106    4
     perc.mummi.born.litter		  ,#	 9106    4
     weanings.week			        ,#	 NumberOfWeeks x   ParityFrom1
     total.wean.week			      ,#	 NumberOfWeeks x   ParityFrom1
     diff.wean.week			        ,#	 NumberOfWeeks x   ParityFrom1
     negdiff.wean.week	    	  ,#	 NumberOfWeeks x   ParityFrom1
     weaning.length			        ,#	 9613    4
     total.wean.litter    		  ,#	 9613    4
     negdiff.weaned.litter		  ,#	 9613    4
     perc.negdiff.weaned.litter ,#	 9613    4
     weight.wean.litter		      ,#	 9613    4
     number.exits.week		      ,#	 NumberOfWeeks x   ParityFrom1
     number.deaths.week		      ,#	 NumberOfWeeks x   ParityFrom1
     gilts.deaths.week		      ,#	 NumberOfWeeks x
     piglets.deaths.week		    ,#	 NumberOfWeeks x
     exit.reason.week		        ,#	 NumberOfWeeks x  614
     exit.type.week			        ,#	 NumberOfWeeks x    7
     exit.after.event.week		  ,#	 NumberOfWeeks x    6
     death.after.event.week		  ,#	 NumberOfWeeks x    6
     empty.long.week			      ,#	 NumberOfWeeks x   ParityFrom1

     active.sows.displayID,

     file="data/indicators.RData")


