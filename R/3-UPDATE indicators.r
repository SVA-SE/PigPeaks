# packages ----

packages <- c("ISOweek","lubridate","abind")
install.packages(setdiff(packages, rownames(installed.packages())))

require(ISOweek)
require(lubridate)
require(abind)

#source("Definitions.r") #settings already runs definitions
source("Settings.r")
source("R/Functions.r")


#load("data/individual.sows.RData")
#load("data/animal.RData")
load("data/indicators-ALL.RData")

index.dates.days$ISOweekYear <- as.numeric(substr(as.character(index.dates.days[,"ISOweek"]),1,4))
index.dates.week <- index.dates.days[!duplicated(index.dates.days[,c("ISOweekYear","week")]),
                                     c("ISOweekYear","week","dates")]
colnames(index.dates.week)<- c("ISOweekYear","week","start")



rows.week.update <- index.start.week:dim(index.dates.week)[1]

#intersect(which(indicators.sys==TRUE), which(indicators.type=="C"))

for (i in which(indicators.type=="W")){
  if(length(dim(indicators.data[[i]]))==3){
    indicators.data[[i]] <- indicators.data[[i]][1:(index.start.week-1),,]
  }
  
  if(length(dim(indicators.data[[i]]))==2){
    indicators.data[[i]] <- indicators.data[[i]][1:(index.start.week-1),]
  }
  
  if(is.null(dim(indicators.data[[i]]))){
    indicators.data[[i]] <- indicators.data[[i]][1:(index.start.week-1)]
  }
  
}


for (i in which(indicators.type=="C")){
  max.date <- as.numeric(start.date-1)
  remove.rows <- which(indicators.data[[i]][,"date"]>max.date)
  if(length(remove.rows)>0){
  indicators.data[[i]] <- indicators.data[[i]][-remove.rows,]
  }
}



for (i in 1:length(indicators.all)){
  if(indicators.all.type[i]=="W"){
    if(length(dim(get(indicators.all[[i]])))==3){
      x <- get(indicators.all[[i]])[1:(index.start.week-1),,]
      assign(indicators.all[[i]],x)
    }
    
    if(length(dim(get(indicators.all[[i]])))==2){
      x <- get(indicators.all[[i]])[1:(index.start.week-1),]
      assign(indicators.all[[i]],x)
    }
    
    if(is.null(dim(get(indicators.all[[i]])))){
      x <- get(indicators.all[[i]])[1:(index.start.week-1)]
      assign(indicators.all[[i]],x)
    }
  }
  
  if(indicators.all.type[i]=="C"){
    max.date <- as.numeric(start.date-1)
    remove.rows <- which(get(indicators.all[[i]])[,"date"]>max.date)
    x <- get(indicators.all[[i]])
    if(length(remove.rows)>0){
    x <- x[-remove.rows,]
    }
    assign(indicators.all[[i]],x)
    
  }
}


# number of services ----

total.services <- individual.sows$service[rows.update.index,]


services <- total.services
for (s in 1:dim(total.services)[2]){
  services[which(services[,s]==2),s]<-NA
}

reservices<- total.services
for (s in 1:dim(total.services)[2]){
  reservices[which(reservices[,s]==1),s]<-NA
  reservices[which(reservices[,s]==2),s]<-1
}


services.week.u <- create.counts.week(rows.index.days=index.dates.days[rows.update.index,1],
                                    rows.index.week=index.dates.week[rows.week.update,1],
                                    cols.index=parity,
                                    data.matrix=services, #<--
                                    group.matrix=individual.sows$parity[rows.update.index,])

services.week <- rbind(services.week,services.week.u)

# number of REservices ---

reservices.week.u <- create.counts.week(data.matrix=reservices,
                                        rows.index.days=index.dates.days[rows.update.index,1],
                                        rows.index.week=index.dates.week[rows.week.update,1],
                                        group.matrix=individual.sows$parity[rows.update.index,])
#sum(apply(reservices.week,MARGIN=2,FUN=sum,na.rm=TRUE))
reservices.week <- rbind(reservices.week,reservices.week.u)




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

#to update, it will need to look back those 4 weeks (day 4 weeks + 1 should be the day 1)

look.back.days <- (min(rows.update.index)-(reservice.perc.window*7)):max(rows.update.index)
service.slice <- individual.sows$service[look.back.days,]
parity.slice  <- individual.sows$parity[look.back.days,]
exit.slide    <- individual.sows$exit[look.back.days,]

assumed.pregnant <- array(NA,
                          dim=c(
                            dim(service.slice)[1],
                            dim(service.slice)[2],
                            length(parity)
                          ))



for (r in (reservice.perc.window*7+1):length(look.back.days)){
  for (s in 1:dim(service.slice)[2]){
    if (!is.na(service.slice[(r-(7*reservice.perc.window)+1),s])&
        service.slice[(r-(7*reservice.perc.window)+1),s]==1){

      assumed.pregnant[r,s,parity.slice[(r-(7*reservice.perc.window)+1),s]]<-1

      if (length(which(service.slice[((r-(7*reservice.perc.window)+2):(r)),s]==2))>0){
        assumed.pregnant[r,s,parity.slice[(r-(7*reservice.perc.window)+1),s]]<-2
      }

      if (length(which(!is.na(exit.slide[((r-(7*reservice.perc.window)+2):(r)),s])))>0){
        assumed.pregnant[r,s,parity.slice[(r-(7*reservice.perc.window)+1),s]]<-3
      }
    }
  }
}
#length(which(assumed.pregnant==2))
#length(which(assumed.pregnant==1))
#length(which(assumed.pregnant==3))

assumed.pregnant <- assumed.pregnant[((7*reservice.perc.window)+1):dim(assumed.pregnant)[1],,]


perc.pregnancy.u <-create.perc.week.1(rows.index.week=index.dates.week[rows.week.update,1],
                                    cols.index=parity,
                                    data=assumed.pregnant,
                                    value.to.count=1,
                                    test="equal")  #c("equal","greater")

perc.failure.u <-create.perc.week.1(rows.index.week=index.dates.week[rows.week.update,1],
                                  data=assumed.pregnant,
                                  value.to.count=1,
                                  test="greater")
perc.reservice.u <-create.perc.week.1(rows.index.week=index.dates.week[rows.week.update,1],
                                    data=assumed.pregnant,
                                    value.to.count=2,
                                    test="equal")

perc.pregnancy <- abind(perc.pregnancy,perc.pregnancy.u,along=1)
perc.failure <- abind(perc.failure,perc.failure.u,along=1)
perc.reservice <- abind(perc.reservice,perc.reservice.u,along=1)



# dim(perc.pregnancy)
# sum(perc.pregnancy[,,1])
# sum(perc.failure[,,1])
# sum(perc.reservice[,,1])



# time to reservice, when there is a reservice ----

look.back.days <- ((min(rows.update.index)-(164)):max(rows.update.index))
#look at least a pregnancy length ago, not to miss when the service was
#as then take only the REservices within the dates wished

time.to.reservice.u <- create.nonTS.timeto(group.matrix=individual.sows$parity[look.back.days,],
                                         index.dates=index.dates.days[look.back.days,1],
                                         col1="indicator",
                                         col2="parity",
                                         col3="date",
                                         event1.matrix=individual.sows$service[look.back.days,],
                                         event1.value=1,
                                         event2.matrix=individual.sows$service[look.back.days,],
                                         event2.value=2,
                                         condition="first")

time.to.reservice.u <- time.to.reservice.u[-which(time.to.reservice.u[,"indicator"]==0),]


max.date <- as.numeric(start.date)
remove.rows <- which(time.to.reservice.u[,"date"]<max.date)
if(length(remove.rows)>0){
  time.to.reservice.u <- time.to.reservice.u[-remove.rows,]
}

#dim(time.to.reservice.u) #sum(reservices.week.u,na.rm=T)
#tail(time.to.reservice.u)
#converting back to date: as.Date(time.to.reservice$date,origin="1970-01-01")

time.to.reservice <- rbind(time.to.reservice,time.to.reservice.u)




rereservices.u <- create.nonTS.timeto(group.matrix=individual.sows$parity[look.back.days,],
                                    index.dates=index.dates.days[look.back.days,1],
                                    event1.matrix=individual.sows$service[look.back.days,],
                                    event1.value=2,
                                    event2.matrix=individual.sows$service[look.back.days,],
                                    event2.value=2,
                                    condition="first")
#dim(rereservices.u)
#tail(rereservices)
#rereservices[,1]


rereservices.u <- rereservices.u[rereservices.u[,1]!=0&rereservices.u[,1]<min.pregnancy.length,]

max.date <- as.numeric(start.date)
remove.rows <- which(rereservices.u[,"date"]<max.date)
if(length(remove.rows)>0){
rereservices.u <- rereservices.u[-remove.rows,]
}

rereservices <- rbind(rereservices,rereservices.u)




# time.to.first.service ----
look.back.days <- ((min(rows.update.index)-(400)):max(rows.update.index))
#make sure to include the sow birth even if falls on historical data

time.to.first.service.u <- create.nonTS.timeto(group.matrix=individual.sows$parity[look.back.days,],
                                               index.dates=index.dates.days[look.back.days,1],
                                               event1.matrix=individual.sows$birth[look.back.days,],
                                             event1.value=1,
                                             event2.matrix=individual.sows$service[look.back.days,],
                                             event2.value=1,
                                             condition="first")

max.date <- as.numeric(start.date)
remove.rows <- which(time.to.first.service.u[,"date"]<max.date)
if(length(remove.rows)>0){
time.to.first.service.u <- time.to.first.service.u[-remove.rows,]
}

time.to.first.service <- rbind(time.to.first.service,time.to.first.service.u)


# time.to.first.farrowing ----

time.to.first.farrowing.u <- create.nonTS.timeto(group.matrix=individual.sows$parity[look.back.days,],
                                               index.dates=index.dates.days[look.back.days,1],
                                               event1.matrix=individual.sows$birth[look.back.days,],
                                               event1.value=1,
                                               event2.matrix=individual.sows$farrowing[look.back.days,],
                                               event2.value=1,
                                               condition="first")


remove.rows <- which(time.to.first.farrowing.u[,"date"]<max.date)
if(length(remove.rows)>0){
time.to.first.farrowing.u <- time.to.first.farrowing.u[-remove.rows,]
}

time.to.first.farrowing<- rbind(time.to.first.farrowing,time.to.first.farrowing.u)



# abortion ----

abortions.week.u <- create.counts.week(rows.index.days=index.dates.days[rows.update.index,1],
                                     rows.index.week=index.dates.week[rows.week.update,1],
                                     data.matrix=individual.sows$abortion[rows.update.index,],
                                     group.matrix=individual.sows$parity[rows.update.index,])
abortions.week <- rbind(abortions.week,abortions.week.u)




time.to.abortion.u <- create.nonTS.timeto(group.matrix=individual.sows$parity[look.back.days,],
                                        index.dates=index.dates.days[look.back.days,1],
                                        event1.matrix=individual.sows$service[look.back.days,],
                                        event1.value=c(1,2),
                                        event2.matrix=individual.sows$abortion[look.back.days,],
                                        event2.value=1,
                                        condition=NULL)

remove.rows <- which(time.to.abortion.u[,"date"]<max.date)
if(length(remove.rows)>0){
  time.to.abortion.u <- time.to.abortion.u[-remove.rows,]
  }

time.to.abortion <- rbind(time.to.abortion,time.to.abortion.u)


# days between farrowings ----

days.between.farrowings.u <- create.nonTS.timeto(group.matrix=individual.sows$parity[look.back.days,],
                                               index.dates=index.dates.days[look.back.days,1],
                                               event1.matrix=individual.sows$farrowing[look.back.days,],
                                               event1.value=1,
                                               event2.matrix=individual.sows$farrowing[look.back.days,],
                                               event2.value=1)
days.between.farrowings.u <- days.between.farrowings.u[days.between.farrowings.u[,"indicator"]!=0,]

remove.rows <- which(days.between.farrowings.u[,"date"]<max.date)
if(length(remove.rows)>0){
  days.between.farrowings.u <- days.between.farrowings.u[-remove.rows,]
}

days.between.farrowings <- rbind(days.between.farrowings,days.between.farrowings.u)


# farrowing counts ----

farrowings.week.u <- create.counts.week(rows.index.days=index.dates.days[rows.update.index,1],
                                      rows.index.week=index.dates.week[rows.week.update,1],
                                      data.matrix=individual.sows$farrowing[rows.update.index,],
                                      group.matrix=individual.sows$parity[rows.update.index,])

farrowings.week <- rbind(farrowings.week,farrowings.week.u)





live.born.week.u <- create.counts.week(rows.index.days=index.dates.days[rows.update.index,1],
                                       rows.index.week=index.dates.week[rows.week.update,1],
                                       group.matrix=individual.sows$parity[rows.update.index,],
                                       data.matrix=individual.sows$NrBornAlive[rows.update.index,])
dead.born.week.u <- create.counts.week(rows.index.days=index.dates.days[rows.update.index,1],
                                       rows.index.week=index.dates.week[rows.week.update,1],
                                       group.matrix=individual.sows$parity[rows.update.index,],
                                       data.matrix=individual.sows$NrBornDead[rows.update.index,])
total.born.week.u = live.born.week.u + dead.born.week.u


live.born.week <- rbind(live.born.week,live.born.week.u)
dead.born.week <- rbind(dead.born.week,dead.born.week.u)
total.born.week <- rbind(total.born.week,total.born.week.u)



small.born.week.u <- create.counts.week(rows.index.days=index.dates.days[rows.update.index,1],
                                      rows.index.week=index.dates.week[rows.week.update,1],
                                      group.matrix=individual.sows$parity[rows.update.index,],
                                      data.matrix=individual.sows$NrSmallStillBorn[rows.update.index,])
small.born.week <- rbind(small.born.week,small.born.week.u)

weak.born.week.u <- create.counts.week(rows.index.days=index.dates.days[rows.update.index,1],
                                       rows.index.week=index.dates.week[rows.week.update,1],
                                       group.matrix=individual.sows$parity[rows.update.index,],
                                       data.matrix=individual.sows$NrWeakBorn[rows.update.index,])
weak.born.week <- rbind(weak.born.week,weak.born.week.u)

mummi.born.week.u <- create.counts.week(rows.index.days=index.dates.days[rows.update.index,1],
                                        rows.index.week=index.dates.week[rows.week.update,1],
                                        group.matrix=individual.sows$parity[rows.update.index,],
                                        data.matrix=individual.sows$NrMummified[rows.update.index,])
mummi.born.week <- rbind(mummi.born.week,mummi.born.week.u)


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
pregnancy.length.u <- create.nonTS.timeto(group.matrix=individual.sows$parity[look.back.days,],
                                        index.dates=index.dates.days[look.back.days,1],
                                        event1.matrix=individual.sows$service[look.back.days,],
                                        event1.value=c(1,2),
                                        event2.matrix=individual.sows$farrowing[look.back.days,],
                                        event2.value=1,
                                        condition=NULL)

remove.rows <- which(pregnancy.length.u[,"date"]<max.date)
if(length(remove.rows)>0){
  pregnancy.length.u <- pregnancy.length.u[-remove.rows,]
}

pregnancy.length <- rbind(pregnancy.length,pregnancy.length.u)




services.to.farrow.u <- create.nonTS.eventsto(group.matrix=individual.sows$parity[look.back.days,],
                                            index.dates=index.dates.days[look.back.days,1],
                                            col1="indicator",
                                            col2="parity",
                                            col3="date",
                                            event1.matrix=individual.sows$service[look.back.days,],
                                            event1.value=c(1,2),
                                            event2.matrix=individual.sows$farrowing[look.back.days,],
                                            event2.value=1)
remove.rows <- which(services.to.farrow.u[,"date"]<max.date)
if(length(remove.rows)>0){
  services.to.farrow.u <- services.to.farrow.u[-remove.rows,]
}

services.to.farrow <- rbind(services.to.farrow,services.to.farrow.u)




total.born.litter.u <- create.nonTS.counts(group.matrix=individual.sows$parity[rows.update.index,],
                                         index.dates=index.dates.days[rows.update.index,1],
                                         col1="indicator",
                                         col2="parity",
                                         col3="date",
                                         event.matrix=individual.sows$farrowing[rows.update.index,],
                                         event.value=1,
                                         count.matrix=list(individual.sows$NrBornAlive[rows.update.index,],
                                                           individual.sows$NrBornDead[rows.update.index,]),
                                         denominator.matrix=NULL)

total.born.litter <- rbind(total.born.litter,total.born.litter.u)



live.born.litter.u <- create.nonTS.counts(group.matrix=individual.sows$parity[rows.update.index,],
                                        index.dates=index.dates.days[rows.update.index,1],
                                        event.matrix=individual.sows$farrowing[rows.update.index,],
                                        event.value=1,
                                        count.matrix=list(individual.sows$NrBornAlive[rows.update.index,]),
                                        denominator.matrix=NULL)

live.born.litter <- rbind(live.born.litter,live.born.litter.u)




dead.born.litter.u <- create.nonTS.counts(group.matrix=individual.sows$parity[rows.update.index,],
                                        index.dates=index.dates.days[rows.update.index,1],
                                        event.matrix=individual.sows$farrowing[rows.update.index,],
                                        event.value=1,
                                        count.matrix=list(individual.sows$NrBornDead[rows.update.index,]),
                                        denominator.matrix=NULL)
dead.born.litter <- rbind(dead.born.litter,dead.born.litter.u)




perc.dead.born.litter.u <- create.nonTS.counts(group.matrix=individual.sows$parity[rows.update.index,],
                                             index.dates=index.dates.days[rows.update.index,1],
                                             event.matrix=individual.sows$farrowing[rows.update.index,],
                                             event.value=1,
                                             count.matrix=list(individual.sows$NrBornDead[rows.update.index,]),
                                             denominator.matrix=list(individual.sows$NrBornAlive[rows.update.index,],
                                                                     individual.sows$NrBornDead[rows.update.index,]))
perc.dead.born.litter.u[, "indicator"] <-round(perc.dead.born.litter.u[, "indicator"]*100, 2)
perc.dead.born.litter <- rbind(perc.dead.born.litter,perc.dead.born.litter.u)



small.born.litter.u <- create.nonTS.counts(group.matrix=individual.sows$parity[rows.update.index,],
                                         index.dates=index.dates.days[rows.update.index,1],
                                         event.matrix=individual.sows$farrowing[rows.update.index,],
                                         event.value=1,
                                         count.matrix=list(individual.sows$NrSmallStillBorn[rows.update.index,]),
                                         denominator.matrix=NULL)
small.born.litter <- rbind(small.born.litter,small.born.litter.u)



perc.small.born.litter.u <- create.nonTS.counts(group.matrix=individual.sows$parity[rows.update.index,],
                                              index.dates=index.dates.days[rows.update.index,1],
                                              event.matrix=individual.sows$farrowing[rows.update.index,],
                                              event.value=1,
                                              count.matrix=list(individual.sows$NrSmallStillBorn[rows.update.index,]),
                                              denominator.matrix=list(individual.sows$NrBornAlive[rows.update.index,],
                                                                      individual.sows$NrBornDead[rows.update.index,]))
perc.small.born.litter.u[, "indicator"] <-round(perc.small.born.litter.u[, "indicator"]*100, 2)
perc.small.born.litter <- rbind(perc.small.born.litter,perc.small.born.litter.u)


weak.born.litter.u <- create.nonTS.counts(group.matrix=individual.sows$parity[rows.update.index,],
                                        index.dates=index.dates.days[rows.update.index,1],
                                        event.matrix=individual.sows$farrowing[rows.update.index,],
                                        event.value=1,
                                        count.matrix=list(individual.sows$NrWeakBorn[rows.update.index,]),
                                        denominator.matrix=NULL)
weak.born.litter <- rbind(weak.born.litter,weak.born.litter.u)


perc.weak.born.litter.u <- create.nonTS.counts(group.matrix=individual.sows$parity[rows.update.index,],
                                             index.dates=index.dates.days[rows.update.index,1],
                                             event.matrix=individual.sows$farrowing[rows.update.index,],
                                              event.value=1,
                                              count.matrix=list(individual.sows$NrWeakBorn[rows.update.index,]),
                                              denominator.matrix=list(individual.sows$NrBornAlive[rows.update.index,],
                                                                      individual.sows$NrBornDead[rows.update.index,]))
perc.weak.born.litter.u[, "indicator"] <-round(perc.weak.born.litter.u[, "indicator"]*100, 2)
perc.weak.born.litter <- rbind(perc.weak.born.litter,perc.weak.born.litter.u)


mummi.born.litter.u <- create.nonTS.counts(group.matrix=individual.sows$parity[rows.update.index,],
                                         index.dates=index.dates.days[rows.update.index,1],
                                         event.matrix=individual.sows$farrowing[rows.update.index,],
                                         event.value=1,
                                         count.matrix=list(individual.sows$NrMummified[rows.update.index,]),
                                         denominator.matrix=NULL)
mummi.born.litter <- rbind(mummi.born.litter,mummi.born.litter.u)

perc.mummi.born.litter.u <- create.nonTS.counts(group.matrix=individual.sows$parity[rows.update.index,],
                                              index.dates=index.dates.days[rows.update.index,1],
                                              event.matrix=individual.sows$farrowing[rows.update.index,],
                                              event.value=1,
                                              count.matrix=list(individual.sows$NrMummified[rows.update.index,]),
                                              denominator.matrix=list(individual.sows$NrBornAlive[rows.update.index,],
                                                                      individual.sows$NrBornDead[rows.update.index,]))
perc.mummi.born.litter.u[, "indicator"] <-round(perc.mummi.born.litter.u[, "indicator"]*100, 2)
perc.mummi.born.litter <- rbind(perc.mummi.born.litter,perc.mummi.born.litter.u)



# weaning indicators ----

weanings.week.u <- create.counts.week(rows.index.days=index.dates.days[rows.update.index,1],
                                    rows.index.week=index.dates.week[rows.week.update,1],
                                    group.matrix=individual.sows$parity[rows.update.index,],
                                    data.matrix=individual.sows$weaning[rows.update.index,])
weanings.week <- rbind(weanings.week,weanings.week.u)



total.wean.week.u <- create.counts.week(rows.index.days=index.dates.days[rows.update.index,1],
                                      rows.index.week=index.dates.week[rows.week.update,1],
                                      group.matrix=individual.sows$parity[rows.update.index,],
                                      data.matrix=individual.sows$NrWeaned[rows.update.index,])
total.wean.week <- rbind(total.wean.week,total.wean.week.u)


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




diff.wean.week.u <- create.counts.week(rows.index.days=index.dates.days[rows.update.index,1],
                                       rows.index.week=index.dates.week[rows.week.update,1],
                                       group.matrix=individual.sows$parity[rows.update.index,],
                                       data.matrix=individual.sows$DiffWeaned[rows.update.index,])
diff.wean.week <- rbind(diff.wean.week,diff.wean.week.u)


negdiff.wean.week.u <- create.counts.week(rows.index.days=index.dates.days[rows.update.index,1],
                                        rows.index.week=index.dates.week[rows.week.update,1],
                                        group.matrix=individual.sows$parity[rows.update.index,],
                                        data.matrix=individual.sows$NegDiffWeaned[rows.update.index,])
negdiff.wean.week <- rbind(negdiff.wean.week,negdiff.wean.week.u)



weaning.length.u <- create.nonTS.timeto(group.matrix=individual.sows$parity[look.back.days,],
                                      index.dates=index.dates.days[look.back.days,1],
                                      event1.matrix=individual.sows$farrowing[look.back.days,],
                                       event1.value=1,
                                       event2.matrix=individual.sows$weaning[look.back.days,],
                                       event2.value=1)
remove.rows <- which(weaning.length.u[,"date"]<max.date)
if(length(remove.rows)>0){
  weaning.length.u <- weaning.length.u[-remove.rows,]
}
weaning.length <- rbind(weaning.length,weaning.length.u)



total.wean.litter.u <- create.nonTS.counts(group.matrix=individual.sows$parity[rows.update.index,],
                                         index.dates=index.dates.days[rows.update.index,1],
                                         event.matrix=individual.sows$weaning[rows.update.index,],
                                         event.value=1,
                                         count.matrix=list(individual.sows$NrWeaned[rows.update.index,]),
                                         denominator.matrix=NULL)
total.wean.litter <- rbind(total.wean.litter,total.wean.litter.u)


negdiff.weaned.litter.u <- create.nonTS.counts(group.matrix=individual.sows$parity[rows.update.index,],
                                             index.dates=index.dates.days[rows.update.index,1],
                                             event.matrix=individual.sows$weaning[rows.update.index,],
                                             event.value=1,
                                             count.matrix=list(individual.sows$NegDiffWeaned[rows.update.index,]),
                                             denominator.matrix=NULL)
negdiff.weaned.litter <- rbind(negdiff.weaned.litter,negdiff.weaned.litter.u)


perc.negdiff.weaned.litter.u <- create.nonTS.counts(group.matrix=individual.sows$parity[rows.update.index,],
                                                  index.dates=index.dates.days[rows.update.index,1],
                                                  event.matrix=individual.sows$weaning[rows.update.index,],
                                                  event.value=1,
                                                  count.matrix=list(individual.sows$NegDiffWeaned[rows.update.index,]),
                                                  denominator.matrix=list(individual.sows$NrWeaned[rows.update.index,]))
perc.negdiff.weaned.litter.u[, "indicator"] <-round(perc.negdiff.weaned.litter.u[, "indicator"]*100, 2)
perc.negdiff.weaned.litter <- rbind(perc.negdiff.weaned.litter,perc.negdiff.weaned.litter.u)


weight.wean.litter.u <- create.nonTS.counts(group.matrix=individual.sows$parity[rows.update.index,],
                                          index.dates=index.dates.days[rows.update.index,1],
                                          event.matrix=individual.sows$weaning[rows.update.index,],
                                          event.value=1,
                                          count.matrix=list(individual.sows$WeanedTotalWeight[rows.update.index,]),
                                          denominator.matrix=NULL)
weight.wean.litter <- rbind(weight.wean.litter,weight.wean.litter.u)



# exit indicators ----
number.exits.week.u <- create.counts.week(rows.index.days=index.dates.days[rows.update.index,1],
                                        rows.index.week=index.dates.week[rows.week.update,1],
                                        group.matrix=individual.sows$parity[rows.update.index,],
                                        data.matrix=individual.sows$exit[rows.update.index,])
number.exits.week <- rbind(number.exits.week,number.exits.week.u)


# deaths ----
number.deaths.week.u <- create.counts.week(rows.index.days=index.dates.days[rows.update.index,1],
                                         rows.index.week=index.dates.week[rows.week.update,1],
                                         group.matrix=individual.sows$parity[rows.update.index,],
                                         data.matrix=individual.sows$death[rows.update.index,])
number.deaths.week <- rbind(number.deaths.week,number.deaths.week.u)


#mortality break down by parity groups gilts and parity 1
#getting directly from exit

death.gilts <- exit[(exit$ExitType==3|exit$ExitType==4)&(
  exit$AnimalType==3),]


gilts.deaths.days <- rep(0,dim(index.dates.days)[1])
if(dim(death.gilts)[1]>1){
  for (d in rows.update.index){
    gilts.deaths.days[d] <- length(which(death.gilts$EventDate==index.dates.days$dates[d]))
  }
}


gilts.deaths.week <- c(gilts.deaths.week,rep(NA,length(rows.week.update)))
for (r in rows.week.update){
  gilts.deaths.week[r] <- sum(gilts.deaths.days[((((r-1)*7)+1):min(c((r*7),dim(index.dates.days)[1])))],na.rm=TRUE)
  }


# mortality weaned pigs ----


piglets.deaths.days <- rep(0,dim(index.dates.days)[1])
if(dim(progeny.dead)[1]>1){
  for (d in rows.update.index){
    piglets.deaths.days[d] <- sum(progeny.dead$NumOfPigs[(which(progeny.dead$EventDate==index.dates.days$dates[d]))])
  }
}

piglets.deaths.week <- c(piglets.deaths.week,rep(NA,length(rows.week.update)))
for (r in rows.week.update){
  piglets.deaths.week[r] <- sum(piglets.deaths.days[((((r-1)*7)+1):min(c((r*7),dim(index.dates.days)[1])))],na.rm=TRUE)
}






# exit reason ----

# exit.reasons <- cause[(cause$ID2==language),"ID1"]
# exit.reason.days <- matrix(NA,
#                            nrow=dim(index.dates.days)[1],
#                            ncol=length(exit.reasons)
# )
# 
# colnames(exit.reason.days)<-as.character(cause[(cause$ID2==language),3])
# 
# 
# for (r in 1:dim(index.dates.days)[1]){
#   for (e in 1:length(exit.reasons)){
#     exit.reason.days[r,e]<- length(which(individual.sows$ExitReason[r,]==exit.reasons[e]))
#   }
# }
# 
# 
# exit.reason.week <- matrix(NA,nrow=dim(index.dates.week)[1],ncol=dim(exit.reason.days)[2])
# colnames(exit.reason.week)<- colnames(exit.reason.days)
# 
# for (r in 1:dim(index.dates.week)[1]){
#   for (c in 1:dim(exit.reason.days)[2]){
#     exit.reason.week[r,c] <- sum(exit.reason.days[((((r-1)*7)+1):min(c((r*7),dim(index.dates.days)[1]))),c],na.rm=TRUE)
#   }}


# exit type ----

exit.type.days <- matrix(NA,
                         nrow=dim(index.dates.days)[1],
                         ncol=ncol(exit.type)
)

colnames(exit.type.days)<-colnames(exit.type)

for (r in rows.update.index){
  for (e in 1:ncol(exit.type)){
    exit.type.days[r,e]<- length(which(individual.sows$ExitType[r,]==exit.type[1,e]))
  }
}


exit.type.week.u <- matrix(NA,nrow=length(rows.week.update),ncol=dim(exit.type.days)[2])
colnames(exit.type.week.u)<- colnames(exit.type.days)
exit.type.week <- rbind(exit.type.week,exit.type.week.u)

for (r in rows.week.update){
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

for (r in rows.update.index){
  for (e in 1:length(sow.events.before.exit)){
    exit.after.event.days[r,e]<- length(which(individual.sows$ExitAfter[r,]==e))
  }
}
#apply(exit.after.event.days,2,sum)

exit.after.event.week.u <- matrix(NA,nrow=length(rows.week.update),ncol=dim(exit.after.event.days)[2])
colnames(exit.after.event.week.u)<- colnames(exit.after.event.days)
exit.after.event.week <- rbind(exit.after.event.week,exit.after.event.week.u)

for (r in rows.week.update){
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

for (r in rows.update.index){
  for (e in 1:length(sow.events.before.exit)){
    death.after.event.days[r,e]<- length(which(individual.sows$DeathAfter[r,]==e))
  }
}
#apply(exit.after.event.days,2,sum)

death.after.event.week.u <- matrix(NA,nrow=length(rows.week.update),ncol=dim(death.after.event.days)[2])
colnames(death.after.event.week.u)<- colnames(death.after.event.days)
death.after.event.week <- rbind(death.after.event.week,death.after.event.week.u)

for (r in rows.week.update){
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


for (r in rows.update.index){
  for (c in 1:dim(empty.sows)[2]){
    parity.sows <- which(individual.sows$parity[r,]==colnames(empty.sows)[c])
    empty.sows[r,c] <- length(which(status.sows[r,parity.sows]==0|status.sows[r,parity.sows]==1))
  }}




# empty days ----
empty.days <- create.nonTS.timeto(group.matrix=individual.sows$parity[look.back.days,],
                                  index.dates=index.dates.days[look.back.days,1],
                                  event1.matrix=individual.sows$weaning[look.back.days,],
                                  event1.value=1,
                                  event2.matrix=individual.sows$service[look.back.days,],
                                  event2.value=1)

remove.rows <- which(empty.days[,"date"]<max.date)
if(length(remove.rows)>0){
  empty.days <- empty.days[-remove.rows,]
}

# empty more than 4 days every week ----
#empty.long.week

empty.long.week.u <- matrix(NA,
                          nrow=dim(index.dates.week)[1],
                          ncol=length(parity))
colnames(empty.long.week.u) <- parity

for (r in rows.week.update){
  for (c in parity){

    year=index.dates.week$ISOweekYear[r]
    week=index.dates.week$week[r]
    week.rows <- which(index.dates.days$ISOweekYear==year&index.dates.days$week==week)
    week.dates <- as.numeric(index.dates.days[week.rows,1])

    empty.long.week.u[r,c] <- length(which(empty.days[,3]%in%week.dates&
                                           empty.days[,2]==c&
                                           empty.days[,1]>empty.days.target))

  }
}

empty.long.week <- rbind(empty.long.week,
                         empty.long.week.u[rows.week.update,])


## ALL INDICATORS ----

save(individual.sows,file="data/individual.sows2.RData")

save(services.week              ,#   NumberOfWeeks x   ParityFrom1
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
#     exit.reason.week		        ,#	 NumberOfWeeks x  614
     exit.type.week			        ,#	 NumberOfWeeks x    7
     exit.after.event.week		  ,#	 NumberOfWeeks x    6
     death.after.event.week		  ,#	 NumberOfWeeks x    6
     empty.long.week			      ,#	 NumberOfWeeks x   ParityFrom1

     file="data/indicators-ALL.RData")


    #those are now defined in the Settings/excel
#indicators.to.keep <- indicators.all.labels %in% indicators.to.keep.labels
#indicators.to.keep.numerical <- which(indicators.to.keep==T)


indicators.data   <- list()

for (i in 1:length(indicators.to.keep.numerical)){
  indicators.data[[i]] <- get(indicators.all[indicators.to.keep.numerical[i]])
}
names(indicators.data)<-indicators.all[indicators.to.keep.numerical]

save(index.dates.days,  index.dates.week,
     indicators.data,
     indicators.labels,
     indicators.type,
     indicators.sys,
     indicators.limits,
     #indicators.categories,
     active.sows.displayID,

     file="data/indicators.RData")

