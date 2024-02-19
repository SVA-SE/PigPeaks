# packages ----

packages <- c("ISOweek","lubridate","abind", "tidyverse")
uninstalled <- setdiff(packages, rownames(installed.packages()))
if (length(uninstalled))
        install.packages(uninstalled)

invisible(lapply(packages, library, character.only = TRUE))

#source("Definitions.r") #settings already runs definitions
source("Settings.r")
source("R/Functions.r")


#load("data/individual.sows2.RData")
#load("data/animal.RData")
#load("data/indicators.RData")




###indicators.time.series



# structure weekly indicators with SyS ----

for (i in intersect(which(indicators.sys==TRUE), which(indicators.type=="W"))) {

  old <- indicators.time.series[[i]][1:(index.start.week -1),]
  new <- weekly.indicators(indicator=indicators.data[[i]],
                           range.weekly=rows.week.update)
  
  indicators.time.series[[i]] <- rbind(old,new)
}


# structure continuous indicators with SyS ----

for (i in intersect(which(indicators.sys==TRUE), which(indicators.type=="C"))) {
  
  remove.rows <- which(indicators.time.series[[i]][,"date"]>=start.date)
  if(length(remove.rows)>0){
  old <- indicators.time.series[[i]][-remove.rows,]
  }else{
    old <- indicators.time.series[[i]]
  }
  
  keep.rows <- which(indicators.data[[i]][,"date"]>=start.date)
  if(length(keep.rows)>0){
  new.data <- indicators.data[[i]][keep.rows,]
    new <- continuous.indicators(indicator=new.data)
  
  indicators.time.series[[i]] <- rbind(old,new)
  }else{
    indicators.time.series[[i]] <- old
  }
  
}


# structure non-sys indicators ----
for (i in intersect(which(indicators.sys==FALSE), which(indicators.type=="W"))) {
  
  old <- indicators.time.series[[i]][1:(index.start.week -1),]
  new <- non.sys.indicators(indicator=indicators.data[[i]],
                           range.weekly=rows.week.update,
                           indicator.type=indicators.type[i])
  
  indicators.time.series[[i]] <- rbind(old,new)
}



for (i in intersect(which(indicators.sys==FALSE), which(indicators.type=="C"))) {
  
  remove.rows <- which(indicators.time.series[[i]][,"date"]>=start.date)
  if(length(remove.rows)>0){
    old <- indicators.time.series[[i]][-remove.rows,]
  }else{
    old <- indicators.time.series[[i]]
  }
  
  keep.rows <- which(indicators.data[[i]][,"date"]>=start.date)
  
  if(length(keep.rows)>0){
    new.data <- indicators.data[[i]][keep.rows,]
    new <- non.sys.indicators(indicator=new.data,
                              indicator.type=indicators.type[i])
    
    indicators.time.series[[i]] <- rbind(old,new)
  }else{
    indicators.time.series[[i]] <- old
  }
}



# clean baseline ----
#not applied on updated


# apply EWMA ----

for (i in which(indicators.sys==TRUE)) {
  
  if (indicators.limits[i]=="limit.upp"){
      
    correct.baseline.UCL.ewma = TRUE
    correct.baseline.LCL.ewma = FALSE
  }
  
  if (indicators.limits[i]=="limit.lw") {
    
    correct.baseline.UCL.ewma = FALSE
    correct.baseline.LCL.ewma = TRUE
  }
  
  if (indicators.limits[i]=="both") {
    
    correct.baseline.UCL.ewma = TRUE
    correct.baseline.LCL.ewma = TRUE
  }

  indicators.time.series[[i]] <- apply_ewma(df.indicator=indicators.time.series[[i]],
                                            evaluate.weekly.window=length(rows.week.update),
                                            baseline.weekly.window=baseline.weekly.window,
                                            continuous.window=continuous.window,
                                            lambda=lambda,
                                            limit.sd=limit.sd,
                                            guard.band.weekly=guard.band.weekly,
                                            correct.baseline.UCL.ewma=correct.baseline.UCL.ewma,
                                            correct.baseline.LCL.ewma=correct.baseline.LCL.ewma,
                                            UCL.ewma=UCL.ewma,
                                            LCL.ewma=LCL.ewma,
                                            indicator.type=indicators.type[i])
}


# apply Shewhart ----

for (i in which(indicators.sys==TRUE)) {
  
  if (indicators.limits[i]=="limit.upp"){
    
    correct.baseline.UCL.shew = TRUE
    correct.baseline.LCL.shew = FALSE
  }
  
  if (indicators.limits[i]=="limit.lw") {
    
    correct.baseline.UCL.shew = FALSE
    correct.baseline.LCL.shew = TRUE
  }
  
  if (indicators.limits[i]=="both") {
    
    correct.baseline.UCL.shew = TRUE
    correct.baseline.LCL.shew = TRUE
  }

  indicators.time.series[[i]] <- shew_apply(df.indicator=indicators.time.series[[i]],
                                            evaluate.weekly.window=evaluate.weekly.window,
                                            baseline.weekly.window=baseline.weekly.window,
                                            continuous.window=continuous.window,
                                            limit.sd=limit.sd,
                                            guard.band.weekly=guard.band.weekly,
                                            correct.baseline.UCL.shew=correct.baseline.UCL.shew,
                                            correct.baseline.LCL.shew=correct.baseline.LCL.shew,
                                            UCL.shew=UCL.shew,
                                            LCL.shew=LCL.shew,
                                            indicator.type=indicators.type[i])
}



#Indicators continuous to weekly for dashboard ----


## structure continuous indicators with SyS to weekly ----

for (i in intersect(which(indicators.sys==TRUE), which(indicators.type=="C"))) {
  
  old <- indicators.continuous.to.weekly[[i]][1:(index.start.week -1),]
  
  keep.rows <- which(indicators.time.series[[i]][,"date"]>=start.date)
  new.data <- indicators.time.series[[i]][keep.rows,]
  new <- continuous.to.weekly(df.indicator=new.data,
                              limits=indicators.limits[[i]],
                              index.dates.week.f = index.dates.week[rows.week.update,])
  
  indicators.continuous.to.weekly[[i]] <- rbind(old,new)
}



save(indicators.time.series,indicators.continuous.to.weekly,
     file="data/indicators.results.RData")

