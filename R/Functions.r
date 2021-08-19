packages <- c("ISOweek","RColorBrewer", "caTools", "lubridate", "abind", "qcc", "dplyr")
install.packages(setdiff(packages, rownames(installed.packages())))

require(ISOweek)
require(RColorBrewer)
require(lubridate)
require(caTools)
require(abind)
require(qcc)
require(dplyr)

# background functions ----

lastmon <- function(x) {
  7 * floor(as.numeric(x-1+4)/7) + as.Date(1-4, origin="1970-01-01")
}



dates_df <- function (min.date, max.date,
                      by="days",
                      date.format="%d/%m/%Y") {

  start <- as.Date(min.date, format = date.format)
  end   <- as.Date(max.date, format = date.format)

  require(ISOweek)

  dates.v <- seq(from=start,to=end, by=by)
  date <- strptime (as.character(dates.v), format = "%Y-%m-%d")
  year <- (date$year+1900)
  month <- date$mon
  mday <- date$mday
  yday <- date$yday
  dow <- date$wday
  week <- as.numeric(substring(ISOweek::ISOweek(date),7,8))
  weekday <- rep(1,length(date))
  weekday[dow==0|dow==6] <- 0
  dates <- as.data.frame(dates.v)
  colnames(dates) <- "dates"
  dates <- cbind(dates, mday, month, year, yday, week, dow, weekday)

  return(dates)

}


# RETRO indicators functions ----

## count functions

# create.counts.days <- function(rows.index=index.dates.days[,1],
#                                cols.index=parity,
#                                data.matrix=individual.sows$indicator,
#                                parity.matrix=individual.sows$parity){
#
#   matrix.days <- matrix(NA,nrow=length(rows.index),ncol=length(cols.index))
#   colnames(matrix.days)<- cols.index
#
#   for (r in 1:length(rows.index)){
#     for (c in 1:length(cols.index)){
#       matrix.days[r,c] <- sum(data.matrix[r,which(parity.matrix[r,]==cols.index[c])],na.rm=TRUE)
#     }}
#
#   return(matrix.days)
#
# }


create.counts.week <- function(rows.index.days=index.dates.days[,1],
                               rows.index.week=index.dates.week[,1],
                               cols.index=parity,
                               data.matrix=individual.sows$indicator,
                               group.matrix=individual.sows$parity){

  matrix.days <- matrix(NA,nrow=length(rows.index.days),ncol=length(cols.index))
  for (r in 1:length(rows.index.days)){
    for (c in 1:length(cols.index)){
      matrix.days[r,c] <- sum(data.matrix[r,which(group.matrix[r,]==cols.index[c])],na.rm=TRUE)
    }}


  matrix.week <- matrix(NA,nrow=length(rows.index.week),ncol=length(cols.index))
  colnames(matrix.week)<- cols.index

  for (r in 1:length(rows.index.week)){
    for (c in 1:length(cols.index)){
      matrix.week[r,c] <- sum(matrix.days[((((r-1)*7)+1):min(c((r*7),length(rows.index.days)))),c],na.rm=TRUE)
    }}

  return(matrix.week)

}



# NON-TIME series

create.nonTS.timeto <- function(group.matrix=individual.sows$parity,
                                index.dates=index.dates.days[,1],
                                col1="indicator",
                                col2="parity",
                                col3="date",
                                event1.matrix=individual.sows$indicator,
                                event1.value=1,
                                event2.matrix=individual.sows$indicator,
                                event2.value=2,
                                condition=NULL){

  matrix.timeto <- matrix(NA,nrow=1,ncol=4)
  colnames(matrix.timeto)<- c(col1,col2,col3,"sowINDEX")

  for (r in 2:length(index.dates)){
    for (s in 1:dim(group.matrix)[2]){

      if (!is.na(event2.matrix[r,s])&
          event2.matrix[r,s]==event2.value){

        if(length(condition)>0){
          if(condition=="first"){
            if(r>min(which(event2.matrix[,s]==event2.value))){
              next
            }
          }
        }

        matrix.timeto.c1 <- 0
        if(length(which(event1.matrix[1:(r-1),s]%in%event1.value))>0){
          matrix.timeto.c1 <- (r - max(which(event1.matrix[1:(r-1),s]%in%event1.value)))
        }

        matrix.timeto.c2 <- group.matrix[r,s]

        matrix.timeto.c3 <- as.numeric(index.dates[r])

        matrix.timeto.c4 <- s
        matrix.timeto <- rbind(matrix.timeto,
                               c(matrix.timeto.c1,
                                 matrix.timeto.c2,
                                 matrix.timeto.c3,
                                 matrix.timeto.c4))


      }
    }
  }

  if (all(is.na(matrix.timeto[1,]))){
    matrix.timeto<- matrix.timeto[-1,]
  }

  return(matrix.timeto)

}



create.nonTS.eventsto <- function(group.matrix=individual.sows$parity,
                                  index.dates=index.dates.days[,1],
                                  col1="indicator",
                                  col2="parity",
                                  col3="date",
                                  event1.matrix=individual.sows$indicator,
                                  event1.value=c(1,2),
                                  event2.matrix=individual.sows$indicator,
                                  event2.value=1){

  matrix.eventsto <- matrix(NA,nrow=1,ncol=4)

  colnames(matrix.eventsto)<- c(col1,col2,col3,"sowINDEX")


  for (r in 2:length(index.dates)){
    for (s in 1:dim(group.matrix)[2]){

      if (!is.na(event2.matrix[r,s])&
          event2.matrix[r,s]==event2.value){

        rmin <- 1
        if(length(which(event2.matrix[1:(r-1),s]==event2.value))>0){
          rmin <- max(which(event2.matrix[1:(r-1),s]==event2.value))
        }

        matrix.eventsto.c1 <- length(which(event1.matrix[(rmin+1):r,s]%in%event1.value))
        matrix.eventsto.c2 <- group.matrix[r,s]
        matrix.eventsto.c3 <- as.numeric(index.dates[r])
        matrix.eventsto.c4 <- s

        matrix.eventsto <- rbind(matrix.eventsto,
                               c(matrix.eventsto.c1,
                                 matrix.eventsto.c2,
                                 matrix.eventsto.c3,
                                 matrix.eventsto.c4))

      }
    }
  }

  if (all(is.na(matrix.eventsto[1,]))){
    matrix.eventsto<- matrix.eventsto[-1,]
  }

  return(matrix.eventsto)

}


create.nonTS.counts <- function(group.matrix=individual.sows$parity,
                                index.dates=index.dates.days[,1],
                                col1="indicator",
                                col2="parity",
                                col3="date",
                                event.matrix=individual.sows$indicator,
                                event.value=1,
                                count.matrix=list(individual.sows$indicator),
                                denominator.matrix=NULL){

  matrix.counts <- matrix(NA,nrow=1,ncol=4)

  colnames(matrix.counts)<- c(col1,col2,col3,"sowINDEX")


  for (r in 2:length(index.dates)){
    for (s in 1:dim(group.matrix)[2]){

      if (!is.na(event.matrix[r,s])&
          event.matrix[r,s]==event.value){

        num <- sum(count.matrix[[1]][r,s],na.rm=T)
        if(length(count.matrix)>1){
          for(l in 2:length(count.matrix)){
            num <-  num + sum(count.matrix[[l]][r,s],na.rm=T)
          }
        }

        den <- sum(denominator.matrix[[1]][r,s],na.rm=T)
        if(length(denominator.matrix)>1){
          for(l in 2:length(denominator.matrix)){
            den <- den+sum(denominator.matrix[[l]][r,s],na.rm=T)
          }
        }



        if(is.null(denominator.matrix)){
          matrix.counts.c1 <- num
        }else{
          matrix.counts.c1 <- num/den
        }

        matrix.counts.c2 <- group.matrix[r,s]
        matrix.counts.c3 <- as.numeric(index.dates[r])
        matrix.counts.c4 <- s

        matrix.counts <- rbind(matrix.counts,
                               c(matrix.counts.c1,
                                 matrix.counts.c2,
                                 matrix.counts.c3,
                                 matrix.counts.c4))

      }
    }
  }

  if (all(is.na(matrix.counts[1,]))){
    matrix.counts<- matrix.counts[-1,]
  }

  return(matrix.counts)

}


# weekly PERCENTAGE functions -----
create.perc.week.1 <- function(rows.index.week=index.dates.week[,1],
                               cols.index=parity,
                               data=assumed.pregnant,
                               value.to.count=1,
                               test=c("equal","greater")
){
  matrix.perc <- array(NA,dim=c(length(rows.index.week),length(cols.index),2))
  dimnames(matrix.perc)[[2]] <- cols.index
  dimnames(matrix.perc)[[3]] <- c("numerator","denominator")

  for (r in 1:length(rows.index.week)){
    for (c in 1:length(cols.index)){

      if(test=="equal"){
        matrix.perc[r,c,1] <- length(which((data[(((r-1)*7)+1):min(c(r*7,dim(data)[1])),,c])==value.to.count))
      }else{
        matrix.perc[r,c,1] <- length(which((data[(((r-1)*7)+1):min(c(r*7,dim(data)[1])),,c])>value.to.count))
      }

      matrix.perc[r,c,2] <- length(which(!is.na(data[(((r-1)*7)+1):min(c(r*7,dim(data)[1])),,c])))

    }}
  return(matrix.perc)
}



# structure indicators ----

# farm range for weekly indicators

range.weekly <- function(indicator=indicator,
                         weekly.window=weekly.window
)

{
  if(is.array(indicator)==TRUE) {   #for indicators with parity (with and without denominator)
                                    #indicator=indicators.data$reservices.week  indicator=indicators.data$perc.failure

    range <- max(1,(dim(indicator)[1]-weekly.window+1)):dim(indicator)[1]

  }
  
  if(is.array(indicator)==FALSE) {  #for indicators without parity
                                    #indicator=indicators.data$gilts.deaths.week
    
    range <- max(1,(length(indicator)-weekly.window+1)):length(indicator)
    
  }

  return(range)
}


## weekly indicators with and without parity into one function

weekly.indicators <- function(indicator=indicator,
                              range.weekly=range_weekly
)
{
  baseline <- c(rep(NA, length(range.weekly)))
  UCL.ewma <- c(rep(NA, length(range.weekly)))
  LCL.ewma <- c(rep(NA, length(range.weekly)))
  UCL.shew <- c(rep(NA, length(range.weekly)))
  LCL.shew <- c(rep(NA, length(range.weekly)))
  alarms.ewma <- c(rep(0, length(range.weekly)))  ## change after choosing what algorithms will be used
  alarms.shew <- c(rep(0, length(range.weekly)))  ## change after choosing what algorithms will be used


  if(is.array(indicator)==TRUE && is.matrix(indicator)==TRUE) {    #for indicators with parity (without denominator)
    #indicator=indicators.data$reservices.week

    observed <- rowSums(indicator)[range.weekly]

    table <- data.frame(observed, baseline, 
                        UCL.ewma, LCL.ewma, alarms.ewma, 
                        UCL.shew, LCL.shew, alarms.shew)


    colnames(table) <- c("observed", "baseline",
                         "UCL EWMA", "LCL EWMA", "alarms EWMA", 
                         "UCL Shewhart", "LCL Shewhart", "alarms Shewhart")
  }
  
  if(is.array(indicator)==TRUE && is.matrix(indicator)==FALSE) {    #for indicators with parity (with denominator)
    #indicator=indicators.data$perc.failure
    
    observed <- round((rowSums(indicator[,,"numerator"])[range.weekly]) / (rowSums(indicator[,,"denominator"])[range.weekly])*100,2)
    
    table <- data.frame(observed, baseline, 
                        UCL.ewma, LCL.ewma, alarms.ewma, 
                        UCL.shew, LCL.shew, alarms.shew)
    
    
    colnames(table) <- c("observed", "baseline",
                         "UCL EWMA", "LCL EWMA", "alarms EWMA", 
                         "UCL Shewhart", "LCL Shewhart", "alarms Shewhart")
      }

  if(is.array(indicator)==FALSE) {                                #for indicators without parity
    #indicator=indicators.data$gilts.deaths.week

    observed <- indicator[range.weekly]

    table <- data.frame(observed, baseline, 
                        UCL.ewma, LCL.ewma, alarms.ewma, 
                        UCL.shew, LCL.shew, alarms.shew)

    colnames(table) <- c("observed", "baseline",
                         "UCL EWMA", "LCL EWMA", "alarms EWMA", 
                         "UCL Shewhart", "LCL Shewhart", "alarms Shewhart")
  }
  
  return(table)
}


## for continuous indicators taking parity into account

continuous.indicators <- function(indicator=indicator,       #indicator=indicators.data$perc.dead.born.litter
                                  continuous.window=continuous.window

)
{
  range <- max(1,(dim(indicator)[1]-continuous.window+1)):dim(indicator)[1]

  date <- as.Date(indicator[,"date"],origin="1970-01-01")[range]
  week <- isoweek(as.Date(date,origin="1970-01-01"))
  year <- isoyear(as.Date(date,origin="1970-01-01"))
  sowINDEX <- indicator[,"sowINDEX"][range]
  observed <- indicator[,"indicator"][range]
  baseline <- c(rep(NA, length(range)))
  UCL.ewma <- c(rep(NA, length(range)))
  LCL.ewma <- c(rep(NA, length(range)))
  UCL.shew <- c(rep(NA, length(range)))
  LCL.shew <- c(rep(NA, length(range)))
  alarms.ewma <- c(rep(0, length(range)))  ## change after choosing what algorithms will be used
  alarms.shew <- c(rep(0, length(range)))  ## change after choosing what algorithms will be used
  
  table <- data.frame(date, week, year,
                      sowINDEX, observed, baseline, 
                      UCL.ewma, LCL.ewma, alarms.ewma, 
                      UCL.shew, LCL.shew, alarms.shew)

  colnames(table) <- c("date", "week", "year",
                       "sowINDEX", "observed", "baseline",
                       "UCL EWMA", "LCL EWMA", "alarms EWMA", 
                       "UCL Shewhart", "LCL Shewhart", "alarms Shewhart")
  return(table)
}


## for non-sys indicators

non.sys.indicators <- function (indicator=indicator,
                                range.weekly=range_weekly,
                                continuous.window=continuous.window
)
{
  if (dim(indicator)[2]==15) {    # or length(parity.group2$parity)   #for weekly indicators with parity
    
    observed <- rowSums(indicator)[range.weekly]
    
    table <- data.frame(observed)
    
    colnames(table) <- "observed"
    
  }
  if (dim(indicator)[2]==4) {    #for continuous indicators
    
    range.continuous <- max(1,(dim(indicator)[1]-continuous.window+1)):dim(indicator)[1]
    
    date <- as.Date(indicator[,"date"],origin="1970-01-01")[range.continuous]
    week <- isoweek(as.Date(date,origin="1970-01-01"))
    year <- isoyear(as.Date(date,origin="1970-01-01"))
    sowINDEX <- indicator[,"sowINDEX"][range.continuous]
    observed <- indicator[,"indicator"][range.continuous]
    
    table <- data.frame(date, week, year,
                        sowINDEX, observed)
    
    colnames(table) <- c("date", "week", "year",
                         "sowINDEX", "observed")
    
  }
  if (dim(indicator)[2]!=4 && dim(indicator)[2]!=15 && is.null(dim(indicator))==FALSE) {
    #for weekly indicators composed
    
    observed <- indicator[range.weekly,]
    
    table <- data.frame(observed)
    
  }
  if (is.null(dim(indicator))==TRUE) {   #for weekly indicators without parity
    
    observed <- indicator[range.weekly]
    
    table <- data.frame(observed)
    
    colnames(table) <- "observed"
    
  }
  return(table)
}


# clean baseline non-parametric ----

##'The cleaning is non-parametric, based on moving
##' percentiles. The user sets a window of time points, around each time point,
##' which will be used to calculate the percentile set in the user in the argument
##' limit. Any observations falling outside that percentile are removed
##' and substituted by the percentile itself.

clean_baseline_perc <- function (df.indicator=df.indicator,
                                 limit.upp=limit.upp,
                                 limit.lw=limit.lw,
                                 run.window.weekly=run.window.weekly,
                                 nr.production.cycles=nr.production.cycles
)
{
  if (dim(df.indicator)[2]==8) {     # for weekly indicators
                                     #df.indicator=indicators.time.series$`time to abortion`

      df.indicator[,"baseline"] <- df.indicator[,"observed"]

      #require(caTools)

      #pulling data form the object to work out of the object
      observed.matrix=df.indicator[,"observed"]

      #if both upper and lower limits are not NULL

      if(!is.null(limit.upp) & !is.null(limit.lw)){

        days = observed.matrix

        limitV.upp <- runquantile(days, run.window.weekly,
                                  probs=limit.upp, endrule="quantile")

        peaks.upp <- which(days > round(limitV.upp))
        x.smooth <- days
        x.smooth [peaks.upp] <- round(limitV.upp[peaks.upp])

        df.indicator[,"baseline"] <- x.smooth

        
        limitV.lw <- runquantile(days, run.window.weekly,
                                 probs=limit.lw, endrule="quantile")

        peaks.lw <- which(days < round(limitV.lw))
        x.smooth [peaks.lw] <- round(limitV.lw[peaks.lw])

        df.indicator[,"baseline"] <- x.smooth
      }


      #if only upper limit is not NULL

      if(!is.null(limit.upp) & is.null(limit.lw)){

        days = observed.matrix

        limitV.upp <- runquantile(days, run.window.weekly,
                                  probs=limit.upp, endrule="quantile")

        peaks.upp <- which(days > round(limitV.upp))
        x.smooth <- days
        x.smooth [peaks.upp] <- round(limitV.upp[peaks.upp])

        df.indicator[,"baseline"] <- x.smooth
      }


      #if only lower limit is not NULL

      if(!is.null(limit.lw) & is.null(limit.upp)){

        days = observed.matrix

        limitV.lw <- runquantile(days, run.window.weekly,
                                 probs=limit.lw, endrule="quantile")

        peaks.lw <- which(days < round(limitV.lw))
        x.smooth <- days
        x.smooth [peaks.lw] <- round(limitV.lw[peaks.lw])

        df.indicator[,"baseline"] <- x.smooth
      }
  }
  
  if (dim(df.indicator)[2]==12) {       # for continuous indicators
    #df.indicator=indicators.time.series$`Time to reservice`

    df.indicator[,"baseline"] <- df.indicator[,"observed"]
    
    i.date <- first(df.indicator[, "date"])
    f.date <- last(df.indicator[, "date"])
    
    median.days.production.cycles <- median(indicators.time.series$`days between farrowings`[,"observed"])* nr.production.cycles
    
    run.window.continuous <- round((median.days.production.cycles*dim(df.indicator)[1])/
      as.numeric(difftime(as.POSIXct(f.date), as.POSIXct(i.date, tz="UTC"), units="days")),0)
      

    #require(caTools)
    
    #pulling data form the object to work out of the object
    observed.matrix=df.indicator[,"observed"]
    
    #if both upper and lower limits are not NULL
    
    if(!is.null(limit.upp) & !is.null(limit.lw)){
      
      days = observed.matrix
      
      limitV.upp <- runquantile(days, run.window.continuous,
                                probs=limit.upp, endrule="quantile")
      
      peaks.upp <- which(days > round(limitV.upp))
      x.smooth <- days
      x.smooth [peaks.upp] <- round(limitV.upp[peaks.upp])
      
      df.indicator[,"baseline"] <- x.smooth
      
      
      
      limitV.lw <- runquantile(days, run.window.continuous,
                               probs=limit.lw, endrule="quantile")
      
      peaks.lw <- which(days < round(limitV.lw))
      x.smooth [peaks.lw] <- round(limitV.lw[peaks.lw])
      
      df.indicator[,"baseline"] <- x.smooth
    }
    
    
    #if only upper limit is not NULL
    
    if(!is.null(limit.upp) & is.null(limit.lw)){
      
      days = observed.matrix
      
      limitV.upp <- runquantile(days, run.window.continuous,
                                probs=limit.upp, endrule="quantile")
      
      peaks.upp <- which(days > round(limitV.upp))
      x.smooth <- days
      x.smooth [peaks.upp] <- round(limitV.upp[peaks.upp])
      
      df.indicator[,"baseline"] <- x.smooth
    }
    
    
    #if only lower limit is not NULL
    
    if(!is.null(limit.lw) & is.null(limit.upp)){
      
      days = observed.matrix
      
      limitV.lw <- runquantile(days, run.window.continuous,
                               probs=limit.lw, endrule="quantile")
      
      peaks.lw <- which(days < round(limitV.lw))
      x.smooth <- days
      x.smooth [peaks.lw] <- round(limitV.lw[peaks.lw])
      
      df.indicator[,"baseline"] <- x.smooth
      
    }
  }
  return(df.indicator)
}


# apply EWMA control chart ----

apply_ewma <- function(df.indicator=df.indicator,    #df.indicator=indicators.time.series$`abortions per week`
                       evaluate.weekly.window=evaluate.weekly.window,
                       baseline.weekly.window=baseline.weekly.window,
                       lambda=lambda,
                       limit.sd=limit.sd,
                       guard.band.weekly=guard.band.weekly,
                       correct.baseline.UCL.ewma=correct.baseline.UCL.ewma,
                       correct.baseline.LCL.ewma=correct.baseline.LCL.ewma,
                       UCL.ewma=UCL.ewma,
                       LCL.ewma=LCL.ewma
)
{
  if (length(which(is.nan(df.indicator[,"observed"])))>0 | length(which(is.na(df.indicator[,"observed"])))>0){
    
    #replace the NaN values for 0.00
    df.indicator[,"observed"] <- 
      replace(df.indicator[,"observed"],
              which(is.nan(df.indicator[, "observed"])), 0.00)
    
    #replace the Na values for 0.00
    df.indicator[,"observed"] <- 
      replace(df.indicator[,"observed"],
              which(is.na(df.indicator[, "observed"])), 0.00)
  }
  
    if (dim(df.indicator)[2]==8) {         # for weekly indicators, prospective framework
                    
        if(guard.band.weekly<1)(guard.band.weekly<-1)

        #require(abind)

        #number of time points to iterate
        range <- (dim(df.indicator)[1]-evaluate.weekly.window+1):dim(df.indicator)[1]

        for (tpoint in range){ #tpoint=155

          start = tpoint-baseline.weekly.window-guard.band.weekly
          end   = tpoint-1

          to.cc <- c(df.indicator[start:end,"baseline"],df.indicator[tpoint,"observed"])
          correct <- 0


          for (l in 1:length(limit.sd)){ #l=2

            #require(qcc)
            ewma1 = ewma(to.cc,
                         center=mean(to.cc[1:(length(to.cc)-guard.band.weekly)],na.rm=TRUE),
                         std.dev=sd(to.cc[1:(length(to.cc)-guard.band.weekly)],na.rm=TRUE),
                         lambda=lambda,nsigmas=limit.sd[l],plot=FALSE)


            last <- length(to.cc)
            upr.alarm.detected <- 0
            lwr.alarm.detected <- 0

            if(length(ewma1$violations)>0){
              if(ewma1$violations[length(ewma1$violations)]==last&
                 ewma1$y[last]>ewma1$limits[last,2]){
                upr.alarm.detected <- 1
              }}

            if(length(ewma1$violations)>0){
              if(ewma1$violations[length(ewma1$violations)]==last&
                 ewma1$y[last]<ewma1$limits[last,1]){
                lwr.alarm.detected <- 1
              }}

            UCL.value= ceiling(correct  +  ewma1$limits[[length(ewma1$limits[,2]),2]])
            LCL.value= floor(correct    +  ewma1$limits[[length(ewma1$limits[,1]),1]])

            #before deciding if an alarm exists, a zero is automatically added to the
            #time point if this is the first loop for two reasons:
            #1-because if the data were never analysed, the slot had a NA before,
            #and adding 0 will signal that it has now been processed
            #2-because if the data HAS been analyzed before, we want the results of these
            #analyses to OVERRIDE, not to SUM to the previous analyses.
            if(l==1){
              df.indicator[tpoint,"alarms EWMA"]<-0
            }

            if (l==UCL.ewma){
              df.indicator[tpoint,"UCL EWMA"]<-UCL.value
            }

            if (l==LCL.ewma){
              df.indicator[tpoint,"LCL EWMA"]<-LCL.value
            }

            #ADD a one if the result of this loop was a detection
            if (upr.alarm.detected){
              df.indicator[tpoint,"alarms EWMA"]<-df.indicator[tpoint,"alarms EWMA"]+1
            }

            if (lwr.alarm.detected){
              df.indicator[tpoint,"alarms EWMA"]<-df.indicator[tpoint,"alarms EWMA"]-1
            }

            #Correct baseline IF the user indicated so
            if (isTRUE(correct.baseline.UCL.ewma)){
              if (df.indicator[tpoint,"observed"] > max(0,UCL.value)){
                df.indicator[tpoint,"baseline"] <- max(0,round(UCL.value))
              }
            }
            if (isTRUE(correct.baseline.LCL.ewma)){
              if (df.indicator[tpoint,"observed"] < max(0,LCL.value)){
                df.indicator[tpoint,"baseline"] <- max(0,round(LCL.value))
              }
            }
          }
        }
    }
  
  if (dim(df.indicator)[2]==12) {     # for continuous indicators, retrospective framework
    
    data <- df.indicator[,"observed"]
    
    for (l in 1:length(limit.sd)){ #l=2
      
      #require(qcc)
      ewma1 <- ewma(data, lambda = lambda, nsigmas = limit.sd[l], plot = FALSE, na.rm=TRUE)
      
      #choose UCL and LCL
      
      if (l==UCL.ewma){
        df.indicator[,"UCL EWMA"] <- ceiling(ewma1$limits[,"UCL"])
      }
      
      if (l==LCL.ewma){
        df.indicator[,"LCL EWMA"] <- floor(ewma1$limits[,"LCL"])
      }
      
      ##ADD one if the result of this loop was a detection
      
      if(l==1){
        df.indicator[,"alarms EWMA"]<-0
      }
      
      df.indicator[ewma1$violations[ewma1$violations %in% which(ewma1$data>ewma1$center)], "alarms EWMA"]<-
        df.indicator[ewma1$violations[ewma1$violations %in% which(ewma1$data>ewma1$center)], "alarms EWMA"] +1
      
      
      df.indicator[ewma1$violations[ewma1$violations %in% which(ewma1$data<ewma1$center)], "alarms EWMA"] <-
        df.indicator[ewma1$violations[ewma1$violations %in% which(ewma1$data<ewma1$center)], "alarms EWMA"] -1
      
    }
  }
  
  return(df.indicator)
}


# apply Shewhart control chart ----

shew_apply <- function (df.indicator=df.indicator,
                        evaluate.weekly.window=evaluate.weekly.window,
                        baseline.weekly.window=baseline.weekly.window,
                        limit.sd=limit.sd,
                        guard.band.weekly=guard.band.weekly,
                        correct.baseline.UCL.shew=correct.baseline.UCL.shew,
                        correct.baseline.LCL.shew=correct.baseline.LCL.shew,
                        UCL.shew=UCL.shew,
                        LCL.shew=LCL.shew
)
{
  if (length(which(is.nan(df.indicator[,"observed"])))>0 | length(which(is.na(df.indicator[,"observed"])))>0){
    
    #replace the NaN values for 0.00
    df.indicator[,"observed"] <- 
      replace(df.indicator[,"observed"],
              which(is.nan(df.indicator[, "observed"])), 0.00)
    
    #replace the Na values for 0.00
    df.indicator[,"observed"] <- 
      replace(df.indicator[,"observed"],
              which(is.na(df.indicator[, "observed"])), 0.00)
  }
  
  if (dim(df.indicator)[2]==8) {   # for weekly indicators, prospective framework
    
    if(guard.band.weekly<1)(guard.band.weekly<-1)

          #require(abind)

          #number of time points to iterate
          range <- (dim(df.indicator)[1]-evaluate.weekly.window+1):dim(df.indicator)[1]

          for (tpoint in range){  #tpoint=155

            start = tpoint-baseline.weekly.window-guard.band.weekly
            end   = tpoint-1

            to.cc <- c(df.indicator[start:end,"baseline"],df.indicator[tpoint,"observed"])
            correct <- 0

            for (l in 1:length(limit.sd)){

              #require(qcc)
              average.series = to.cc[1:(length(to.cc)-guard.band.weekly)]
              average.series = average.series[which(!is.na(average.series))]
              stats <- stats.xbar.one(average.series)
              sd.xbar <- sd.xbar.one(average.series,
                                     std.dev = "SD", k=2)
              shew <- limits.xbar.one(center=stats$center,
                                      std.dev=as.double(sd.xbar),
                                      conf=limit.sd[l])

              UCL.value= ceiling(correct  +  shew[2])
              LCL.value= floor(correct  + shew[1])

              #before deciding if an alarm exists, a zero is automatically added to the
              #time point if this is the first loop for two reasons:
              #1-because if the data were never analysed, the slot had a NA before,
              #and adding 0 will signal that it has now been processed
              #2-because if the data HAS been analyzed before, we want the results of these
              #analyses to OVERRIDE, not to SUM to the previous analyses.

              if(l==1){
                df.indicator[tpoint,"alarms Shewhart"]<-0
              }

              if (l==UCL.shew){
                df.indicator[tpoint,"UCL Shewhart"]<-UCL.value
              }

              if (l==UCL.shew){
                df.indicator[tpoint,"LCL Shewhart"]<-LCL.value
              }

              #ADD a one if the result of this loop was a detection
              if (df.indicator[tpoint,"observed"]>max(0,UCL.value)){
                df.indicator[tpoint,"alarms Shewhart"]<-df.indicator[tpoint,"alarms Shewhart"]+1
              }

              if (df.indicator[tpoint,"observed"]<max(0,LCL.value)){
                df.indicator[tpoint,"alarms Shewhart"]<-df.indicator[tpoint,"alarms Shewhart"]-1
              }

              #Correct baseline IF the user indicated so
              if (isTRUE(correct.baseline.UCL.shew)){
                if (df.indicator[tpoint,"observed"] > max(0,UCL.value)){
                  df.indicator[tpoint,"baseline"] <- max(0,round(UCL.value))
                }
              }
              if (isTRUE(correct.baseline.LCL.shew)){
                if (df.indicator[tpoint,"observed"] < max(0,LCL.value)){
                  df.indicator[tpoint,"baseline"] <- max(0,round(LCL.value))
                }
              }
            }
          }
  }
  
  if (dim(df.indicator)[2]==12) {        # for continuous indicators, retrospective framework
    
    data <- df.indicator[,"observed"]
    
    for (l in 1:length(limit.sd)){ #l=2
      
      #require(qcc)
      stats <- stats.xbar.one(data)
      sd.xbar <- sd.xbar.one(data,
                             std.dev = "SD", k=2)
      shew <- limits.xbar.one(center=stats$center,
                              std.dev=as.double(sd.xbar),
                              conf=limit.sd[l])
      
      UCL.value= ceiling(shew[2])
      LCL.value= floor(shew[1])
      
      #choose UCL and LCL
      
      if (l==UCL.shew){
        df.indicator[,"UCL Shewhart"] <- UCL.value
        df.indicator[,"LCL Shewhart"] <- LCL.value
      }
      
      ##ADD or SUBTRACT one if the result of this loop was a detection
      
      if(l==1){
        df.indicator[,"alarms Shewhart"]<-0
      }
      
      df.indicator[data>shew[,"UCL"],"alarms Shewhart"] <- df.indicator[data>shew[,"UCL"],"alarms Shewhart"] +1
      
      df.indicator[data<max(0,shew[,"LCL"]),"alarms Shewhart"] <- df.indicator[data<max(0,shew[,"LCL"]),"alarms Shewhart"] -1
      
    }
  }
  
  return(df.indicator)
  }

# plotting functions ----


# parity colouring
#parity.group <- data.frame(parity = c1, group=c2, group.name = c3)
parity.group <- data.frame(parity = c1, group.name = ordered(c3,levels=c("gilt","young","prime","mature")))


qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_parity = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

parity.group$color1 <- col_parity[as.numeric(as.factor(parity.group$group.name))]
parity.group$color2 <- col_parity[as.numeric(as.factor(parity.group$parity))]

parity.group2 <- parity.group[-1,]

colors.custom<- c(rep("#4287f5",1),
                  rep("#28ab1f",2),
                  rep("#f5942c",3),
                  rep("#a15a4c",9)
)
parity.group2 <-cbind(parity.group2,colors.custom)
parity.group2$colors.custom<-colors.custom

color.pg <- c("#4287f5","#28ab1f","#f5942c","#a15a4c")




# for (l in levels(parity.group2$group.name)) { #l="gilt"   l="prime"
#
#   parity.count = parity.count+1
#
#
#   if (length(parity.group2$parity[parity.group2$group.name==l]) == 1) {
#
#     observed <- i[,(parity.group2$parity[parity.group2$group.name==l])][range]
#
#   }
#
#   if (length(parity.group2$parity[parity.group2$group.name==l]) > 1) {
#
#     observed <- rowSums(i[,(parity.group2$parity[parity.group2$group.name==l])])[range]
#
#   }
#
#   parity.name <- c(rep(l, length(range)))
#


# for (l in levels(parity.group2$group.name)) { #l="gilt"   l="mature"
#
#   matrices.count = matrices.count+1
#
#   matrix <- i[i[, "parity"] %in% c(parity.group2$parity[parity.group2$group.name==l]),]