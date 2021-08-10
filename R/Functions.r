packages <- c("ISOweek","RColorBrewer", "lubridate", "abind", "qcc")
install.packages(setdiff(packages, rownames(installed.packages())))

require(ISOweek)
require(RColorBrewer)
require(lubridate)
require(caTools)
require(abind)
require(qcc)

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

create.counts.days <- function(rows.index=index.dates.days[,1],
                               cols.index=parity,
                               data.matrix=individual.sows$indicator,
                               parity.matrix=individual.sows$parity){

  matrix.days <- matrix(NA,nrow=length(rows.index),ncol=length(cols.index))
  colnames(matrix.days)<- cols.index

  for (r in 1:length(rows.index)){
    for (c in 1:length(cols.index)){
      matrix.days[r,c] <- sum(data.matrix[r,which(parity.matrix[r,]==cols.index[c])],na.rm=TRUE)
    }}

  return(matrix.days)

}


create.counts.week <- function(rows.index.days=index.dates.days[,1],
                               rows.index.week=index.dates.week[,1],
                               cols.index=parity,
                               data.matrix=individual.sows$indicator,
                               parity.matrix=individual.sows$parity){

  matrix.days <- matrix(NA,nrow=length(rows.index.days),ncol=length(cols.index))
  for (r in 1:length(rows.index.days)){
    for (c in 1:length(cols.index)){
      matrix.days[r,c] <- sum(data.matrix[r,which(parity.matrix[r,]==cols.index[c])],na.rm=TRUE)
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

create.nonTS.timeto <- function(parity.matrix=individual.sows$parity,
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
    for (s in 1:dim(parity.matrix)[2]){

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

        matrix.timeto.c2 <- parity.matrix[r,s]

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



# structure indicators ----

## weekly indicators with and without parity into one function

weekly.indicators <- function(indicators.data=list(reservices.week=reservices.week,
                                                   number.deaths.week=number.deaths.week,
                                                   piglets.deaths.week=piglets.deaths.week),
                              weekly.window=weekly.window

)
{
  parity.count = 0
  nonparity.count = 0


  for (i in indicators.data) {  #i=indicators.data[[1]]
    #for (i in 1:length(indicators.data)) { #indicators.data[[i]]

    if(is.matrix(i)==TRUE) {    #for indicators that are a matrix, and therefore they have parity

      matrix.count = matrix.count +1

      range <- max(1,(dim(i)[1]-weekly.window+1)):dim(i)[1]

      date <- index.dates.week$start[range]
      week <- index.dates.week$week[range]
      year <- index.dates.week$ISOweekYear[range]
      week.quarter <- week-(floor((week-1)/13)*13)
      quarter <- c(rep(NA, length(range)))
      quarter <- ifelse(week<=13, paste(year,1, sep = "."), quarter)
      quarter <- ifelse(week>13 & week<=26, paste(year,2, sep = "."), quarter)
      quarter <- ifelse(week>26 & week<=39, paste(year,3, sep = "."), quarter)
      quarter <- ifelse(week>39, paste(year,4, sep = "."), quarter)

      baseline <- c(rep(NA, length(range)))
      UCL <- c(rep(NA, length(range)))
      LCL <- c(rep(NA, length(range)))
      alarms.ewma <- c(rep(0, length(range)))  ## change after choosing what algorithms will be used
      alarms.shew <- c(rep(0, length(range)))  ## change after choosing what algorithms will be used


      for (l in levels(parity.group2$group.name)) { #l="gilt"   l="prime"

        parity.count = parity.count+1


        if (length(parity.group2$parity[parity.group2$group.name==l]) == 1) {

          observed <- i[,(parity.group2$parity[parity.group2$group.name==l])][range]

        }

        if (length(parity.group2$parity[parity.group2$group.name==l]) > 1) {

          observed <- rowSums(i[,(parity.group2$parity[parity.group2$group.name==l])])[range]

        }

        parity.name <- c(rep(l, length(range)))

        table <- data.frame(date, week, year, week.quarter, quarter,
                            observed, baseline, UCL, LCL,
                            alarms.ewma, alarms.shew, parity.name)


        colnames(table) <- c("date", "week", "year", "week quarter", "quarter",
                             "observed", "baseline", "UCL", "LCL",
                             "alarms EWMA", "alarms Shewhart", "parity")


        if(parity.count==1){
          outputs.parity <- table
        }else{
          outputs.parity <- list(outputs.parity, table)
      }
    }
  }
     #names(outputs.parity.matrix) <- names(indicators.data)


    if(is.matrix(i)==FALSE) {   #i=indicators.data[[3]]
                                #for indicators that are not a matrix, and therefore they have no parity

      nonparity.count = nonparity.count+1

      range <- max(1,(length(i)-weekly.window+1)):length(i)

      date <- index.dates.week$start[range]
      week <- index.dates.week$week[range]
      year <- index.dates.week$ISOweekYear[range]
      week.quarter <- week-(floor((week-1)/13)*13)
      quarter <- c(rep(NA, length(range)))
      quarter <- ifelse(week<=13, paste(year,1, sep = "."), quarter)
      quarter <- ifelse(week>13 & week<=26, paste(year,2, sep = "."), quarter)
      quarter <- ifelse(week>26 & week<=39, paste(year,3, sep = "."), quarter)
      quarter <- ifelse(week>39, paste(year,4, sep = "."), quarter)

      baseline <- c(rep(NA, length(range)))
      UCL <- c(rep(NA, length(range)))
      LCL <- c(rep(NA, length(range)))
      alarms.ewma <- c(rep(0, length(range)))  ## change after choosing what algorithms will be used
      alarms.shew <- c(rep(0, length(range)))  ## change after choosing what algorithms will be used


      observed <- i[range]

      table <- data.frame(date, week, year, week.quarter, quarter,
                          observed, baseline, UCL, LCL,
                          alarms.ewma, alarms.shew)

      colnames(table) <- c("date", "week", "year", "week quarter", "quarter",
                           "observed", "baseline", "UCL", "LCL",
                           "alarms EWMA", "alarms Shewhart")


      if(nonparity.count==1){
        outputs.nonparity <- table
      }else{
        outputs.nonparity <- list(outputs.nonparity, table)
      }
    }

    # names(outputs.final.nonparity) <- names(indicators.data)
  }

  outputs <- list(outputs.parity, outputs.nonparity)
  return(outputs)
}


## for continuous indicators taking parity into account

continuous.indicators <- function(indicators.data=list(days.between.farrowings=days.between.farrowings),
                                  continuous.window=continuous.window

)
{

  matrices.count = 0


  for (i in indicators.data) {        #i=indicators.data[[1]]

    for (l in levels(parity.group2$group.name)) { #l="gilt"   l="mature"

      matrices.count = matrices.count+1

      matrix <- i[i[, "parity"] %in% c(parity.group2$parity[parity.group2$group.name==l]),]

    #last "continuous.window" observations for each parity
    range <- max(1,(dim(matrix)[1]-continuous.window+1)):dim(matrix)[1]

    date <- as.Date(matrix[,"date"],origin="1970-01-01")[range]
    week <- isoweek(as.Date(date,origin="1970-01-01"))[range]
    year <- isoyear(as.Date(date,origin="1970-01-01"))[range]
    week.quarter <- week-(floor((week-1)/13)*13)
    quarter <- c(rep(NA, length(range)))
    quarter <- ifelse(week<=13, paste(year,1, sep = "."), quarter)
    quarter <- ifelse(week>13 & week<=26, paste(year,2, sep = "."), quarter)
    quarter <- ifelse(week>26 & week<=39, paste(year,3, sep = "."), quarter)
    quarter <- ifelse(week>39, paste(year,4, sep = "."), quarter)
    sowINDEX <- matrix[,"sowINDEX"][range]
    observed <- matrix[,"indicator"][range]
    baseline <- c(rep(NA, length(range)))
    UCL <- c(rep(NA, length(range)))
    LCL <- c(rep(NA, length(range)))
    alarms.ewma <- c(rep(0, length(range)))  ## change after choosing what algorithms will be used
    alarms.shew <- c(rep(0, length(range)))  ## change after choosing what algorithms will be used
    parity <- matrix[,"parity"][range]


      table <- data.frame(date, week, year, week.quarter, quarter,
                          sowINDEX, observed, baseline, UCL, LCL,
                          alarms.ewma, alarms.shew, parity)


      colnames(table) <- c("date", "week", "year", "week quarter", "quarter",
                           "sowINDEX", "observed", "baseline", "UCL", "LCL",
                           "alarms EWMA", "alarms Shewhart", "parity")


      if(matrices.count==1){           ##confirmar se quando se adicionar mais indicadores resulta
        outputs <- table               ##e melhorar posição nas listas
      }else{
        outputs <- list(outputs, table)
      }
    }
  }
  return(outputs)
}

# clean baseline non-parametric ----

##'The cleaning is non-parametric, based on moving
##' percentiles. The user sets a window of time points, around each time point,
##' which will be used to calculate the percentile set in the user in the argument
##' limit. Any observations falling outside that percentile are removed
##' and substituted by the percentile itself.

clean_baseline_perc <- function (list.indicators=c(weekly.indicators(weekly.window = weekly.window),
                                                   continuous.indicators(continuous.window = continuous.window)),
                                 limit.upp=limit.upp,
                                 limit.lw=limit.lw,
                                 run.window.weekly=run.window.weekly,
                                 run.window.continuous=run.window.continuous
)
{



  #only for the indicators to be worked out here,
  #adding data form observed which is only modified if an
  #aberration is detected

  for (i in list.indicators) {  #i=list.indicators[[1]][[1]][[2]]       i=list.indicators[[4]]

    if (("sowINDEX" %in% colnames(i))==TRUE) {       # for continuous indicators

      i[,"baseline"] <- i[,"observed"]

  #require(caTools)

  #pulling data form the object to work out of the object
  observed.matrix=i[,"observed"]

  #if both upper and lower limits are not NULL

  if(!is.null(limit.upp) & !is.null(limit.lw)){

    days = observed.matrix

    limitV.upp <- runquantile(days, run.window.continuous,
                              probs=limit.upp, endrule="quantile")


    peaks.upp <- which(days > round(limitV.upp))
    x.smooth <- days
    x.smooth [peaks.upp] <- round(limitV.upp[peaks.upp])


    i[,"baseline"] <- x.smooth



    limitV.lw <- runquantile(days, run.window.continuous,
                             probs=limit.lw, endrule="quantile")



    peaks.lw <- which(days < round(limitV.lw))
    x.smooth [peaks.lw] <- round(limitV.lw[peaks.lw])


    i[,"baseline"] <- x.smooth
  }


  #if only upper limit is not NULL

  if(!is.null(limit.upp) & is.null(limit.lw)){

    days = observed.matrix

    limitV.upp <- runquantile(days, run.window.continuous,
                              probs=limit.upp, endrule="quantile")



    peaks.upp <- which(days > round(limitV.upp))
    x.smooth <- days
    x.smooth [peaks.upp] <- round(limitV.upp[peaks.upp])



    i[,"baseline"] <- x.smooth
  }


  #if only lower limit is not NULL

  if(!is.null(limit.lw) & is.null(limit.upp)){

    days = observed.matrix

    limitV.lw <- runquantile(days, run.window.continuous,
                             probs=limit.lw, endrule="quantile")



    peaks.lw <- which(days < round(limitV.lw))
    x.smooth <- days
    x.smooth [peaks.lw] <- round(limitV.lw[peaks.lw])



    i[,"baseline"] <- x.smooth

  }
    }else{                   # for weekly indicators

      i[,"baseline"] <- i[,"observed"]

      #require(caTools)

      #pulling data form the object to work out of the object
      observed.matrix=i[,"observed"]

      #if both upper and lower limits are not NULL

      if(!is.null(limit.upp) & !is.null(limit.lw)){

        days = observed.matrix

        limitV.upp <- runquantile(days, run.window.weekly,
                                  probs=limit.upp, endrule="quantile")


        peaks.upp <- which(days > round(limitV.upp))
        x.smooth <- days
        x.smooth [peaks.upp] <- round(limitV.upp[peaks.upp])


        i[,"baseline"] <- x.smooth



        limitV.lw <- runquantile(days, run.window.weekly,
                                 probs=limit.lw, endrule="quantile")



        peaks.lw <- which(days < round(limitV.lw))
        x.smooth [peaks.lw] <- round(limitV.lw[peaks.lw])


        i[,"baseline"] <- x.smooth
      }


      #if only upper limit is not NULL

      if(!is.null(limit.upp) & is.null(limit.lw)){

        days = observed.matrix

        limitV.upp <- runquantile(days, run.window.weekly,
                                  probs=limit.upp, endrule="quantile")



        peaks.upp <- which(days > round(limitV.upp))
        x.smooth <- days
        x.smooth [peaks.upp] <- round(limitV.upp[peaks.upp])



        i[,"baseline"] <- x.smooth
      }


      #if only lower limit is not NULL

      if(!is.null(limit.lw) & is.null(limit.upp)){

        days = observed.matrix

        limitV.lw <- runquantile(days, run.window.weekly,
                                 probs=limit.lw, endrule="quantile")



        peaks.lw <- which(days < round(limitV.lw))
        x.smooth <- days
        x.smooth [peaks.lw] <- round(limitV.lw[peaks.lw])



        i[,"baseline"] <- x.smooth
      }
    }
  }

  return(list.indicators)
}

# apply EWMA control chart ----

apply_ewma <- function(list.indicators=clean_baseline_perc(),
                       evaluate.weekly.window=evaluate.weekly.window,
                       baseline.weekly.window=baseline.weekly.window,
                       lambda=lambda,
                       limit.sd=limit.sd,
                       guard.band.weekly=guard.band.weekly,
                       correct.baseline.UCL=correct.baseline.UCL,
                       correct.baseline.LCL=correct.baseline.LCL,
                       UCL=UCL,
                       LCL=LCL,
                       continuous.window=continuous.window
)
{

  if(guard.band.weekly<1)(guard.band.weekly<-1)

  for (i in list.indicators) {  #i=list.indicators[[1]][[2]]       i=list.indicators[[4]]

    if (("sowINDEX" %in% colnames(i))==TRUE) {       # for continuous indicators, retrospective framework

  data <- tail(i[,"observed"], continuous.window)

  ewma1 <- ewma(data, lambda = 0.2, nsigmas = 2.5)
  ewma2 <- ewma(data, lambda = 0.2, nsigmas = 3)
  ewma3 <- ewma(data, lambda = 0.2, nsigmas = 3.5)

  ##ADD one if the result of this loop was a detection

  i[ewma1$violations[ewma1$violations %in% which(ewma1$data>ewma1$center)], "alarms EWMA"]<-
    i[ewma1$violations[ewma1$violations %in% which(ewma1$data>ewma1$center)], "alarms EWMA"] +1

  i[ewma2$violations[ewma2$violations %in% which(ewma2$data>ewma2$center)], "alarms EWMA"]<-
    i[ewma2$violations[ewma2$violations %in% which(ewma2$data>ewma2$center)], "alarms EWMA"] +1

  i[ewma3$violations[ewma3$violations %in% which(ewma3$data>ewma3$center)], "alarms EWMA"]<-
    i[ewma3$violations[ewma3$violations %in% which(ewma3$data>ewma3$center)], "alarms EWMA"] +1


  i[ewma1$violations[ewma1$violations %in% which(ewma1$data<ewma1$center)], "alarms EWMA"] <-
    i[ewma1$violations[ewma1$violations %in% which(ewma1$data<ewma1$center)], "alarms EWMA"] -1


  i[ewma2$violations[ewma2$violations %in% which(ewma2$data<ewma2$center)], "alarms EWMA"] <-
    i[ewma2$violations[ewma2$violations %in% which(ewma2$data<ewma2$center)], "alarms EWMA"] -1


  i[ewma3$violations[ewma3$violations %in% which(ewma3$data<ewma3$center)], "alarms EWMA"] <-
    i[ewma3$violations[ewma3$violations %in% which(ewma3$data<ewma3$center)], "alarms EWMA"] -1



  #choose UCL and LCL
  i[,"UCL"] <- ewma2$limits[,"UCL"]

  i[,"LCL"] <- ewma2$limits[,"LCL"]


      }else{        # for weekly indicators, prospective framework

        #require(abind)

        #number of time points to iterate
        range <- (dim(i)[1]-evaluate.weekly.window+1):dim(i)[1]

        for (tpoint in range){ #tpoint=155

          start = tpoint-baseline.weekly.window-guard.band.weekly
          end   = tpoint-1

          to.cc <- c(i[start:end,"baseline"],i[tpoint,"observed"])
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
              i[tpoint,"alarms EWMA"]<-0
            }

            if (l==UCL){
              i[tpoint,"UCL"]<-UCL.value
            }

            if (l==LCL){
              i[tpoint,"LCL"]<-LCL.value
            }

            #ADD a one if the result of this loop was a detection
            if (upr.alarm.detected){
              i[tpoint,"alarms EWMA"]<-i[tpoint,"alarms EWMA"]+1
            }

            if (lwr.alarm.detected){
              i[tpoint,"alarms EWMA"]<-i[tpoint,"alarms EWMA"]-1
            }


            #Correct baseline IF the user indicated so
            if (isTRUE(correct.baseline.UCL)){
              if (i[tpoint,"observed"] > max(0,UCL.value)){
                i[tpoint,"baseline"] <- max(0,round(UCL.value))
              }
            }
            if (isTRUE(correct.baseline.LCL)){
              if (i[tpoint,"observed"] < max(0,LCL.value)){
                i[tpoint,"baseline"] <- max(0,round(LCL.value))
              }
            }
          }
        }
      }
  }
  return(list.indicators)
}

# apply Shewhart control chart ----

shew_apply <- function (list.indicators=ewma_apply(),
                        evaluate.weekly.window=evaluate.weekly.window,
                        baseline.weekly.window=baseline.weekly.window,
                        limit.sd=limit.sd,
                        guard.band.weekly=guard.band.weekly,
                        #correct.baseline.UCL=correct.baseline.UCL,  #should be possible to correct the baseline with Shewhart also?
                        #correct.baseline.LCL=correct.baseline.LCL,
                        #UCL=UCL,                 #should be possible to choose if they want to put the values of ewma or shew in the columns?
                        #LCL=LCL,
                        continuous.window=continuous.window
)
{

  if(guard.band.weekly<1)(guard.band.weekly<-1)

  for (i in list.indicators) {  #i=list.indicators[[1]][[2]]       i=list.indicators[[4]]

    if (("sowINDEX" %in% colnames(i))==TRUE) {       # for continuous indicators, retrospective framework

        data <- tail(i[,"observed"], continuous.window)

        stats <- stats.xbar.one(data)
        sd.xbar <- sd.xbar.one(data,
                               std.dev = "SD", k=2)
        shew1 <- limits.xbar.one(center=stats$center,
                                 std.dev=as.double(sd.xbar),
                                 conf=2.5)
        shew2 <- limits.xbar.one(center=stats$center,
                                 std.dev=as.double(sd.xbar),
                                 conf=3)
        shew3 <- limits.xbar.one(center=stats$center,
                                 std.dev=as.double(sd.xbar),
                                 conf=3.5)


        ##ADD or SUBTRACT one if the result of this loop was a detection

        i[data>shew1[,"UCL"], "alarms Shewhart"] <- i[data>shew1[,"UCL"], "alarms Shewhart"] +1

        i[data>shew2[,"UCL"], "alarms Shewhart"] <- i[data>shew2[,"UCL"], "alarms Shewhart"] +1

        i[data>shew3[,"UCL"], "alarms Shewhart"] <- i[data>shew3[,"UCL"], "alarms Shewhart"] +1


        i[data<max(0,shew1[,"LCL"]), "alarms Shewhart"] <- i[data<max(0,shew1[,"LCL"]), "alarms Shewhart"] -1

        i[data<max(0,shew2[,"LCL"]), "alarms Shewhart"] <- i[data<max(0,shew2[,"LCL"]), "alarms Shewhart"] -1

        i[data<max(0,shew3[,"LCL"]), "alarms Shewhart"] <- i[data<max(0,shew3[,"LCL"]), "alarms Shewhart"] -1

        }else{        # for weekly indicators, prospective framework

          #require(abind)

          #number of time points to iterate
          range <- (dim(i)[1]-evaluate.weekly.window+1):dim(i)[1]

          for (tpoint in range){

            start = tpoint-baseline.weekly.window-guard.band.weekly
            end   = tpoint-1

            to.cc <- c(i[start:end,"baseline"],i[tpoint,"observed"])
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
                i[tpoint,"alarms Shewhart"]<-0
              }

              # if (l==UCL){
              #   i[tpoint,"UCL"]<-UCL.value
              # }
              #
              # if (l==LCL){
              #   i[tpoint,"LCL"]<-LCL.value
              # }

              #ADD a one if the result of this loop was a detection
              if (i[tpoint,"observed"]>max(0,UCL.value)){
                i[tpoint,"alarms Shewhart"]<-i[tpoint,"alarms Shewhart"]+1
              }

              if (i[tpoint,"observed"]<max(0,LCL.value)){
                i[tpoint,"alarms Shewhart"]<-i[tpoint,"alarms Shewhart"]-1
              }

              # if (isTRUE(correct.baseline.UCL)){
              #   if (i[tpoint,"observed"] > max(0,UCL.value)){
              #     i[tpoint,"baseline"] <- max(0,round(UCL.value))
              #   }
              # }
              # if (isTRUE(correct.baseline.LCL)){
              #   if (i[tpoint,"observed"] < max(0,LCL.value)){
              #     i[tpoint,"baseline"] <- max(0,round(LCL.value))
              #   }
              # }
            }
          }
        }
  }
          return(list.indicators)
        }

# evaluation: PRRS outbreaks injection ----
##NOT WORKING YET

add.outbreaks.reservices <- function(list.indicators=c(weekly.indicators(weekly.window = weekly.window),
                                                       continuous.indicators(continuous.window = continuous.window)),
                                     quarter = c("2015.2", "2015.3", "2015.4", "2016.1", "2016.2", "2016.3", "2016.4",
                                                 "2017.1", "2017.2", "2017.3", "2017.4", "2018.1")

)
{
  indicators.count = 0

  for (i in list.indicators) {

    indicators.count = indicators.count + 1

    data = i[,"observed"]

  for ( q in quarter){ #q="2015.2"

    if(i==reservices.week){     #see if it works when the lists have name

      start = first(which(i[, "quarter"] == q))
      end = last(which(i[, "quarter"] == q))

      y <- data[(start):(end)]

    ## increased in week t3 with max value in t8, then decreased until t28 (36-8)
    ## lets consider that reservices tripled (*2) in week t8

    lgn.reservices <- 2* plnorm(c(1,1,(100/6),(100/6*2),(100/6*3),(100/6*4),(100/6*5),(100/6*6),
                                  100-(100/28),100-(100/28*2),100-(100/28*3),100-(100/28*4),100-(100/28*5)),
                                meanlog=4, sdlog=0.3, lower.tail=TRUE, log.p=FALSE)

    #plot(lgn.reservices, type="l")

    baseline.total <- sum(y)/length(y)  # Additive
    simulated.outbreak <- c(y[c(1,2)], ceiling(lgn.reservices[c(3:13)]*baseline.total)+y[c(3:13)])

    add.observed <-
      data.frame(replace(i[,"observed"], which(i[, "quarter"] == q), simulated.outbreak))
    }

    if(i==number.deaths.week){

      start = first(which(i[, "quarter"] == q))
      end = last(which(i[, "quarter"] == q))

      y <- data[(start):(end)]

      ## increased 10% in week t3 and then decreased until t7 (during 4 weeks (7-3))

      lgn.mortality.sows <- 0.1* (plnorm(c(100/3,(100/3*2),(100/3*3),
                                           100-(100/4),100-(100/4*2),100-(100/4*3),100-(100/4*4),100-(100/4*4),
                                           100-(100/4*4),100-(100/4*4),100-(100/4*4),100-(100/4*4),100-(100/4*4)),
                                         meanlog=4, sdlog=0.3, lower.tail=TRUE, log.p=FALSE))

      #plot(lgn.mortality.sows, type="l")

      baseline.total <- sum(y)/length(y)  # Additive
      simulated.outbreak <- ceiling(lgn.mortality.sows*baseline.total)+y

      add.observed <-
        data.frame(replace(i[,"observed"], which(i[, "quarter"] == q), simulated.outbreak))

    }

    ## Clean Baseline

    table <- clean_baseline_perc(list.indicators=i,
                                 limit.upp=limit.upp,
                                 limit.lw=limit.lw,
                                 run.window.weekly=run.window.weekly,
                                 run.window.continuous=run.window.continuous)
    ## Applying EWMA

    table <- apply_ewma(list.indicators=i,
                        evaluate.weekly.window=evaluate.weekly.window,
                        baseline.weekly.window=baseline.weekly.window,
                        lambda=lambda,
                        limit.sd=limit.sd,
                        guard.band.weekly=guard.band.weekly,
                        correct.baseline.UCL=correct.baseline.UCL,
                        correct.baseline.LCL=correct.baseline.LCL,
                        UCL=UCL,
                        LCL=LCL,
                        continuous.window=continuous.window)

    ## Applying Shewhart

   table <- shew_apply(list.indicators=i,
                       evaluate.weekly.window=evaluate.weekly.window,
                       baseline.weekly.window=baseline.weekly.window,
                       limit.sd=limit.sd,
                       guard.band.weekly=guard.band.weekly,
                       #correct.baseline.UCL=correct.baseline.UCL,  #should be possible to correct the baseline with Shewhart also?
                       #correct.baseline.LCL=correct.baseline.LCL,
                       #UCL=UCL,                 #should be possible to choose if they want to put the values of ewma or shew in the columns?
                       #LCL=LCL,
                       continuous.window=continuous.window)

   if(matrices.count==1){
     add.indicators <- table
   }else{
     add.indicators <- list(add.indicators, table)
   }
  }
  }
  return(add.indicators)
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


