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

# farm range for weekly indicators

range.weekly <- function(indicator=indicator,        #indicator=reservices.week    indicator=piglets.deaths.week
                         weekly.window=weekly.window
)

{      
  if(is.matrix(indicator)==TRUE) {    #for indicators that are a matrix, and therefore they have parity
    
    range <- max(1,(dim(indicator)[1]-weekly.window+1)):dim(indicator)[1]

  } 
  
  if(is.matrix(indicator)==FALSE) {   #for indicators that are not matrix, and therefore they have no parity
    
    range <- max(1,(length(indicator)-weekly.window+1)):length(indicator)
    
  }
  
  return(range)
}


## weekly indicators with and without parity into one function

weekly.indicators <- function(indicator=indicator,
                              range=range_weekly

)
{
  date <- index.dates.week$start[range]
  week <- index.dates.week$week[range]
  year <- index.dates.week$ISOweekYear[range]
  
  baseline <- c(rep(NA, length(range)))
  UCL <- c(rep(NA, length(range)))
  LCL <- c(rep(NA, length(range)))
  alarms.ewma <- c(rep(0, length(range)))  ## change after choosing what algorithms will be used
  alarms.shew <- c(rep(0, length(range)))  ## change after choosing what algorithms will be used
  

    if(is.matrix(indicator)==TRUE) {    #for indicators that are a matrix, and therefore they have parity
                                        #indicator=reservices.week
      
      observed <- rowSums(indicator)[range]
      
        table <- data.frame(date, week, year,
                            observed, baseline, UCL, LCL,
                            alarms.ewma, alarms.shew)


        colnames(table) <- c("date", "week", "year",
                             "observed", "baseline", "UCL", "LCL",
                             "alarms EWMA", "alarms Shewhart")
        
     #names(table) <- indicator
    }


    if(is.matrix(indicator)==FALSE) {   #for indicators that are not a matrix, and therefore they have no parity
                                        #indicator=piglets.deaths.week
      
      observed <- indicator[range]

      table <- data.frame(date, week, year,
                          observed, baseline, UCL, LCL,
                          alarms.ewma, alarms.shew)

      colnames(table) <- c("date", "week", "year",
                           "observed", "baseline", "UCL", "LCL",
                           "alarms EWMA", "alarms Shewhart")

    #names(table) <- indicator
    }
  
  return(table)
}


## for continuous indicators taking parity into account

continuous.indicators <- function(indicator=indicator,       #indicator=days.between.farrowings
                                  continuous.window=continuous.window

)
{
    range <- max(1,(dim(indicator)[1]-continuous.window+1)):dim(indicator)[1]

    date <- as.Date(indicator[,"date"],origin="1970-01-01")[range]
    week <- isoweek(as.Date(date,origin="1970-01-01"))[range]
    year <- isoyear(as.Date(date,origin="1970-01-01"))[range]
    sowINDEX <- indicator[,"sowINDEX"][range]
    observed <- indicator[,"indicator"][range]
    baseline <- c(rep(NA, length(range)))
    UCL <- c(rep(NA, length(range)))
    LCL <- c(rep(NA, length(range)))
    alarms.ewma <- c(rep(0, length(range)))  ## change after choosing what algorithms will be used
    alarms.shew <- c(rep(0, length(range)))  ## change after choosing what algorithms will be used
    
    observed <- indicator[, "indicator"][range]
    
      table <- data.frame(date, week, year,
                          sowINDEX, observed, baseline, 
                          UCL, LCL, alarms.ewma, alarms.shew)


      colnames(table) <- c("date", "week", "year",
                           "sowINDEX", "observed", "baseline",
                           "UCL", "LCL", "alarms EWMA", "alarms Shewhart")

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
                                 run.window.continuous=run.window.continuous
)
{

    if (("sowINDEX" %in% colnames(df.indicator))==TRUE) {       # for continuous indicators
                                                                #df.indicator=df.reservices.week

      df.indicator[,"baseline"] <- df.indicator[,"observed"]

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
    }else{                   # for weekly indicators

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

  return(df.indicator)
}

# apply EWMA control chart ----

apply_ewma <- function(df.indicator=df.indicator,
                       evaluate.weekly.window=evaluate.weekly.window,
                       baseline.weekly.window=baseline.weekly.window,
                       lambda=lambda,
                       limit.sd=limit.sd,
                       guard.band.weekly=guard.band.weekly,
                       correct.baseline.UCL=correct.baseline.UCL,
                       correct.baseline.LCL=correct.baseline.LCL,
                       UCL=UCL,
                       LCL=LCL
)
{
    if (("sowINDEX" %in% colnames(df.indicator))==TRUE) {   # for continuous indicators, retrospective framework

  data <- df.indicator[,"observed"]

  ewma1 <- ewma(data, lambda = 0.2, nsigmas = 2.5)
  ewma2 <- ewma(data, lambda = 0.2, nsigmas = 3)
  ewma3 <- ewma(data, lambda = 0.2, nsigmas = 3.5)

  ##ADD one if the result of this loop was a detection

  df.indicator[ewma1$violations[ewma1$violations %in% which(ewma1$data>ewma1$center)], "alarms EWMA"]<-
    df.indicator[ewma1$violations[ewma1$violations %in% which(ewma1$data>ewma1$center)], "alarms EWMA"] +1

  df.indicator[ewma2$violations[ewma2$violations %in% which(ewma2$data>ewma2$center)], "alarms EWMA"]<-
    df.indicator[ewma2$violations[ewma2$violations %in% which(ewma2$data>ewma2$center)], "alarms EWMA"] +1

  df.indicator[ewma3$violations[ewma3$violations %in% which(ewma3$data>ewma3$center)], "alarms EWMA"]<-
    df.indicator[ewma3$violations[ewma3$violations %in% which(ewma3$data>ewma3$center)], "alarms EWMA"] +1


  df.indicator[ewma1$violations[ewma1$violations %in% which(ewma1$data<ewma1$center)], "alarms EWMA"] <-
    df.indicator[ewma1$violations[ewma1$violations %in% which(ewma1$data<ewma1$center)], "alarms EWMA"] -1

  df.indicator[ewma2$violations[ewma2$violations %in% which(ewma2$data<ewma2$center)], "alarms EWMA"] <-
    df.indicator[ewma2$violations[ewma2$violations %in% which(ewma2$data<ewma2$center)], "alarms EWMA"] -1

  df.indicator[ewma3$violations[ewma3$violations %in% which(ewma3$data<ewma3$center)], "alarms EWMA"] <-
    df.indicator[ewma3$violations[ewma3$violations %in% which(ewma3$data<ewma3$center)], "alarms EWMA"] -1


  #choose UCL and LCL
  df.indicator[,"UCL"] <- ewma2$limits[,"UCL"]   #how to do it for user to choose

  df.indicator[,"LCL"] <- ewma2$limits[,"LCL"]


      }else{        # for weekly indicators, prospective framework
                    #df.indicator=df.reservices.week
        
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

            if (l==UCL){
              df.indicator[tpoint,"UCL"]<-UCL.value
            }

            if (l==LCL){
              df.indicator[tpoint,"LCL"]<-LCL.value
            }

            #ADD a one if the result of this loop was a detection
            if (upr.alarm.detected){
              df.indicator[tpoint,"alarms EWMA"]<-df.indicator[tpoint,"alarms EWMA"]+1
            }

            if (lwr.alarm.detected){
              df.indicator[tpoint,"alarms EWMA"]<-df.indicator[tpoint,"alarms EWMA"]-1
            }


            #Correct baseline IF the user indicated so
            if (isTRUE(correct.baseline.UCL)){
              if (df.indicator[tpoint,"observed"] > max(0,UCL.value)){
                df.indicator[tpoint,"baseline"] <- max(0,round(UCL.value))
              }
            }
            if (isTRUE(correct.baseline.LCL)){
              if (df.indicator[tpoint,"observed"] < max(0,LCL.value)){
                df.indicator[tpoint,"baseline"] <- max(0,round(LCL.value))
              }
            }
          }
        }
      }
  return(df.indicator)
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