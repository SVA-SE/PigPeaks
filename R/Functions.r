packages <- c("RColorBrewer", "lubridate")
install.packages(setdiff(packages, rownames(installed.packages())))

require(RColorBrewer)
require(lubridate)


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

## for weekly indicators taking parity into account

weekly.indicators.parity <- function(indicators.data=list(reservices.week=reservices.week, 
                                                          number.deaths.week=number.deaths.week),
                                     weekly.window=weekly.window
                                 
)
{
  ##all indicators in indicators.data list have the same dimensions
  range <- max(1,(dim(indicators.data[[1]])[1]-weekly.window+1)):dim(indicators.data[[1]])[1]
  
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
  
  
  #indicators.count = 0
  
  for (i in 1:length(indicators.data)) {        #i=reservices.week i=1  indicators.data[[i]]

    #indicators.count = indicators.count+1
    
      
    observed.gilt <- indicators.data[[i]][,1][range]
    parity.gilt <- c(rep("gilt", length(range)))
    
    observed.young <- indicators.data[[i]][,2][range]
    parity.young <- c(rep("young", length(range)))
  
    observed.prime <- rowSums(indicators.data[[i]][,c(3:5)])[range]
    parity.prime <- c(rep("prime", length(range)))
    
    observed.mature <- rowSums(indicators.data[[i]][,c(6:15)])[range]
    parity.mature <- c(rep("mature", length(range)))
    
    
  table.gilt <- data.frame(date, week, year, week.quarter, quarter,
                           observed.gilt, baseline, UCL, LCL,
                           alarms.ewma, alarms.shew, parity.gilt)
  
  table.young <- data.frame(date, week, year, week.quarter, quarter,
                            observed.young, baseline, UCL, LCL,
                            alarms.ewma, alarms.shew, parity.young)
  
  table.prime <- data.frame(date, week, year, week.quarter, quarter,
                            observed.prime, baseline, UCL, LCL,
                            alarms.ewma, alarms.shew, parity.prime)
  
  table.mature <- data.frame(date, week, year, week.quarter, quarter,
                             observed.mature, baseline, UCL, LCL,
                             alarms.ewma, alarms.shew, parity.mature)
  
  
  colnames(table.gilt) <- c("date", "week", "year", "week quarter", "quarter",
                            "observed", "baseline", "UCL", "LCL", 
                            "alarms EWMA", "alarms Shewhart", "parity")
  
  colnames(table.young) <- c("date", "week", "year", "week quarter", "quarter",
                             "observed", "baseline", "UCL", "LCL", 
                             "alarms EWMA", "alarms Shewhart", "parity")
  
  colnames(table.prime) <- c("date", "week", "year", "week quarter", "quarter",
                             "observed", "baseline", "UCL", "LCL", 
                             "alarms EWMA", "alarms Shewhart", "parity")
  
  colnames(table.mature) <- c("date", "week", "year", "week quarter", "quarter",
                             "observed", "baseline", "UCL", "LCL", 
                             "alarms EWMA", "alarms Shewhart", "parity")
  
  
  outputs <- list(gilt=table.gilt, young=table.young, prime=table.prime, mature=table.mature)
  
  if(i==1){
    outputs.final <- outputs
  }else{
    outputs.final <- list(outputs.final, outputs)
  }
  }
  
  names(outputs.final) <- names(indicators.data)
  
  return(outputs.final)
  }


## for weekly indicators without parity

weekly.indicators.nonparity <- function(indicators.data=list(piglets.deaths.week),
                                        weekly.window=weekly.window
)
  
{
  
  ##all indicators in indicators.data list have the same dimensions
  range <- max(1,(length(indicators.data[[1]])[1]-weekly.window+1)):length(indicators.data[[1]])[1]
  
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
  
  indicators.count = 0
  
  for (i in indicators.data) {
    
    indicators.count = indicators.count+1
    
    observed <- i[range]
    
    i <- data.frame(date, week, year, week.quarter, quarter,
                    observed, baseline, UCL, LCL, 
                    alarms.ewma, alarms.shew)
    
    colnames(i) <- c("date", "week", "year", "week quarter", "quarter", 
                     "observed", "baseline", "UCL", "LCL",
                     "alarms EWMA", "alarms Shewhart")
    
    outputs <- list(i=i)
    
    if(indicators.count==1){
      outputs.final <- outputs
    }else{
      outputs.final <- list(outputs.final, outputs)
    }
    
    
    return(outputs.final)
  }
}


## trying to join indicators weekly with parity with weekly without parity into one function

weekly.indicators <- function(indicators.data=list(reservices.week=reservices.week, 
                                                   number.deaths.week=number.deaths.week,
                                                   piglets.deaths.week),
                              weekly.window=weekly.window
                              
)
{
  
  #indicators.count = 0
  
  for (i in 1:length(indicators.data)) {        #i=reservices.week i=1  indicators.data[[i]]
    
    if(is.numeric(i)==FALSE) {
      
      #indicators.count = indicators.count+1
      
      range <- max(1,(dim(indicators.data[[i]])[1]-weekly.window+1)):dim(indicators.data[[i]])[1]
      
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
      
      #observed.gilt <- indicators.data[[i]][match(parity.group2$parity,colnames(indicators.data[[i]])),(parity.group2$group.name)=="gilt"][range]
      #observed.gilt <- i[match(parity.group2$parity[(parity.group2$group.name)=="gilt"], colnames(i))][range]
      
      observed.gilt <- indicators.data[[i]][,1][range]
      parity.gilt <- c(rep("gilt", length(range)))
      
      observed.young <- indicators.data[[i]][,2][range]
      parity.young <- c(rep("young", length(range)))
      
      observed.prime <- rowSums(indicators.data[[i]][,c(3:5)])[range]
      parity.prime <- c(rep("prime", length(range)))
      
      observed.mature <- rowSums(indicators.data[[i]][,c(6:15)])[range]
      parity.mature <- c(rep("mature", length(range)))
      
      
      table.gilt <- data.frame(date, week, year, week.quarter, quarter,
                               observed.gilt, baseline, UCL, LCL,
                               alarms.ewma, alarms.shew, parity.gilt)
      
      table.young <- data.frame(date, week, year, week.quarter, quarter,
                                observed.young, baseline, UCL, LCL,
                                alarms.ewma, alarms.shew, parity.young)
      
      table.prime <- data.frame(date, week, year, week.quarter, quarter,
                                observed.prime, baseline, UCL, LCL,
                                alarms.ewma, alarms.shew, parity.prime)
      
      table.mature <- data.frame(date, week, year, week.quarter, quarter,
                                 observed.mature, baseline, UCL, LCL,
                                 alarms.ewma, alarms.shew, parity.mature)
      
      
      colnames(table.gilt) <- c("date", "week", "year", "week quarter", "quarter",
                                "observed", "baseline", "UCL", "LCL", 
                                "alarms EWMA", "alarms Shewhart", "parity")
      
      colnames(table.young) <- c("date", "week", "year", "week quarter", "quarter",
                                 "observed", "baseline", "UCL", "LCL", 
                                 "alarms EWMA", "alarms Shewhart", "parity")
      
      colnames(table.prime) <- c("date", "week", "year", "week quarter", "quarter",
                                 "observed", "baseline", "UCL", "LCL", 
                                 "alarms EWMA", "alarms Shewhart", "parity")
      
      colnames(table.mature) <- c("date", "week", "year", "week quarter", "quarter",
                                  "observed", "baseline", "UCL", "LCL", 
                                  "alarms EWMA", "alarms Shewhart", "parity")
      
      
      outputs.parity <- list(gilt=table.gilt, young=table.young, prime=table.prime, mature=table.mature)
      
      if(i==1){
        outputs.final.parity <- outputs.parity
      }else{
        outputs.final.parity <- list(outputs.final.parity, outputs.parity)
      }
    }
    
    # names(outputs.final.parity) <- names(indicators.data)
    
    
    if(is.numeric(i)==TRUE) {   #i=piglets.deaths.week
      
      range <- max(1,(length(indicators.data[[i]])-weekly.window+1)):length(indicators.data[[1]])
      
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
      
      #indicators.count = 0
      
      #indicators.count = indicators.count+1
      
      observed <- indicators.data[[i]][range]
      
      table <- data.frame(date, week, year, week.quarter, quarter,
                          observed, baseline, UCL, LCL, 
                          alarms.ewma, alarms.shew)
      
      colnames(table) <- c("date", "week", "year", "week quarter", "quarter", 
                           "observed", "baseline", "UCL", "LCL",
                           "alarms EWMA", "alarms Shewhart")
      
      outputs.nonparity <- list(table=table)
      
      if(i==1){
        outputs.final.nonparity <- outputs.nonparity
      }else{
        outputs.final.nonparity <- list(outputs.final.nonparity, outputs.nonparity)
      }
    }
    
    # names(outputs.final.nonparity) <- names(indicators.data)
    
    return(outputs.final.parity,outputs.final.nonparity)
  }
}


## for continuous indicators taking parity into account

continuous.indicators <- function(indicators.data=list(days.between.farrowings),
                                  continuous.window=continuous.window
                                  
)
{
  indicators.count = 0
  matrices.count = 0
  
  
  for (i in indicators.data) {        #i=days.between.farrowings
    
    
    indicators.count = indicators.count+1
    
    matrix.gilt <- i[i[, "parity"] <= 2,]
    matrix.young <- i[i[, "parity"] == 3,]
    matrix.prime <- i[i[, "parity"] >= 4 & i[, "parity"] <= 6,]
    matrix.mature <- i[i[, "parity"] >= 7,]
    
    matrices <- list(matrix.gilt=matrix.gilt, matrix.young=matrix.young, 
                     matrix.prime=matrix.prime, matrix.mature=matrix.mature)
    
    
    for (m in matrices) {    #m=matrix.gilt
      
      matrices.count = matrices.count+1
      
      range <- max(1,(dim(m)[1]-continuous.window+1)):dim(m)[1]
      
      date <- as.Date(m[,"date"],origin="1970-01-01")[range]
      week <- isoweek(as.Date(date,origin="1970-01-01"))[range]
      year <- isoyear(as.Date(date,origin="1970-01-01"))[range]
      week.quarter <- week-(floor((week-1)/13)*13)
      quarter <- c(rep(NA, length(range)))
      quarter <- ifelse(week<=13, paste(year,1, sep = "."), quarter)
      quarter <- ifelse(week>13 & week<=26, paste(year,2, sep = "."), quarter)
      quarter <- ifelse(week>26 & week<=39, paste(year,3, sep = "."), quarter)
      quarter <- ifelse(week>39, paste(year,4, sep = "."), quarter)
      sowINDEX <- m[,"sowINDEX"][range]
      observed <- m[,"indicator"][range]
      baseline <- c(rep(NA, length(range)))
      UCL <- c(rep(NA, length(range)))
      LCL <- c(rep(NA, length(range)))
      alarms.ewma <- c(rep(0, length(range)))  ## change after choosing what algorithms will be used
      alarms.shew <- c(rep(0, length(range)))  ## change after choosing what algorithms will be used
      parity <- m[,"parity"][range]
      
      
      table <- data.frame(date, week, year, week.quarter, quarter,
                          sowINDEX, observed, baseline, UCL, LCL,
                          alarms.ewma, alarms.shew, parity)
      
      
      colnames(table) <- c("date", "week", "year", "week quarter", "quarter",
                           "sowINDEX", "observed", "baseline", "UCL", "LCL", 
                           "alarms EWMA", "alarms Shewhart", "parity")
      
      outputs <- list(i=table)
      
      if(matrices.count==1){           ##confirmar se quando se adicionar mais indicadores resulta
        outputs.final <- outputs       ##e melhorar posição nas listas
      }else{
        outputs.final <- list(outputs.final, outputs)
      }
    }
    
    # if(indicators.count==1){          
    #   outputs.final <- table
    # }else{
    #   outputs.final <- list(outputs.final, table)
    # }
  }
  
  return(outputs.final)
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


