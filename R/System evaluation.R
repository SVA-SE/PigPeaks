# packages ----

packages <- c("dplyr", "tibble", "qcc", "abind")
uninstalled <- setdiff(packages, rownames(installed.packages()))
if (length(uninstalled))
        install.packages(uninstalled)

invisible(lapply(packages, library, character.only = TRUE))

load('Data_Example/indicatorsExample.RData')
source('Settings.r')
source('R/Functions.r')
source('R/Functions-plotting.r')
source('R/4-detection.r')



# inject outbreaks in quarters

quarters.list = c("2017.2", "2017.3", "2017.4",
                  "2018.1", "2018.2", "2018.3", "2018.4")

# evaluation: PRRS outbreaks injection
##based on papers (Valdes-Donoso et al. 2018) and (Pejsak and Markowska-Daniel 1997) 

## Reservices per week  ----

add.outbreaks.reservices <- function(indicator=indicators.data$reservices.week,
                                     quarters.list=quarters.list
)
{
  range_weekly <- max(1,(dim(index.dates.week)[1]-weekly.window+1)):dim(index.dates.week)[1]
                           
  df.indicator.evaluate.system <- weekly.indicators(indicator=indicators.data$reservices.week,
                                                    range.weekly=1:dim(index.dates.week)[1])
  
  data = df.indicator.evaluate.system[,"observed"]
  
  week.quarter <- index.dates.week$week-(floor((index.dates.week$week-1)/13)*13)
  
  quarter <- c(rep(NA, dim(df.indicator.evaluate.system)[1]))
  
  quarter <- ifelse(index.dates.week$week<=13, 
                    paste(index.dates.week$ISOweekYear,1, sep = "."), quarter)
  
  quarter <- ifelse(index.dates.week$week>13 & 
                      index.dates.week$week<=26, 
                    paste(index.dates.week$ISOweekYear,2, sep = "."), quarter)
  
  quarter <- ifelse(index.dates.week$week>26 & 
                      index.dates.week$week<=39, 
                    paste(index.dates.week$ISOweekYear,3, sep = "."), quarter)
  
  quarter <- ifelse(index.dates.week$week>39, 
                    paste(index.dates.week$ISOweekYear,4, sep = "."), quarter)
  
  for ( q in quarters.list){ #q="2018.1"
   
    start = first(which(quarter==q))
    end = last(which(quarter==q))
    
    y <- data[(start):(end)]
    
    ## increased in week t3 with max value in t8, then decreased until t28 (36-8)
    ## lets consider that reservices tripled (*2) in week t8
    
    lgn.reservices <- 2* plnorm(c(1,1,(100/6),(100/6*2),(100/6*3),(100/6*4),(100/6*5),(100/6*6),
                                  100-(100/28),100-(100/28*2),100-(100/28*3),100-(100/28*4),100-(100/28*5)),
                                meanlog=4, sdlog=0.3, lower.tail=TRUE, log.p=FALSE)
    
    #plot(lgn.reservices, type="l")
    
   
    baseline.total <- sum(y)/length(y)  # Additive
    simulated.outbreak <- c(y[c(1,2)], ceiling(lgn.reservices[c(3:13)]*baseline.total)+y[c(3:13)])
    
    add.reservices.per.week.observed <-
      replace(data, which(quarter==q), simulated.outbreak)
    
    
    add.df.indicator.evaluate.system <- data.frame(df.indicator.evaluate.system$gilt,
                                                   df.indicator.evaluate.system$young,
                                                   df.indicator.evaluate.system$prime,
                                                   df.indicator.evaluate.system$mature,
                                                   add.reservices.per.week.observed,
                                                   df.indicator.evaluate.system$baseline,
                                                   df.indicator.evaluate.system$`UCL EWMA`,
                                                   df.indicator.evaluate.system$`LCL EWMA`,
                                                   df.indicator.evaluate.system$`alarms EWMA`,
                                                   df.indicator.evaluate.system$`UCL Shewhart`,
                                                   df.indicator.evaluate.system$`LCL Shewhart`,
                                                   df.indicator.evaluate.system$`alarms Shewhart`)
    
    colnames(add.df.indicator.evaluate.system) <- c("gilt", "young", "prime", "mature",
                                                    "observed", "baseline", "UCL EWMA", "LCL EWMA",
                                                    "alarms EWMA", "UCL Shewhart", 
                                                    "LCL Shewhart", "alarms Shewhart")
    
      
    ## Clean Baseline
    
    add.df.indicator.evaluate.system <- clean_baseline_perc(df.indicator=add.df.indicator.evaluate.system,
                                                            limit.upp=0.95,
                                                            limit.lw=NULL,
                                                            run.window.weekly=104,
                                                            median.days.production.cycles=NULL,
                                                            nr.production.cycles=2,
                                                            range=range_weekly,
                                                            indicator.type="W")
    
    
    ## Applying EWMA
    
    add.df.indicator.evaluate.system <- apply_ewma(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=106,
                                                   baseline.weekly.window=52,
                                                   lambda=0.2,
                                                   limit.sd=c(2.5,3,3.5),
                                                   guard.band.weekly=2,
                                                   correct.baseline.UCL.ewma=TRUE,
                                                   correct.baseline.LCL.ewma=TRUE,
                                                   UCL.ewma=2,
                                                   LCL.ewma=2,
                                                   indicator.type="W")
    
    ## Applying Shewhart
    
    add.df.indicator.evaluate.system <- shew_apply(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=106,
                                                   baseline.weekly.window=52,
                                                   limit.sd=c(2.5,3,3.5),
                                                   guard.band.weekly=2,
                                                   correct.baseline.UCL.shew=FALSE,
                                                   correct.baseline.LCL.shew=FALSE,
                                                   UCL.shew=2,
                                                   LCL.shew=2,
                                                   indicator.type="W")
    
    #delete the row which is week 53
    add.df.indicator.evaluate.system <-
      add.df.indicator.evaluate.system[-c(which(index.dates.week$week==53)),]
    
    ## construct final table
    
    dates <- data.frame(index.dates.week$start[-(which(index.dates.week$week==53))],
                        index.dates.week$week[-(which(index.dates.week$week==53))],
                        index.dates.week$ISOweekYear[-(which(index.dates.week$week==53))],
                        week.quarter[-(which(index.dates.week$week==53))],
                        quarter[-(which(index.dates.week$week==53))])
    
    table.evaluate.system <- data.frame(dates, add.df.indicator.evaluate.system)
    
    
    colnames(table.evaluate.system) <- c("date", "week", "year", "week quarter", "quarter",
                                         "gilt", "young", "prime", "mature",
                                         "observed", "baseline", "UCL EWMA", "LCL EWMA",
                                         "alarms EWMA", "UCL Shewhart", 
                                         "LCL Shewhart", "alarms Shewhart")
  }

  return(table.evaluate.system)
}


## Cluster all in one list

outbreaks.reservices.results <- list()

for (quarter in quarters.list){
  
  outbreaks.reservices.results[[quarter]] <- add.outbreaks.reservices(indicator=indicators.data$reservices.week,
                                                                      quarters.list=quarter)
}

#View(outbreaks.reservices.results)
#View(outbreaks.reservices.results[["2017.4"]])



# pregnancy length ----

add.outbreaks.pregnancy.length <- function(indicator=indicators.data$pregnancy.length,
                                           quarters.list=quarters.list
)
{
  range.indicator <- max(1,(dim(indicators.data$pregnancy.length)[1]-continuous.window+1)):dim(indicators.data$pregnancy.length)[1]
  
  df.indicator.evaluate.system <- continuous.indicators(indicator=indicators.data$pregnancy.length)
  
  data = df.indicator.evaluate.system[,"observed"]
  
  week.quarter <- df.indicator.evaluate.system$week-(floor((df.indicator.evaluate.system$week-1)/13)*13)
  
  
  quarter <- c(rep(NA, dim(df.indicator.evaluate.system)[1]))
  
  quarter <- ifelse(df.indicator.evaluate.system$week<=13, 
                    paste(df.indicator.evaluate.system$year,1, sep = "."), quarter)
  
  quarter <- ifelse(df.indicator.evaluate.system$week>13 & 
                      df.indicator.evaluate.system$week<=26, 
                    paste(df.indicator.evaluate.system$year,2, sep = "."), quarter)
  
  quarter <- ifelse(df.indicator.evaluate.system$week>26 & 
                      df.indicator.evaluate.system$week<=39, 
                    paste(df.indicator.evaluate.system$year,3, sep = "."), quarter)
  
  quarter <- ifelse(df.indicator.evaluate.system$week>39, 
                    paste(df.indicator.evaluate.system$year,4, sep = "."), quarter)
  
  
  for ( q in quarters.list){ #q="2017.2"
    
    start = first(which(quarter==q))
    end = last(which(quarter==q))
    
    y <- data[(start):(end)]
    
    ## log-normal curve was not used because we don't want values lower than 105 and bigger than 120
    
    ## 6.63% of sows farrowed before the 110th day of pregnancy between t1 and t4,
    ## (including 0.5% that farrowed before the 105th day of pregnancy)
    ## 25.6 % farrowed before the 110th day of pregnancy (premature farrowing) between t5 and t8
    ## 7.85 % farrowed before the 110th day of pregnancy (premature farrowing) between t9 and t12
    ## then decreased progressively
    
    weeks <- df.indicator.evaluate.system$week[(which(quarter==q))]
    weeks.corrected <- weeks-(floor((weeks-1)/13)*13)
    
    #y(observed) of some weeks
    y.weeks.1.4 <- y[weeks.corrected<=4]
    y.weeks.5.8 <- y[weeks.corrected>=5 & weeks.corrected<=8]
    y.weeks.9.13 <- y[weeks.corrected>=9 & weeks.corrected<=13]
    
    #create random numbers 
    y.weeks.1.4.affected <- round(runif(length(y.weeks.1.4)*0.07, min=105, max=110))
    y.weeks.5.8.affected <- round(runif(length(y.weeks.5.8)*0.26, min=105, max=110))
    y.weeks.9.13.affected <- round(runif(length(y.weeks.9.13)*0.08, min=105, max=110))
    
    #change the values
    y.weeks.1.4[round(runif(length(y.weeks.1.4.affected),min=1,max=length(y.weeks.1.4)))] <- 
      min(y.weeks.1.4.affected,y.weeks.1.4[round(runif(length(y.weeks.1.4.affected),min=1,max=length(y.weeks.1.4)))],
          na.rm=T)
    y.weeks.5.8[round(runif(length(y.weeks.5.8.affected),min=1,max=length(y.weeks.5.8)))] <- 
      min(y.weeks.5.8.affected,y.weeks.5.8[round(runif(length(y.weeks.5.8.affected),min=1,max=length(y.weeks.5.8)))],
          na.rm=T)
    y.weeks.9.13[round(runif(length(y.weeks.9.13.affected),min=1,max=length(y.weeks.9.13)))] <- 
      min(y.weeks.9.13.affected,y.weeks.9.13[round(runif(length(y.weeks.9.13.affected),min=1,max=length(y.weeks.9.13)))],
          na.rm=T)
    
    simulated.outbreak <- c(y.weeks.1.4,y.weeks.5.8,y.weeks.9.13)
    
    add.pregnancy.length.df.observed <- 
      data.frame(replace(data, which(quarter==q), simulated.outbreak))
    
    
    add.df.indicator.evaluate.system <- data.frame(df.indicator.evaluate.system$date,
                                                   df.indicator.evaluate.system$week,
                                                   df.indicator.evaluate.system$year,
                                                   df.indicator.evaluate.system$sowINDEX,
                                                   df.indicator.evaluate.system$parity,
                                                   add.pregnancy.length.df.observed,
                                                   df.indicator.evaluate.system$baseline,
                                                   df.indicator.evaluate.system$`UCL EWMA`,
                                                   df.indicator.evaluate.system$`LCL EWMA`,
                                                   df.indicator.evaluate.system$`alarms EWMA`,
                                                   df.indicator.evaluate.system$`UCL Shewhart`,
                                                   df.indicator.evaluate.system$`LCL Shewhart`,
                                                   df.indicator.evaluate.system$`alarms Shewhart`)
    
    colnames(add.df.indicator.evaluate.system) <- c("date", "week", "year", "sowINDEX", "parity",
                                                    "observed", "baseline", "UCL EWMA", "LCL EWMA",
                                                    "alarms EWMA", "UCL Shewhart", 
                                                    "LCL Shewhart", "alarms Shewhart")
    
    ## Clean Baseline
    
    add.df.indicator.evaluate.system <- clean_baseline_perc(df.indicator=add.df.indicator.evaluate.system,
                                                            limit.upp=NULL,
                                                            limit.lw=0.05,
                                                            median.days.production.cycles=300,
                                                            nr.production.cycles=2,
                                                            range=range.indicator,
                                                            indicator.type="C")
    ## Applying EWMA
    
    add.df.indicator.evaluate.system <- apply_ewma(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=NULL,
                                                   baseline.weekly.window=NULL,
                                                   continuous.window=5000,
                                                   lambda=0.2,
                                                   limit.sd=c(2.5,3,3.5),
                                                   correct.baseline.UCL.ewma=FALSE,
                                                   correct.baseline.LCL.ewma=FALSE,
                                                   UCL.ewma=2,
                                                   LCL.ewma=2,
                                                   indicator.type="C")
    
    ## Applying Shewhart
    
    add.df.indicator.evaluate.system <- shew_apply(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=NULL,
                                                   baseline.weekly.window=NULL,
                                                   continuous.window=5000,
                                                   limit.sd=c(2.5,3,3.5),
                                                   correct.baseline.UCL.shew=FALSE,
                                                   correct.baseline.LCL.shew=FALSE,
                                                   UCL.shew=2,
                                                   LCL.shew=2,
                                                   indicator.type="C")
    
    
    #delete the row which is week 53
    add.df.indicator.evaluate.system <-
      add.df.indicator.evaluate.system[-c(which(df.indicator.evaluate.system$week==53)),]
    
    
    ## delete the row which is week 53 from week.quarter and quarter
    week.quarter <- week.quarter[-c(which(df.indicator.evaluate.system$week==53))]
    quarter <- quarter[-c(which(df.indicator.evaluate.system$week==53))]
    
    
    ## construct final table
    
    table.evaluate.system <- 
      add_column(add.df.indicator.evaluate.system, week.quarter, .after = 3)
    
    table.evaluate.system <- 
    add_column(table.evaluate.system, quarter, .after = 4)
    
    
    colnames(table.evaluate.system) <- c("date", "week", "year", "week quarter", "quarter",
                                         "sowINDEX", "parity", "observed", "baseline", 
                                         "UCL EWMA", "LCL EWMA", "alarms EWMA", 
                                         "UCL Shewhart", "LCL Shewhart", "alarms Shewhart")
  }
  
  return(table.evaluate.system)
}


## Cluster all in one list

outbreaks.pregnancy.length.results <- list()

for (quarter in quarters.list){
  
  outbreaks.pregnancy.length.results[[quarter]] <- add.outbreaks.pregnancy.length(indicator=indicators.data$pregnancy.length,
                                                                                  quarters.list=quarter)
}

#View(outbreaks.pregnancy.length.results[["2018.2"]])



# abortions per week ----

add.outbreaks.abortions <- function(indicator=indicators.data$abortions.week,
                                    quarters.list=quarters.list
                                    
)
{
  range_weekly <- max(1,(dim(index.dates.week)[1]-weekly.window+1)):dim(index.dates.week)[1]
  
  df.indicator.evaluate.system <- weekly.indicators(indicator=indicators.data$abortions.week,
                                                    range.weekly=1:dim(index.dates.week)[1])
  
  
  data = df.indicator.evaluate.system[,"observed"]
  
  week.quarter <- index.dates.week$week-(floor((index.dates.week$week-1)/13)*13)
  
  quarter <- c(rep(NA, dim(df.indicator.evaluate.system)[1]))
  
  quarter <- ifelse(index.dates.week$week<=13, 
                    paste(index.dates.week$ISOweekYear,1, sep = "."), quarter)
  
  quarter <- ifelse(index.dates.week$week>13 & 
                      index.dates.week$week<=26, 
                    paste(index.dates.week$ISOweekYear,2, sep = "."), quarter)
  
  quarter <- ifelse(index.dates.week$week>26 & 
                      index.dates.week$week<=39, 
                    paste(index.dates.week$ISOweekYear,3, sep = "."), quarter)
  
  quarter <- ifelse(index.dates.week$week>39, 
                    paste(index.dates.week$ISOweekYear,4, sep = "."), quarter)
  
  for ( q in quarters.list){ #q="2018.4"
    
    start = first(which(quarter==q))
    end = last(which(quarter==q))
    
    y <- data[(start):(end)]
    
    ## doubled in week t1 and quintupled in week t2 (*4), in t3 decreased during 20 weeks
    
    lgn.abortions <- 4* plnorm(c((100/2),(100/2*2),
                                 100-(100/20),100-(100/20*2),100-(100/20*3),100-(100/20*4),
                                 100-(100/20*5),100-(100/20*6),100-(100/20*7),100-(100/20*8),
                                 100-(100/20*9),100-(100/20*10),100-(100/20*11)),
                               meanlog=4, sdlog=0.3, lower.tail=TRUE, log.p=FALSE)
    
    
    #plot(lgn.abortions, type="l", ylim=c(0,4)) 
    
    ## binomial attempt
    #increase curve = lgn.abortions
    #actual probability of abortion in any given week = sum(data)/length(data)
    #simulated increase in the probability of an abortion in a given week:
    simulated.probabilities.outbreak <- lgn.abortions * sum(data)/length(data)
    simulated.increase <- rbinom(13,1,simulated.probabilities.outbreak)
    
    simulated.outbreak <- simulated.increase+y
    
    #plot(simulated.outbreak,type="l")
    
    add.abortions.per.week.observed <- 
      data.frame(replace(data, which(quarter==q), simulated.outbreak))
    
    
    add.df.indicator.evaluate.system <- data.frame(df.indicator.evaluate.system$gilt,
                                                   df.indicator.evaluate.system$young,
                                                   df.indicator.evaluate.system$prime,
                                                   df.indicator.evaluate.system$mature,
                                                   add.abortions.per.week.observed,
                                                   df.indicator.evaluate.system$baseline,
                                                   df.indicator.evaluate.system$`UCL EWMA`,
                                                   df.indicator.evaluate.system$`LCL EWMA`,
                                                   df.indicator.evaluate.system$`alarms EWMA`,
                                                   df.indicator.evaluate.system$`UCL Shewhart`,
                                                   df.indicator.evaluate.system$`LCL Shewhart`,
                                                   df.indicator.evaluate.system$`alarms Shewhart`)
    
    colnames(add.df.indicator.evaluate.system) <- c("gilt", "young", "prime", "mature",
                                                    "observed", "baseline", "UCL EWMA", "LCL EWMA",
                                                    "alarms EWMA", "UCL Shewhart", 
                                                    "LCL Shewhart", "alarms Shewhart")
    
    
    ## Clean Baseline
    
    add.df.indicator.evaluate.system <- clean_baseline_perc(df.indicator=add.df.indicator.evaluate.system,
                                                            limit.upp=0.95,
                                                            limit.lw=NULL,
                                                            run.window.weekly=104,
                                                            median.days.production.cycles=NULL,
                                                            nr.production.cycles=2,
                                                            range=range_weekly,
                                                            indicator.type="W")
    ## Applying EWMA
    
    add.df.indicator.evaluate.system <- apply_ewma(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=106,
                                                   baseline.weekly.window=52,
                                                   lambda=0.2,
                                                   limit.sd=c(2.5,3,3.5),
                                                   guard.band.weekly=2,
                                                   correct.baseline.UCL.ewma=TRUE,
                                                   correct.baseline.LCL.ewma=TRUE,
                                                   UCL.ewma=2,
                                                   LCL.ewma=2,
                                                   indicator.type="W")
    
    ## Applying Shewhart
    
    add.df.indicator.evaluate.system <- shew_apply(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=106,
                                                   baseline.weekly.window=52,
                                                   limit.sd=c(2.5,3,3.5),
                                                   guard.band.weekly=2,
                                                   correct.baseline.UCL.shew=FALSE,
                                                   correct.baseline.LCL.shew=FALSE,
                                                   UCL.shew=2,
                                                   LCL.shew=2,
                                                   indicator.type="W")
    
    #delete the row which is week 53
    add.df.indicator.evaluate.system <-
      add.df.indicator.evaluate.system[-c(which(index.dates.week$week==53)),]
    
    ## construct final table
    
    dates <- data.frame(index.dates.week$start[-(which(index.dates.week$week==53))],
                        index.dates.week$week[-(which(index.dates.week$week==53))],
                        index.dates.week$ISOweekYear[-(which(index.dates.week$week==53))],
                        week.quarter[-(which(index.dates.week$week==53))],
                        quarter[-(which(index.dates.week$week==53))])
    
    table.evaluate.system <- data.frame(dates, add.df.indicator.evaluate.system)
    
    
    colnames(table.evaluate.system) <- c("date", "week", "year", "week quarter", "quarter",
                                         "gilt", "young", "prime", "mature",
                                         "observed", "baseline", "UCL EWMA", "LCL EWMA",
                                         "alarms EWMA", "UCL Shewhart", 
                                         "LCL Shewhart", "alarms Shewhart")
  }
  
  return(table.evaluate.system)
}


## Cluster all in one list

outbreaks.abortions.results <- list()

for (quarter in quarters.list){
  
  outbreaks.abortions.results[[quarter]] <- add.outbreaks.abortions(indicator=indicators.data$abortions.week,
                                                                    quarters.list=quarter)
}

#View(outbreaks.abortions.results[["2018.4"]])



# live born per farrowing ----

add.outbreaks.live.piglets <- function(indicator=indicators.data$live.born.litter,
                                       quarters.list=quarters.list
)
{
  range.indicator <- max(1,(dim(indicators.data$live.born.litter)[1]-continuous.window+1)):dim(indicators.data$live.born.litter)[1]
  
  df.indicator.evaluate.system <- continuous.indicators(indicator=indicators.data$live.born.litter)
  
  data = df.indicator.evaluate.system[,"observed"]
  
  week.quarter <- df.indicator.evaluate.system$week-(floor((df.indicator.evaluate.system$week-1)/13)*13)
  
  
  quarter <- c(rep(NA, dim(df.indicator.evaluate.system)[1]))
  
  quarter <- ifelse(df.indicator.evaluate.system$week<=13, 
                    paste(df.indicator.evaluate.system$year,1, sep = "."), quarter)
  
  quarter <- ifelse(df.indicator.evaluate.system$week>13 & 
                      df.indicator.evaluate.system$week<=26, 
                    paste(df.indicator.evaluate.system$year,2, sep = "."), quarter)
  
  quarter <- ifelse(df.indicator.evaluate.system$week>26 & 
                      df.indicator.evaluate.system$week<=39, 
                    paste(df.indicator.evaluate.system$year,3, sep = "."), quarter)
  
  quarter <- ifelse(df.indicator.evaluate.system$week>39, 
                    paste(df.indicator.evaluate.system$year,4, sep = "."), quarter)
  
  
  for ( q in quarters.list){  #q="2018.2"
    
    start = first(which(quarter==q))
    end = last(which(quarter==q))
    
    y <- data[(start):(end)]
    
    ## decreased 1 animal (8%) between t3 and t20, biggest decline in t4 and t5
    
    lgn.live.piglets <- 0.1* (plnorm(c(1,1,(100/3),(100/3*2),(100/3*3),
                                       100-(100/28),100-(100/28*2),100-(100/28*3),100-(100/28*4),100-(100/28*5), 
                                       100-(100/28*6), 100-(100/28*7), 100-(100/28*8)),
                                     meanlog=4, sdlog=0.3, lower.tail=TRUE, log.p=FALSE))
    
    #plot(lgn.live.piglets, type="l") 
    
    
    ## from t1 to t4 2.54% of the total piglets farrowed were mummified
    ## from t5 to t8 21.8% of the total piglets farrowed were mummified
    ## then started to decrease until t12 (about 6.6%) 
    ## and returned to baseline values after 11 months (t44) - lets say 28 weeks
    ## mean(lgn.mummi[5:8])=0.21, mean(lgn.mummi[1:4])=0.03
    
    lgn.mummi <- 0.25* (plnorm(c((100/8),(100/8*2),(100/8*3),(100/8*4),(100/8*5),(100/8*6),
                                 (100/8*7),(100/8*8),
                                 100-(100/28*10),100-(100/28*11),100-(100/28*12),100-(100/28*13),100-(100/28*14)),
                               meanlog=4, sdlog=0.3, lower.tail=TRUE, log.p=FALSE))
    
    #plot(lgn.mummi, type="l") 
    
    
    weeks <- df.indicator.evaluate.system$week[(which(quarter==q))]
    weeks.corrected <- weeks-(floor((weeks-1)/13)*13)
    
    factor <- lgn.live.piglets[weeks.corrected]
    born.dead <- round(y*factor)   ##born.dead could be stillbirths or mummified
    
    simulated.outbreak <- pmax(0,y-born.dead)
    
    
    born.mummi <- ceiling(lgn.mummi[weeks.corrected]*(y+born.dead))
    
    
    add.live.piglets.per.farrowing.observed <- 
      data.frame(replace(data, which(quarter==q), simulated.outbreak))
    
  
    add.df.indicator.evaluate.system <- data.frame(df.indicator.evaluate.system$date,
                                                   df.indicator.evaluate.system$week,
                                                   df.indicator.evaluate.system$year,
                                                   df.indicator.evaluate.system$sowINDEX,
                                                   df.indicator.evaluate.system$parity,
                                                   add.live.piglets.per.farrowing.observed,
                                                   df.indicator.evaluate.system$baseline,
                                                   df.indicator.evaluate.system$`UCL EWMA`,
                                                   df.indicator.evaluate.system$`LCL EWMA`,
                                                   df.indicator.evaluate.system$`alarms EWMA`,
                                                   df.indicator.evaluate.system$`UCL Shewhart`,
                                                   df.indicator.evaluate.system$`LCL Shewhart`,
                                                   df.indicator.evaluate.system$`alarms Shewhart`)
    
    colnames(add.df.indicator.evaluate.system) <- c("date", "week", "year", "sowINDEX", "parity",
                                                    "observed", "baseline", "UCL EWMA", "LCL EWMA",
                                                    "alarms EWMA", "UCL Shewhart", 
                                                    "LCL Shewhart", "alarms Shewhart")
    
   ## Clean Baseline
    
    add.df.indicator.evaluate.system <- clean_baseline_perc(df.indicator=add.df.indicator.evaluate.system,
                                                            limit.upp=NULL,
                                                            limit.lw=0.05,
                                                            median.days.production.cycles=300,
                                                            nr.production.cycles=2,
                                                            range=range.indicator,
                                                            indicator.type="C")
    ## Applying EWMA
    
    add.df.indicator.evaluate.system <- apply_ewma(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=NULL,
                                                   baseline.weekly.window=NULL,
                                                   continuous.window=5000,
                                                   lambda=0.2,
                                                   limit.sd=c(2.5,3,3.5),
                                                   correct.baseline.UCL.ewma=FALSE,
                                                   correct.baseline.LCL.ewma=FALSE,
                                                   UCL.ewma=2,
                                                   LCL.ewma=2,
                                                   indicator.type="C")
    
    ## Applying Shewhart
    
    add.df.indicator.evaluate.system <- shew_apply(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=NULL,
                                                   baseline.weekly.window=NULL,
                                                   continuous.window=5000,
                                                   limit.sd=c(2.5,3,3.5),
                                                   correct.baseline.UCL.shew=FALSE,
                                                   correct.baseline.LCL.shew=FALSE,
                                                   UCL.shew=2,
                                                   LCL.shew=2,
                                                   indicator.type="C")
    
    
    #delete the row which is week 53
    add.df.indicator.evaluate.system <-
      add.df.indicator.evaluate.system[-c(which(df.indicator.evaluate.system$week==53)),]
    
    ## delete the row which is week 53 from week.quarter and quarter
    week.quarter <- week.quarter[-c(which(df.indicator.evaluate.system$week==53))]
    quarter <- quarter[-c(which(df.indicator.evaluate.system$week==53))]
    
    
    ## construct final table
    
    table.evaluate.system <- 
      add_column(add.df.indicator.evaluate.system, week.quarter, .after = 3)
    
    table.evaluate.system <- 
      add_column(table.evaluate.system, quarter, .after = 4)
    
    
    colnames(table.evaluate.system) <- c("date", "week", "year", "week quarter", "quarter",
                                         "sowINDEX", "parity", "observed", "baseline", 
                                         "UCL EWMA", "LCL EWMA", "alarms EWMA", 
                                         "UCL Shewhart", "LCL Shewhart", "alarms Shewhart")
  }
  
  outputs <- list(lgn.live.piglets, born.mummi, table.evaluate.system)
  return(outputs)
}


## Cluster all 3 results in one list called results

outbreaks.live.piglets.results <- list()
lgn.live.piglets.list <- list()
outbreaks.born.mummi.list <- list()

for (quarter in quarters.list){
  
  results <- add.outbreaks.live.piglets(indicator=indicators.data$live.born.litter,
                                        quarters.list=quarter)
  
  lgn.live.piglets.list[[quarter]] <- results[[1]]
  outbreaks.born.mummi.list[[quarter]] <- results[[2]]
  outbreaks.live.piglets.results[[quarter]] <- results[[3]]
  
}

#View(outbreaks.live.piglets.results[["2018.2"]])



# % dead born per farrowing ----

add.outbreaks.perc.dead.piglets <- function(indicator=indicators.data$perc.dead.born.litter,
                                            quarters.list=quarters.list
)
{
  range.indicator <- max(1,(dim(indicators.data$perc.dead.born.litter)[1]-continuous.window+1)):dim(indicators.data$perc.dead.born.litter)[1]
  
  df.indicator.evaluate.system <- continuous.indicators(indicator=indicators.data$perc.dead.born.litter)
  
  data = df.indicator.evaluate.system[,"observed"]
  
  week.quarter <- df.indicator.evaluate.system$week-(floor((df.indicator.evaluate.system$week-1)/13)*13)
  
  
  quarter <- c(rep(NA, dim(df.indicator.evaluate.system)[1]))
  
  quarter <- ifelse(df.indicator.evaluate.system$week<=13, 
                    paste(df.indicator.evaluate.system$year,1, sep = "."), quarter)
  
  quarter <- ifelse(df.indicator.evaluate.system$week>13 & 
                      df.indicator.evaluate.system$week<=26, 
                    paste(df.indicator.evaluate.system$year,2, sep = "."), quarter)
  
  quarter <- ifelse(df.indicator.evaluate.system$week>26 & 
                      df.indicator.evaluate.system$week<=39, 
                    paste(df.indicator.evaluate.system$year,3, sep = "."), quarter)
  
  quarter <- ifelse(df.indicator.evaluate.system$week>39, 
                    paste(df.indicator.evaluate.system$year,4, sep = "."), quarter)
  
  
  for ( q in quarters.list){  #q="2018.2"
    
    start = first(which(quarter==q))
    end = last(which(quarter==q))
    
    y <- data[(start):(end)]
    
    lgn.live.piglets <- lgn.live.piglets.list[[q]]
    
    ## increased from t2 to t18 with the maximum value in t14 of 2 stillbirths per farrowing (not percentage) - (53%)
    ## but we use the same log normal used for indicator live piglets per farrowing (done as it was UCL),
    ## because dead piglets per farrowing should be the opposite of live piglets per farrowing 
    ## (even though in the paper they are not)
    
    
    simulated.outbreak <- round(y+(lgn.live.piglets*100), 2)    ## because these indicator is in percentage
    
    
    add.perc.dead.piglets.per.farrowing.observed <- 
      data.frame(replace(data, which(quarter==q), simulated.outbreak))
    
    
    add.df.indicator.evaluate.system <- data.frame(df.indicator.evaluate.system$date,
                                                   df.indicator.evaluate.system$week,
                                                   df.indicator.evaluate.system$year,
                                                   df.indicator.evaluate.system$sowINDEX,
                                                   df.indicator.evaluate.system$parity,
                                                   add.perc.dead.piglets.per.farrowing.observed,
                                                   df.indicator.evaluate.system$baseline,
                                                   df.indicator.evaluate.system$`UCL EWMA`,
                                                   df.indicator.evaluate.system$`LCL EWMA`,
                                                   df.indicator.evaluate.system$`alarms EWMA`,
                                                   df.indicator.evaluate.system$`UCL Shewhart`,
                                                   df.indicator.evaluate.system$`LCL Shewhart`,
                                                   df.indicator.evaluate.system$`alarms Shewhart`)
    
    colnames(add.df.indicator.evaluate.system) <- c("date", "week", "year", "sowINDEX", "parity",
                                                    "observed", "baseline", "UCL EWMA", "LCL EWMA",
                                                    "alarms EWMA", "UCL Shewhart", 
                                                    "LCL Shewhart", "alarms Shewhart")
    
    ## Clean Baseline
    
    add.df.indicator.evaluate.system <- clean_baseline_perc(df.indicator=add.df.indicator.evaluate.system,
                                                            limit.upp=0.95,
                                                            limit.lw=NULL,
                                                            median.days.production.cycles=300,
                                                            nr.production.cycles=2,
                                                            range=range.indicator,
                                                            indicator.type="C")
    ## Applying EWMA
    
    add.df.indicator.evaluate.system <- apply_ewma(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=NULL,
                                                   baseline.weekly.window=NULL,
                                                   continuous.window=5000,
                                                   lambda=0.2,
                                                   limit.sd=c(2.5,3,3.5),
                                                   correct.baseline.UCL.ewma=FALSE,
                                                   correct.baseline.LCL.ewma=FALSE,
                                                   UCL.ewma=2,
                                                   LCL.ewma=2,
                                                   indicator.type="C")
    
    ## Applying Shewhart
    
    add.df.indicator.evaluate.system <- shew_apply(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=NULL,
                                                   baseline.weekly.window=NULL,
                                                   continuous.window=5000,
                                                   limit.sd=c(2.5,3,3.5),
                                                   correct.baseline.UCL.shew=FALSE,
                                                   correct.baseline.LCL.shew=FALSE,
                                                   UCL.shew=2,
                                                   LCL.shew=2,
                                                   indicator.type="C")
    
    #delete the row which is week 53
    add.df.indicator.evaluate.system <-
      add.df.indicator.evaluate.system[-c(which(df.indicator.evaluate.system$week==53)),]
    
    ## delete the row which is week 53 from week.quarter and quarter
    week.quarter <- week.quarter[-c(which(df.indicator.evaluate.system$week==53))]
    quarter <- quarter[-c(which(df.indicator.evaluate.system$week==53))]
    
    
    ## construct final table
    
    table.evaluate.system <- 
      add_column(add.df.indicator.evaluate.system, week.quarter, .after = 3)
    
    table.evaluate.system <- 
      add_column(table.evaluate.system, quarter, .after = 4)
    
    
    colnames(table.evaluate.system) <- c("date", "week", "year", "week quarter", "quarter",
                                         "sowINDEX", "parity", "observed", "baseline", 
                                         "UCL EWMA", "LCL EWMA", "alarms EWMA", 
                                         "UCL Shewhart", "LCL Shewhart", "alarms Shewhart")
 
  }   
  return(table.evaluate.system)
}


## Cluster all in one list

outbreaks.perc.dead.piglets.results <- list()

for (quarter in quarters.list){
  
  outbreaks.perc.dead.piglets.results[[quarter]] <- 
    add.outbreaks.perc.dead.piglets(indicator=indicators.data$perc.dead.born.litter,
                                    quarters.list=quarter)
}

#View(outbreaks.perc.dead.piglets.results[["2018.2"]])



# mummified per farrowing ----

add.outbreaks.mummi.piglets <- function(indicator=indicators.data$mummi.born.litter,
                                        quarters.list=quarters.list
)
{
  range.indicator <- max(1,(dim(indicators.data$mummi.born.litter)[1]-continuous.window+1)):dim(indicators.data$mummi.born.litter)[1]
  
  df.indicator.evaluate.system <- continuous.indicators(indicator=indicators.data$mummi.born.litter)
  
  data = df.indicator.evaluate.system[,"observed"]
  
  week.quarter <- df.indicator.evaluate.system$week-(floor((df.indicator.evaluate.system$week-1)/13)*13)
  
  
  quarter <- c(rep(NA, dim(df.indicator.evaluate.system)[1]))
  
  quarter <- ifelse(df.indicator.evaluate.system$week<=13, 
                    paste(df.indicator.evaluate.system$year,1, sep = "."), quarter)
  
  quarter <- ifelse(df.indicator.evaluate.system$week>13 & 
                      df.indicator.evaluate.system$week<=26, 
                    paste(df.indicator.evaluate.system$year,2, sep = "."), quarter)
  
  quarter <- ifelse(df.indicator.evaluate.system$week>26 & 
                      df.indicator.evaluate.system$week<=39, 
                    paste(df.indicator.evaluate.system$year,3, sep = "."), quarter)
  
  quarter <- ifelse(df.indicator.evaluate.system$week>39, 
                    paste(df.indicator.evaluate.system$year,4, sep = "."), quarter)
  
  
  for ( q in quarters.list){  #q="2018.2"
    
    start = first(which(quarter==q))
    end = last(which(quarter==q))
    
    y <- data[(start):(end)]
    
    
    y.born.mummi <- outbreaks.born.mummi.list[[q]]
    
    if(length(y)!=length(y.born.mummi))(print("ERROR!, not same length"))
    
    
    ## lgn function is in live piglets' function
    
    simulated.outbreak <- y+y.born.mummi
    
    add.mummi.piglets.per.farrowing.observed <- 
      data.frame(replace(data, which(quarter==q), simulated.outbreak))
    
    
    add.df.indicator.evaluate.system <- data.frame(df.indicator.evaluate.system$date,
                                                   df.indicator.evaluate.system$week,
                                                   df.indicator.evaluate.system$year,
                                                   df.indicator.evaluate.system$sowINDEX,
                                                   df.indicator.evaluate.system$parity,
                                                   add.mummi.piglets.per.farrowing.observed,
                                                   df.indicator.evaluate.system$baseline,
                                                   df.indicator.evaluate.system$`UCL EWMA`,
                                                   df.indicator.evaluate.system$`LCL EWMA`,
                                                   df.indicator.evaluate.system$`alarms EWMA`,
                                                   df.indicator.evaluate.system$`UCL Shewhart`,
                                                   df.indicator.evaluate.system$`LCL Shewhart`,
                                                   df.indicator.evaluate.system$`alarms Shewhart`)
    
    colnames(add.df.indicator.evaluate.system) <- c("date", "week", "year", "sowINDEX", "parity",
                                                    "observed", "baseline", "UCL EWMA", "LCL EWMA",
                                                    "alarms EWMA", "UCL Shewhart", 
                                                    "LCL Shewhart", "alarms Shewhart")
    
    
    ## Clean Baseline
    
    add.df.indicator.evaluate.system <- clean_baseline_perc(df.indicator=add.df.indicator.evaluate.system,
                                                            limit.upp=0.95,
                                                            limit.lw=NULL,
                                                            median.days.production.cycles=300,
                                                            nr.production.cycles=2,
                                                            range=range.indicator,
                                                            indicator.type="C")
    ## Applying EWMA
    
    add.df.indicator.evaluate.system <- apply_ewma(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=NULL,
                                                   baseline.weekly.window=NULL,
                                                   continuous.window=5000,
                                                   lambda=0.2,
                                                   limit.sd=c(2.5,3,3.5),
                                                   correct.baseline.UCL.ewma=FALSE,
                                                   correct.baseline.LCL.ewma=FALSE,
                                                   UCL.ewma=2,
                                                   LCL.ewma=2,
                                                   indicator.type="C")
    
    ## Applying Shewhart
    
    add.df.indicator.evaluate.system <- shew_apply(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=NULL,
                                                   baseline.weekly.window=NULL,
                                                   continuous.window=5000,
                                                   limit.sd=c(2.5,3,3.5),
                                                   correct.baseline.UCL.shew=FALSE,
                                                   correct.baseline.LCL.shew=FALSE,
                                                   UCL.shew=2,
                                                   LCL.shew=2,
                                                   indicator.type="C")
    
    
    #delete the row which is week 53
    add.df.indicator.evaluate.system <-
      add.df.indicator.evaluate.system[-c(which(df.indicator.evaluate.system$week==53)),]
    
    ## delete the row which is week 53 from week.quarter and quarter
    week.quarter <- week.quarter[-c(which(df.indicator.evaluate.system$week==53))]
    quarter <- quarter[-c(which(df.indicator.evaluate.system$week==53))]
    
    
    ## construct final table
    
    table.evaluate.system <- 
      add_column(add.df.indicator.evaluate.system, week.quarter, .after = 3)
    
    table.evaluate.system <- 
      add_column(table.evaluate.system, quarter, .after = 4)
    
    
    colnames(table.evaluate.system) <- c("date", "week", "year", "week quarter", "quarter",
                                         "sowINDEX", "parity", "observed", "baseline", 
                                         "UCL EWMA", "LCL EWMA", "alarms EWMA", 
                                         "UCL Shewhart", "LCL Shewhart", "alarms Shewhart")
    
  }   
  return(table.evaluate.system)
}


## Cluster all in one list

outbreaks.mummi.piglets.results <- list()

for (quarter in quarters.list){
  
  outbreaks.mummi.piglets.results[[quarter]] <- add.outbreaks.mummi.piglets(indicator=indicators.data$mummi.born.litter,
                                                                            quarters.list=quarter)
}

#View(outbreaks.mummi.piglets.results[["2018.2"]])



# piglets weaned per week ----

add.outbreaks.piglets.weaned.week <- function(indicator=indicators.data$total.wean.week,
                                              quarters.list=quarters.list
                                              
)
{
  range_weekly <- max(1,(dim(index.dates.week)[1]-weekly.window+1)):dim(index.dates.week)[1]
  
  df.indicator.evaluate.system <- weekly.indicators(indicator=indicators.data$total.wean.week,
                                                    range.weekly=1:dim(index.dates.week)[1])
  
  data = df.indicator.evaluate.system[,"observed"]
  
  week.quarter <- index.dates.week$week-(floor((index.dates.week$week-1)/13)*13)
  
  quarter <- c(rep(NA, dim(df.indicator.evaluate.system)[1]))
  
  quarter <- ifelse(index.dates.week$week<=13, 
                    paste(index.dates.week$ISOweekYear,1, sep = "."), quarter)
  
  quarter <- ifelse(index.dates.week$week>13 & 
                      index.dates.week$week<=26, 
                    paste(index.dates.week$ISOweekYear,2, sep = "."), quarter)
  
  quarter <- ifelse(index.dates.week$week>26 & 
                      index.dates.week$week<=39, 
                    paste(index.dates.week$ISOweekYear,3, sep = "."), quarter)
  
  quarter <- ifelse(index.dates.week$week>39, 
                    paste(index.dates.week$ISOweekYear,4, sep = "."), quarter)
  
  
  for ( q in quarters.list){ #q="2018.4"
    
    start = first(which(quarter==q))
    end = last(which(quarter==q))
    
    y <- data[(start):(end)]
    
    ## decreased from week t1 until week t7, and in week t7 decreased 23% in comparison with the baseline ,
    ## then increased until t35 (during 28 weeks (35-7))
    
    lgn.weaned.week <- 0.23*(plnorm(c((100/7),(100/7*2),(100/7*3),(100/7*4),(100/7*5),(100/7*6),(100/7*7),
                                      100-(100/28),100-(100/28*2),100-(100/28*3),100-(100/28*4),
                                      100-(100/28*5),100-(100/28*6)),
                                    meanlog=4, sdlog=0.3, lower.tail=TRUE, log.p=FALSE))
    
    #plot(lgn.weaned.week, type="l") 
    
    simulated.outbreak <- pmax(0,y-(ceiling(y*lgn.weaned.week)))
    #plot(simulated.outbreak,type="l")
    
    
    add.piglets.weaned.per.week.observed <-
      replace(data, which(quarter==q), simulated.outbreak)
    
    add.df.indicator.evaluate.system <- data.frame(df.indicator.evaluate.system$gilt,
                                                   df.indicator.evaluate.system$young,
                                                   df.indicator.evaluate.system$prime,
                                                   df.indicator.evaluate.system$mature,
                                                   add.piglets.weaned.per.week.observed,
                                                   df.indicator.evaluate.system$baseline,
                                                   df.indicator.evaluate.system$`UCL EWMA`,
                                                   df.indicator.evaluate.system$`LCL EWMA`,
                                                   df.indicator.evaluate.system$`alarms EWMA`,
                                                   df.indicator.evaluate.system$`UCL Shewhart`,
                                                   df.indicator.evaluate.system$`LCL Shewhart`,
                                                   df.indicator.evaluate.system$`alarms Shewhart`)
    
    colnames(add.df.indicator.evaluate.system) <- c("gilt", "young", "prime", "mature",
                                                    "observed", "baseline", "UCL EWMA", "LCL EWMA",
                                                    "alarms EWMA", "UCL Shewhart", 
                                                    "LCL Shewhart", "alarms Shewhart")
    
    ## Clean Baseline
    
    add.df.indicator.evaluate.system <- clean_baseline_perc(df.indicator=add.df.indicator.evaluate.system,
                                                            limit.upp=NULL,
                                                            limit.lw=0.05,
                                                            run.window.weekly=104,
                                                            median.days.production.cycles=NULL,
                                                            nr.production.cycles=2,
                                                            range=range_weekly,
                                                            indicator.type="W")
    ## Applying EWMA
    
    add.df.indicator.evaluate.system <- apply_ewma(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=106,
                                                   baseline.weekly.window=52,
                                                   lambda=0.2,
                                                   limit.sd=c(2.5,3,3.5),
                                                   guard.band.weekly=2,
                                                   correct.baseline.UCL.ewma=TRUE,
                                                   correct.baseline.LCL.ewma=TRUE,
                                                   UCL.ewma=2,
                                                   LCL.ewma=2,
                                                   indicator.type="W")
    
    ## Applying Shewhart
    
    add.df.indicator.evaluate.system <- shew_apply(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=106,
                                                   baseline.weekly.window=52,
                                                   limit.sd=c(2.5,3,3.5),
                                                   guard.band.weekly=2,
                                                   correct.baseline.UCL.shew=FALSE,
                                                   correct.baseline.LCL.shew=FALSE,
                                                   UCL.shew=2,
                                                   LCL.shew=2,
                                                   indicator.type="W")
    
    #delete the row which is week 53
    add.df.indicator.evaluate.system <-
      add.df.indicator.evaluate.system[-c(which(index.dates.week$week==53)),]
    
    ## construct final table
    
    dates <- data.frame(index.dates.week$start[-(which(index.dates.week$week==53))],
                        index.dates.week$week[-(which(index.dates.week$week==53))],
                        index.dates.week$ISOweekYear[-(which(index.dates.week$week==53))],
                        week.quarter[-(which(index.dates.week$week==53))],
                        quarter[-(which(index.dates.week$week==53))])
    
    table.evaluate.system <- data.frame(dates, add.df.indicator.evaluate.system)
    
    
    colnames(table.evaluate.system) <- c("date", "week", "year", "week quarter", "quarter",
                                         "gilt", "young", "prime", "mature",
                                         "observed", "baseline", "UCL EWMA", "LCL EWMA",
                                         "alarms EWMA", "UCL Shewhart", 
                                         "LCL Shewhart", "alarms Shewhart")
  }
  
  return(table.evaluate.system)
}


## Cluster all in one list

outbreaks.piglets.weaned.week.results <- list()

for (quarter in quarters.list){
  
  outbreaks.piglets.weaned.week.results[[quarter]] <- 
    add.outbreaks.piglets.weaned.week(indicator=indicators.data$total.wean.week,
                                      quarters.list=quarter)
}

#View(outbreaks.piglets.weaned.week.results[["2018.4"]])



# piglets weaned per weaning ----

add.outbreaks.piglets.weaned.litter <- function(indicator=indicators.data$total.wean.litter,
                                                quarters.list=quarters.list
)
{
  
  range.indicator <- max(1,(dim(indicators.data$total.wean.litter)[1]-continuous.window+1)):dim(indicators.data$total.wean.litter)[1]
  
  df.indicator.evaluate.system <- continuous.indicators(indicator=indicators.data$total.wean.litter)
  
  data = df.indicator.evaluate.system[,"observed"]
  
  week.quarter <- df.indicator.evaluate.system$week-(floor((df.indicator.evaluate.system$week-1)/13)*13)
  
  
  quarter <- c(rep(NA, dim(df.indicator.evaluate.system)[1]))
  
  quarter <- ifelse(df.indicator.evaluate.system$week<=13, 
                    paste(df.indicator.evaluate.system$year,1, sep = "."), quarter)
  
  quarter <- ifelse(df.indicator.evaluate.system$week>13 & 
                      df.indicator.evaluate.system$week<=26, 
                    paste(df.indicator.evaluate.system$year,2, sep = "."), quarter)
  
  quarter <- ifelse(df.indicator.evaluate.system$week>26 & 
                      df.indicator.evaluate.system$week<=39, 
                    paste(df.indicator.evaluate.system$year,3, sep = "."), quarter)
  
  quarter <- ifelse(df.indicator.evaluate.system$week>39, 
                    paste(df.indicator.evaluate.system$year,4, sep = "."), quarter)
  
  
  for ( q in quarters.list){  #q="2018.2"
    
    start = first(which(quarter==q))
    end = last(which(quarter==q))
    
    y <- data[(start):(end)]
    
    ## decreased from week t1 until week t7, and in week t7 decreased 15% in comparison with the baseline ,
    ## then increased until t35 (during 28 weeks (35-7))
    
    lgn.weaned.litter <- 0.15*(plnorm(c((100/7),(100/7*2),(100/7*3),(100/7*4),(100/7*5),(100/7*6),(100/7*7),
                                        100-(100/28),100-(100/28*2),100-(100/28*3),100-(100/28*4),100-(100/28*5), 
                                        100-(100/28*6)),
                                      meanlog=4, sdlog=0.3, lower.tail=TRUE, log.p=FALSE))
    
    
    #plot(lgn.weaned.litter, type="l") 
    
    weeks <- df.indicator.evaluate.system$week[(which(quarter==q))]
    weeks.corrected <- weeks-(floor((weeks-1)/13)*13)
    
    simulated.outbreak <- pmax(0,y-ceiling(y*lgn.weaned.litter[weeks.corrected]))
    
    #plot(simulated.outbreak,type="l")
    
    
    add.piglets.weaned.per.litter.observed <- 
      data.frame(replace(data, which(quarter==q), simulated.outbreak))
    
    
    add.df.indicator.evaluate.system <- data.frame(df.indicator.evaluate.system$date,
                                                   df.indicator.evaluate.system$week,
                                                   df.indicator.evaluate.system$year,
                                                   df.indicator.evaluate.system$sowINDEX,
                                                   df.indicator.evaluate.system$parity,
                                                   add.piglets.weaned.per.litter.observed,
                                                   df.indicator.evaluate.system$baseline,
                                                   df.indicator.evaluate.system$`UCL EWMA`,
                                                   df.indicator.evaluate.system$`LCL EWMA`,
                                                   df.indicator.evaluate.system$`alarms EWMA`,
                                                   df.indicator.evaluate.system$`UCL Shewhart`,
                                                   df.indicator.evaluate.system$`LCL Shewhart`,
                                                   df.indicator.evaluate.system$`alarms Shewhart`)
    
    colnames(add.df.indicator.evaluate.system) <- c("date", "week", "year", "sowINDEX", "parity",
                                                    "observed", "baseline", "UCL EWMA", "LCL EWMA",
                                                    "alarms EWMA", "UCL Shewhart", 
                                                    "LCL Shewhart", "alarms Shewhart")
    
    ## Clean Baseline
    
    add.df.indicator.evaluate.system <- clean_baseline_perc(df.indicator=add.df.indicator.evaluate.system,
                                                            limit.upp=NULL,
                                                            limit.lw=0.05,
                                                            median.days.production.cycles=300,
                                                            nr.production.cycles=2,
                                                            range=range.indicator,
                                                            indicator.type="C")
    ## Applying EWMA
    
    add.df.indicator.evaluate.system <- apply_ewma(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=NULL,
                                                   baseline.weekly.window=NULL,
                                                   continuous.window=5000,
                                                   lambda=0.2,
                                                   limit.sd=c(2.5,3,3.5),
                                                   correct.baseline.UCL.ewma=FALSE,
                                                   correct.baseline.LCL.ewma=FALSE,
                                                   UCL.ewma=2,
                                                   LCL.ewma=2,
                                                   indicator.type="C")
    
    ## Applying Shewhart
    
    add.df.indicator.evaluate.system <- shew_apply(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=NULL,
                                                   baseline.weekly.window=NULL,
                                                   continuous.window=5000,
                                                   limit.sd=c(2.5,3,3.5),
                                                   correct.baseline.UCL.shew=FALSE,
                                                   correct.baseline.LCL.shew=FALSE,
                                                   UCL.shew=2,
                                                   LCL.shew=2,
                                                   indicator.type="C")
    
    
    #delete the row which is week 53
    add.df.indicator.evaluate.system <-
      add.df.indicator.evaluate.system[-c(which(df.indicator.evaluate.system$week==53)),]
    
    ## delete the row which is week 53 from week.quarter and quarter
    week.quarter <- week.quarter[-c(which(df.indicator.evaluate.system$week==53))]
    quarter <- quarter[-c(which(df.indicator.evaluate.system$week==53))]
    
    
    ## construct final table
    
    table.evaluate.system <- 
      add_column(add.df.indicator.evaluate.system, week.quarter, .after = 3)
    
    table.evaluate.system <- 
      add_column(table.evaluate.system, quarter, .after = 4)
    
    
    colnames(table.evaluate.system) <- c("date", "week", "year", "week quarter", "quarter",
                                         "sowINDEX", "parity", "observed", "baseline", 
                                         "UCL EWMA", "LCL EWMA", "alarms EWMA", 
                                         "UCL Shewhart", "LCL Shewhart", "alarms Shewhart")
    
  }   
  return(table.evaluate.system)
}


## Cluster all in one list

outbreaks.piglets.weaned.litter.results <- list()

for (quarter in quarters.list){
  
  outbreaks.piglets.weaned.litter.results[[quarter]] <- 
    add.outbreaks.piglets.weaned.litter(indicator=indicators.data$total.wean.litter,
                                        quarters.list=quarter)
}

#View(outbreaks.piglets.weaned.litter.results[["2018.2"]])



# expected-weaned weaned per week  ----
## (deaths in weaning piglets per week)

add.outbreaks.weaning.deaths <- function(indicator=indicators.data$negdiff.wean.week,
                                         quarters.list=quarters.list
                                         
)
{
  range_weekly <- max(1,(dim(index.dates.week)[1]-weekly.window+1)):dim(index.dates.week)[1]
  
  df.indicator.evaluate.system <- weekly.indicators(indicator=indicators.data$negdiff.wean.week,
                                                    range.weekly=1:dim(index.dates.week)[1])
  
  data = df.indicator.evaluate.system[,"observed"]
  
  week.quarter <- index.dates.week$week-(floor((index.dates.week$week-1)/13)*13)
  
  quarter <- c(rep(NA, dim(df.indicator.evaluate.system)[1]))
  
  quarter <- ifelse(index.dates.week$week<=13, 
                    paste(index.dates.week$ISOweekYear,1, sep = "."), quarter)
  
  quarter <- ifelse(index.dates.week$week>13 & 
                      index.dates.week$week<=26, 
                    paste(index.dates.week$ISOweekYear,2, sep = "."), quarter)
  
  quarter <- ifelse(index.dates.week$week>26 & 
                      index.dates.week$week<=39, 
                    paste(index.dates.week$ISOweekYear,3, sep = "."), quarter)
  
  quarter <- ifelse(index.dates.week$week>39, 
                    paste(index.dates.week$ISOweekYear,4, sep = "."), quarter)
  
  
  for ( q in quarters.list){ #q="2018.4"
    
    start = first(which(quarter==q))
    end = last(which(quarter==q))
    
    y <- data[(start):(end)]
    
    ## in t2 increased 26% and the max increase was in t3 of 47%, then decreased until t12 (during 9 weeks (12-3))
    
    lgn.weaning.deaths <- 0.47* plnorm(c(1,(100/2),(100/2*2),
                                         100-(100/9),100-(100/9*2),100-(100/9*3),100-(100/9*4),100-(100/9*5),
                                         100-(100/9*6),100-(100/9*7),100-(100/9*8),100-(100/9*9),100-(100/9*9)),
                                       meanlog=4, sdlog=0.3, lower.tail=TRUE, log.p=FALSE)
    
    
    #plot(lgn.weaning.deaths, type="l") 
    
    simulated.outbreak <- c(y[1], ceiling(y[c(2:13)]*lgn.weaning.deaths[c(2:13)])+y[c(2:13)])
    
    #plot(simulated.outbreak,type="l")
    
    add.negdiff.wean.week.df.observed <-
      replace(data, which(quarter==q), simulated.outbreak)
    
    add.df.indicator.evaluate.system <- data.frame(df.indicator.evaluate.system$gilt,
                                                   df.indicator.evaluate.system$young,
                                                   df.indicator.evaluate.system$prime,
                                                   df.indicator.evaluate.system$mature,
                                                   add.negdiff.wean.week.df.observed,
                                                   df.indicator.evaluate.system$baseline,
                                                   df.indicator.evaluate.system$`UCL EWMA`,
                                                   df.indicator.evaluate.system$`LCL EWMA`,
                                                   df.indicator.evaluate.system$`alarms EWMA`,
                                                   df.indicator.evaluate.system$`UCL Shewhart`,
                                                   df.indicator.evaluate.system$`LCL Shewhart`,
                                                   df.indicator.evaluate.system$`alarms Shewhart`)
    
    colnames(add.df.indicator.evaluate.system) <- c("gilt", "young", "prime", "mature",
                                                    "observed", "baseline", "UCL EWMA", "LCL EWMA",
                                                    "alarms EWMA", "UCL Shewhart", 
                                                    "LCL Shewhart", "alarms Shewhart")
    
    
    ## Clean Baseline
    
    add.df.indicator.evaluate.system <- clean_baseline_perc(df.indicator=add.df.indicator.evaluate.system,
                                                            limit.upp=0.95,
                                                            limit.lw=NULL,
                                                            run.window.weekly=104,
                                                            median.days.production.cycles=NULL,
                                                            nr.production.cycles=2,
                                                            range=range_weekly,
                                                            indicator.type="W")
    ## Applying EWMA
    
    add.df.indicator.evaluate.system <- apply_ewma(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=106,
                                                   baseline.weekly.window=52,
                                                   lambda=0.2,
                                                   limit.sd=c(2.5,3,3.5),
                                                   guard.band.weekly=2,
                                                   correct.baseline.UCL.ewma=TRUE,
                                                   correct.baseline.LCL.ewma=TRUE,
                                                   UCL.ewma=2,
                                                   LCL.ewma=2,
                                                   indicator.type="W")
    
    ## Applying Shewhart
    
    add.df.indicator.evaluate.system <- shew_apply(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=106,
                                                   baseline.weekly.window=52,
                                                   limit.sd=c(2.5,3,3.5),
                                                   guard.band.weekly=2,
                                                   correct.baseline.UCL.shew=FALSE,
                                                   correct.baseline.LCL.shew=FALSE,
                                                   UCL.shew=2,
                                                   LCL.shew=2,
                                                   indicator.type="W")
    
    #delete the row which is week 53
    add.df.indicator.evaluate.system <-
      add.df.indicator.evaluate.system[-c(which(index.dates.week$week==53)),]
    
    ## construct final table
    
    dates <- data.frame(index.dates.week$start[-(which(index.dates.week$week==53))],
                        index.dates.week$week[-(which(index.dates.week$week==53))],
                        index.dates.week$ISOweekYear[-(which(index.dates.week$week==53))],
                        week.quarter[-(which(index.dates.week$week==53))],
                        quarter[-(which(index.dates.week$week==53))])
    
    table.evaluate.system <- data.frame(dates, add.df.indicator.evaluate.system)
    
    
    colnames(table.evaluate.system) <- c("date", "week", "year", "week quarter", "quarter",
                                         "gilt", "young", "prime", "mature",
                                         "observed", "baseline", "UCL EWMA", "LCL EWMA",
                                         "alarms EWMA", "UCL Shewhart", 
                                         "LCL Shewhart", "alarms Shewhart")
  }
  
  return(table.evaluate.system)
}
  

## Cluster all in one list

outbreaks.weaning.deaths.results <- list()

for (quarter in quarters.list){
  
  outbreaks.weaning.deaths.results[[quarter]] <- 
    add.outbreaks.weaning.deaths(indicator=indicators.data$negdiff.wean.week,
                                 quarters.list=quarter)
}

#View(outbreaks.weaning.deaths.results[["2018.3"]])



# sow deaths per week ----

add.outbreaks.mortality.sows <- function(indicator=indicators.data$number.deaths.week,
                                         quarters.list=quarters.list
                                         
)
{
  range_weekly <- max(1,(dim(index.dates.week)[1]-weekly.window+1)):dim(index.dates.week)[1]
  
  df.indicator.evaluate.system <- weekly.indicators(indicator=indicators.data$number.deaths.week,
                                                    range.weekly=1:dim(index.dates.week)[1])
  
  data = df.indicator.evaluate.system[,"observed"]
  
  week.quarter <- index.dates.week$week-(floor((index.dates.week$week-1)/13)*13)
  
  quarter <- c(rep(NA, dim(df.indicator.evaluate.system)[1]))
  
  quarter <- ifelse(index.dates.week$week<=13, 
                    paste(index.dates.week$ISOweekYear,1, sep = "."), quarter)
  
  quarter <- ifelse(index.dates.week$week>13 & 
                      index.dates.week$week<=26, 
                    paste(index.dates.week$ISOweekYear,2, sep = "."), quarter)
  
  quarter <- ifelse(index.dates.week$week>26 & 
                      index.dates.week$week<=39, 
                    paste(index.dates.week$ISOweekYear,3, sep = "."), quarter)
  
  quarter <- ifelse(index.dates.week$week>39, 
                    paste(index.dates.week$ISOweekYear,4, sep = "."), quarter)
  
  
  for ( q in quarters.list){ #q="2018.4"
    
    start = first(which(quarter==q))
    end = last(which(quarter==q))
    
    y <- data[(start):(end)]
    
    
    ## increased 10% in week t3 and then decreased until t7 (during 4 weeks (7-3))
    
    lgn.mortality.sows <- 0.1* (plnorm(c(100/3,(100/3*2),(100/3*3),
                                         100-(100/4),100-(100/4*2),100-(100/4*3),100-(100/4*4),100-(100/4*4),
                                         100-(100/4*4),100-(100/4*4),100-(100/4*4),100-(100/4*4),100-(100/4*4)),
                                       meanlog=4, sdlog=0.3, lower.tail=TRUE, log.p=FALSE))
    
    #plot(lgn.mortality.sows, type="l") 
    
    baseline.total <- sum(y)/length(y)  # Additive
    simulated.outbreak <- ceiling(lgn.mortality.sows*baseline.total)+y
    #plot(simulated.outbreak,type="l")
    
    
    add.mortality.sows.observed <-
      replace(data, which(quarter==q), simulated.outbreak)
    
    
    add.df.indicator.evaluate.system <- data.frame(df.indicator.evaluate.system$gilt,
                                                   df.indicator.evaluate.system$young,
                                                   df.indicator.evaluate.system$prime,
                                                   df.indicator.evaluate.system$mature,
                                                   add.mortality.sows.observed,
                                                   df.indicator.evaluate.system$baseline,
                                                   df.indicator.evaluate.system$`UCL EWMA`,
                                                   df.indicator.evaluate.system$`LCL EWMA`,
                                                   df.indicator.evaluate.system$`alarms EWMA`,
                                                   df.indicator.evaluate.system$`UCL Shewhart`,
                                                   df.indicator.evaluate.system$`LCL Shewhart`,
                                                   df.indicator.evaluate.system$`alarms Shewhart`)
    
    colnames(add.df.indicator.evaluate.system) <- c("gilt", "young", "prime", "mature",
                                                    "observed", "baseline", "UCL EWMA", "LCL EWMA",
                                                    "alarms EWMA", "UCL Shewhart", 
                                                    "LCL Shewhart", "alarms Shewhart")
    
    
    ## Clean Baseline
    
    add.df.indicator.evaluate.system <- clean_baseline_perc(df.indicator=add.df.indicator.evaluate.system,
                                                            limit.upp=0.95,
                                                            limit.lw=NULL,
                                                            run.window.weekly=104,
                                                            median.days.production.cycles=NULL,
                                                            nr.production.cycles=2,
                                                            range=range_weekly,
                                                            indicator.type="W")
    ## Applying EWMA
    
    add.df.indicator.evaluate.system <- apply_ewma(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=106,
                                                   baseline.weekly.window=52,
                                                   lambda=0.2,
                                                   limit.sd=c(2.5,3,3.5),
                                                   guard.band.weekly=2,
                                                   correct.baseline.UCL.ewma=TRUE,
                                                   correct.baseline.LCL.ewma=TRUE,
                                                   UCL.ewma=2,
                                                   LCL.ewma=2,
                                                   indicator.type="W")
    
    ## Applying Shewhart
    
    add.df.indicator.evaluate.system <- shew_apply(df.indicator=add.df.indicator.evaluate.system,
                                                   evaluate.weekly.window=106,
                                                   baseline.weekly.window=52,
                                                   limit.sd=c(2.5,3,3.5),
                                                   guard.band.weekly=2,
                                                   correct.baseline.UCL.shew=FALSE,
                                                   correct.baseline.LCL.shew=FALSE,
                                                   UCL.shew=2,
                                                   LCL.shew=2,
                                                   indicator.type="W")
    
    #delete the row which is week 53
    add.df.indicator.evaluate.system <-
      add.df.indicator.evaluate.system[-c(which(index.dates.week$week==53)),]
    
    ## construct final table
    
    dates <- data.frame(index.dates.week$start[-(which(index.dates.week$week==53))],
                        index.dates.week$week[-(which(index.dates.week$week==53))],
                        index.dates.week$ISOweekYear[-(which(index.dates.week$week==53))],
                        week.quarter[-(which(index.dates.week$week==53))],
                        quarter[-(which(index.dates.week$week==53))])
    
    table.evaluate.system <- data.frame(dates, add.df.indicator.evaluate.system)
    
    
    colnames(table.evaluate.system) <- c("date", "week", "year", "week quarter", "quarter",
                                         "gilt", "young", "prime", "mature",
                                         "observed", "baseline", "UCL EWMA", "LCL EWMA",
                                         "alarms EWMA", "UCL Shewhart", 
                                         "LCL Shewhart", "alarms Shewhart")
  }
  
  return(table.evaluate.system)
}


## Cluster all in one list

outbreaks.mortality.sows.results <- list()

for (quarter in quarters.list){
  
  outbreaks.mortality.sows.results[[quarter]] <- add.outbreaks.mortality.sows(indicator=indicators.data$number.deaths.week,
                                                                              quarters.list=quarter)
}

#View(outbreaks.mortality.sows.results[["2017.4"]])


# join all results of the same quarter

outbreaks.results <- list()

for (quarter in quarters.list){

  outbreaks.results[[quarter]] <- list(outbreaks.reservices.results[[quarter]],
                                       outbreaks.pregnancy.length.results[[quarter]],
                                       outbreaks.abortions.results[[quarter]],
                                       outbreaks.live.piglets.results[[quarter]],
                                       outbreaks.perc.dead.piglets.results[[quarter]],
                                       outbreaks.mummi.piglets.results[[quarter]],
                                       outbreaks.piglets.weaned.week.results[[quarter]],
                                       outbreaks.piglets.weaned.litter.results[[quarter]],
                                       outbreaks.weaning.deaths.results[[quarter]],
                                       outbreaks.mortality.sows.results[[quarter]])

  names(outbreaks.results[[quarter]]) <- c("reservices.week","pregnancy.length", "abortions.week",
                                           "live.born.litter", "perc.dead.born.litter",
                                           "mummi.born.litter", "total.wean.week", "total.wean.litter",
                                           "negdiff.wean.week", "number.deaths.week")

}

#View(outbreaks.results[["2017.2"]])



#join the indicators without outbreaks with the ones with outbreaks
##quarter chosen: 2018.4

replace.indicators <- c("Reservices per week", "pregnancy length","abortions per week",
                        "live born per farrowing", "% dead born per farrowing", "mummified per farrowing",
                        "piglets weaned per week", "piglets weaned per weaning",
                        "expected-weaned weaned per week","sow deaths per week")

indicators.time.series[replace.indicators] <- outbreaks.results[["2018.4"]]
