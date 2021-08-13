# inject outbreaks in quarters

quarters.list = c("2015.2", "2015.3", "2015.4", "2016.1", "2016.2", "2016.3", "2016.4",   
                 "2017.1", "2017.2", "2017.3", "2017.4", "2018.1")                       


# evaluation: PRRS outbreaks injection ----
##NOT WORKING YET

evaluate_system <- function(indicator=indicator,         #indicator=reservices.week
                            df.indicator=df.indicator,   #df.indicator=df.reservices.week
                            quarters.list=quarters.list
                            
)
{
    df.indicator.evaluate.system <- weekly.indicators(indicator=indicator)
  
    data = df.indicator.evaluate.system[,"observed"]
    
    week.quarter <- df.indicator.evaluate.system[,"week"]-(floor((df.indicator.evaluate.system[,"week"]-1)/13)*13)
    
    quarter <- c(rep(NA, dim(df.indicator.evaluate.system)[1]))
    
    quarter <- ifelse(df.indicator.evaluate.system[,"week"]<=13, 
                      paste(df.indicator.evaluate.system[,"year"],1, sep = "."), quarter)
    
    quarter <- ifelse(df.indicator.evaluate.system[,"week"]>13 & 
                        df.indicator.evaluate.system[,"week"]<=26, 
                      paste(df.indicator.evaluate.system[,"year"],2, sep = "."), quarter)
    
    quarter <- ifelse(df.indicator.evaluate.system[,"week"]>26 & 
                        df.indicator.evaluate.system[,"week"]<=39, 
                      paste(df.indicator.evaluate.system[,"year"],3, sep = "."), quarter)
    
    quarter <- ifelse(df.indicator.evaluate.system[,"week"]>39, 
                      paste(df.indicator.evaluate.system[,"year"],4, sep = "."), quarter)
    
    table.evaluate.system <- data.frame(df.indicator.evaluate.system[,"date"], df.indicator.evaluate.system[,"week"],
                                        df.indicator.evaluate.system[,"year"], week.quarter, quarter, 
                                        data, df.indicator.evaluate.system[,"baseline"], 
                                        df.indicator.evaluate.system[,"UCL"], df.indicator.evaluate.system[,"LCL"], 
                                        df.indicator.evaluate.system[,"alarms EWMA"], 
                                        df.indicator.evaluate.system[,"alarms Shewhart"])
    
    colnames(table.evaluate.system) <- c("date", "week", "year", "week quarter", "quarter",
                                         "observed", "baseline", "UCL", "LCL",
                                         "alarms EWMA", "alarms Shewhart")
    
      
      if(df.indicator==df.reservices.week){
        
        for ( q in quarters.list){ #q="2015.3"

        start = first(which(table.evaluate.system[, "quarter"] == q))
        end = last(which(table.evaluate.system[, "quarter"] == q))

        y <- data[(start):(end)]

        ## increased in week t3 with max value in t8, then decreased until t28 (36-8)
        ## lets consider that reservices tripled (*2) in week t8

        lgn.reservices <- 2* plnorm(c(1,1,(100/6),(100/6*2),(100/6*3),(100/6*4),(100/6*5),(100/6*6),
                                      100-(100/28),100-(100/28*2),100-(100/28*3),100-(100/28*4),100-(100/28*5)),
                                    meanlog=4, sdlog=0.3, lower.tail=TRUE, log.p=FALSE)

        #plot(lgn.reservices, type="l")

        baseline.total <- sum(y)/length(y)  # Additive
        simulated.outbreak <- c(y[c(1,2)], ceiling(lgn.reservices[c(3:13)]*baseline.total)+y[c(3:13)])

        table.evaluate.system[,"observed"] <-
          replace(table.evaluate.system[,"observed"], which(table.evaluate.system[, "quarter"] == q), simulated.outbreak)


        ## Clean Baseline

        table.evaluate.system <- clean_baseline_perc(df.indicator=table.evaluate.system,
                                                     limit.upp=0.95,
                                                     limit.lw=NULL,
                                                     run.window.weekly=104,
                                                     run.window.continuous=NULL)
        ## Applying EWMA

        table.evaluate.system <- apply_ewma(df.indicator=table.evaluate.system,
                                            evaluate.weekly.window=165,
                                            baseline.weekly.window=104,
                                            lambda=0.2,
                                            limit.sd=c(2.5,3,3.5),
                                            guard.band.weekly=2,
                                            correct.baseline.UCL=TRUE,
                                            correct.baseline.LCL=TRUE,
                                            UCL=2,
                                            LCL=2)

        ## Applying Shewhart

        table.evaluate.system <- shew_apply(df.indicator=table.evaluate.system,
                                            evaluate.weekly.window=165,
                                            baseline.weekly.window=104,
                                            limit.sd=c(2.5,3,3.5),
                                            guard.band.weekly=2,
                                            correct.baseline.UCL=FALSE,
                                            correct.baseline.LCL=FALSE,
                                            UCL=FALSE,
                                            LCL=FALSE)

        }
      }


    if(df.indicator==df.number.deaths.week){
      
      for ( q in quarters.list){ #q="2015.2"

      start = first(which(table.evaluate.system[, "quarter"] == q))
      end = last(which(table.evaluate.system[, "quarter"] == q))

      y <- data[(start):(end)]

      ## increased 10% in week t3 and then decreased until t7 (during 4 weeks (7-3))

      lgn.mortality.sows <- 0.1* (plnorm(c(100/3,(100/3*2),(100/3*3),
                                           100-(100/4),100-(100/4*2),100-(100/4*3),100-(100/4*4),100-(100/4*4),
                                           100-(100/4*4),100-(100/4*4),100-(100/4*4),100-(100/4*4),100-(100/4*4)),
                                         meanlog=4, sdlog=0.3, lower.tail=TRUE, log.p=FALSE))

      #plot(lgn.mortality.sows, type="l")

      baseline.total <- sum(y)/length(y)  # Additive
      simulated.outbreak <- ceiling(lgn.mortality.sows*baseline.total)+y

      table.evaluate.system[,"observed"] <-
        data.frame(replace(table.evaluate.system[,"observed"], which(table.evaluate.system[, "quarter"] == q), simulated.outbreak))


    ## Clean Baseline

      table.evaluate.system <- clean_baseline_perc(df.indicator=table.evaluate.system,
                                                   limit.upp=0.95,
                                                   limit.lw=NULL,
                                                   run.window.weekly=104,
                                                   run.window.continuous=NULL)
    ## Applying EWMA

      table.evaluate.system <- apply_ewma(df.indicator=table.evaluate.system,
                                          evaluate.weekly.window=165,
                                          baseline.weekly.window=104,
                                          lambda=0.2,
                                          limit.sd=c(2.5,3,3.5),
                                          guard.band.weekly=2,
                                          correct.baseline.UCL=TRUE,
                                          correct.baseline.LCL=TRUE,
                                          UCL=2,
                                          LCL=2)

    ## Applying Shewhart

      table.evaluate.system <- shew_apply(df.indicator=table.evaluate.system,
                                          evaluate.weekly.window=165,
                                          baseline.weekly.window=104,
                                          limit.sd=c(2.5,3,3.5),
                                          guard.band.weekly=2,
                                          correct.baseline.UCL=FALSE,
                                          correct.baseline.LCL=FALSE,
                                          UCL=FALSE,
                                          LCL=FALSE)

  }
  }
  return(table.evaluate.system)
}

# Services ----

## Reservices per week

### Cluster all 12 copies in one list

evaluate.system.results.reservices <- list()

for (quarter in quarters.list){
  
  evaluate.system.results.reservices[[quarter]] <- evaluate_system(indicator=reservices.week,
                                                                   df.indicator=df.reservices.week,
                                                                   quarters.list=c("2015.2", "2015.3", "2015.4", 
                                                                                  "2016.1", "2016.2", "2016.3", "2016.4",   
                                                                                  "2017.1", "2017.2", "2017.3", "2017.4", "2018.1"))
}

View(evaluate.system.results.reservices[["2015.3"]])




# Exit ----

## Dead sows per week

### Cluster all 12 copies in one list

evaluate.system.results.number.deaths.week <- list()

for (quarter in quarters.list){
  
evaluate.system.results.number.deaths.week[[quarter]] <- evaluate_system(indicator=number.deaths.week,
                                                                         df.indicator=df.number.deaths.week,
                                                                         quarters.list=c("2015.2", "2015.3", "2015.4", 
                                                                                        "2016.1", "2016.2", "2016.3", "2016.4",
                                                                                        "2017.1", "2017.2", "2017.3", "2017.4", "2018.1"))      
}

View(evaluate.system.results.number.deaths.week[["2015.2"]])
