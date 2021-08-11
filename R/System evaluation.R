# inject outbreaks in quarters

week.quarter <- week-(floor((week-1)/13)*13)
quarter <- c(rep(NA, length(range)))
quarter <- ifelse(week<=13, paste(year,1, sep = "."), quarter)
quarter <- ifelse(week>13 & week<=26, paste(year,2, sep = "."), quarter)
quarter <- ifelse(week>26 & week<=39, paste(year,3, sep = "."), quarter)
quarter <- ifelse(week>39, paste(year,4, sep = "."), quarter)

quarter = c("2015.2", "2015.3", "2015.4", "2016.1", "2016.2", "2016.3", "2016.4",   
            "2017.1", "2017.2", "2017.3", "2017.4", "2018.1")                       


# evaluation: PRRS outbreaks injection ----
##NOT WORKING YET

evaluate_system <- function(list.indicators=c(weekly.indicators(weekly.window = weekly.window),
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