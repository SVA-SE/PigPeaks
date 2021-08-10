# database connection settings ----
server="XXXX" #private information referring to the local computer, see documentation
database="XXXXX" #name of the WinPig database on your computer, see documentation
farm.name="farm01" #name will be used to save base files

language=19 #19 is swedish, see documentation for other languages


# grouping parity ----
c1 <- 0:15
c2 <- c(rep("gilt",2),
        rep("2",1),
        rep("3-5",3),
        rep(">5",10)
)
c3 <- c(rep("gilt",2),
        rep("young",1),
        rep("prime",3),
        rep("mature",10)
)

# functions arguments ----

weekly.window <- 271           #weeks to see in each indicator
continuous.window <- 5500      #observations to see in each indicator

limit.upp <- 0.95              #upper limit (upper percentile) to clean baseline non-parametric
limit.lw <- 0.05               #lower limit (lower percentile) to clean baseline non-parametric

run.window.weekly <- 104       #window of time points which will be used to calculate the percentile set (weekly indicators)
run.window.continuous <- 1300  #window of time points which will be used to calculate the percentile set (continuous indicators)

evaluate.weekly.window=165     #number of time points to be evaluated by the algorithm for weekly indicators
baseline.weekly.window=104     #baseline used to train the algorithm in order to provide a forecast for weekly indicators
guard.band.weekly=2            #number of time units used to separate the current time unit evaluated and the baseline window for weekly indicators

lambda=0.2                     #EWMA parameter (lambda) 
limit.sd=c(2.5,3,3.5)          #3 limits (standard deviations)

correct.baseline.UCL=TRUE      #the algorithm can also be used to correct the data, correct for the UCL
correct.baseline.LCL=TRUE      #the algorithm can also be used to correct the data, correct for the LCL
UCL=2                          #the minimum number that would have generated an alarm, for every time point, can be recorded in the indicator's column UCL.The user must provide the INDEX in the limit.sd vector for which the UCL values should be corrected 
LCL=2                          #the maximum number that would have generated an alarm, for every time point, can be recorded in the indicator's column LCL.The user must provide the INDEX in the limit.sd vector for which the LCL values should be corrected 

quarter = c("2015.2", "2015.3", "2015.4", "2016.1", "2016.2", "2016.3", "2016.4",   #quarters inject outbreaks
            "2017.1", "2017.2", "2017.3", "2017.4", "2018.1")                       #to evaluate the system


# plotting and dashboard -----

plot.years=2
weeks.to.show <- 78
nonTS.to.show <- 100
#days.ago.nonTS <- 114
group.window <- 114



# statistical settings ----
baseline.years=3

