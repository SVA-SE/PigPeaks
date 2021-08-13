source("Definitions.r")


# database connection settings ----
server="XXXX" #private information referring to the local computer, see documentation
database="XXXXX" #name of the WinPig database on your computer, see documentation
farm.name="farm01" #name will be used to save base files

language=19 #19 is swedish, see documentation for other languages


# desired indicators ----- (see list of all at the end of the script)
indicators.to.keep.labels <- c("Services per week","Sows empty longer than target, weekly",
                       "Reservices per week","Time to reservice",
                       "% failure, weekly","% reservice, weekly",
                       "time to first service","time to first farrowing",
                       "abortions per week","time to abortion",
                       "pregnancy length","days between farrowings",
                       "farrowings per week",
                       "piglets per farrowing","live born per farrowing","dead born per farrowing","% dead born per farrowing",
                       "mummified per farrowing",
                       "weanings per week","piglets weaned per week","expected-weaned weaned per week",
                       "days to weaning",
                       "piglets weaned per weaning",
                       "average weight at weaning",
                       "sow deaths per week","gilt deaths per week","weaned piglet deaths per week",
                       "death after event, weekly",
                       "number of exits per week","exit type, weekly","exit after event, weekly")


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


# plotting and dashboard -----

plot.years=2
weeks.to.show <- 78
nonTS.to.show <- 100
#days.ago.nonTS <- 114
group.window <- 114



# statistical settings ----
baseline.years=3



# ALL INDICATORS POSSIBLE -----
# indicators.labels <- c("Services per week","Sows empty longer than target, weekly",
#                        "Reservices per week","Time to reservice","Sows re-serviced twice",
#                        "% pregnancy, weekly","% failure, weekly","% reservice, weekly",
#                        "time to first service","time to first farrowing",
#                        "abortions per week","time to abortion",
#                        "services to farrow","pregnancy length","days between farrowings",
#                        "farrowings per week",
#                        "total born per week","live born per week","dead born per week",
#                        "small born per week","weak born per week","mummified born per week",
#                        "% born dead, weekly","% small born, weekly","% weak born, weekly","% mummified, weekly",
#                        "piglets per farrowing","live born per farrowing","dead born per farrowing","% dead born per farrowing",
#                        "small born per farrowing","% small born per farrowing","weak born per farrowing","% weak born per farrowing",
#                        "mummified per farrowing","% mummified per farrowing",
#                        "weanings per week","piglets weaned per week","weaned-expected weaned per week","expected-weaned weaned per week",
#                        "days to weaning",
#                        "piglets weaned per weaning","weaned-expected weaned per weaning","% weaned-expected weaned per weaning",
#                        "average weight at weaning",
#                        "sow deaths per week","gilt deaths per week","weaned piglet deaths per week",
#                        "death after event, weekly",
#                        "number of exits per week","exit reason, weekly","exit type, weekly","exit after event, weekly")