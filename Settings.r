packages <- c("utils")
install.packages(setdiff(packages, rownames(installed.packages()))) 

require(utils)

source("Definitions.r")

# database connection settings ----
server="XXXX" #private information referring to the local computer, see documentation
database="XXXXX" #name of the WinPig database on your computer, see documentation
farm.name="farm01" #name will be used to save base files

language=19 #19 is swedish, see documentation for other languages

# desired indicators from EXCEL ---- 

indicators.excel <- read.table(file="Table_Indicators_CSV.csv", header=TRUE, sep=";")

##"days between farrowings" has to be chosen
indicators.to.keep.excel <- indicators.excel[indicators.excel$indicators.to.keep==TRUE,]

# desired indicators ----- (see list of all at the end of the script) #"days between farrowings" has to be chosen
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

weekly.window <- 271            #weeks to see in each indicator
continuous.window <- 5500       #observations to see in each indicator

limit.upp <- 0.95               #upper limit (upper percentile) to clean baseline non-parametric
limit.lw <- 0.05                #lower limit (lower percentile) to clean baseline non-parametric

run.window.weekly <- 104        #window of time points which will be used to calculate the percentile set (weekly indicators)
nr.production.cycles <- 2       #number of production cycles to consider to construct a window of time points which will be used to calculate the percentile set (continuous indicators)

evaluate.weekly.window=165      #number of time points to be evaluated by the algorithm for weekly indicators
baseline.weekly.window=104      #baseline used to train the algorithm in order to provide a forecast for weekly indicators
guard.band.weekly=2             #number of time units used to separate the current time unit evaluated and the baseline window for weekly indicators

lambda=0.2                      #EWMA parameter (lambda)
limit.sd=c(2.5,3,3.5)           #3 limits (standard deviations)

correct.baseline.UCL.ewma=TRUE  #the algorithm can also be used to correct the data, correct for the UCL with ewma algorithm
correct.baseline.LCL.ewma=TRUE  #the algorithm can also be used to correct the data, correct for the LCL with ewma algorithm
correct.baseline.UCL.shew=FALSE #the algorithm can also be used to correct the data, correct for the UCL with shew algorithm
correct.baseline.LCL.shew=FALSE #the algorithm can also be used to correct the data, correct for the LCL with shew algorithm
UCL.ewma=2                      #the minimum number that would have generated an alarm, for every time point, can be recorded in the indicator's column UCL ewma.The user must provide the INDEX in the limit.sd vector for which the UCL ewma values should be corrected
LCL.ewma=2                      #the maximum number that would have generated an alarm, for every time point, can be recorded in the indicator's column LCL ewma.The user must provide the INDEX in the limit.sd vector for which the LCL ewma values should be corrected
UCL.shew=2                      #the minimum number that would have generated an alarm, for every time point, can be recorded in the indicator's column UCL shew.The user must provide the INDEX in the limit.sd vector for which the UCL shew values should be corrected
LCL.shew=2                      #the maximum number that would have generated an alarm, for every time point, can be recorded in the indicator's column LCL shew.The user must provide the INDEX in the limit.sd vector for which the LCL shew values should be corrected

# plotting and dashboard -----

plot.years=2
weeks.to.show <- 78
nonTS.to.show <- 100
#days.ago.nonTS <- 114
group.window <- 114

indicator.label = "indicator"
target = NULL
target.unit = "vector"          #c("value","vector"), defaults to vector
UCL.EWMA = TRUE       
LCL.EWMA = TRUE       
UCL.SHEW = TRUE      
LCL.SHEW = TRUE       
alarms.EWMA.UPP = TRUE 
alarms.EWMA.LW = TRUE
alarms.SHEW.UPP = TRUE 
alarms.SHEW.LW = TRUE
series.label="sows"
vertical.line = NULL            #vector of dates
vertical.line.label= NULL


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