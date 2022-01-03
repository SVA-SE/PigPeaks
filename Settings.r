packages <- c("utils", "RColorBrewer")
install.packages(setdiff(packages, rownames(installed.packages())))

require(utils)
require(RColorBrewer)

source("Definitions.r")

# database connection settings ----
server="XXXX" #private information referring to the local computer, see documentation
database="XXXXX" #name of the WinPig database on your computer, see documentation
farm.name="farm01" #name will be used to save base files

language=19 #19 is swedish, see documentation for other languages


# desired indicators from EXCEL ----

indicators.excel <- read.table(file="Table_Indicators_CSV.csv", header=TRUE, sep=";")
indicators.to.keep <- indicators.all %in% indicators.excel$indicators[indicators.excel$indicators.to.keep==TRUE]
        #indicators.to.keep <- rep(TRUE,length(indicators.all))
indicators.to.keep.numerical <- which(indicators.to.keep==T)

indicators.labels <- indicators.excel$indicators.labels[indicators.excel$indicators.to.keep==TRUE]
#assumes that a user will NOT change the order of the indicators in the Excel, but
#CAN modify the labels in the column labels if desired


indicators.type   <- indicators.all.type[indicators.to.keep.numerical]
indicators.sys    <- indicators.all.sys[indicators.to.keep.numerical]
indicators.limits <- indicators.all.limits[indicators.to.keep.numerical]
indicators.categories <- indicators.all.categories[indicators.to.keep.numerical]

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

weekly.window <- 271               #weeks to see in each indicator
                                        #if you are using the data example provided put: weekly.window <- 160
continuous.window <- 5500          #observations to see in each indicator
                                        #if you are using the data example provided put: continuous.window <- 5000

limit.upp <- 0.95                  #upper limit (upper percentile) to clean baseline non-parametric
limit.lw <- 0.05                   #lower limit (lower percentile) to clean baseline non-parametric

run.window.weekly <- 104           #window of time points which will be used to calculate the percentile set (weekly indicators)
nr.production.cycles <- 2          #number of production cycles to consider to construct a window of time points which will be used to calculate the percentile set (continuous indicators). Set to NULL, if median.days.production.cycles is a value (not NULL).
median.days.production.cycles=NULL #if left as NULL, it will be calculated based on the median value in days of 1 production cycle, using the indicator days.between.farrowings, and multiplied by the nr.production.cycles chosen. Therefore, if set to NULL, the indicator days.between.farrowings has to belong to indicators.to.keep.
                                   #if you want to set a fixed median value in days, set for instance: (note that the value chosen will not be multiplied by nr.production.cycles) 
                                   #median.days.production.cycles <- 300

evaluate.weekly.window=12      #number of time points to be evaluated by the algorithm for weekly indicators
                                        #if you are using the data example provided put: evaluate.weekly.window <- 106

baseline.weekly.window=104      #baseline used to train the algorithm in order to provide a forecast for weekly indicators
                                        #if you are using the data example provided put: baseline.weekly.window <- 52

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
weeks.to.show <- 78             #weeks to show on the weekly graphs
nonTS.to.show <- 100            #events to show on the continuous graphs
                                        #if you are using the data example provided to inject outbreaks (in script: "system evaluation), put: 
                                        #nonTS.to.show <- 300
#days.ago.nonTS <- 114
group.window <- 114
years.to.see <- 3               #number of events and alarms seen in the last x years to see 
moving_average <- 8             #moving average in weeks to represent parity in continuous graphs

target = NULL                   #the target value for each indicator
target.unit = "vector"          #c("value","vector"), defaults to vector
series.label="sows"             #time-series label, defaults to sows


# statistical settings ----
baseline.years=2


# parity colouring ----
#parity.group <- data.frame(parity = c1, group=c2, group.name = c3)
parity.group <- data.frame(parity = c1, group.name = ordered(c3,levels=c("gilt","young","prime","mature")))


qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_parity = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

parity.group$color1 <- col_parity[as.numeric(as.factor(parity.group$group.name))]
parity.group$color2 <- col_parity[as.numeric(as.factor(parity.group$parity))]

#parity.group2 <- parity.group[-1,]
parity.group2 <- parity.group

# colors.custom<- c(rep("#4287f5",1),
#                   rep("#28ab1f",2),
#                   rep("#f5942c",3),
#                   rep("#a15a4c",9)
# )

colors.custom<- c(rep("#4287f5",2),
                  rep("#28ab1f",2),
                  rep("#f5942c",3),
                  rep("#a15a4c",9)
)

parity.group2 <-cbind(parity.group2,colors.custom)
parity.group2$colors.custom<-colors.custom

color.pg <- c("#4287f5","#28ab1f","#f5942c","#a15a4c")


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