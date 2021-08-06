
# indicators construction ----

parity = 1:15

reservice.threshold <- 90
reservice.perc.window <- 4     #(weeks)

empty.days.target <- 4
opendays.target <- 4

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

# events ----
sow.events <- c("birth","service","pregnancyTest","abortion","farrowing","weaning","exit","death")
sow.events.before.exit <- c("birth","service","reservice","abortion","farrowing","weaning")

sow.info <- c("parity","status")
events.info <- c("NrBornAlive","NrBornDead","NrSmallStillBorn","NrWeakBorn","NrMummified",
                 "NrMoved","NrWeaned","WeanedTotalWeight","ExitReason", "ExitType")


#codes from Winpig ----
exit.type <- data.frame(Slaughtered=1,
                        Sold=2,
                        Euthanized=3,
                        Dead=4,
                        Sold.Pregnant=5,
                        Missing=6,
                        Exported=7)

animal.type <- data.frame(Farm.animal=0,
                              Parent=1,
                              Insemination.donor=2,
                              Gylts.produced=3)

# coding events ----
service.coding <- data.frame(code=c(1,2),
                             meaning=c("service","reservice"))
pregnancy.coding <- data.frame(code=c(0,2), #2 because that's how it was on WinPig
                               meaning=c("not pregnant","pregnant")) #NA will be the unknown
status.coding   <- data.frame(code=c(0,1,2,3,4,5),
                              meaning=c("gilt","empty","assumedPregnant","Pregnant","nursing","exited"))








