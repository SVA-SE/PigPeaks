
# indicators construction ----

#number of births of a sow
parity = 1:15

#number of days to consider a second insemination to be a "reservice" from the first
reservice.threshold <- 90
reservice.perc.window <- 4     #(weeks to calculate success of service)

empty.days.target <- 4         #(normal maximum days for sows being empty)
opendays.target <- 4

min.pregnancy.length <- 110    #(normal minimum pregnancy length)


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





#list of all possible indicatos:----

indicators.all <- c("services.week","empty.long.week",
                    "reservices.week","time.to.reservice","rereservices",
                    "perc.pregnancy","perc.failure","perc.reservice",
                    "time.to.first.service","time.to.first.farrowing",
                    "abortions.week","time.to.abortion",
                    "services.to.farrow","pregnancy.length","days.between.farrowings",
                    "farrowings.week",
                    "total.born.week","live.born.week","dead.born.week",
                    "small.born.week","weak.born.week","mummi.born.week",
                    "perc.dead.born.week","perc.small.born.week","perc.weak.born.week","perc.mummi.born.week",
                    "total.born.litter","live.born.litter","dead.born.litter","perc.dead.born.litter",
                    "small.born.litter","perc.small.born.litter","weak.born.litter","perc.weak.born.litter",
                    "mummi.born.litter","perc.mummi.born.litter",
                    "weanings.week","total.wean.week","diff.wean.week","negdiff.wean.week",
                    "weaning.length",
                    "total.wean.litter","negdiff.weaned.litter","perc.negdiff.weaned.litter",
                    "weight.wean.litter",
                    "number.deaths.week","gilts.deaths.week","piglets.deaths.week",
                    "death.after.event.week",
                    "number.exits.week","exit.type.week","exit.after.event.week")

indicators.all.labels <- c("Services per week","Sows empty longer than target, weekly",
                       "Reservices per week","Time to reservice","Sows re-serviced twice",
                       "% pregnancy, weekly","% failure, weekly","% reservice, weekly",
                       "time to first service","time to first farrowing",
                       "abortions per week","time to abortion",
                       "services to farrow","pregnancy length","days between farrowings",
                       "farrowings per week",
                       "total born per week","live born per week","dead born per week",
                       "small born per week","weak born per week","mummified born per week",
                       "% born dead, weekly","% small born, weekly","% weak born, weekly","% mummified, weekly",
                       "piglets per farrowing","live born per farrowing","dead born per farrowing","% dead born per farrowing",
                       "small born per farrowing","% small born per farrowing","weak born per farrowing","% weak born per farrowing",
                       "mummified per farrowing","% mummified per farrowing",
                       "weanings per week","piglets weaned per week","weaned-expected weaned per week","expected-weaned weaned per week",
                       "days to weaning",
                       "piglets weaned per weaning","weaned-expected weaned per weaning","% weaned-expected weaned per weaning",
                       "average weight at weaning",
                       "sow deaths per week","gilt deaths per week","weaned piglet deaths per week",
                       "death after event, weekly",
                       "number of exits per week","exit type, weekly","exit after event, weekly")

indicators.all.type <- c("W","W","W","C","C","W","W","W","C","C","W","C",
                         "C","C","C","W","W","W","W","W","W","W","W","W",
                         "W","W","C","C","C","C","C","C","C","C","C","C",
                         "W","W","W","W","C","C","C","C","C","W","W","W",
                         "W","W","W","W")

indicators.all.sys <- c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
                        TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,
                        FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,
                        FALSE, FALSE)

indicators.all.limits <- c("non-sys", "limit.upp", "limit.upp", "both", "limit.upp",
                           "limit.lw", "limit.upp", "limit.upp", "non-sys", "non-sys",
                           "limit.upp", "non-sys", "both", "both", "both",
                           "limit.lw", "limit.lw", "limit.lw", "limit.upp", "limit.upp",
                           "limit.upp", "limit.upp", "limit.upp", "limit.upp", "limit.upp",
                           "limit.upp", "both", "limit.lw", "limit.upp", "limit.upp",
                           "limit.upp", "limit.upp", "limit.upp", "limit.upp", "limit.upp",
                           "limit.upp", "non-sys", "limit.lw", "limit.lw", "limit.upp",
                           "non-sys", "limit.lw", "limit.lw", "limit.lw", "limit.lw",
                           "limit.upp", "limit.upp", "limit.upp", "non-sys", "both",
                           "non-sys", "non-sys")

indicators.all.categories <- c("services", "empty sows", "services", "services", "services",
                               "pregnancy", "services", "services", "gilts", "gilts",
                               "pregnancy", "pregnancy", "pregnancy", "pregnancy", "farrowing",
                               "farrowing", "farrowing", "farrowing", "farrowing", "farrowing",
                               "farrowing", "farrowing", "farrowing", "farrowing", "farrowing",
                               "farrowing", "farrowing", "farrowing", "farrowing", "farrowing",
                               "farrowing", "farrowing", "farrowing", "farrowing", "farrowing",
                               "farrowing", "weaning", "weaning", "weaning", "weaning",
                               "weaning", "weaning", "weaning", "weaning", "weaning",
                               "exit", "exit", "post-weaning", "exit", "exit",
                               "exit", "exit")


# services.week              #   NumberOfWeeks x   ParityFrom1
# reservices.week	           #	 NumberOfWeeks x   ParityFrom1
# perc.pregnancy	           #	 NumberOfWeeks x   ParityFrom1   x 2 (numerator and denominator)
# perc.failure		           #   NumberOfWeeks x   ParityFrom1   x 2 (numerator and denominator)
# perc.reservice	           #	 NumberOfWeeks x   ParityFrom1   x 2 (numerator and denominator)
# time.to.reservice          #	 EVENTS x 4 (indicator parity  date sowINDEX)
# rereservices		           #   EVENTS x 4 (indicator parity  date sowINDEX) 
# time.to.first.service      #	 EVENTS x 4 (indicator parity  date sowINDEX)
# time.to.first.farrowing    #	 EVENTS x 4 (indicator parity  date sowINDEX)
# abortions.week	           #	 NumberOfWeeks x   ParityFrom1
# time.to.abortion	         #	 EVENTS x 4 (indicator parity  date sowINDEX)
# days.between.farrowings    #	 EVENTS x 4 (indicator parity  date sowINDEX)
# farrowings.week	           #	 NumberOfWeeks x   ParityFrom1
# total.born.week	           #	 NumberOfWeeks x   ParityFrom1
# live.born.week	           #	 NumberOfWeeks x   ParityFrom1
# dead.born.week	           #	 NumberOfWeeks x   ParityFrom1
# small.born.week	           #	 NumberOfWeeks x   ParityFrom1
# weak.born.week	           #	 NumberOfWeeks x   ParityFrom1
# mummi.born.week	           #	 NumberOfWeeks x   ParityFrom1
# perc.dead.born.week        #	 NumberOfWeeks x   ParityFrom1   2 (numerator and denominator)
# perc.small.born.week	     #	 NumberOfWeeks x   ParityFrom1   2 (numerator and denominator)
# perc.weak.born.week	       #	 NumberOfWeeks x   ParityFrom1   2 (numerator and denominator)
# perc.mummi.born.week	     #	 NumberOfWeeks x   ParityFrom1   2 (numerator and denominator)
# pregnancy.length	         #	 EVENTS x 4 (indicator parity  date sowINDEX)
# services.to.farrow	       #	 EVENTS x 4 (indicator parity  date sowINDEX)
# total.born.litter	         #	 EVENTS x 4 (indicator parity  date sowINDEX)
# live.born.litter	         #	 EVENTS x 4 (indicator parity  date sowINDEX)
# dead.born.litter	         #	 EVENTS x 4 (indicator parity  date sowINDEX)
# perc.dead.born.litter	     #	 EVENTS x 4 (indicator parity  date sowINDEX)
# small.born.litter	         #	 EVENTS x 4 (indicator parity  date sowINDEX)
# perc.small.born.litter     #	 EVENTS x 4 (indicator parity  date sowINDEX)
# weak.born.litter	         #	 EVENTS x 4 (indicator parity  date sowINDEX)
# perc.weak.born.litter	     #	 EVENTS x 4 (indicator parity  date sowINDEX)
# mummi.born.litter	         #	 EVENTS x 4 (indicator parity  date sowINDEX)
# perc.mummi.born.litter     #	 EVENTS x 4 (indicator parity  date sowINDEX)
# weanings.week              #	 NumberOfWeeks x   ParityFrom1
# total.wean.week            #	 NumberOfWeeks x   ParityFrom1
# diff.wean.week	           #	 NumberOfWeeks x   ParityFrom1
# negdiff.wean.week	         #	 NumberOfWeeks x   ParityFrom1
# weaning.length	           #	 EVENTS x 4 (indicator parity  date sowINDEX)
# total.wean.litter    	     #	 EVENTS x 4 (indicator parity  date sowINDEX)
# negdiff.weaned.litter	     #	 EVENTS x 4 (indicator parity  date sowINDEX)
# perc.negdiff.weaned.litter #	 EVENTS x 4 (indicator parity  date sowINDEX)
# weight.wean.litter	       #	 EVENTS x 4 (indicator parity  date sowINDEX)
# number.exits.week	         #	 NumberOfWeeks x   ParityFrom1
# number.deaths.week	       #	 NumberOfWeeks x   ParityFrom1
# gilts.deaths.week	         #	 NumberOfWeeks 
# piglets.deaths.week	       #	 NumberOfWeeks 
# death.after.event.week     #	 NumberOfWeeks x    6 (birth service reservice abortion farrowing weaning)
# number.exits.week          #	 NumberOfWeeks x   ParityFrom1
# exit.type.week	           #	 NumberOfWeeks x    7 (Slaughtered Sold Euthanized Dead Sold.Pregnant Missing Exported)
# exit.after.event.week	     #	 NumberOfWeeks x    6 (birth service reservice abortion farrowing weaning)


