
# indicators construction ----

parity = 1:15

reservice.threshold <- 90
reservice.perc.window <- 4     #(weeks to calculate success of service)

empty.days.target <- 4
opendays.target <- 4

min.pregnancy.length <- 110


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
                    "number.exits.week","exit.reason.week","exit.type.week","exit.after.event.week")

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
                       "number of exits per week","exit reason, weekly","exit type, weekly","exit after event, weekly")

# services.week              #   NumberOfWeeks x   ParityFrom1
# reservices.week	           #	 NumberOfWeeks x   ParityFrom1
# perc.pregnancy	           #	 NumberOfWeeks x   ParityFrom1   x 2
# perc.failure		           #   NumberOfWeeks x   ParityFrom1   x 2
# perc.reservice	           #	 NumberOfWeeks x   ParityFrom1   x 2
# time.to.reservice          #	 579   4
# rereservices		           #   0 4
# time.to.first.service      #	 2353    4
# time.to.first.farrowing    #	 2172    4
# abortions.week	           #	 NumberOfWeeks x   ParityFrom1
# time.to.abortion	         #	 34  4
# days.between.farrowings    #	 6934    4
# farrowings.week	           #	 NumberOfWeeks x   ParityFrom1
# total.born.week	           #	 NumberOfWeeks x   ParityFrom1
# live.born.week	           #	 NumberOfWeeks x   ParityFrom1
# dead.born.week	           #	 NumberOfWeeks x   ParityFrom1
# small.born.week	           #	 NumberOfWeeks x   ParityFrom1
# weak.born.week	           #	 NumberOfWeeks x   ParityFrom1
# mummi.born.week	           #	 NumberOfWeeks x   ParityFrom1
# perc.dead.born.week        #	 NumberOfWeeks x   ParityFrom1   2
# perc.small.born.week	     #	 NumberOfWeeks x   ParityFrom1   2
# perc.weak.born.week	       #	 NumberOfWeeks x   ParityFrom1   2
# perc.mummi.born.week	     #	 NumberOfWeeks x   ParityFrom1   2
# pregnancy.length	         #	 9106    4
# services.to.farrow	       #	 9106    4
# total.born.litter	         #	 9106    4
# live.born.litter	         #	 9106    4
# dead.born.litter	         #	 9106    4
# perc.dead.born.litter	     #	 9106    4
# small.born.litter	         #	 9106    4
# perc.small.born.litter     #	 9106    4
# weak.born.litter	         #	 9106    4
# perc.weak.born.litter	     #	 9106    4
# mummi.born.litter	         #	 9106    4
# perc.mummi.born.litter     #	 9106    4
# weanings.week              #	 NumberOfWeeks x   ParityFrom1
# total.wean.week            #	 NumberOfWeeks x   ParityFrom1
# diff.wean.week	           #	 NumberOfWeeks x   ParityFrom1
# negdiff.wean.week	         #	 NumberOfWeeks x   ParityFrom1
# weaning.length	           #	 9613    4
# total.wean.litter    	     #	 9613    4
# negdiff.weaned.litter	     #	 9613    4
# perc.negdiff.weaned.litter #	 9613    4
# weight.wean.litter	       #	 9613    4
# number.exits.week	         #	 NumberOfWeeks x   ParityFrom1
# number.deaths.week	       #	 NumberOfWeeks x   ParityFrom1
# gilts.deaths.week	         #	 NumberOfWeeks x
# piglets.deaths.week	       #	 NumberOfWeeks x
# exit.reason.week	         #	 NumberOfWeeks x  614
# exit.type.week	           #	 NumberOfWeeks x    7
# exit.after.event.week	     #	 NumberOfWeeks x    6
# death.after.event.week     #	 NumberOfWeeks x    6
# empty.long.week	           #	 NumberOfWeeks x   ParityFrom1
# piglets.deaths.week	       #	 NumberOfWeeks x ,


