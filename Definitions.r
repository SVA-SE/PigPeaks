
#indicators construction ----

parity = 1:15

reservice.threshold <- 90
reservice.perc.window <- 4 #(weeks)

empty.days.target <- 4
opendays.target <- 4


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

animal.category <- data.frame(Farm.animal=0,
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








