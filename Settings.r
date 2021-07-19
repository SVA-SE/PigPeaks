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


# plotting and dashboard -----

plot.years=2
weeks.to.show <- 78
nonTS.to.show <- 100
#days.ago.nonTS <- 114
group.window <- 114



# statistical settings ----
baseline.years=3
