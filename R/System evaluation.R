# inject outbreaks in quarters

week.quarter <- week-(floor((week-1)/13)*13)
quarter <- c(rep(NA, length(range)))
quarter <- ifelse(week<=13, paste(year,1, sep = "."), quarter)
quarter <- ifelse(week>13 & week<=26, paste(year,2, sep = "."), quarter)
quarter <- ifelse(week>26 & week<=39, paste(year,3, sep = "."), quarter)
quarter <- ifelse(week>39, paste(year,4, sep = "."), quarter)

quarter = c("2015.2", "2015.3", "2015.4", "2016.1", "2016.2", "2016.3", "2016.4",   
            "2017.1", "2017.2", "2017.3", "2017.4", "2018.1")                       
