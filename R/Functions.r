packages <- c("RColorBrewer")
install.packages(setdiff(packages, rownames(installed.packages())))

require(RColorBrewer)


# background functions ----

lastmon <- function(x) {
  7 * floor(as.numeric(x-1+4)/7) + as.Date(1-4, origin="1970-01-01")
}



dates_df <- function (min.date, max.date,
                      by="days",
                      date.format="%d/%m/%Y") {

  start <- as.Date(min.date, format = date.format)
  end   <- as.Date(max.date, format = date.format)

  require(ISOweek)

  dates.v <- seq(from=start,to=end, by=by)
  date <- strptime (as.character(dates.v), format = "%Y-%m-%d")
  year <- (date$year+1900)
  month <- date$mon
  mday <- date$mday
  yday <- date$yday
  dow <- date$wday
  week <- as.numeric(substring(ISOweek::ISOweek(date),7,8))
  weekday <- rep(1,length(date))
  weekday[dow==0|dow==6] <- 0
  dates <- as.data.frame(dates.v)
  colnames(dates) <- "dates"
  dates <- cbind(dates, mday, month, year, yday, week, dow, weekday)

  return(dates)

}


#plotting functions ----

#parity.group <- data.frame(parity = c1, group=c2, group.name = c3)
parity.group <- data.frame(parity = c1, group.name = ordered(c3,levels=c("gilt","young","prime","mature")))



qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_parity = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

parity.group$color1 <- col_parity[as.numeric(as.factor(parity.group$group.name))]
parity.group$color2 <- col_parity[as.numeric(as.factor(parity.group$parity))]

parity.group2 <- parity.group[-1,]

colors.custom<- c(rep("#4287f5",1),
                  rep("#28ab1f",2),
                  rep("#f5942c",3),
                  rep("#a15a4c",9)
)
parity.group2 <-cbind(parity.group2,colors.custom)
parity.group2$colors.custom<-colors.custom

color.pg <- c("#4287f5","#28ab1f","#f5942c","#a15a4c")


