


# RETRO indicators functions ----

# count functions ----

create.counts.days <- function(rows.index=index.dates.days[,1],
                       cols.index=parity,
                       data=individual.sows,
                       count.column="indicator",
                       groups.column="parity"){
  
matrix.days <- matrix(NA,nrow=length(rows.index),ncol=length(cols.index))
colnames(matrix.days)<- cols.index

for (r in 1:length(rows.index)){
  for (c in 1:length(cols.index)){
    matrix.days[r,c] <- sum(data[r,count.column,which(data[r,groups.column,]==cols.index[c])],na.rm=TRUE)
  }}

return(matrix.days)

}


create.counts.week <- function(rows.index.days=index.dates.days[,1],
                              rows.index.week=index.dates.week[,1],
                              cols.index=parity,
                              data=individual.sows,
                              count.column="indicator",
                              groups.column="parity"){
  
  matrix.days <- matrix(NA,nrow=length(rows.index.days),ncol=length(cols.index))
  for (r in 1:length(rows.index.days)){
    for (c in 1:length(cols.index)){
      matrix.days[r,c] <- sum(data[r,count.column,which(data[r,groups.column,]==cols.index[c])],na.rm=TRUE)
    }}
  
  
  matrix.week <- matrix(NA,nrow=length(rows.index.week),ncol=length(cols.index))
  colnames(matrix.week)<- cols.index
  
  for (r in 1:length(rows.index.week)){
    for (c in 1:length(cols.index)){
      matrix.week[r,c] <- sum(matrix.days[((((r-1)*7)+1):min(c((r*7),length(rows.index.days)))),c],na.rm=TRUE)
    }}
  
  return(matrix.week)
  
}


# weekly PERCENTAGE functions -----
create.perc.week.1 <- function(rows.index.week=index.dates.week[,1],
                             cols.index=parity,
                             data=assumed.pregnant,
                             value.to.count=1,
                             test=c("equal","greater")
                             ){
  matrix.perc <- array(NA,dim=c(length(rows.index.week),length(cols.index),2))
  dimnames(matrix.perc)[[2]] <- cols.index
  dimnames(matrix.perc)[[3]] <- c("numerator","denominator")
  
  for (r in 1:length(rows.index.week)){
    for (c in 1:length(cols.index)){
      
      if(test=="equal"){
        matrix.perc[r,c,1] <- length(which((data[(((r-1)*7)+1):min(c(r*7,dim(data)[1])),,c])==value.to.count))
      }else{
        matrix.perc[r,c,1] <- length(which((data[(((r-1)*7)+1):min(c(r*7,dim(data)[1])),,c])>value.to.count))
      }
      
      matrix.perc[r,c,2] <- length(which(!is.na(data[(((r-1)*7)+1):min(c(r*7,dim(data)[1])),,c])))
      
    }}
  return(matrix.perc)
}


#actually needs to be daily first, then rows, because of identifying relevant rows for parity
# create.perc.week.2 <- function(rows.index.week=index.dates.week[,1],
#                                groups.column="parity",
#                                cols.index=parity,
#                                data=individual.sows,
#                                numerator.col=NULL,
#                                denominator.col=NULL
# ){
#   matrix.perc <- array(NA,dim=c(length(rows.index.week),length(cols.index),2))
#   dimnames(matrix.perc)[[2]] <- cols.index
#   dimnames(matrix.perc)[[3]] <- c("numerator","denominator")
#   
#   for (r in 1:length(rows.index.week)){
#     for (c in 1:length(cols.index)){
#       
#       matrix.perc[r,c,1] <- sum(data[(((r-1)*7)+1):min(c(r*7,dim(data)[1])),numerator.col,which(data[r,groups.column,]==cols.index[c])],na.rm=TRUE)
#       matrix.perc[r,c,2] <- sum(data[(((r-1)*7)+1):min(c(r*7,dim(data)[1])),denominator.col,which(data[r,groups.column,]==cols.index[c])],na.rm=TRUE)
#       
#     }}
#   return(matrix.perc)
# }






# NON-TIME series ----
create.nonTS.timeto <- function(data=individual.sows,
                                index.dates=index.dates.days[,1],
                                col1="indicator",
                                col2="parity",
                                col3="date",
                                groups.column="parity",
                                event1.col="service",
                                event1.value=1,
                                event2.col="service",
                                event2.value=2,
                                condition=NULL){
  
    matrix.timeto <- matrix(NA,nrow=1,ncol=4)
    colnames(matrix.timeto)<- c(col1,col2,col3,"sowINDEX")
  
  
  
  for (r in 2:dim(data)[1]){
    for (s in 1:dim(data)[3]){
      
      if (!is.na(data[r,event2.col,s])&
          data[r,event2.col,s]==event2.value){
        
        if(length(condition)>0){
          if(condition=="first"){
            if(r>min(which(data[,event2.col,s]==event2.value))){
              next
            }
          }
        }
        
        matrix.timeto.c1 <- 0
        if(length(which(data[1:(r-1),event1.col,s]%in%event1.value))>0){
          matrix.timeto.c1 <- (r - max(which(data[1:(r-1),event1.col,s]%in%event1.value)))
        }
        
        matrix.timeto.c2 <- data[r,groups.column,s]
        
        matrix.timeto.c3 <- as.numeric(index.dates[r])
  
          matrix.timeto.c4 <- s
          matrix.timeto <- rbind(matrix.timeto,
                                 c(matrix.timeto.c1,
                                   matrix.timeto.c2,
                                   matrix.timeto.c3,
                                   matrix.timeto.c4))
          
       
      }
    }
  }
  
  if (all(is.na(matrix.timeto[1,]))){
    matrix.timeto<- matrix.timeto[-1,]
  }
  
  return(matrix.timeto)
  
}

create.nonTS.eventsto <- function(data=individual.sows,
                                index.dates=index.dates.days[,1],
                                col1="indicator",
                                col2="parity",
                                col3="date",
                                groups.column="parity",
                                event1.col="service",
                                event1.value=c(1,2),
                                event2.col="farrowing",
                                event2.value=1){
  
  matrix.timeto <- matrix(NA,nrow=1,ncol=4)
  
  colnames(matrix.timeto)<- c(col1,col2,col3,"sowINDEX")
  
  
  for (r in 2:dim(data)[1]){
    for (s in 1:dim(data)[3]){
      
      if (!is.na(data[r,event2.col,s])&
          data[r,event2.col,s]==event2.value){
        
        rmin <- 1
        if(length(which(data[1:(r-1),event2.col,s]==event2.value))>0){
        rmin <- max(which(data[1:(r-1),event2.col,s]==event2.value))
        }
        
        matrix.timeto.c1 <- length(which(data[(rmin+1):r,event1.col,s]%in%event1.value))
        matrix.timeto.c2 <- data[r,groups.column,s]
        matrix.timeto.c3 <- as.numeric(index.dates[r])
        matrix.timeto.c4 <- s
        
        matrix.timeto <- rbind(matrix.timeto,
                               c(matrix.timeto.c1,
                                 matrix.timeto.c2,
                                 matrix.timeto.c3,
                                 matrix.timeto.c4))
        
      }
    }
  }
  
  if (all(is.na(matrix.timeto[1,]))){
    matrix.timeto<- matrix.timeto[-1,]
  }
  
  return(matrix.timeto)
  
}

create.nonTS.counts <- function(data=individual.sows,
                                  index.dates=index.dates.days[,1],
                                  col1="indicator",
                                  col2="parity",
                                  col3="date",
                                  groups.column="parity",
                                  event.col="farrowing",
                                  event.value=1,
                                  count.column=c("NrBornAlive","NrBornDead"),
                                  denominator.col=NULL
){
  
  matrix.timeto <- matrix(NA,nrow=1,ncol=4)
  
  colnames(matrix.timeto)<- c(col1,col2,col3,"sowINDEX")
  
  
  for (r in 2:dim(data)[1]){
    for (s in 1:dim(data)[3]){
      
      if (!is.na(data[r,event.col,s])&
          data[r,event.col,s]==event.value){
        
        num <- sum(data[r,count.column,s],na.rm=T)
        den <- sum(data[r,denominator.col,s],na.rm=T)
        
        if(is.null(denominator.col)){
          matrix.timeto.c1 <- num
        }else{
          matrix.timeto.c1 <- num/den
        }
        
        matrix.timeto.c2 <- data[r,groups.column,s]
        matrix.timeto.c3 <- as.numeric(index.dates[r])
        matrix.timeto.c4 <- s
        
        matrix.timeto <- rbind(matrix.timeto,
                               c(matrix.timeto.c1,
                                 matrix.timeto.c2,
                                 matrix.timeto.c3,
                                 matrix.timeto.c4))
        
      }
    }
  }
  
  if (all(is.na(matrix.timeto[1,]))){
    matrix.timeto<- matrix.timeto[-1,]
  }
  
  return(matrix.timeto)
  
}


time.to.dataframe <- function(status.col2 = "status",
                              status.value2 = 0,
                              time.point.day = today.day.index,
                              status.sows = status.sows,
                              individual.sows = individual.sows,
                              status.col1 = "birth",
                              status.value1 = 1,
                              per.parity = FALSE,
                              parity.group = parity.group){
  
  sows.index <- which(status.sows[time.point.day,status.col2,]==status.value2)
  
  days.sows <- rep(NA,length(sows.index))
  for (g in 1:length(sows.index)){
    days.sows[g] <- (time.point.day) - max(which(individual.sows[1:time.point.day,status.col1,sows.index[g]]%in%status.value1),na.rm=T)
  }
  
  
  sows.time <- data.frame(id=dimnames(individual.sows)[[3]][sows.index],
                          index=sows.index, 
                          days=days.sows)
  
  if (per.parity==TRUE){
    parity.v=individual.sows[time.point.day,"parity",sows.index]
    sows.time <- data.frame(sows.time, parity=parity.v)
    
    sows.time <- merge(sows.time,
                       parity.group,
                       by = "parity",
                       sort=FALSE)
  }
  
  sows.order <- order(sows.time$days,decreasing = T)
  
  sows.time <- sows.time[sows.order,]
  return(sows.time)
  
}




per.parity.indicator.grouping <- function(parity.group = parity.group2,
                                          indicator.matrix = indicator.matrix,
                                          percentage = FALSE){
  
  
  indicator.pg <- matrix(NA,
                         ncol=length(unique(parity.group$group.name)),
                         nrow=dim(indicator.matrix)[1])
  colnames(indicator.pg)<-unique(parity.group$group.name)
  
  for(c in 1:dim(indicator.pg)[2]){
    
    if(percentage==TRUE){
      to.sum <- indicator.matrix[,which(parity.group$group.name==colnames(indicator.pg)[c]),,drop=F]
      if(dim(to.sum)[2]==1){
        indicator.pg[,c]<-to.sum[,,1]/to.sum[,,2]
      } else{
        indicator.pg[,c]<- apply(to.sum[,,1],1,sum,na.rm=T)/apply(to.sum[,,2],1,sum,na.rm=T)
      }
      
    }else{
      to.sum <- indicator.matrix[,which(parity.group$group.name==colnames(indicator.pg)[c])]
      if(is.null(dim(to.sum))){
        indicator.pg[,c]<-to.sum
      } else{
        indicator.pg[,c]<- apply(to.sum,1,sum,na.rm=T)
      }
    }
  }
  
  
  return(indicator.pg)
}



per.parity.nonTS.grouping <- function(parity.group = parity.group2,
                                      nonTS.indicator = nonTS.indicator){
  
  rows.index <- 1:dim(nonTS.indicator)[1]
  nonTS.indicator <- cbind(nonTS.indicator, rows.index)
  
  nonTS.indicator <- merge(nonTS.indicator,
                           parity.group,
                           by = "parity")
  
  nonTS.indicator <- nonTS.indicator[order(nonTS.indicator$rows.index),]
  nonTS.indicator$rows.index <- NULL
  
  nonTS.indicator.pg <- split(nonTS.indicator,nonTS.indicator$group.name)
  
  return(nonTS.indicator.pg)
  
}

