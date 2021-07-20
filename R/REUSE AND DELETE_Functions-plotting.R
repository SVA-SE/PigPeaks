TS.barplot <- function(series = series,
                          indicator.label="indicator",
                          show.window = weeks.to.show,
                          index.dates = index.dates.week,
                          ylabel = 'Number of sows',
                          xlabel = 'Week',
                          target.vector = NULL,
                          shading.matrix = NULL,
                          limits = NULL){
  
  plot.range <- max(1,(length(series)-show.window+1)):length(series)
  
  y =  series[plot.range]
  
  
  x=index.dates.week$start[plot.range]
  x.week=paste(index.dates$ISOweekYear[plot.range],index.dates$week[plot.range],sep="-")
  
  
  #labels
  
  text=str_c(indicator.label,":",y,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  
  target.vector = target.vector[plot.range]
  
  if(!is.null(shading.matrix)){
    shading.matrix=shading.matrix[plot.range,]
  }
  
  if(!is.null(shading.matrix)){
    LL3 <- shading.matrix[,1]
    LL2 <- shading.matrix[,2]  
    LL1 <- shading.matrix[,3]  
    UL1 <- shading.matrix[,4]  
    UL2 <- shading.matrix[,5]  
    UL3 <- shading.matrix[,6]  
  }
  
  if(!is.null(limits)){
    if(limits=="low"){
      UL1 <- max(y)
      UL2 <- max(y)
      UL3 <- max(y)
    }
    if(limits=="high"){
      LL1 <- min(0,min(data,na.rm=T),na.rm=T)
      LL2 <- min(0,min(data,na.rm=T),na.rm=T)
      LL3 <- min(0,min(data,na.rm=T),na.rm=T)
    }
  }
  
  
  
  
  plot <-
    plot_ly()
  
  if(!is.null(shading.matrix)){
    plot <- plot %>%
      add_trace(x=x,y = rep(max(y,na.rm=T),show.window),
                name = '99%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='tomato',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = UL3,
                name = '95%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='orange',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = UL2,
                name = '90%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='yellow',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = UL1,
                name = 'normal', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='lightgreen',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = LL1,
                name = '90%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='yellow',
                fill = 'tozeroy',showlegend = FALSE) %>%   
      add_trace(x=x,y = LL2,
                name = '95%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='orange',
                fill = 'tozeroy',showlegend = FALSE) %>%    
      add_trace(x=x,y = LL3,
                name = '99%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='tomato',
                fill = 'tozeroy',showlegend = FALSE)
    
  }
  
  
  
  plot <- plot %>%
    add_trace(x=x,y = y,type='bar',yaxis="y2",
              text = text, hoverinfo = 'text') %>%
    layout(yaxis = list(side = 'right', title = "", range = c(min(y,na.rm=T), max(y,na.rm=T))),
           yaxis2 = list(side = 'left', title = ylabel,overlaying = "y",range = c(min(y,na.rm=T), max(y,na.rm=T))),
           xaxis = list(title = xlabel),
           barmode = 'stack',
           legend=list(orientation="h",
                       x=0.25,y=max(y,na.rm=T),
                       traceorder='normal'))
  
  
  
  if(!is.null(target.vector)){
    plot <- plot %>%
      add_lines(
        x=x,
        y=target.vector,
        name='indicator target',yaxis="y2",
        line=list(color = '#fc2821'), showlegend = FALSE
      )
  }
  
  
  
  
  return(plot)    
}




TS.barplot.pg <- function(series = series.pg,
                          indicator.label="indicator",
                              show.window = weeks.to.show,
                              index.dates = index.dates.week,
                              ylabel = 'Number of sows',
                              xlabel = 'Week',
                              target.vector = NULL,
                              shading.matrix = NULL,
                              limits = NULL,
                          group.labels=c('gilts','young','prime','mature')
                          
){
  
  plot.range <- max(1,(dim(series)[1]-show.window+1)):dim(series)[1]
  
  data =  as.data.frame(series[plot.range,])
  

  x=index.dates.week$start[plot.range]
  x.week=paste(index.dates$ISOweekYear[plot.range],index.dates$week[plot.range],sep="-")
  y1 = data$gilt
  y2 = data$young
  y3 = data$prime
  y4 = data$mature
  y = y1+y2+y3+y4
  
  #labels
  t1 = group.labels[1]
  t2 = group.labels[2]
  t3 = group.labels[3]
  t4 = group.labels[4]
  
  text1=str_c("Parity group:",t1,
              "<br>",indicator.label,":",y1,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  text2=str_c("Parity group:",t2,
              "<br>",indicator.label,":",y2,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  text3=str_c("Parity group:",t3,
              "<br>",indicator.label,":",y3,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  text4=str_c("Parity group:",t4,
              "<br>",indicator.label,":",y4,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  
  
  
  target.vector = target.vector[plot.range]
  
  if(!is.null(shading.matrix)){
    shading.matrix=shading.matrix[plot.range,]
  }
  
  if(!is.null(shading.matrix)){
    LL3 <- shading.matrix[,1]
    LL2 <- shading.matrix[,2]  
    LL1 <- shading.matrix[,3]  
    UL1 <- shading.matrix[,4]  
    UL2 <- shading.matrix[,5]  
    UL3 <- shading.matrix[,6]  
  }
  
  if(!is.null(limits)){
  if(limits=="low"){
    UL1 <- max(y)
    UL2 <- max(y)
    UL3 <- max(y)
  }
  if(limits=="high"){
    LL1 <- min(0,min(data,na.rm=T),na.rm=T)
    LL2 <- min(0,min(data,na.rm=T),na.rm=T)
    LL3 <- min(0,min(data,na.rm=T),na.rm=T)
  }
  }
  
  
 
  
  color1=color.pg[1]
  color2=color.pg[2]
  color3=color.pg[3]
  color4=color.pg[4]
  
  
  plot <-
    plot_ly()
  
  if(!is.null(shading.matrix)){
    plot <- plot %>%
      add_trace(x=x,y = rep(max(y,na.rm=T),show.window),
                name = '99%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='tomato',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = UL3,
                name = '95%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='orange',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = UL2,
                name = '90%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='yellow',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = UL1,
                name = 'normal', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='lightgreen',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = LL1,
                name = '90%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='yellow',
                fill = 'tozeroy',showlegend = FALSE) %>%   
      add_trace(x=x,y = LL2,
                name = '95%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='orange',
                fill = 'tozeroy',showlegend = FALSE) %>%    
      add_trace(x=x,y = LL3,
                name = '99%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='tomato',
                fill = 'tozeroy',showlegend = FALSE)
    
  }
  
  
  
  plot <- plot %>%
    add_trace(x=x,y = y1,name=t1,marker=list(color=color1),type='bar',yaxis="y2",
              text = text1, hoverinfo = 'text') %>%
    add_trace(x=x,y = y2,name=t2,marker=list(color=color2),type='bar',yaxis="y2",
              text = text2, hoverinfo = 'text') %>%
    add_trace(x=x,y = y3,name=t3,marker=list(color=color3),type='bar',yaxis="y2",
              text = text3, hoverinfo = 'text') %>%
    add_trace(x=x,y = y4,name=t4,marker=list(color=color4),type='bar',yaxis="y2",
              text = text4, hoverinfo = 'text') %>%
    layout(yaxis = list(side = 'right', title = "", range = c(min(data,na.rm=T), max(y,na.rm=T))),
           yaxis2 = list(side = 'left', title = ylabel,overlaying = "y",range = c(min(data,na.rm=T), max(y,na.rm=T))),
           xaxis = list(title = xlabel),
           barmode = 'stack',
           legend=list(orientation="h",
                       x=0.25,y=max(y,na.rm=T),
                       traceorder='normal'))

  
  
    if(!is.null(target.vector)){
      plot <- plot %>%
        add_lines(
          x=x,
          y=target.vector,
          name='indicator target',yaxis="y2",
          line=list(color = '#fc2821'), showlegend = FALSE
        )
    }
  
  
  

  return(plot)    
}

TS.barplot.pg.perc <- function(series.bar = series.bar,
                               series.line = series.line,
                               series.line2 = NULL,
                               series2.name = NULL,
                          indicator.label='indicator',
                          show.window = weeks.to.show,
                          index.dates = index.dates.week,
                          ylabel = 'Number of sows',
                          xlabel = 'Week',
                          y2label = '% of',
                          y2range = c(0,1),
                          target.vector = NULL,
                          group.labels=c('gilts','young','prime','mature')
                          ){
  
  plot.range <- max(1,(dim(series.bar)[1]-show.window+1)):dim(series.bar)[1]
  
  data =  as.data.frame(series.bar[plot.range,])
  
  
  x=index.dates.week$start[plot.range]
  x.week=paste(index.dates$ISOweekYear[plot.range],index.dates$week[plot.range],sep="-")
  y1 = data$gilt
  y2 = data$young
  y3 = data$prime
  y4 = data$mature
  y = y1+y2+y3+y4
  
  
  #labels
  t1 = group.labels[1]
  t2 = group.labels[2]
  t3 = group.labels[3]
  t4 = group.labels[4]
  
  text1=str_c("Parity group:",t1,
              "<br>",indicator.label,":",y1,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  text2=str_c("Parity group:",t2,
              "<br>",indicator.label,":",y2,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  text3=str_c("Parity group:",t3,
              "<br>",indicator.label,":",y3,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  text4=str_c("Parity group:",t4,
              "<br>",indicator.label,":",y4,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  
  
  target.vector = target.vector[plot.range]
  
  series.line = series.line[plot.range]
  series.line2 = series.line2[plot.range]
  
  color1=color.pg[1]
  color2=color.pg[2]
  color3=color.pg[3]
  color4=color.pg[4]
  
  
  
    plot <- plot_ly() %>%
   
    add_trace(x=x,y = y1,name=t1,marker=list(color=color1),type='bar',
              text = text1, hoverinfo = 'text') %>%
    add_trace(x=x,y = y2,name=t2,marker=list(color=color2),type='bar',
              text = text2, hoverinfo = 'text') %>%
    add_trace(x=x,y = y3,name=t3,marker=list(color=color3),type='bar',
              text = text3, hoverinfo = 'text') %>%
    add_trace(x=x,y = y4,name=t4,marker=list(color=color4),type='bar',
              text = text4, hoverinfo = 'text') %>%
    #add_trace(x=x,y = series.line,name=y2label,marker=list(color=color4),type='scatter') %>%
    
      add_lines(
        x=x,
        y=series.line,
        name=y2label,yaxis="y2",
        line=list(color = 'black'), showlegend = TRUE
      )%>%
      # add_markers(
      #   x=x[x.dots],
      #   y = y.dots,
      #   name=dots.label,
      #   marker=list(color = '#fc2821',symbol="star",size=10), showlegend = FALSE
      # ) %>%
    layout(yaxis = list(title = ylabel, range = c(min(data,na.rm=T), max(y,na.rm=T))),
           yaxis2 = list(side = 'right', title = y2label, overlaying = "y",range = y2range,showgrid = FALSE),
           xaxis = list(title = xlabel),
           barmode = 'stack',
           legend=list(orientation="h",
                       x=0.25,y=max(y,na.rm=T),
                       traceorder='normal'))
  
  if(!is.null(target.vector)){
    plot <- plot %>%
      add_lines(
        x=x,
        y=target.vector,
        name='indicator target',yaxis="y2",
        line=list(color = '#fc2821'), showlegend = FALSE
      )
  }
  
  
    if(!is.null(series.line2)){
      
      plot <- plot %>%
      
      add_lines(
        x=x,
        y=series.line2,
        name=series2.name, yaxis="y2",
        line=list(color = 'red')
      )
      
    }
    
    
  
  
  return(plot)    
}


nonTS.barplot.pg <- function(series = indicator.series,
                              indicator.label="indicator",
                              show.window = weeks.to.show,
                              show.window.unit="weeks", #c("days","events","weeks"), defaults to days
                              vertical.line = NULL, #VECTOR OF DATES
                             vertical.line.label=NULL,
                             index.dates = index.dates.days,
                             ylabel = 'Time between events',
                              xlabel = 'Date of event',
                              target = NULL,
                              target.unit="value", #c("value","vector), defaults to vector
                             target.label="target",
                              #shading.matrix = NULL,
                              #limits = NULL,
                             group.labels=c('gilts','young','prime','mature'),
                             barmode = 'group' #"overlay"
                             
){
  
  last.x <- tail(index.dates[,1],1)
  
  
  range.length <- show.window
  if(show.window.unit=="weeks"){
    range.length <- show.window*7
  }
  if(show.window.unit=="events"){
    range.length <- as.numeric(last.x)-as.numeric(series[(dim(series)[1]-show.window+1),"date"])
  }
  
  first.x <- last.x - range.length
  
  first.x.numeric <- as.numeric(first.x)
  first.x.row <- min(which(series[,"date"]>=first.x.numeric),na.rm=T)
  
  series.range <- series[first.x.row:dim(series)[1],]
  
  
  series.pg <- per.parity.nonTS.grouping(nonTS.indicator = series.range)
  
  
  x=seq(first.x,last.x,by="day")
  
  y1 = series.pg[[1]][,"indicator"]
  y2 = series.pg[[2]][,"indicator"]
  y3 = series.pg[[3]][,"indicator"]
  y4 = series.pg[[4]][,"indicator"]
  #y.sum=y1+y2+y3+y4
  y.all=c(y1,y2,y3,y4)
  
  x1 = as.Date(series.pg[[1]][,"date"],origin="1970-01-01")
  x2 = as.Date(series.pg[[2]][,"date"],origin="1970-01-01")
  x3 = as.Date(series.pg[[3]][,"date"],origin="1970-01-01")
  x4 = as.Date(series.pg[[4]][,"date"],origin="1970-01-01")
  
  
  
  #labels
  t1 = group.labels[1]
  t2 = group.labels[2]
  t3 = group.labels[3]
  t4 = group.labels[4]
  
  #colors
  
  text1=str_c("Parity group:",t1,
              "<br>",indicator.label,":",y1,
              "<br>Week:",date2ISOweek(x1),
              "<br>date:",x1,
              "<br>sowID:",active.sows.displayID[match(dimnames(individual.sows)[[3]][series.pg[[1]][,"sowINDEX"]],active.sows.displayID[,"codesID"]),"displayID"])
  text2=str_c("Parity group:",t2,
              "<br>",indicator.label,":",y2,
              "<br>Week:",date2ISOweek(x2),
              "<br>date:",x2,
              "<br>sowID:",active.sows.displayID[match(dimnames(individual.sows)[[3]][series.pg[[2]][,"sowINDEX"]],active.sows.displayID[,"codesID"]),"displayID"])
  text3=str_c("Parity group:",t3,
              "<br>",indicator.label,":",y3,
              "<br>Week:",date2ISOweek(x3),
              "<br>date:",x3,
              "<br>sowID:",active.sows.displayID[match(dimnames(individual.sows)[[3]][series.pg[[3]][,"sowINDEX"]],active.sows.displayID[,"codesID"]),"displayID"])
  text4=str_c("Parity group:",t4,
              "<br>",indicator.label,":",y4,
              "<br>Week:",date2ISOweek(x4),
              "<br>date:",x4,
              "<br>sowID:",active.sows.displayID[match(dimnames(individual.sows)[[3]][series.pg[[4]][,"sowINDEX"]],active.sows.displayID[,"codesID"]),"displayID"])
  

  
  target.vector = target
  if(!is.null(target.unit)){
    if(target.unit=="value"){
      target.vector <- rep(target,length(x))
    }}
  
  color1=color.pg[1]
  color2=color.pg[2]
  color3=color.pg[3]
  color4=color.pg[4]
  
  
  plot <-
    plot_ly(x=x)
  
  
  plot <- plot %>%
    add_trace(x=x1,y = y1,name=t1,marker=list(color=color1),type='bar',
              text = text1, hoverinfo = 'text') %>%
    add_trace(x=x2,y = y2,name=t2,marker=list(color=color2),type='bar',
              text = text2, hoverinfo = 'text') %>%
    add_trace(x=x3,y = y3,name=t3,marker=list(color=color3),type='bar',
              text = text3, hoverinfo = 'text') %>%
    add_trace(x=x4,y = y4,name=t4,marker=list(color=color4),type='bar',
              text = text4, hoverinfo = 'text') %>%
    layout(yaxis = list(side = 'left', title = ylabel, range = c(min(y.all,na.rm=T), max(y.all,na.rm=T))),
           xaxis = list(title = xlabel),
           barmode = barmode,
           legend=list(orientation="h",
                       x=0.25,y=max(y.all,na.rm=T),
                       traceorder='normal'))
  
  
  
  if(!is.null(target)){
    plot <- plot %>%
      add_lines(
        x=x,
        y=target.vector,
        line=list(color = '#fc2821'), name=target.label
      )
  }
  
  
  
  if(!is.null(vertical.line)){
    for(vl in 1:length(vertical.line)){
      
      if(is.null(vertical.line.label)){
      plot <- plot %>%
        add_segments(x = vertical.line[vl], xend = vertical.line[vl], y = 0, yend = max(y.all,na.rm=T),
                     line=list(color = '#F320D7'),showlegend = FALSE)
      }else{
        
        if(is.na(vertical.line.label[vl])){
          plot <- plot %>%
            add_segments(x = vertical.line[vl], xend = vertical.line[vl], y = 0, yend = max(y.all,na.rm=T),
                         line=list(color = '#F320D7'),showlegend = FALSE)
        }else{
        
        plot <- plot %>%
          add_segments(x = vertical.line[vl], xend = vertical.line[vl], y = 0, yend = max(y.all,na.rm=T),
                       name=vertical.line.label[vl],
                       line=list(color = '#F320D7'))
        }
      }
      
    }
    
  }
  
  
  return(plot)    
}


nonTS.barplot.pg.timeless <- function(series = indicator.series,
                             indicator.label="indicator",
                             series.line = NULL,
                             show.window = nonTS.to.show, #always as number of events
                             vertical.line = NULL, #VECTOR OF DATES
                             vertical.line.label=NULL,
                             index.dates = index.dates.days,
                             ylabel = 'Time between events',
                             xlabel = 'Date of event',
                             y2label = '% of',
                             y2range = c(0,1),
                             target = NULL,
                             target.unit="value", #c("value","vector), defaults to vector
                             target.label="target",
                             #shading.matrix = NULL,
                             #limits = NULL,
                             #group.labels=c('gilts','young','prime','mature') #from data,
                             use.minimum.y="min",  #"zero",
                             barmode = 'group' #"overlay"
                             
                             
){
  
  range <- max((dim(series)[1]-show.window+1),1,na.rm=T):(dim(series)[1])
  
  series.range <- series[range,]

  
  y = series.range[,"indicator"]
  
  x = 1:min(dim(series.range)[1],show.window)
  
  x.dates = as.Date(series.range[,"date"],origin="1970-01-01")
  
  parity.group = parity.group2$group.name[series.range[,"parity"]]
  
  series.range <- as.data.frame(series.range)
  series.range <- as.data.frame(cbind(series.range,x,x.dates,parity.group))
  
  series.pg <- split(series.range,series.range$parity.group)
  
  #labels
  t1 = names(series.pg)[1]
  t2 = names(series.pg)[2]
  t3 = names(series.pg)[3]
  t4 = names(series.pg)[4]
  
  y1 = series.pg[[1]][,"indicator"]
  y2 = series.pg[[2]][,"indicator"]
  y3 = series.pg[[3]][,"indicator"]
  y4 = series.pg[[4]][,"indicator"]
  y.all=c(y1,y2,y3,y4)
  
  min.y=0
    if(use.minimum.y=="min")(min.y=min(y.all,na.rm=T))
  
  
  #colors
  
  text1=str_c("Parity group:",t1,
              "<br>",indicator.label,":",y1,
              "<br>Week:",date2ISOweek(series.pg[[1]][,"x.dates"]),
              "<br>date:",series.pg[[1]][,"x.dates"],
              "<br>sowID:",active.sows.displayID[match(dimnames(individual.sows)[[3]][series.pg[[1]][,"sowINDEX"]],active.sows.displayID[,"codesID"]),"displayID"])
  text2=str_c("Parity group:",t2,
              "<br>",indicator.label,":",y2,
              "<br>Week:",date2ISOweek(series.pg[[2]][,"x.dates"]),
              "<br>date:",series.pg[[2]][,"x.dates"],
              "<br>sowID:",active.sows.displayID[match(dimnames(individual.sows)[[3]][series.pg[[2]][,"sowINDEX"]],active.sows.displayID[,"codesID"]),"displayID"])
  text3=str_c("Parity group:",t3,
              "<br>",indicator.label,":",y3,
              "<br>Week:",date2ISOweek(series.pg[[3]][,"x.dates"]),
              "<br>date:",series.pg[[3]][,"x.dates"],
              "<br>sowID:",active.sows.displayID[match(dimnames(individual.sows)[[3]][series.pg[[3]][,"sowINDEX"]],active.sows.displayID[,"codesID"]),"displayID"])
  text4=str_c("Parity group:",t4,
              "<br>",indicator.label,":",y4,
              "<br>Week:",date2ISOweek(series.pg[[4]][,"x.dates"]),
              "<br>date:",series.pg[[4]][,"x.dates"],
              "<br>sowID:",active.sows.displayID[match(dimnames(individual.sows)[[3]][series.pg[[4]][,"sowINDEX"]],active.sows.displayID[,"codesID"]),"displayID"])
  
  
  
  target.vector = target
  if(!is.null(target.unit)){
    if(target.unit=="value"){
      target.vector <- rep(target,length(x))
    }}
  
  series.line = series.line[range]
  
  color1=color.pg[1]
  color2=color.pg[2]
  color3=color.pg[3]
  color4=color.pg[4]
  
  
  plot <-
    plot_ly(x=x)
  
  
  plot <- plot %>%
    add_trace(x=series.pg[[1]][,"x"],y = y1,name=t1,marker=list(color=color1),type='bar',
              text = text1, hoverinfo = 'text') %>%
    add_trace(x=series.pg[[2]][,"x"],y = y2,name=t2,marker=list(color=color2),type='bar',
              text = text2, hoverinfo = 'text') %>%
    add_trace(x=series.pg[[3]][,"x"],y = y3,name=t3,marker=list(color=color3),type='bar',
              text = text3, hoverinfo = 'text') %>%
    add_trace(x=series.pg[[4]][,"x"],y = y4,name=t4,marker=list(color=color4),type='bar',
              text = text4, hoverinfo = 'text')  
  
  if(!is.null(series.line)){
    plot <- plot %>%
      add_lines(
        x=x,
        y=series.line,
        name=y2label,yaxis="y2",
        line=list(color = 'black'), showlegend = TRUE
      )%>%
      layout(yaxis = list(side = 'left', title = ylabel, range = c(min.y, max(y.all,na.rm=T))),
             yaxis2 = list(side = 'right', title = y2label, overlaying = "y",range = y2range,showgrid = FALSE),
             xaxis = list(title = xlabel),
             barmode = barmode,
             legend=list(orientation="h",
                         
                         x=0.25,y=max(y.all,na.rm=T),
                         traceorder='normal'))
    
  }else{
    plot <- plot %>%
      layout(yaxis = list(side = 'left', title = ylabel, range = c(min.y, max(y.all,na.rm=T))),
             xaxis = list(title = xlabel),
             barmode = barmode,
             legend=list(orientation="h",
                         
                         x=0.25,y=max(y.all,na.rm=T),
                         traceorder='normal'))
}
 
  
  if(!is.null(target)){
    plot <- plot %>%
      add_lines(
        x=x,
        y=target.vector,
        name='indicator target',
        line=list(color = '#fc2821'), name=target.label
      )
  }
  
  
  
  if(!is.null(vertical.line)){
    for(vl in 1:length(vertical.line)){
      
      x.vl <- which.min(abs(series.range[,"x.dates"]-vertical.line[vl]))
      
      if(is.null(vertical.line.label)|vl>1){
        plot <- plot %>%
          add_segments(x = x.vl, xend = x.vl, y = 0, yend = max(y.all,na.rm=T),
                       line=list(color = '#F320D7'),showlegend = FALSE)
      }else{
        plot <- plot %>%
          add_segments(x = x.vl, xend = x.vl, y = 0, yend = max(y.all,na.rm=T),
                       name=vertical.line.label[vl],
                       line=list(color = '#F320D7'))
      }
      
    }
    
  }
  
  
  
  
  return(plot)    
}




single.indicator.barplot <- function(
  x = indicator,
  SowID = NULL,
  indicator.text.label = "indicator",
  indicator.legend.label = "indicator",
  xlabel=NULL,
  sq.x0 = NULL,
  sq.x1 = NULL,
  line.x = NULL,
  target.legend.label=NULL,
  orientation="v"
  
){


text = str_c("SowID: ", SowID,
             "<br>", indicator.text.label, ": ", x)
t1 = indicator.legend.label
t2 = target.legend.label

if(orientation=="h"){

plot <-  plot_ly(
  x=x,
  type = 'bar', orientation = 'h',
  text = text,
  hoverinfo = 'text',
  name=t1) %>%
  layout(xaxis = list(
    title = xlabel),
    yaxis = list(
      title = "",
      showticklabels = FALSE),
    shapes=list(
      list(type = "rect",
           fillcolor = "orange", line = list(color = "orange"), opacity = 0.3,
           x0 = sq.x0, x1 = sq.x1, xref = "x",
           y0 = 0, y1 = length(x), yref = "y")
    )
  )%>% 
  add_lines(
    x=rep(line.x,length(x)),
    y=0:(length(x)-1),
    name=t2
  )
}else{


plot <-  plot_ly(
  y=x,
  type = 'bar', 
  text = text,
  hoverinfo = 'text',
  name=t1) %>%
  layout(yaxis = list(
    title = xlabel),
    xaxis = list(
      title = "",
      showticklabels = FALSE),
    legend=list(orientation="h"),
    shapes=list(
      list(type = "rect",
           fillcolor = "orange", line = list(color = "orange"), opacity = 0.3,
           y0 = sq.x0, y1 = sq.x1, xref = "y",
           x0 = 0, x1 = length(x), yref = "x")
    )
  )%>% 
  add_lines(
    y=rep(line.x,length(x)),
    x=0:(length(x)-1),
    name=t2
  )
}

return(plot)
}




nonTS.barplot <- function(series = indicator.series,
                             indicator.label="indicator",
                             show.window = weeks.to.show,
                             show.window.unit="weeks", #c("days","events","weeks"), defaults to days
                             vertical.line = NULL, #VECTOR OF DATES
                             vertical.line.label=NULL,
                             index.dates = index.dates.days,
                             ylabel = 'Time between events',
                             xlabel = 'Date of event',
                             target = NULL,
                             target.unit="value", #c("value","vector), defaults to vector
                             target.label="target",
                          series.label="sows"
                             
){
  
  last.x <- tail(index.dates[,1],1)
  
  
  range.length <- show.window
  if(show.window.unit=="weeks"){
    range.length <- show.window*7
  }
  if(show.window.unit=="events"){
    range.length <- as.numeric(last.x)-as.numeric(series[(dim(series)[1]-show.window+1),"date"])
  }
  
  first.x <- last.x - range.length
  
  first.x.numeric <- as.numeric(first.x)
  first.x.row <- min(which(series[,"date"]>=first.x.numeric),na.rm=T)
  
  series.range <- series[first.x.row:dim(series)[1],]
  
  
  x=seq(first.x,last.x,by="day")
  
  y = series.range[,"indicator"]
  
  x1 = as.Date(series.range[,"date"],origin="1970-01-01")
  
  #labels
  t = series.label
  
  #colors
  
  text=str_c(indicator.label,":",y,
              "<br>Week:",date2ISOweek(x1),
              "<br>date:",x1,
              "<br>sowID:",active.sows.displayID[match(dimnames(individual.sows)[[3]][series.range[,"sowINDEX"]],active.sows.displayID[,"codesID"]),"displayID"])
  
  
  target.vector = target
  if(!is.null(target.unit)){
    if(target.unit=="value"){
    target.vector <- rep(target,length(x))
  }}
  
 
  plot <-
    plot_ly(x=x)
  
  
  plot <- plot %>%
    add_trace(x=x1,y = y,name=t,type='bar',
              text = text, hoverinfo = 'text') %>%
    layout(yaxis = list(side = 'left', title = ylabel),
           xaxis = list(title = xlabel),
           barmode = 'overlay',
           legend=list(orientation="h",
                       x=0.25,y=max(y,na.rm=T),
                       traceorder='normal'))
  
  
  
  if(!is.null(target)){
    plot <- plot %>%
      add_lines(
        x=x,
        y=target.vector,
        line=list(color = '#fc2821'), name=target.label
      )
  }
  
  
  
  if(!is.null(vertical.line)){
    for(vl in 1:length(vertical.line)){
      
      if(is.null(vertical.line.label)){
        plot <- plot %>%
          add_segments(x = vertical.line[vl], xend = vertical.line[vl], y = 0, yend = max(y,na.rm=T),
                       line=list(color = '#F320D7'),showlegend = FALSE)
      }else{
        
        if(is.na(vertical.line.label[vl])){
          plot <- plot %>%
            add_segments(x = vertical.line[vl], xend = vertical.line[vl], y = 0, yend = max(y,na.rm=T),
                         line=list(color = '#F320D7'),showlegend = FALSE)
        }else{
          
          plot <- plot %>%
            add_segments(x = vertical.line[vl], xend = vertical.line[vl], y = 0, yend = max(y,na.rm=T),
                         name=vertical.line.label[vl],
                         line=list(color = '#F320D7'))
        }
      }
      
    }
    
  }
  
  
  return(plot)    
}


nonTS.barplot.timeless <- function(series = indicator.series,
                                      indicator.label="indicator",
                                      show.window = nonTS.to.show, #always as number of events
                                      vertical.line = NULL, #VECTOR OF DATES
                                      vertical.line.label=NULL,
                                      index.dates = index.dates.days,
                                      ylabel = 'Time between events',
                                      xlabel = 'Date of event',
                                      target = NULL,
                                      target.unit="value", #c("value","vector), defaults to vector
                                      target.label="target",
                                      series.label="sows"
                                      
){
  
  range <- max((dim(series)[1]-show.window+1),1,na.rm=T):(dim(series)[1])
  
  series.range <- series[range,]
  
  y = series.range[,"indicator"]
  
  x = 1:min(dim(series.range)[1],show.window)
  
  x.dates = as.Date(series.range[,"date"],origin="1970-01-01")
  
  t = series.label
  
  y = series.range[,"indicator"]
  
  
  text=str_c(indicator.label,":",y,
              "<br>Week:",date2ISOweek(x.dates),
              "<br>date:",x.dates,
              "<br>sowID:",active.sows.displayID[match(dimnames(individual.sows)[[3]][series.range[,"sowINDEX"]],active.sows.displayID[,"codesID"]),"displayID"])
 
  
  target.vector = target
  
  target.vector = target
  if(!is.null(target.unit)){
    if(target.unit=="value"){
      target.vector <- rep(target,length(x))
    }}
  
  
  
  plot <-
    plot_ly(x=x)
  
  
  plot <- plot %>%
    add_trace(x=x,y = y,name=t,type='bar',
              text = text, hoverinfo = 'text') %>%
    layout(yaxis = list(side = 'left', title = ylabel),
           xaxis = list(title = xlabel),
           barmode = 'group',
           legend=list(orientation="h",
                       x=0.25,y=max(y,na.rm=T),
                       traceorder='normal'))
  
  
  
  if(!is.null(target)){
    plot <- plot %>%
      add_lines(
        x=x,
        y=target.vector,
        name='indicator target',
        line=list(color = '#fc2821'), name=target.label
      )
  }
  
  
  
  if(!is.null(vertical.line)){
    for(vl in 1:length(vertical.line)){
      
      x.vl <- which.min(abs(x.dates-vertical.line[vl]))
      
      if(is.null(vertical.line.label)|vl>1){
        plot <- plot %>%
          add_segments(x = x.vl, xend = x.vl, y = 0, yend = max(y,na.rm=T),
                       line=list(color = '#F320D7'),showlegend = FALSE)
      }else{
        plot <- plot %>%
          add_segments(x = x.vl, xend = x.vl, y = 0, yend = max(y,na.rm=T),
                       name=vertical.line.label[vl],
                       line=list(color = '#F320D7'))
      }
      
    }
    
  }
  
  
  
  
  return(plot)    
}


TS.exit <- function(series = series.matrix,
                          indicator.label="Number of sows",
                          show.window = weeks.to.show,
                          index.dates = index.dates.week,
                          ylabel = 'Number of sows',
                          xlabel = 'Week',
                          target.vector = NULL,
                          shading.matrix = NULL,
                          limits = NULL,
                          group.labels=c('birth','service','reservice','abortion','farrowing','weaning')
                          
){
  
  plot.range <- max(1,(dim(series)[1]-show.window+1)):dim(series)[1]
  
  data =  as.data.frame(series[plot.range,])
  
  
  x=index.dates.week$start[plot.range]
  x.week=paste(index.dates$ISOweekYear[plot.range],index.dates$week[plot.range],sep="-")
  y1 = data[,1]
  y2 = data[,2]
  y3 = data[,3]
  y4 = data[,4]
  y5 = data[,5]
  y6 = data[,6]
  
  y = y1+y2+y3+y4+y5+y6
  
  #labels
  t1 = group.labels[1]
  t2 = group.labels[2]
  t3 = group.labels[3]
  t4 = group.labels[4]
  t5 = group.labels[5]
  t6 = group.labels[6]
  
  text1=str_c("Exit after:",t1,
              "<br>",indicator.label,":",y1,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  text2=str_c("Exit after:",t2,
              "<br>",indicator.label,":",y2,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  text3=str_c("Exit after:",t3,
              "<br>",indicator.label,":",y3,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  text4=str_c("Exit after:",t4,
              "<br>",indicator.label,":",y4,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  text5=str_c("Exit after:",t5,
              "<br>",indicator.label,":",y5,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  text6=str_c("Exit after:",t6,
              "<br>",indicator.label,":",y6,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  
  
  
  target.vector = target.vector[plot.range]
  
  if(!is.null(shading.matrix)){
    shading.matrix=shading.matrix[plot.range,]
  }
  
  if(!is.null(shading.matrix)){
    LL3 <- shading.matrix[,1]
    LL2 <- shading.matrix[,2]  
    LL1 <- shading.matrix[,3]  
    UL1 <- shading.matrix[,4]  
    UL2 <- shading.matrix[,5]  
    UL3 <- shading.matrix[,6]  
  }
  
  if(!is.null(limits)){
    if(limits=="low"){
      UL1 <- max(y)
      UL2 <- max(y)
      UL3 <- max(y)
    }
    if(limits=="high"){
      LL1 <- min(0,min(data,na.rm=T),na.rm=T)
      LL2 <- min(0,min(data,na.rm=T),na.rm=T)
      LL3 <- min(0,min(data,na.rm=T),na.rm=T)
    }
  }
  
  
  
  plot <-
    plot_ly()
  
  if(!is.null(shading.matrix)){
    plot <- plot %>%
      add_trace(x=x,y = rep(max(y,na.rm=T),show.window),
                name = '99%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='tomato',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = UL3,
                name = '95%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='orange',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = UL2,
                name = '90%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='yellow',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = UL1,
                name = 'normal', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='lightgreen',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = LL1,
                name = '90%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='yellow',
                fill = 'tozeroy',showlegend = FALSE) %>%   
      add_trace(x=x,y = LL2,
                name = '95%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='orange',
                fill = 'tozeroy',showlegend = FALSE) %>%    
      add_trace(x=x,y = LL3,
                name = '99%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='tomato',
                fill = 'tozeroy',showlegend = FALSE)
    
  }
  
  
  
  plot <- plot %>%
    add_trace(x=x,y = y1,name=t1,type='bar',yaxis="y2", 
              text = text1, hoverinfo = 'text') %>%
    add_trace(x=x,y = y2,name=t2,type='bar',yaxis="y2",
              text = text2, hoverinfo = 'text') %>%
    add_trace(x=x,y = y3,name=t3,type='bar',yaxis="y2",
              text = text3, hoverinfo = 'text') %>%
    add_trace(x=x,y = y4,name=t4,type='bar',yaxis="y2",
              text = text4, hoverinfo = 'text') %>%
    add_trace(x=x,y = y5,name=t5,type='bar',yaxis="y2",
              text = text5, hoverinfo = 'text') %>%
    add_trace(x=x,y = y6,name=t6,type='bar',yaxis="y2",
              text = text6, hoverinfo = 'text') %>%
    layout(yaxis = list(side = 'right', title = "", range = c(min(data,na.rm=T), max(y,na.rm=T))),
           yaxis2 = list(side = 'left', title = ylabel,overlaying = "y",range = c(min(data,na.rm=T), max(y,na.rm=T))),
           xaxis = list(title = xlabel),
           barmode = 'stack',
           legend=list(orientation="h",
                       x=0.25,y=max(y,na.rm=T),
                       traceorder='normal'))
  
  
  
  if(!is.null(target.vector)){
    plot <- plot %>%
      add_lines(
        x=x,
        y=target.vector,
        name='indicator target',yaxis="y2",
        line=list(color = '#fc2821'), showlegend = FALSE
      )
  }
  
  
  
  
  return(plot)    
}

######## old codes --------

parity.meter <- function(stats.recent,
                         stats.thisweek,
                         x,
                         ygroups = c("gilt","young","prime","mature")
                         ){


marker.list = list(size = 35,
                   color = "#FFFF66",
                   line = list(color = "#FF8000",
                               width = 2)
)
marker.list2 = list(size = 40,
                    color = 'red',
                    line = list(color = 'red',width=7),
                    symbol="line-ns"
                    
)

t1 = 'last 4 weeks'
t2 = 'THIS WEEK'

p<- 
  plot_ly(alpha=0.2)%>%
  add_heatmap(
    type = "heatmap",
    y=c("a","b","c","d"),
    z = rbind(-10:10,-10:10,-10:10,-10:10),
    x=-10:10,
    #colors = colorRamp(c("blue","yellow", "green","orange","red")),
    colors = colorRamp(c("darkblue","blue", "green","orange","red")),
    ygap=7,
    showscale=F
  )%>%
  add_markers(
    y=ygroups[1],
    x=stats.recent[,1],
    marker=marker.list,
    opacity=0.5,
    name=t1
  )%>%
  add_markers(
    y=ygroups[2],
    x=stats.recent[,2],
    marker=marker.list,
    opacity=0.5,
    showlegend = FALSE
  )%>%
  add_markers(
    y=ygroups[3],
    x=stats.recent[,3],
    marker=marker.list,
    opacity=0.5,
    showlegend = FALSE
  ) %>%
  add_markers(
    y=ygroups[4],
    x=stats.recent[,4],
    marker=marker.list,
    opacity=0.5,
    showlegend = FALSE
  )%>%
  
  add_markers(
    y=ygroups[1],
    x=stats.thisweek[1],
    marker=marker.list2,
    name=t2
  )%>%
  add_markers(
    y=ygroups[2],
    x=stats.thisweek[2],
    marker=marker.list2,
    showlegend = FALSE
  )%>%
  add_markers(
    y=ygroups[3],
    x=stats.thisweek[3],
    marker=marker.list2,
    showlegend = FALSE
  ) %>%
  add_markers(
    y=ygroups[4],
    x=stats.thisweek[4],
    marker=marker.list2,
    showlegend = FALSE
  )%>%
  layout(
    xaxis = list(showticklabels=F),
    legend = list(orientation = 'h')
  )

return(p)
}




gauge.meter.plotly <- function(statistic = statistic,
                        min=-10,
                        max=10,
                        label.main='% reservices',
                        number.main=statistic){
  
  range = max-min
  
  h = 0.24
  k = 0.5
  r = 0.15
  
  my_raw_value = ifelse(statistic<min,min,statistic)
  my_raw_value = ifelse(statistic>max,max,my_raw_value)
  
  my_raw_value = my_raw_value-min #so that it starts from 0
  my_raw_value = my_raw_value
  
  theta = my_raw_value * 180/range
  theta = theta * pi / 180
  x = h - r*cos(theta)#*0.25
  y = k + r*sin(theta)
  path = paste('M 0.235 0.5 L' , x, y, 'L 0.245 0.5 Z')
  
  
  base_plot <- plot_ly(
    type = "pie",
    values = c(40, 10, 10, 10, 10, 10, 10),
    labels = c("-", "0", "-6", "-2", "2", "6", "10"),
    rotation = 108,
    direction = "clockwise",
    hole = 0.4,
    textinfo = "label",
    textposition = "outside",
    hoverinfo = "none",
    domain = list(x = c(0, 0.48), y = c(0, 1)),
    marker = list(colors = c('rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)')),
    showlegend = FALSE
  )
  base_plot <- add_trace(
    base_plot,
    type = "pie",
    values = c(50, 10, 10, 10, 10, 10),
    labels = c(label.main, "too low", "low", "normal", "high", "very high"),
    rotation = 90,
    direction = "clockwise",
    hole = 0.3,
    textinfo = "label",
    textposition = "inside",
    hoverinfo = "none",
    domain = list(x = c(0, 0.48), y = c(0, 1)),
    marker = list(colors = c("rgb(255, 255, 255)","rgba(30, 33, 117,0.5)","rgba(7, 125, 242,0.5)", "rgba(19, 125, 52,0.5)",
                             "rgba(224, 154, 25,0.5)","rgba(214, 49, 34,0.5)")),
    showlegend= FALSE
  )
  base_plot <- layout(
    base_plot,
    shapes = list(
      list(
        type = 'path',
        path = path,
        xref = 'paper',
        yref = 'paper',
        fillcolor = 'rgba(44, 160, 101, 0.5)'
      )
    ),
    annotations = list(
      list(
        xref = 'paper',
        yref = 'paper',
        x = 0.23,
        y = 0.45,
        showarrow = FALSE,
        text = number.main
      )
    )
  )
  return(base_plot)
}

gauge.meter.flexdash <-function(statistic = statistic,
                       abs.value=value.to.display,
               statistic.min=-10,
               statistic.max=10,
               symbol='%',
               label.main='group',
               divert.if.zero=TRUE,
               min.if.zero=0,
               max.if.zero=100){
  
  if (divert.if.zero==F|abs.value!=0){
  range = statistic.max-statistic.min
  
  min=0
  max= round((abs.value*range)/(statistic-statistic.min),0)
  
  
  success = c(statistic.min,(max/3)+statistic.min)
  warning = c((max/3)+statistic.min,((max/3)*2)+statistic.min)
  danger = c(((max/3)*2)+statistic.min,statistic.max)
  
  
  g <- gauge(abs.value, min = min, max = max, symbol = symbol, 
      gaugeSectors(success = success, warning = warning, danger = danger),
      label=label.main)
  }else{
    g <- gauge(abs.value, min = min.if.zero, max = max.if.zero, symbol = symbol, 
               gaugeSectors(success = c(min.if.zero,round((max.if.zero/3))), warning = c(round((max.if.zero/3)),round((max.if.zero/3)*2)), danger = c(round((max.if.zero/3)*2),max.if.zero)),
               label=label.main)
  }
  
  
  
  return(g)
  
}


TS.barplot.pg.ewma <- function(series = series.pg,
                                   lambda = 0.6,
                                   plot.range = plot.range.week,
                                   index.dates = index.dates.week,
                                   ylabel = 'Number of sows',
                                   xlabel = 'Week'
                                   
                              ){
  
  ewma.series <- apply(series,1,sum,na.rm=T)
  
  ewma.series <- ewma.series[plot.range]
  ewma.results <- ewma(ewma.series,lambda=lambda)
  
  
  data =  as.data.frame(series[plot.range,])
  x= paste(index.dates$ISOweekYear[plot.range],
           index.dates$week[plot.range],sep="-")
  y1 = data$gilt
  y2 = data$young
  y3 = data$prime
  y4 = data$mature
  y = ewma.series
  
  t1 = 'gilts'
  t2 = 'young'
  t3 = 'prime'
  t4 = 'mature'
  
  color1=color.pg[1]
  color2=color.pg[2]
  color3=color.pg[3]
  color4=color.pg[4]
  
  #lcl = ewma.results$limits[,1]
  #lcl.label = 'expectation - lower limit'
  ucl = ewma.results$limits[,2]
  ucl.label = 'expectation - higher limit'
  x.dots = ewma.results$violations
  y.dots = y[ewma.results$violations]
  dots.label = 'unexpected observations'
  yrange = c(min(y,na.rm=T),max(c(y,ewma.results$limits[,2]),na.rm=T))
  
  ylabel = ylabel
  xlabel = xlabel
  
  
  service.p2 <-
    plot_ly() %>%
    add_trace(x=x,y = y1,name=t1,marker=list(color=color1),type="bar") %>%
    add_trace(x=x,y = y2,name=t2,marker=list(color=color2),type="bar") %>%
    add_trace(x=x,y = y3,name=t3,marker=list(color=color3),type="bar") %>%
    add_trace(x=x,y = y4,name=t4,marker=list(color=color4),type="bar") %>%
    #  add_lines(
    #    x=x,
    #    y=lcl,
    #    name=lcl.label
    #  )%>%
    add_lines(
      x=x,
      y=ucl,
      name=ucl.label,
      line=list(color = '#fc2821'), showlegend = FALSE
    )%>%
    add_markers(
      x=x[x.dots],
      y = y.dots,
      name=dots.label,
      marker=list(color = '#fc2821',symbol="star",size=10), showlegend = FALSE
    ) %>%
    layout(yaxis = list(title = ylabel),
           xaxis = list(title = xlabel),
           barmode = 'stack',
           legend=list(orientation="h",
                       x=0.25,y=max(y,na.rm=T)))
}
