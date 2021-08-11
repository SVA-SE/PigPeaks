# For weekly time-series ----

TS.barplot <- function(series = series,
                       indicator.label="indicator",
                       show.window = weeks.to.show,
                       index.dates = index.dates.week,
                       ylabel = 'Number of sows',
                       xlabel = 'Week',
                       target.vector = NULL,
                       target.vector.UCL = NULL, #Added
                       target.vector.LCL = NULL, #Added
                       alarms.ewma = NULL,       #Added
                       alarms.shew = NULL,       #Added
                       shading.matrix = NULL,
                       limits = NULL,
                       series.label="sows"
){
 
  plot.range <- max(1,(length(series)-show.window+1)):length(series)
  
  y =  series[plot.range]
  
  x=index.dates$start[plot.range]

  x.week=paste(index.dates$ISOweekYear[plot.range],index.dates$week[plot.range],sep="-")
  
  t = series.label #Added
  
  #labels
  
  text=str_c(indicator.label,":",y,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  
  target.vector = target.vector[plot.range]
  target.vector.UCL = target.vector.UCL[plot.range] #Added
  target.vector.LCL = target.vector.LCL[plot.range] #Added
  
  plot <-
    plot_ly()
  
  plot <- plot %>%
    add_trace(x=x,y = y,type='bar', name=series.label,
              text = text, hoverinfo = 'text') %>%
    layout(yaxis = list(title = ylabel, overlaying = "y", range = c(max(0,min(y,na.rm=T)-1), max(y,na.rm=T))), #Changed from range = c(min(y,na.rm=T), max(y,na.rm=T)))
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
        name='Target', 
        line=list(color = '#32cd32'), showlegend = TRUE
      )
  }
  
  
  if(!is.null(target.vector.UCL)){    #Added 
    if(sum(target.vector.UCL,na.rm=T)>0){
    plot <- plot %>%
      add_lines(
        x=x,
        y=target.vector.UCL,
        name='UCL',
        line=list(color = '#ff0000'), showlegend = TRUE
      )
    }
  }
  
  
  if(!is.null(target.vector.LCL)){      #Added
    plot <- plot %>%
      add_lines(
        x=x,
        y=target.vector.LCL,
        name='LCL',
        line=list(color = '#800080'), showlegend = TRUE
      )
  }
  
  # if(!is.null(alarms)){                #Added
  #   alarms <- replace(alarms, alarms==0, NA)
  #   alarmsPOS<- replace(alarms, alarms<=0, NA)
  #   alarmsNEG<- replace(alarms, alarms>=0, NA)
  #   if (!is.null(target.vector.UCL)){
  #     plot <- plot %>%
  #       add_markers(x = x, y = y, yaxis="y2", size = ~abs(alarmsPOS), colors = '#ff0000',
  #                   sizes = c(20,50,100),
  #                   marker = list(opacity = 0.5, sizemode = 'area',  sizeref = 0.2),
  #                   text = str_c('Alarm:', alarmsPOS),
  #                   name='Alarm UCL')
  #   }
  #   if (!is.null(target.vector.LCL)){
  #     plot <- plot %>%
  #       add_markers(x = x, y = y, yaxis="y2", size = ~abs(alarmsNEG), colors = '#800080',
  #                   sizes = c(20,50,100),
  #                   marker = list(opacity = 0.5, sizemode = 'area',  sizeref = 0.2),
  #                   text = str_c('Alarm:', alarmsNEG),
  #                   name='Alarm LCL')
  #   }
  # }
  
  if(!is.null(alarms.ewma)){          #Added
    for(a in 1:length(alarms.ewma)) {
      if (!is.null(target.vector.UCL)) {
        if(alarms.ewma[a]==1){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list(opacity = 0.5, color = '#ff0000',  size = 10, sizemode = 'area'),
                        text = str_c('Score:', alarms.ewma[a]),
                        name ='Alarm EWMA',
                        showlegend = FALSE)
        }
        if(alarms.ewma[a]==2){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list(opacity = 0.5, color = '#ff0000', size = 20, sizemode = 'area'),
                        text = str_c('Score:', alarms.ewma[a]),
                        name ='Alarm EWMA',
                        showlegend = FALSE)
        }
        if(alarms.ewma[a]==3){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list(opacity = 0.5, color = '#ff0000', size = 30, sizemode = 'area'),
                        text = str_c('Score:', alarms.ewma[a]),
                        name ='Alarm EWMA',
                        showlegend = FALSE)
        }
      }
      if (!is.null(target.vector.LCL)) {
        if(alarms.ewma[a]==-1){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list(opacity = 0.5, color = '#800080',  size = 10, sizemode = 'area'),
                        text = str_c('Score:', alarms.ewma[a]),
                        name ='Alarm EWMA',
                        showlegend = FALSE)
        }
        if(alarms.ewma[a]==-2){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list(opacity = 0.5, color = '#800080', size = 20, sizemode = 'area'),
                        text = str_c('Score:', alarms.ewma[a]),
                        name ='Alarm EWMA',
                        showlegend = FALSE)
        }
        if(alarms.ewma[a]==-3){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list(opacity = 0.5, color = '#800080', size = 30, sizemode = 'area'),
                        text = str_c('Score:', alarms.ewma[a]),
                        name ='Alarm EWMA',
                        showlegend = FALSE)
          
        }
      }
    } 
  }
  
  
  if(!is.null(alarms.shew)){          #Added
    for(a in 1:length(alarms.shew)) {
      if (!is.null(target.vector.UCL)) {
        if(alarms.shew[a]==1){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list(symbol ='asterisk-open', opacity = 0.5, color = '#ff0000',  size = 10, sizemode = 'area'),
                        text = str_c('Score:', alarms.shew[a]),
                        name ='Alarm Shewhart',
                        showlegend = FALSE)
        }
        if(alarms.shew[a]==2){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list(symbol ='asterisk-open', opacity = 0.5, color = '#ff0000', size = 20, sizemode = 'area'),
                        text = str_c('Score:', alarms.shew[a]),
                        name ='Alarm Shewhart',
                        showlegend = FALSE)
        }
        if(alarms.shew[a]==3){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list(symbol ='asterisk-open', opacity = 0.5, color = '#ff0000', size = 30, sizemode = 'area'),
                        text = str_c('Score:', alarms.shew[a]),
                        name ='Alarm Shewhart',
                        showlegend = FALSE)
        }
      }
      if (!is.null(target.vector.LCL)) {
        if(alarms.shew[a]==-1){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list(symbol ='asterisk-open', opacity = 0.5, color = '#800080',  size = 10, sizemode = 'area'),
                        text = str_c('Score:', alarms.shew[a]),
                        name ='Alarm Shewhart',
                        showlegend = FALSE)
        }
        if(alarms.shew[a]==-2){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list(symbol ='asterisk-open', opacity = 0.5, color = '#800080', size = 20, sizemode = 'area'),
                        text = str_c('Score:', alarms.shew[a]),
                        name ='Alarm Shewhart',
                        showlegend = FALSE)
        }
        if(alarms.shew[a]==-3){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list(symbol ='asterisk-open',opacity = 0.5, color = '#800080', size = 30, sizemode = 'area'),
                        text = str_c('Score:', alarms.shew[a]),
                        name ='Alarm Shewhart',
                        showlegend = FALSE)
          
        }
      }
    }
  }
  
  return(plot)    
}

# For continuous time-series ----

nonTS.barplot.timeless <- function(series = indicator.series,
                                   indicator.label="indicator", 
                                   show.window = nonTS.to.show, #always as number of events
                                   vertical.line = NULL, #VECTOR OF DATES
                                   vertical.line.label=NULL,
                                   index.dates = index.dates.days,
                                   ylabel = 'Time between events',
                                   xlabel = 'Date of event',
                                   target = NULL,
                                   target.UCL = NULL,
                                   target.LCL = NULL,
                                   #alarms = NULL,
                                   alarms.ewma = NULL,    #Added
                                   alarms.shew = NULL,    #Added
                                   target.unit="value", #c("value","vector), defaults to vector
                                   target.label="target",
                                   series.label="sows"
                                      
){
  
  range <- max((dim(series)[1]-show.window+1),1,na.rm=T):(dim(series)[1])
  
  series.range <- series[range,] 
  
  y = series.range[,"observed"]  
  
  x = 1:min(dim(series.range)[1],show.window)
  
  x.dates = as.Date(series.range[,"date"],origin="1970-01-01")

  t = series.label
  
  y = series.range[,"observed"]  
  
  text=str_c(indicator.label,":",y,
              "<br>Week:",date2ISOweek(x.dates),
              "<br>date:",x.dates,
              "<br>sowID:",active.sows.displayID[match(dimnames(individual.sows)[[3]][series.range[,"sowINDEX"]],active.sows.displayID[,"codesID"]),"displayID"])
 
  
  
  target.vector = target
  if(!is.null(target.unit)){
    if(target.unit=="value"){
      target.vector <- rep(target,length(x))
    }}
  
  
  target.vector.UCL = target.UCL  #Added 
  if(!is.null(target.unit)){
    if(target.unit=="value"){
      target.vector.UCL <- rep(target.UCL,length(x))
    }}
  
  
  target.vector.LCL = target.LCL  #Added
  if(!is.null(target.unit)){
    if(target.unit=="value"){
      target.vector.LCL <- rep(target.LCL,length(x))
    }}
  
  
  plot <-
    plot_ly(x=x)
  
  
  plot <- plot %>%
    add_trace(x=x,y = y,name=t,type='bar',
              text = text, hoverinfo = 'text') %>%
    layout(yaxis = list(side = 'left', title = ylabel, range = c(max(0,min(y,na.rm=T)-1), max(y,na.rm=T))), #Changed from range = c(min(y,na.rm=T), max(y,na.rm=T))),
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
        name='Target',
        line=list(color = '#32cd32'), name=target.label
      )
  }
  
  
  if(!is.null(target.UCL)){  #Added
    plot <- plot %>%
      add_lines(
        x=x,
        y=target.vector.UCL,  #Added
        name='UCL',
        line=list(color = '#ff0000'), name=target.label
      )
  }
  
  
  if(!is.null(target.LCL)){  #Added
    plot <- plot %>%
      add_lines(
        x=x,
        y=target.vector.LCL,
        name='LCL',
        line=list(color = '#800080'), name=target.label
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
  
  # if(!is.null(alarms)){  #Added
  #   alarms <- replace(alarms, alarms==0, NA)
  #   alarmsPOS<- replace(alarms, alarms<=0, NA)
  #   alarmsNEG<- replace(alarms, alarms>=0, NA)
  #   if (!is.null(target.UCL)){
  #       plot <- plot %>%
  #       add_markers(x = x, y = y, size = ~abs(alarmsPOS), colors = '#ff0000',
  #                sizes = c(20,50,100),
  #                marker = list(opacity = 0.5, sizemode = 'area',  sizeref = 0.2),
  #                text = str_c('Alarm:', alarmsPOS),
  #                name='Alarm UCL')
  #   }
  #   if (!is.null(target.LCL)){
  #     plot <- plot %>%
  #       add_markers(x = x, y = y, size = ~abs(alarmsNEG), colors = '#800080',
  #                   sizes = c(20,50,100),
  #                   marker = list(opacity = 0.5, sizemode = 'area',  sizeref = 0.2),
  #                   text = str_c('Alarm:', alarmsNEG),
  #                   name='Alarm LCL')
  #   }
  # }

  if(!is.null(alarms.ewma)){          #Added
    for(a in 1:length(alarms.ewma)) {
      if (!is.null(target.UCL)) {
        if(alarms.ewma[a]==1){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list(opacity = 0.5, color = '#ff0000',  size = 10, sizemode = 'area'),
                        text = str_c('Score:', alarms.ewma[a]),
                        name ='Alarm EWMA',
                        showlegend = FALSE)
      }
      if(alarms.ewma[a]==2){
        plot <- plot %>%
          add_markers(x = x[a], y = y[a],
                      marker = list(opacity = 0.5, color = '#ff0000', size = 20, sizemode = 'area'),
                      text = str_c('Score:', alarms.ewma[a]),
                      name ='Alarm EWMA',
                      showlegend = FALSE)
      }
      if(alarms.ewma[a]==3){
        plot <- plot %>%
          add_markers(x = x[a], y = y[a],
                      marker = list(opacity = 0.5, color = '#ff0000', size = 30, sizemode = 'area'),
                      text = str_c('Score:', alarms.ewma[a]),
                      name ='Alarm EWMA',
                      showlegend = FALSE)
      }
      }
      if (!is.null(target.LCL)) {
        if(alarms.ewma[a]==-1){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list(opacity = 0.5, color = '#800080',  size = 10, sizemode = 'area'),
                        text = str_c('Score:', alarms.ewma[a]),
                        name ='Alarm EWMA',
                        showlegend = FALSE)
        }
        if(alarms.ewma[a]==-2){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list(opacity = 0.5, color = '#800080', size = 20, sizemode = 'area'),
                        text = str_c('Score:', alarms.ewma[a]),
                        name ='Alarm EWMA',
                        showlegend = FALSE)
        }
        if(alarms.ewma[a]==-3){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list(opacity = 0.5, color = '#800080', size = 30, sizemode = 'area'),
                        text = str_c('Score:', alarms.ewma[a]),
                        name ='Alarm EWMA',
                        showlegend = FALSE)

       }
      }
    }
}
  
  
  if(!is.null(alarms.shew)){          #Added
    for(a in 1:length(alarms.shew)) {
      if (!is.null(target.UCL)) {
        if(alarms.shew[a]==1){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list( symbol ='asterisk-open', opacity = 0.5, color = '#ff0000',  size = 10, sizemode = 'area'),
                        text = str_c('Score:', alarms.shew[a]),
                        name ='Alarm Shewhart',
                        showlegend = FALSE)
        }
        if(alarms.shew[a]==2){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list( symbol ='asterisk-open', opacity = 0.5, color = '#ff0000', size = 20, sizemode = 'area'),
                        text = str_c('Score:', alarms.shew[a]),
                        name ='Alarm Shewhart',
                        showlegend = FALSE)
        }
        if(alarms.shew[a]==3){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list( symbol ='asterisk-open', opacity = 0.5, color = '#ff0000', size = 30, sizemode = 'area'),
                        text = str_c('Score:', alarms.shew[a]),
                        name ='Alarm Shewhart',
                        showlegend = FALSE)
        }
      }
      if (!is.null(target.LCL)) {
        if(alarms.shew[a]==-1){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list( symbol ='asterisk-open', opacity = 0.5, color = '#800080',  size = 10, sizemode = 'area'),
                        text = str_c('Score:', alarms.shew[a]),
                        name ='Alarm Shewhart',
                        showlegend = FALSE)
        }
        if(alarms.shew[a]==-2){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list( symbol ='asterisk-open', opacity = 0.5, color = '#800080', size = 20, sizemode = 'area'),
                        text = str_c('Score:', alarms.shew[a]),
                        name ='Alarm Shewhart',
                        showlegend = FALSE)
        }
        if(alarms.shew[a]==-3){
          plot <- plot %>%
            add_markers(x = x[a], y = y[a],
                        marker = list( symbol ='asterisk-open', opacity = 0.5, color = '#800080', size = 30, sizemode = 'area'),
                        text = str_c('Score:', alarms.shew[a]),
                        name ='Alarm Shewhart',
                        showlegend = FALSE)
          
        }
      }
    }
  }  
  
  
  # if(!is.null(alarms)){    #Added
  #     if(!is.null(target.UCL)) {
  #       for(a in 1:length(alarms)) {
  #         if(alarms[a]==1|alarms[a]==2|alarms[a]==3){
  #             plot <- plot %>%
  #             add_markers(x = x[a], y = y[a], size = ~abs(alarms), color = '#ff0000',
  #                         marker = list(opacity = 0.5, sizemode = 'area', sizeref = 0.5),
  #                         text = text2,
  #                         name='Alarm UCL')
  # 
  #     }
  #       }
  #     }
  #     if(!is.null(target.LCL)) {
  #       for(a in 1:length(alarms)) {
  #        if(alarms[a]==-1|alarms[a]==-2|alarms[a]==-3){
  #             plot <- plot %>%
  #             add_markers(x = x[a], y = y[a], size = ~abs(alarms), color = '#800080',
  #                         marker = list(opacity = 0.5, sizemode = 'area', sizeref = 0.5),
  #                         text = text2,
  #                         name='Alarm LCL')
  # 
  #     }
  #   }
  #   }
  #     }

  
  return(plot)    
}

