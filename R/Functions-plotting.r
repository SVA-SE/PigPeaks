packages <- c("plotly", "RColorBrewer")
install.packages(setdiff(packages, rownames(installed.packages())))

require(plotly)
require(RColorBrewer)


#  For weekly time-series ----
  
  TS.barplot <- function(#indicator = indicator,                #indicator=indicators.data$abortions.week
                         df.indicator = df.indicator,          #df.indicator=indicators.time.series$`abortions per week`
                         indicator.label = indicator.label,
                         show.window = weeks.to.show,
                         index.dates = index.dates.week,
                         ylabel = "Number of sows",
                         xlabel = "Week",
                         target = target,  
                         target.unit = target.unit,             #c("value","vector"), defaults to vector
                         UCL.EWMA = UCL.EWMA,                   #Added
                         LCL.EWMA = LCL.EWMA,                   #Added
                         UCL.SHEW = UCL.SHEW,                   #Added
                         LCL.SHEW = LCL.SHEW,                   #Added
                         alarms.EWMA.UPP = alarms.EWMA.UPP,     #Added
                         alarms.EWMA.LW = alarms.EWMA.LW,       #Added
                         alarms.SHEW.UPP = alarms.SHEW.UPP,     #Added
                         alarms.SHEW.LW = alarms.SHEW.LW,       #Added
                         series.label = series.label
  ){

    series <- df.indicator$observed
    
    #data.weekly.removed <- dim(indicator)[1]-dim(df.indicator)[1]
    
    plot.range <- max(1,(length(series)-show.window+1)):length(series)
    
    #index.dates <- index.dates[data.weekly.removed:(dim(indicator)[1]),]
    
    #plot.range.dates <- (data.weekly.removed)+(max(1,(length(series)-show.window+1)):length(series))
    
    y = series[plot.range]
    
    x = index.dates$start[plot.range]
    
    x.week = paste(index.dates$ISOweekYear[plot.range],index.dates$week[plot.range],sep="-")
    
    t = series.label #Added
    
    #labels
    
    text=str_c(indicator.label,":",y,
               "<br>Week:",x.week,
               "<br>WeekMonday:",x)
    
    
    target.vector = target
    if(!is.null(target.unit)){
      if(target.unit=="value"){
        target.vector <- rep(target,length(x))
      }}
    

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
    
    
    if(!is.null(target)){
      plot <- plot %>%
        add_lines(
          x=x,
          y=target.vector,
          name='Target',
          line=list(color = '#32cd32'), showlegend = TRUE
        )
    }
    
    if(isTRUE(UCL.EWMA)){          #Added 
        plot <- plot %>%
          add_lines(
            x=x,
            y=df.indicator$`UCL EWMA`[plot.range],
            name='UCL EWMA',
            line=list(color = '#ff0000'), showlegend = TRUE
          )
      }
    
    
    if(isTRUE(LCL.EWMA)){          #Added 
        plot <- plot %>%
          add_lines(
            x=x,
            y=df.indicator$`LCL EWMA`[plot.range],
            name='LCL EWMA',
            line=list(color = '#800080'), showlegend = TRUE
          )
      }
    
    
    if(isTRUE(UCL.SHEW)){          #Added
      plot <- plot %>%
        add_lines(
          x=x,
          y=df.indicator$`UCL Shewhart`[plot.range],
          name='UCL SHEW',
          line=list(color = '#ff0000', dash="dot"), showlegend = TRUE
        )
    }
    
    if(isTRUE(LCL.SHEW)){          #Added
      plot <- plot %>%
        add_lines(
          x=x,
          y=df.indicator$`LCL Shewhart`[plot.range],
          name='LCL SHEW',
          line=list(color = '#800080',dash="dot"), showlegend = TRUE
        )
    }

    
    alarms.ewma <- tail(df.indicator$`alarms EWMA`, show.window)
    alarms.shew <- tail(df.indicator$`alarms Shewhart`, show.window)
    
    
    if(isTRUE(alarms.EWMA.UPP)){          #Added
      
      for(a in 1:length(alarms.ewma)) {           
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
    }
    
        if (isTRUE(alarms.EWMA.LW)) {
          
          for(a in 1:length(alarms.ewma)) {           
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
    
    if(isTRUE(alarms.SHEW.UPP)){          #Added
      
      for(a in 1:length(alarms.shew)) {
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
    }
        if (isTRUE(alarms.SHEW.LW)) {
          
          for(a in 1:length(alarms.shew)) {
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
    
    return(plot)    
  }
  


#  For continuous time-series ----

nonTS.barplot.timeless <- function(df.indicator = df.indicator,       #df.indicator=indicators.time.series$`days between farrowings`
                                   indicator.label = indicator.label, 
                                   show.window = nonTS.to.show,
                                   index.dates = index.dates.days,
                                   ylabel = 'Time between events',
                                   xlabel = 'Last 100 events',
                                   target = target,
                                   target.unit = target.unit,                  #c("value","vector"), defaults to vector
                                   UCL.EWMA = UCL.EWMA,                        #Added
                                   LCL.EWMA = LCL.EWMA,                        #Added
                                   UCL.SHEW = UCL.SHEW,                        #Added
                                   LCL.SHEW = LCL.SHEW,                        #Added
                                   alarms.EWMA.UPP = alarms.EWMA.UPP,          #Added
                                   alarms.EWMA.LW = alarms.EWMA.LW,            #Added
                                   alarms.SHEW.UPP = alarms.SHEW.UPP,          #Added
                                   alarms.SHEW.LW = alarms.SHEW.LW,            #Added
                                   series.label = series.label,
                                   vertical.line = vertical.line,              #vector of dates
                                   vertical.line.label= vertical.line.label
                                   
){
  range <- max((dim(df.indicator)[1]-show.window+1),1,na.rm=T):(dim(df.indicator)[1])
  
  series.range <- df.indicator[range,] 
  
  y = series.range[,"observed"]  
  
  x = 1:min(dim(series.range)[1],show.window)
  
  x.dates = series.range[,"date"]
  
  t = series.label
  
  text=str_c(indicator.label,":",y,
             "<br>Week:",date2ISOweek(x.dates),
             "<br>date:",x.dates,
             "<br>sowID:",active.sows.displayID[match(dimnames(individual.sows)[[3]][series.range[,"sowINDEX"]],active.sows.displayID[,"codesID"]),"displayID"])
    #improve sowID
  
  
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
        line=list(color = '#32cd32'), showlegend = TRUE
      )
  }
  
  if(isTRUE(UCL.EWMA)){          #Added 
    plot <- plot %>%
      add_lines(
        x=x,
        y=series.range[,"UCL EWMA"],
        name='UCL EWMA',
        line=list(color = '#ff0000'), showlegend = TRUE
      )
  }
  
  
  if(isTRUE(LCL.EWMA)){          #Added 
    plot <- plot %>%
      add_lines(
        x=x,
        y=series.range[,"LCL EWMA"],
        name='LCL EWMA',
        line=list(color = '#800080'), showlegend = TRUE
      )
  }
  
  
  if(isTRUE(UCL.SHEW)){          #Added
    plot <- plot %>%
      add_lines(
        x=x,
        y=series.range[,"UCL Shewhart"],
        name='UCL SHEW',
        line=list(color = '#ff0000', dash="dot"), showlegend = TRUE
      )
  }
  
  if(isTRUE(LCL.SHEW)){          #Added
    plot <- plot %>%
      add_lines(
        x=x,
        y=series.range[,"LCL Shewhart"],
        name='LCL SHEW',
        line=list(color = '#800080',dash="dot"), showlegend = TRUE
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
  
  alarms.ewma <- tail(df.indicator$`alarms EWMA`, show.window)
  alarms.shew <- tail(df.indicator$`alarms Shewhart`, show.window)
  
  
  if(isTRUE(alarms.EWMA.UPP)){          #Added
    
    for(a in 1:length(alarms.ewma)) {
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
  }
  
  if(isTRUE(alarms.EWMA.LW)){          #Added
    
        for(a in 1:length(alarms.ewma)) {
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
 
  if(isTRUE(alarms.SHEW.UPP)){          #Added
    
    for(a in 1:length(alarms.shew)) {
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
  }
  
  if(isTRUE(alarms.SHEW.LW)){          #Added
    
    for(a in 1:length(alarms.shew)) {
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
  
  return(plot)    
}



# For exit category weekly indicators non-sys ----

TS.exit <- function(#indicator = indicator,            #indicator =indicators.data$exit.after.event.week
                    df.indicator = df.indicator,      #df.indicator=indicators.time.series$`exit after event, weekly`
                    indicator.label = indicator.label,
                    show.window = weeks.to.show,
                    index.dates = index.dates.week,
                    ylabel = 'Number of sows',
                    xlabel = 'Week',
                    target = target,  
                    target.unit = target.unit,        #c("value","vector"), defaults to vector                    
                    shading.matrix = shading.matrix,
                    limits = limits,
                    group.labels = group.labels
                    
){ 
  #data.weekly.removed <- dim(indicator)[1]-dim(df.indicator)[1]
  
  plot.range <- max(1,(dim(df.indicator)[1]-show.window+1)):dim(df.indicator)[1]
  
  #index.dates = index.dates[data.weekly.removed:(dim(indicator)[1]),]
  
  #plot.range.dates <- data.weekly.removed+(max(1,(dim(df.indicator)[1]-show.window+1)):dim(df.indicator)[1])
  
  data = as.data.frame(df.indicator[plot.range,])
  
  
  x=index.dates$start[plot.range]
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
  

  target.vector = target
  if(!is.null(target.unit)){
    if(target.unit=="value"){
      target.vector <- rep(target,length(x))
    }}
  
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
  
  
  if(!is.null(target)){
    plot <- plot %>%
      add_lines(
        x=x,
        y=target.vector,
        name='Target', yaxis="y2",
        line=list(color = '#32cd32'), showlegend = FALSE
      )
  }
  
  
  return(plot)    
}



# parity colouring ----
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

