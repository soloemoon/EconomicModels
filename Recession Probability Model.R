if (!require("dplyr")) {install.packages("dplyr"); library('dplyr')}
if (!require("forecast")) {install.packages("forecast"); library('forecast')}
if (!require("lubridate")) {install.packages("lubridate"); library('lubridate')}
if (!require("ggplot2")) {install.packages("ggplot2"); library('ggplot2')}
if (!require("fredr")) {install.packages("fredr"); library('fredr')}
if (!require("ecoMoon")) {install.packages("ecoMoon"); library('ecoMoon')}
if (!require("ggthemes")) {install.packages("ggthemes"); library('ggthemes')}

RP <-eco.download('T10Y3MM','Prob') %>% na.omit() 
#RP <-tail(RP, 60)
RP$Prob <-pnorm(-0.5333 - 0.6330 * RP$Prob) * 100
RP$Prob <- round(RP$Prob,2)
RP$ForecastDate <-RP$date + 365
  
RP <-subset(RP, RP$date >= as.Date('2000-01-01'))

# Generate Plot
start <- RP$date[which(diff(RP$recession) == 1)]
end <-RP$date[which(diff(RP$recession) == -1)]

if(length(start)>length(end)){
  end<-c(end, tail(RP$date,1))
  }
if(length(end)>length(start)){
  start<-c(min(RP$date), start)
  }
      recession.plot <-ggplot()+
        geom_line(aes(x=RP$date, y=RP$Prob,colour = '#014d64'),size=.7)+
        labs(subtitle='Based on 3 Month Treasury Spread', y='%',x='',title='Recession Probability',caption='Source: Treasury Model')+
        scale_x_date(date_breaks = '1 year',labels = date_format('%y'))+ 
        scale_color_manual(values = c('#014d64'), labels = c('12M Recession Probability')) + 
        geom_rect(data=recession.df,aes(xmin=start,xmax=end, ymin=-Inf,ymax=max(RP$Prob)),alpha=.3,color='grey80')+
        theme_economist_white(gray_bg = FALSE)+
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size=15),
              axis.text.y = element_text(size=15),
              axis.title = element_text(size=15),
              plot.caption = element_text(size=13),
              legend.text = element_text(size = 15))

      recession.plot      
