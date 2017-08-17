library('prophet')
library(readr)
library(lubridate)
df <- read_csv('data.csv')
df$key<-paste0(df$x,'^',df$y,'^',df$z,'^',df$p)
Keys<-unique(df$key)
finalForecasts<-NULL
for(i in 1:length(Keys)){
dataset1=df[df$key==Keys[i],]
dataset1<-subset(dataset1,select=c('date','price'))
dataset1$PRD_DATE<-mdy_hms(dataset1$PRD_DATE)
dataset1$SALESDOLLARS<-as.double(dataset1$SALESDOLLARS)
dataset1<-dataset1[order(dataset1$PRD_DATE),]
names(dataset1)<-c('ds','y')
dataset1$ds<-as.Date(dataset1$ds)
dataset1<-as.data.frame(dataset1)
m <- prophet(dataset1,yearly.seasonality = TRUE)
future <- make_future_dataframe(m, periods = 52,freq = "w")
tail(future)
forecast <- predict(m,future)
forecast$Key<-Keys[i]
finalForecasts<-rbind(finalForecasts,forecast)
}
head(forecast)
plot(m, forecast)
prophet_plot_components(m, forecast)
