library(neuralnet)

Table<-read.csv2("/Users/jas/Desktop/Data Set/table.csv",sep = ",")

TableData<-Table

TableData$Date<-NULL

View(TableData)

m<-model.matrix(~Volume+Open+High+Low+Close,data = TableData)

head(m)

TableData2<-neuralnet(High ~ Open+Low+Close,data = TableData,algorithm ="rprop+",hidden = 2,learningrate = 0.5,linear.output = FALSE)



View(infert)

Neural<-neuralnet(case~age+parity+induced+spontaneous,data = infert,algorithm ="backprop",hidden = 2,learningrate = 0.5,linear.output = FALSE)



Neural2<-neuralnet(case~age+parity+induced+spontaneous,data = infert,algorithm ="rprop+",hidden = 2,learningrate = 0.5,act.fct ="logistic",err.fct = "ce",linear.output = FALSE)


plot(Neural2)
Neural2$net.result

Neural12 = ifelse(Neural2$net.result[[1]]>0.5,1,0)

Error1<-mean(infert$case != Neural12)
Error1
Neural$net.result[[1]]

Neural1 = ifelse(Neural$net.result[[1]]>0.5,1,0)

Error<-mean(infert$case != Neural1)

Error
plot(Neural)

?"neuralnet"

WeatherData<-read.csv2("/Users/jas/Desktop/weather.csv",sep = ",")
View(WeatherData)
  
WeatherDataMod<-WeatherData
dim(WeatherData)
WeatherDataMod$Date<-NULL
WeatherDataMod$ChangeTempDir<-NULL
WeatherDataMod$ChangeTempMag<-NULL
WeatherDataMod$ChangeWindDirect<-NULL
WeatherDataMod$MaxWindPeriod<-NULL
WeatherDataMod$PressureChange<-NULL
WeatherDataMod$Pressure9am<-NULL
WeatherDataMod$Pressure3pm<-NULL

View(WeatherDataMod)

head(WeatherDataMod)

str(WeatherDataMod)

dataaa<-model.matrix(~RainTomorrow+MaxWindSpeed+RelHumid9am+Cloud9am+WindSpeed9am+RelHumid3pm+Cloud3pm+WindSpeed3pm+RainToday,data = WeatherDataMod)
 
WeatherReport<-neuralnet(RainTomorrow~MaxWindSpeed+RelHumid9am+Cloud9am+WindSpeed9am+RelHumid3pm+Cloud3pm+WindSpeed3pm+RainToday,data = dataaa,algorithm ="rprop+",hidden = 5,err.fct = "ce",linear.output = FALSE)

plot(WeatherReport)

ErrorNet<-ifelse(WeatherReport$net.result[[1]] > 0.5,1,0)

ErrorNet

ErrorMean<-mean(WeatherDataMod$RainTomorrow != ErrorNet)
  
length(ErrorNet)
  
length(WeatherDataMod$RainTomorrow)

WeatherReport$net.result





