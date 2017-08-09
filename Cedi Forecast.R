dir()
datass <- read.csv("cediusd_2007_2017.csv")

st= as.Date("2007/6/1")
end = as.Date("2017/5/1")
month_year <-
  data.frame(
    date=seq(st,end, by = "month"),
    var0 = c(datass$BID),
    var1 = c(datass$ASK)
  )

tsAud <- ts(month_year$var0, start=c(2007, 6), end=c(2017, 5), frequency=11, col)
plot.ts(tsAud)

tsAudComp <- decompose(tsAud)
dev.new()
plot.ts(tsAudComp$seasonal)
dev.new()
plot.ts(tsAudComp$trend) 

tsAudSeasonalAdjusted <- tsAud -tsAudComp$seasonal
dev.new()
plot.ts(tsAudSeasonalAdjusted )

library(forecast)
audHW <-HoltWinters(tsAudSeasonalAdjusted ,beta=FALSE,gamma=FALSE)
plot(audHW)
audHWforecast <- forecast.HoltWinters(audHW,h=5)
plot.forecast(audHWforecast)

hist(audHWforecast$residuals, col="yellow")
dnormResiduals <- dnorm(audHWforecast$residuals,mean=mean(audHWforecast$residuals), sd=sd(audHWforecast$residuals) )
dev.new()
plot(audHWforecast$residuals,dnormResiduals,col="blue") 