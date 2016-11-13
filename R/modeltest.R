#data <- read.csv("cabletheft_ts.csv", header=TRUE, stringsAsFactors=FALSE)
#data$Date <- as.Date(data$Date, "%d/%m/%Y")
#ts_data <- xts(data$Count, order.by = data$Date)
#plot(ts_data)

data <- "Date Incidents NumberProtests NumberPatrols
11/30/2014   132   3   2
12/31/2014   152  1  3
1/31/2015   156  2  4
2/28/2015   145  4  2
3/31/2015   136  3  2
4/30/2015   126  2  4
5/31/2015   125  3  6
6/30/2015   143  1  7
7/31/2015   115  2  6
8/31/2015   121  2  3
9/30/2015   127  1  6
10/31/2015   121  3  5
11/30/2015   118  1  8
12/31/2015   109  1  9"
library(xts)
# use fill=TRUE because you only provided data for 4 composites
allcomp <- read.table(text=data, header=TRUE, fill=TRUE)
# create an xts object from the remaining data
allcomp$Date <- as.Date(allcomp[,1], "%m/%d/%Y")
allcomp_xts <- xts(allcomp[,-1], as.Date(allcomp[,1], "%m/%d/%Y"))
plot(allcomp_xts$Incidents)
test_regressors <- xts(allcomp_xts[,2:3], order.by = allcomp$Date)
#library(forecast)
#auto.arima(allcomp_xts$Incidents)
#model_fit <- Arima(allcomp_xts$Incidents, xreg=test_regressors, order = c(0,1,1), seasonal = list(order = c(1,1,0)))
#our_forecast <- predict(model_fit, newxreg=test_regressors, n.ahead = 2)
#allcomp$Date <- as.Date(allcomp$Date, "%d/%m/%Y")
#forecasted_viewers <- xts(our_forecast$pred,allcomp$Date)
#plot.xts(allcomp_xts,main="")
#lines(forecasted_viewers, col=c("red"))

fit <- arima(allcomp_xts$Incidents, order=c(1,0,0), list(order=c(2,1,0)))
fore <- predict(fit, n.ahead=2)
# error bounds at 95% confidence level
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
ts.plot(ts(allcomp_xts$Incidents), fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), col=c(1,2,4), lty=c(1,1,2))

update_model <- function(title = "Predict next 2 months incidents"){
  data <- "Date Incidents NumberProtests NumberPatrols
11/30/2014   132   3   2
  12/31/2014   152  1  3
  1/31/2015   156  2  4
  2/28/2015   145  4  2
  3/31/2015   136  3  2
  4/30/2015   126  2  4
  5/31/2015   125  3  6
  6/30/2015   143  1  7
  7/31/2015   115  2  6
  8/31/2015   121  2  3
  9/30/2015   127  1  6
  10/31/2015   121  3  5
  11/30/2015   118  1  8
  12/31/2015   109  1  9"
  library(xts)
  # use fill=TRUE because you only provided data for 4 composites
  allcomp <- read.table(text=data, header=TRUE, fill=TRUE)
  # create an xts object from the remaining data
  allcomp$Date <- as.Date(allcomp[,1], "%m/%d/%Y")
  allcomp_xts <- xts(allcomp[,-1], as.Date(allcomp[,1], "%m/%d/%Y"))

  fit <- arima(allcomp_xts$Incidents, order=c(1,0,0), list(order=c(2,1,0)))
  fore <- predict(fit, n.ahead=2)
  # error bounds at 95% confidence level
  U <- fore$pred + 2*fore$se
  L <- fore$pred - 2*fore$se
  ts.plot(ts(allcomp_xts$Incidents), fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
  legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), col=c(1,2,4), lty=c(1,1,2))
  
}


