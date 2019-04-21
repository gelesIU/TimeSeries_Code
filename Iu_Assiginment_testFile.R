# Reading The Data

unempData =read.csv(url("https://data.ny.gov/api/views/5hyu-bdh8/rows.csv?accessType=DOWNLOAD"),header=TRUE, stringsAsFactors = TRUE)

# Fixing the Date into a YYYY-MM format
unempData$Date <- as.Date(paste(unempData$Year, unempData$Month, sep="-"), "%Y-%M")
# Droping the redundant Year and month 
unempData <- subset(unempData, select=-c(Year,Month))
# odering the Data
unempData <- unempData[with(unempData,order(Area,Date)),]

summary(unempData)

tail(unempData)

#Creating Time Series Data for Albany City

AC_DATA <- subset(unempData, Area=='Albany City')
AC_DATA_UER <- select(AC_DATA,Unemployment.Rate)
AC_DATA_U <- select(AC_DATA,Unemployed)
AC_DATA_E <- select(AC_DATA,Employed)
AC_DATA_LF <- select(AC_DATA,Labor.Force)

ts_AC<-ts(AC_DATA,start=c(1990,1),end=c(2019,2),frequency=12)
ts_AC_UER<-ts(AC_DATA_UER,start=c(1990,1),end=c(2019,2),frequency=12)
ts_AC_U<-ts(AC_DATA_U,start=c(1990,1),end=c(2019,2),frequency=12)
ts_AC_E<-ts(AC_DATA_E,start=c(1990,1),end=c(2019,2),frequency=12)
ts_AC_LF<-ts(AC_DATA_LF,start=c(1990,1),end=c(2019,2),frequency=12)


library(ggplot2)
head(ts_AC_UER)

print(" Length ")
length(ts_AC)

ggplot(data = AC_DATA, aes(x = Date, y = Unemployment.Rate)) +geom_line( color = "#FC4E07")
ggplot(data = AC_DATA, aes(x = Date, y = Unemployment.Rate)) +geom_line(aes(size = Unemployment.Rate/Labor.Force), color = "#FC4E07")

plot(ts_AC)
plot(ts_AC_UER)

print("Start")
start(ts_AC)
print("end")
end(ts_AC)

print("deltat")
# function returns the fixed time interval between observations
deltat(ts_AC)

print("frequency")
#function returns the number of observations per unit time
frequency(ts_AC)

print("time")
time(ts_AC)

print("cycle")
#cycle() function returns the position in the cycle of each observation
cycle(ts_AC)


print("The Total Missing Values are")
sum( is.na( ts_AC ) )

diff_TSAC_UER <- diff(ts_AC_UER,lag = 4)

# Plot dz
ts.plot(diff_TSAC_UER)  

# View the length of z and dz, respectively
length(diff_TSAC)
length(ts_AC)

UER_model <- arima(diff_TSAC_UER, order = c(0, 0, 0))
mean(diff_TSAC_UER)
var(diff_TSAC_UER)
print("Model")
UER_model
