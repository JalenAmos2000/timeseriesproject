#=== The Preamble (Fpp3)

install.packages('fpp3')

rm(list= ls())
library(easypackages)
libraries('fpp3','lubridate',"tidyverse","reshape2","foreign","forecast","tseries","lubridate","xts","tsbox","fpp2")

#==== Fpp2 version of Reading the Data
Appl <- read.csv("C:/Users/jalen/Downloads/EXST_7087_FinalProject/Data/Appl2.csv",header = TRUE, sep = ",")

df=Appl%>%filter(Date>=as.Date("2020-07-24"))
Df1 <-ts(df$Close, frequency = 365.25, start=c(2020,07,24))
autoplot(Df1)
p1=ggplot()+
    geom_line(data = df, aes(x = Date, y = Close, group =1)) + 
    ylab('Closing Price')+
    theme_bw()
p1
#===== Fpp3 (Reading the data and plotting the data)
Appl5 <- readr::read_csv("C:/Users/jalen/Downloads/EXST_7087_Project/Data/AA1.csv") 
Appl6 <- Appl5 %>% distinct(Close, .keep_all = TRUE) #If I want to do further analysis with Fpp3, then I will have to make sure the values of Closing price are unique 
Appl6 <- Appl6|>
    mutate(Month = yearmonth(Date))|>
    select(-Date)|>
    as_tsibble(index = Month) 
Appl6 # the Data is collected Monthly 

Appl7 <- tsibble::fill_gaps(Appl6) # Although the data has been collected monthly, there may be  some missing data. As a results, I have to fill in those gaps with NA.

autoplot(Appl7, Close) +
    labs(y = "Close $",
         title = "Monthly Closing Price of Apple Stock Price")

#==== Seasonal Plots (Fpp3)

Appl7 |>
    gg_season(Close, labels = "both") +
    labs(y = "Close $",
         title = "Monthly Closing Price of Apple Stock Price")

Appl7 |>
    gg_season(Close, period = 'year') +
    theme(legend.position ='none') +
    labs(y = "Close $",
         title = "Yearly Closing Price of Apple Stock Price")
 


#===== Lag Plots (Fpp3)



Appl7 |>
    gg_lag(Close, geom = "point") +
    labs(x = "lag(Close, k)")

#=== ACF (Fpp3)


Appl7 |> ACF(Close, lag_max = 9)

Appl7 |>
    ACF(Close) |>
    autoplot() + labs(title="Monthly Closing Price of Apple Stock Price")

#== time series decomposition

# Estimating time series components ( trend, seasonal, and cyclic )

# we are spilting the data up (Remainder is what is leftover) Additive decomposition

# taking logs turns multiplicative into an additive relationship

# STL Decomposition (is only additive)
# Seasonal and Trend Decomposition using Loess
dcmp <- Appl7 |>
    model(stl = STL(Close))
components(dcmp)

# Taking the Log on Close and it is equivalent to the multiplicative 
lg_dcmp <- Appl7 |>
    model(stl = STL(log(Close)))
components(lg_dcmp)

# Trend of the Closing Price 

components(dcmp) |>
    as_tsibble() |>
    autoplot(Close, colour="gray") +
    geom_line(aes(y=trend), colour = "#D55E00") +
    labs(
        y = "Closing Price",
        title = "Monthly Closing Price of Apple Stock"
    )
components(dcmp) |>
    as_tsibble() |>
    autoplot(Close, colour = "gray") +
    geom_line(aes(y=season_adjust), colour = "#0072B2") +
    labs(       y = "Closing Price",
                title = "Monthly Closing Price of Apple Stock")

components(lg_dcmp) |> autoplot()

# Decompositon of the moving average 
# We use the moving average to get an estimate of the trend component 
Appl7 |>
    model(
        classical_decomposition(Close, type = "additive")
    ) |>
    components() |>
    autoplot() +
    labs(title = "Classical additive decomposition of Apple's 
           Closing Price ")

#=== Stationarity and Differencing

Appl7 |> ACF(difference(Close)) |>
    autoplot() + labs(subtitle = "Changes in Apple's  closing stock price")
#unit root test 
Appl7 |>
    mutate(diff_close = difference(Close)) |>
    features(diff_close, unitroot_kpss)

# log and first order difference 
Appl7 |> ACF(difference(log(Close))) |>
    autoplot() + labs(subtitle = "Changes in Apple's log closing stock price")
# ljung_box statistics 
Appl7 |>
    mutate(diff_lgclose = difference(log(Close))) |>
    features(diff_lgclose, ljung_box, lag = 10)

#unit root test 
Appl7 |>
    mutate(diff_close = difference(log(Close))) |>
    features(diff_close, unitroot_kpss)


#=== non-seasonal ARIMA models

# log transformation ARIMA model

fit <- Appl7 |>
    model(ARIMA(log(Close)))
report(fit)

# No transformation 

Fit <- Appl7 |>
    model(ARIMA(Close))
report(Fit)

# Forecasting 

library(fable)

Naive_fit <- Appl7 |>
        model(NAIVE(log(Close))) |>
        forecast(h = 30) |>
        autoplot(Appl7) +
        labs(title="Apple's monthly closing stock price", y="$US" )
Naive_fit

#== Forecasting an Arima model

ARIMA_Fit <- Appl7 |>
        model(ARIMA(log(Close))) |>
        forecast(h="3 years") |>
        autoplot(Appl7) +
        labs(title = "Apple's Monthly Closing Price",
             y = "Closing Price")
ARIMA_Fit

#=== Forecasting with decomposition

fit_dcmp <- Appl7 |>
    model(stlf = decomposition_model(
        STL(log(Close) ~ trend(window = 7), robust = TRUE),
        NAIVE(season_adjust)
    ))

fit_dcmp |>
    forecast() |>
    autoplot(Appl7)+
    labs(y = "Closing Price",
         title = "Apple's Stock")

#=== Forecasting using Neural Networks
Apple <- Appl7 |> as_tsibble()

fit1 <- Apple |>
    model(NNETAR(log(Close)))
fit1 |>
    forecast(h = 5) |>
    autoplot(Apple) +
    labs(x = "month", y = "Closing Price", title = "Monthly Closing Price")

fit1 |>
    generate(times = 4, h = 30) |>
    autoplot(.sim) +
    autolayer(Apple, Close) +
    theme(legend.position = "none")