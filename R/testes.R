# teste de produto cartesiano
library(data.table)
A <- c(1,2,3)
B <- factor(c('x', 'y'))
C <- c(0.1,0.5)

# testes de time series
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries <- ts(kings)

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries

x <- as.POSIXct(c("2011-02-01", "2011-02-01", "2011-02-01"))
mo <- strftime(x, "%m")
yr <- strftime(x, "%Y")
amt <- runif(3)
dd <- data.frame(mo, yr, amt)

library(lubridate)
df <- data.frame(
    date = today() + days(1:300),
    x = runif(300)
)
df$my <- floor_date(df$date, "month")

dd.agg <- aggregate(amt ~ mo + yr, dd, FUN = sum)

dd.agg$date <- as.POSIXct(paste(dd.agg$yr, dd.agg$mo, "01", sep = "-")
# nesse caso, para data de acionamento 
# devo totalizar o número de aiconamentos por dia antes
# ABAIXO FUNCIONA
df_acion.dia <- df_tidy

df_acion.dia <-
    df_acion.dia %>%
    select(CONTRATO,DATA.ACION) %>%
    mutate(DIA.ACION = floor_date(df_acion.dia$DATA.ACION, "day")) %>%
    group_by(DIA.ACION) %>%
    summarise(N.ACION = n()) %>%
    arrange(DIA.ACION)
# fazer para dados de acionamento raw
df_raw.dia <- df_acion
df_raw.dia <-
    df_raw.dia %>%
    mutate (DATA.ACION = ymd_hms(DATA.ACION))
            
df_raw.dia <-
    df_raw.dia %>%
    select(CONTRATO,DATA.ACION) %>%
    mutate(DIA.ACION = floor_date(df_raw.dia$DATA.ACION, "day")) %>%
    group_by(DIA.ACION) %>%
    summarise(N.ACION = n()) %>%
    arrange(DIA.ACION)
#v_acion.dia <- df_acion.dia$N.ACION
#ts_acion <- ts(v_acion.dia, frequency=7)
#ts_acion = ts(v_acion.dia,start=c(2014,10,7), frequency=365.25)
#ts_acion = ts(v_acion.dia, frequency=12)
#plot.ts(ts_acion)

# com ZOO
library(zoo)
zoo_acion_raw <- zoo(df_raw.dia$N.ACION, df_raw.dia$DIA.ACION)
plot(zoo_acion_raw)
zoo_acion_tidy <- zoo(df_acion.dia$N.ACION, df_acion.dia$DIA.ACION)
plot(zoo_acion_tidy)

# usando zoo package
## simple creation and plotting of time series
library(zoo)
x.Date <- as.Date("2003-02-01") + c(1, 3, 7, 9, 14) - 1
x <- zoo(rnorm(5), x.Date)
plot(x)
time(x)

# FALTA AGORA COLOCAR LABELS E MELHORAR A APARENCIA DO PLOT



df_acion <- read.csv("./data/Acionamentos Avon-14-12-2015.rpt", skip = 2, sep = "|", header = TRUE)

