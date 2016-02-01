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
# ABAIXO FUNCIONA PARA DIA
df_acion.dia <- df_tidy
# agrupa dados de acionamento por contrato/dia
df_acion.dia <-
    df_acion.dia %>%
    select(CONTRATO,DATA.ACION) %>%
    #mutate(DIA.ACION = floor_date(df_acion.dia$DATA.ACION, "day")) %>%  # POR DIA
    mutate(DIA.ACION = floor_date(df_acion.dia$DATA.ACION, "month")) %>%  # POR MÊS
    group_by(DIA.ACION) %>%
    summarise(N.ACION = n()) %>%
    arrange(DIA.ACION)
# força formato data para dia.acion em dados de acionamento raw
df_raw.dia <- df_acion
df_raw.dia <-
    df_raw.dia %>%
    mutate (DATA.ACION = ymd_hms(DATA.ACION))
# agrupa dados de acionamento por contrato/dia            
df_raw.dia <-
    df_raw.dia %>%
    select(CONTRATO,DATA.ACION) %>%
    #mutate(DIA.ACION = floor_date(df_raw.dia$DATA.ACION, "day")) %>%  # POR DIA
    mutate(DIA.ACION = floor_date(df_raw.dia$DATA.ACION, "month")) %>%  # POR MÊS
    group_by(DIA.ACION) %>%
    summarise(N.ACION = n()) %>%
    arrange(DIA.ACION)
#v_acion.dia <- df_acion.dia$N.ACION
#ts_acion <- ts(v_acion.dia, frequency=7)
#ts_acion = ts(v_acion.dia,start=c(2014,10,7), frequency=365.25)
#ts_acion = ts(v_acion.dia, frequency=12)
#plot.ts(ts_acion)


# POR DIA
ts_acion_raw <- ts(df_raw.dia$N.ACION, frequency=365, start=c(2014,365))
plot(ts_acion_raw)
ts_acion_tidy <- ts(df_acion.dia$N.ACION, frequency=365, start=c(2014,365))
plot(ts_acion_tidy)
# POR MÊS
ts_acion_raw <- ts(df_raw.dia$N.ACION, frequency=12, start=c(2014,10))
plot(ts_acion_raw)
plot(lowess(ts_acion_raw),col="blue", lty="dashed") # aplicando filtro lowess
ts_acion_tidy <- ts(df_acion.dia$N.ACION, frequency=12, start=c(2014,10))
plot(ts_acion_tidy)
plot(diff(log(ts_acion_tidy)), main="logged and diffed")
# testando normality
shapiro.test(diff(log(ts_acion_tidy)))          # test for normality (adequado para p.value < 0.1)
par(mfrow=c(2,1))        # set up the graphics 
hist(diff(log(ts_acion_tidy)), prob=TRUE, 12)
lines(density(diff(log(ts_acion_tidy))))     # smooth it - ?density for details 
qqnorm(dljj)             # normal Q-Q plot  
qqline(dljj)             # add a line 
# correlação da estrutura dos dados
lag1.plot(diff(log(ts_acion_tidy)), 9)  #  also investigate lag.plot 


# USANDO ZOO
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

# astsa Package
#+++ DICA +++
# Linha abaixo executa sempre a funcão quando é carregado o workspace
.First <- function(){require(astsa)}
require(astsa)
jj
time(jj)   
cycle(jj)
frequency(jj)
deltat(jj)
plot(jj, ylab="Earnings per Share", main="J & J")
plot(jj, type="o", col="blue", lty="dashed")
plot(diff(log(jj)), main="logged and diffed") 
# comparando tipos de plots
# se já é objeto ts, plot() funciona
x = -5:5                  # sequence of integers from -5 to 5
y = 5*cos(x)              # guess
par(mfrow=c(3,2))         # multifigure setup: 3 rows, 2 cols
#---  plot:
plot(x, main="plot(x)")
plot(x, y, main="plot(x,y)")
#---  plot.ts:
plot.ts(x, main="plot.ts(x)")
plot.ts(x, y, main="plot.ts(x,y)")
#---  ts.plot:
ts.plot(x, main="ts.plot(x)")
ts.plot(ts(x), ts(y), col=1:2, main="ts.plot(x,y)")  # note- x and y are ts objects 
# usando filter/smooth
k = c(.5,1,1,1,.5)            # k is the vector of weights
(k = k/sum(k))       
[1] 0.125 0.250 0.250 0.250 0.125
fjj = filter(jj, sides=2, k)  # ?filter for help [but you knew that already]
plot(jj)
lines(fjj, col="red")         # adds a line to the existing plot
lines(lowess(jj), col="blue", lty="dashed")

dljj = diff(log(jj))        # difference the logged data
plot(dljj)                  # plot it (not shown)
shapiro.test(dljj)          # test for normality (adequado para p.value < 0.1)

par(mfrow=c(2,1))        # set up the graphics 
hist(dljj, prob=TRUE, 12)   # histogram    
lines(density(dljj))     # smooth it - ?density for details 
qqnorm(dljj)             # normal Q-Q plot  
qqline(dljj)             # add a line 

lag1.plot(dljj, 9)  #  also investigate lag.plot 

par(mfrow=c(2,1)) # The power of accurate observation is commonly called cynicism 
#     by those who have not got it. - George Bernard Shaw
acf(dljj, 20)     # ACF to lag 20 - no graph shown... keep reading
pacf(dljj, 20)    # PACF to lag 20 - no graph shown... keep reading
# !!NOTE!! acf2 on the line below is ONLY available in astsa and tsa3
acf2(dljj)        # this is what you'll see below

plot(dog <- stl(log(jj), "per"))

options(digits=2)  # the default is 7, but it's more than I want now
#+++ DICA +++
# os parentes adicionais abaixo fazem printar imediatamente o resultado do comando
(zardoz = ts(rnorm(48), start=c(2293,6), frequency=12))
# use window() if you want a part of a ts object
(oz = window(zardoz, start=2293, end=c(2295,12)))


#### 2016-01-27 - testes de correlação
# obtar tb nro de acionamentos por contrato para transformar em feature (mas apenas tipo telefone e SMS)
# filtrando somente os acionamentos de telefone (Ativo, Receptivo) e SMS

#+++++++++++++++++++++++++++++++++++++
#####################################
# PARA DADOS DE ACIONAMENTOS TOTAIS
df_acion.sms <-
    df_acion %>%
    filter(grepl("Envio de Sms",OCORRENCIA))

# filtrando por Ativo, Passivo
df_acion.fone <-
    df_acion %>%
    filter(grepl("Ativo|Receptivo",TIPO.ACION))

# somando os dois filtros acima
df_acion.sms_fone <-
    df_acion %>%
    filter(grepl("Ativo|Receptivo",TIPO.ACION) |
           grepl("Envio de Sms",OCORRENCIA))

# soma ativo por contrato
df_acion.sum.ativo <-
    df_acion.sms_fone %>%
    filter(grepl("Ativo",TIPO.ACION)) %>%
    group_by(CONTRATO) %>%
    summarise(nro.atv = n())
plot(df_acion.sum.ativo$nro.atv)
# soma receptivo por contrato
df_acion.sum.recep <-
    df_acion.sms_fone %>%
    filter(grepl("Receptivo",TIPO.ACION)) %>%
    group_by(CONTRATO) %>%
    summarise(nro.rec = n())
plot(df_acion.sum.recep$nro.rec)
# soma sms por contrato
df_acion.sum.sms <-
    df_acion.sms_fone %>%
    filter(grepl("Envio de Sms",OCORRENCIA)) %>%
    group_by(CONTRATO) %>%
    summarise(nro.sms = n())
plot(df_acion.sum.sms$nro.sms)

# merge dos arquivos
df_sum.acion <- merge(df_acion.sum.ativo,df_acion.sum.recep, by = "CONTRATO", all = TRUE)
df_sum.acion <- merge(df_sum.acion,df_acion.sum.sms, by = "CONTRATO", all = TRUE)

# correlacoes
#tirando NAs
df_nona <- na.omit(df_sum.acion)
cor(df_nona$nro.atv, df_nona$nro.rec)

# criar time serie para os valores de sms, ativo e passivo sem somar
#--------------------------------------------------------------------
library(lubridate)
df_acion.sms_fone <-
    df_acion.sms_fone %>%
    mutate (DATA.ACION = ymd_hms(DATA.ACION))

# sumarizando por dia para gerar time series
#df.ts <-
#    df_acion.sms_fone %>%
#    mutate (DATA.ACION = round.POSIXt(df_acion.sms_fone$DATA.ACION, units = "days")) 


# abaixo ok
#y <- df_acion.sms_fone[1:10,]
#y$DIA.ACION <- trunc.POSIXt(y$DATA.ACION, units = "days")

df_acion.sms_fone$DIA.ACION <- as.POSIXct(trunc.POSIXt(df_acion.sms_fone$DATA.ACION, units = "days"))
# agrupando por dia (obs: tem que converter para POSIXct para funcionar com dplyr!!!!)
df_acion_dia <-
    df_acion.sms_fone %>%
    group_by(DIA.ACION) %>%
    summarise(acions.dia = n())
# POR DIA
#ts_acion_dia <- ts(df_acion_dia$acions.dia, frequency=365, start=c(2014,365))
#plot(ts_acion_dia)
# ggplot 
ggplot(df_acion_dia, aes(DIA.ACION, acions.dia)) + geom_line() + geom_smooth() +
      xlab("") + ylab("Daily Views")

### PAREI AQUI
# FALTA: criar diferentes time series para: nro contratos/dia, nro pgtos/dia
# nro sms/dia, nro ativo/dia, nro passivo, dia (combinar cada um no memso plot de pgto)
# criar tb nro pgto/dia x nro/aciona/dia por cada operador


ts_acion_tidy <- ts(df_acion.dia$N.ACION, frequency=365, start=c(2014,365))
plot(ts_acion_tidy)
# POR MÊS
ts_acion_raw <- ts(df_raw.dia$N.ACION, frequency=12, start=c(2014,10))
plot(ts_acion_raw)
plot(lowess(ts_acion_raw),col="blue", lty="dashed") # aplicando filtro lowess
ts_acion_tidy <- ts(df_acion.dia$N.ACION, frequency=12, start=c(2014,10))
plot(ts_acion_tidy)
plot(diff(log(ts_acion_tidy)), main="logged and diffed")



# ATE AQUI ESTA OK. CONTINUANDO PARA GERAR TS.....

# contando sms por contrato
df_acion.sms.contr <-
    df_acion.sms %>%
    group_by(CONTRATO) %>%
    summarise(nro.sms = n())
summary(df_acion.sms.contr)

# agora checando correlação entre nro SMS e pagto
# proporção de acionamentos com sucesso sobre todos os acionamentos
prop.table(table(df_acion$SUCESSO))
table(df_acion$OCORRENCIA,df_acion$SUCESSO)
# a aprtir daqui, considera sucesso    

# PARA DADOS DE ACIONAMENTOS AVON SMS
df_avon.sms <-
    df_acion_avon %>%
    filter(grepl("Envio de Sms",OCORRENCIA))

# contando sms por contrato
df_avon.nsms <-
    df_avon.sms %>%
    select(DATA.ACION, CONTRATO) %>%
    group_by(CONTRATO) %>%
    summarise(nro.sms = n())
summary(df_avon.nsms)

# plotando SMSs por data acion (criar serie temporal)
hist(df_avon.nsms$nro.sms)
# fazer gráfico no tempo de nro sms

# agora checando correlação entre nro SMS e pagto

# TESTE
# somente considerando para o modelo os daso de sms, ativo e passivo
    
