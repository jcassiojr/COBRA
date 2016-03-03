# análises exploratórias de time series
#require("gridExtra")
#require("astsa")
require("ggplot2")
require("dplyr")
require("xlsx")
require("lubridate")
require("grid")

source("~/Documents/MyGit/COBRA/R/f_le_chat.R")
source("~/Documents/MyGit/COBRA/R/f_le_sms.R")
source("~/Documents/MyGit/COBRA/R/f_le_fone.R")
source("~/Documents/MyGit/COBRA/R/f_le_xml.R")
# PGTO 
##############
# obter dados de pgto totais (não somente Avon)
################################################################

getOption("scipen") # obtendo default para display de notação científica em R
options(scipen=999) # removendo display de notação científica em R
#options(scipen=0) # restaurando opção default
# obs: campos de valor já vêem sem vírgula como separador de milhar
# obs: cuidado para limpar colunas de totais do xlsx
#df_pg <- read.xlsx2("./data/Dados Raw-pgtos agencia 4c.csv", sheetIndex = 1, header = TRUE)
df_pg <- read.csv("./data/Dados Raw-pgtos agencia 4c.csv", sep = ",", header = TRUE, 
                  stringsAsFactors = FALSE,na.strings = "NULL")
df_pg <- na.omit(df_pg)
# converting from chr to Date format
df_pg$DTpgto <- as.Date(df_pg$DTpgto, "%m/%d/%y")
# convertendo em POSIXct para funcionar no plot abaixo
#df_pg$DTpgto <- as.POSIXct(trunc.POSIXt(df_pg$DTpgto, units = "days"))
# acertando a coluna data que no excel está numérica
#df_pg <-
#    df_pg %>%
#    mutate(DTpgto = ymd(as.Date(as.numeric(paste(DTpgto)), origin="1899-12-30")) ) %>%
#    rename(CONTRATO = CONTR_CON)
# eliminando virgula de milhar para conversão abaixo não forçar NA neste casos
df_pg <-
    df_pg %>%
    mutate (VlPag = as.numeric(gsub(",","", VlPag))) %>% # forçando Valor numeric
    rename(CONTRATO = CONTR_CON)
# NÃO FUNCIONA NO dplyr! x <- df_pg %>% mutate (Valor = as.numeric(as.charcter(Valor)))
# tem que fazer passo a passo como abaixo
#df_pg <- df_pg %>% mutate (VlPag = as.character(VlPag)) # para acerto do merge com cliav
#df_pg <- df_pg %>% mutate (VlPag = as.numeric(VlPag)) # para acerto do merge com cliav

# número de pgtos agrupando por dia 
#df_pg_dia <-
#    df_pg %>%
#    group_by(DTpgto) %>%
#    summarise(pgto.dia = n(),
#              vlpg.dia = sum(VlPag))

# plot de nro pgtos dia
#pl_pg <- ggplot(df_pg_dia, aes(DTpgto, pgto.dia)) + geom_line() + geom_smooth() +
#    xlab("dia") + ylab("pagamentos") + ggtitle("Número de Pagamentos/Dia") + 
#    xlim(c(min(df_acion_dia$DIA.ACION),max(df_acion_dia$DIA.ACION)))

#pl_vlpg <- ggplot(df_vlpg_dia, aes(DTpgto, vlpg.dia)) + geom_line() + geom_smooth() +
#    xlab("dia") + ylab("pagamentos") + ggtitle("Valor de Pagamentos/Dia") + 
#    xlim(c(min(df_acion_dia$DIA.ACION),max(df_acion_dia$DIA.ACION)))

# IMPORTANTE ABAIXO: considerando somente o primeiro pgto de cada contrato/dia
# considerando somente o primiro pagamento de cada contrato
df_pg.primpg <- 
    df_pg %>%
    group_by(CONTRATO) %>%
    filter(DTpgto == min(DTpgto))
# agrupa por dia
df_pg.primpg_dia <- 
    df_pg.primpg %>%
    group_by(DTpgto) %>%
    summarise(vlpg.dia = sum(VlPag),
              pgto.dia = n())
#a <- df_pg.primpg_dia
# completando os dias de gap com zero (necessário para correta cross correlation!!!!)
# 1. criando data.frame com todas as datas
dts_pg <- as.data.frame(seq(as.Date("2014-09-01"), as.Date("2016-2-12"), "days"))
names(dts_pg) <- "DTpgto"
df_pg.primpg_dia$DTpgto <- as.Date(df_pg.primpg_dia$DTpgto)
df_pg.primpg_dia <- full_join(df_pg.primpg_dia,dts_pg, by = "DTpgto")
df_pg.primpg_dia <-
    df_pg.primpg_dia %>%
    mutate(pgto.dia = ifelse(is.na(pgto.dia), 0, pgto.dia),
    vlpg.dia = ifelse(is.na(vlpg.dia), 0, vlpg.dia))

# TESTANDO OS ARQUIVOS RECEBIDOS DE SMS E TELEFONIA DO RAFAEL
# 1. obter a quantidade de SMS enviados, agrupados por: com ou sem confirmação
# 2. obter o dia do envio para comparar com time serie de pgtos

# LE arquivos SMS
df_sms.2015 <- f_le_sms()
# transformando em caracter o celular
df_sms.2015 <-
    df_sms.2015 %>%
    mutate(Celular = as.character(Celular))
# eliminando números esquisitos: terminados em 0000
df_sms.2015 <-
    df_sms.2015 %>%
    filter(!grepl("0000$",Celular))

# agrupar totais por dia, totais pro dia sem confirmação, totais por dia com confirmação
# estatísticas iniciais
#table(df_sms.2015$Status)
table(df_sms.2015$Status)
prop.table(table(df_sms.2015$Status)) # confirmados: 34%, não confirmados: 40%, bloqueados: 4%, Não recebidos: 23%
# eliminando não recebidos e bloqueados
df_sms.2015 <-
    df_sms.2015 %>%
    filter(!(grepl("Não Recebido|Bloqueado",Status)))
# eliminando não recebidos
#df_sms.2015 <-
#    df_sms.2015 %>%
#    filter(!(grepl("Não Recebido",Status)))

# preparando formato correto de datas para plot
df_sms.2015$Enviado.em <- as.Date(df_sms.2015$Enviado.em, "%d/%m/%Y")
# transformando em PosixCt e truncando para tirar a hora
#df_sms.2015$Enviado.em <- as.POSIXct(df_sms.2015$Enviado.em)
#df_sms.2015$Enviado.em <- as.POSIXct(trunc.POSIXt(df_sms.2015$Enviado.em, units = "days"))
# agrupando por dia 
df_sms.2015.tot <-
    df_sms.2015 %>%
    group_by(Enviado.em) %>%
    summarise(acions.dia = n())

# separando não confirmados de confirmados

# ACIONAMENTOS SMS
##################
# filtrando Confirmados
#########################
df_sms.2015.conf <-
    df_sms.2015 %>%
    filter(grepl("Entregue com Confirmação",Status))

# primeiro cnvertendo coluna em formato Date
#as.Date(d, "%d/%m/%Y")
#  obs: tem que converter para POSIXct para funcionar com dplyr!!!!
#df_sms.2015.conf$Enviado.em <- as.Date(df_sms.2015.conf$Enviado.em, "%d/%m/%Y")
# transformando em PosixCt
#df_sms.2015.conf$Enviado.em <- as.POSIXct(df_sms.2015.conf$Enviado.em)
# agrupando por dia 
df_sms.2015.conf <-
    df_sms.2015.conf %>%
    group_by(Enviado.em) %>%
    summarise(acions.dia = n())
# eliminando gaps
dts_sms.conf <- as.data.frame(seq(as.Date("2015-04-08"), as.Date("2015-11-30"), "days"))
names(dts_sms.conf) <- "Enviado.em"
df_sms.2015.conf$Enviado.em <- as.Date(df_sms.2015.conf$Enviado.em)
df_sms.2015.conf <- full_join(df_sms.2015.conf,dts_sms.conf, by = "Enviado.em")
df_sms.2015.conf <-
    df_sms.2015.conf %>%
    mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia))

# filtrando Não Confirmados
##############################
df_sms.2015.nconf <-
    df_sms.2015 %>%
    filter(grepl("Entregue sem Confirmação",Status))

# primeiro cnvertendo coluna em formato Date
#as.Date(d, "%d/%m/%Y")
#  obs: tem que converter para POSIXct para funcionar com dplyr!!!!
#df_sms.2015.nconf$Enviado.em <- as.Date(df_sms.2015.nconf$Enviado.em, "%d/%m/%Y")
# transformando em PosixCt
#df_sms.2015.nconf$Enviado.em <- as.POSIXct(df_sms.2015.nconf$Enviado.em)
# agrupando por dia 
df_sms.2015.nconf <-
    df_sms.2015.nconf %>%
    group_by(Enviado.em) %>%
    summarise(acions.dia = n())

# eliminando gaps
dts_sms.nconf <- as.data.frame(seq(as.Date("2015-04-08"), as.Date("2015-11-30"), "days"))
names(dts_sms.nconf) <- "Enviado.em"
df_sms.2015.nconf$Enviado.em <- as.Date(df_sms.2015.nconf$Enviado.em)
df_sms.2015.nconf <- full_join(df_sms.2015.nconf,dts_sms.nconf, by = "Enviado.em")
df_sms.2015.nconf <-
    df_sms.2015.nconf %>%
    mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia))

# le arquivos de telefonemasn ativo e receptivo 2015
df_tel.2015 <- f_le_fone()

# preparando formato correto de datas para plot
df_tel.2015$Data <- as.Date(df_tel.2015$Data, "%d/%m/%Y")
# transformando em PosixCt e truncando para tirar a hora
#df_sms.2015$Enviado.em <- as.POSIXct(df_sms.2015$Enviado.em)
#df_tel.2015$Data <- as.POSIXct(trunc.POSIXt(df_tel.2015$Data, units = "days"))

# agrupando por dia total de chamadas
df_tel.2015.tot <-
    df_tel.2015 %>%
    group_by(Data) %>%
    summarise(acions.dia = n())
# eliminando gaps
dts_tel.tot <- as.data.frame(seq(as.Date("2015-01-02"), as.Date("2015-11-30"), "days"))
names(dts_tel.tot) <- "Data"
df_tel.2015.tot$Data <- as.Date(df_tel.2015.tot$Data)
df_tel.2015.tot <- full_join(df_tel.2015.tot,dts_tel.tot, by = "Data")
df_tel.2015.tot <-
    df_tel.2015.tot %>%
    mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia))

# selecionar apenas as ativas manual mas completadas
df_tel.2015.atv.m <-
    df_tel.2015 %>%
    filter(Tipo == "Ativa Manual" & Status == "Completada")
# agrupando por dia total de chamadas ativas manuais
df_tel.2015.atv.m <-
    df_tel.2015.atv.m %>%
    group_by(Data) %>%
    summarise(acions.dia = n())
# eliminando gaps
dts_tel.atvm <- as.data.frame(seq(as.Date("2015-01-02"), as.Date("2015-11-30"), "days"))
names(dts_tel.atvm) <- "Data"
df_tel.2015.atv.m$Data <- as.Date(df_tel.2015.atv.m$Data)
df_tel.2015.atv.m <- full_join(df_tel.2015.atv.m,dts_tel.atvm, by = "Data")
df_tel.2015.atv.m <-
    df_tel.2015.atv.m %>%
    mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia))

# selecionar apenas as ativas auto mas completadas
df_tel.2015.atv.a <-
    df_tel.2015 %>%
    filter(Tipo == "Ativa AutomÃ¡tica" & (Status == "Completada (Chamador)"))
 
# agrupando por dia total de chamadas ativas auto
df_tel.2015.atv.a <-
    df_tel.2015.atv.a %>%
    group_by(Data) %>%
    summarise(acions.dia = n())
# eliminando gaps
dts_tel.atva <- as.data.frame(seq(as.Date("2015-01-02"), as.Date("2015-11-30"), "days"))
names(dts_tel.atva) <- "Data"
df_tel.2015.atv.a$Data <- as.Date(df_tel.2015.atv.a$Data)
df_tel.2015.atv.a <- full_join(df_tel.2015.atv.a,dts_tel.atva, by = "Data")
df_tel.2015.atv.a <-
    df_tel.2015.atv.a %>%
    mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia))

# selecionar apenas as receptivas (considerando as transferências)
df_tel.2015.rec <-
    df_tel.2015 %>%
    filter(Tipo == "Receptiva" & (Status == "Completada (Agente)" | Status == "Completada (Chamador)"))

# percentual de abandono nas receptivas 
aband <-
    df_tel.2015 %>%
    filter(Tipo == "Receptiva")
prop.table(table(aband$Status)) # 7%

# agrupando por dia total de chamadas receptivas completadas
df_tel.2015.rec <-
    df_tel.2015.rec %>%
    group_by(Data) %>%
    summarise(acions.dia = n())
# eliminando gaps
dts_tel.rec <- as.data.frame(seq(as.Date("2015-01-02"), as.Date("2015-11-30"), "days"))
names(dts_tel.rec) <- "Data"
df_tel.2015.rec$Data <- as.Date(df_tel.2015.rec$Data)
df_tel.2015.rec <- full_join(df_tel.2015.rec,dts_tel.rec, by = "Data")
df_tel.2015.rec <-
    df_tel.2015.rec %>%
    mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia))

# CHAMAR AQUI FUNCAO QUE CARREGA CHATS
df_chat.2015 <- f_le_chat()

# PLOTS
pl_prim_npg <- ggplot(df_pg.primpg_dia, aes(DTpgto, pgto.dia)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("pagamentos") + ggtitle("Qtde de 1o Pagamento/Dia") +
    xlim(c(min(df_pg.primpg_dia$DTpgto),max(df_pg.primpg_dia$DTpgto)))

pl_prim_vlpg <- ggplot(df_pg.primpg_dia, aes(DTpgto,vlpg.dia)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("pagamentos") + ggtitle("Valor de 1o Pagamento/Dia") + 
    xlim(c(min(df_pg.primpg_dia$DTpgto),max(df_pg.primpg_dia$DTpgto)))

# plotando time series
pl_sms_tot <- ggplot(df_sms.2015.tot, aes(Enviado.em, acions.dia)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("acionamentos") + ggtitle("SMS Total/Dia") +
    xlim(c(min(df_pg.primpg_dia$DTpgto),max(df_pg.primpg_dia$DTpgto))) +
    ylim(c(min(df_sms.2015.tot$acions.dia),max(df_sms.2015.tot$acions.dia)))

# plotando time series
pl_sms_conf <- ggplot(df_sms.2015.conf, aes(Enviado.em, acions.dia)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("acionamentos") + ggtitle("SMS com Recebimento Confirmado/Dia") +
    xlim(c(min(df_pg.primpg_dia$DTpgto),max(df_pg.primpg_dia$DTpgto))) +
    ylim(c(min(df_sms.2015.conf$acions.dia),max(df_sms.2015.conf$acions.dia)))

# plotando time series
pl_sms_nconf <- ggplot(df_sms.2015.nconf, aes(Enviado.em, acions.dia)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("acionamentos") + ggtitle("SMS sem Recebimento Confirmado/Dia") +
    xlim(c(min(df_pg.primpg_dia$DTpgto),max(df_pg.primpg_dia$DTpgto))) +
    ylim(c(min(df_sms.2015.nconf$acions.dia),max(df_sms.2015.nconf$acions.dia)))

# plotando time series de total de chamadas (muito brutos os dados!)
pl_tel_tot <- ggplot(df_tel.2015.tot, aes(Data, acions.dia)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("acionamentos") + ggtitle("Telefonema Total/Dia") +
    xlim(c(min(df_pg.primpg_dia$DTpgto),max(df_pg.primpg_dia$DTpgto))) +
    ylim(c(min(df_tel.2015.tot$acions.dia),max(df_tel.2015.tot$acions.dia)))

# plotando time series de total de chamadas 
pl_tel_rec <- ggplot(df_tel.2015.rec, aes(Data, acions.dia)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("acionamentos") + ggtitle("Ligação Receptiva/Dia") +
    xlim(c(min(df_pg.primpg_dia$DTpgto),max(df_pg.primpg_dia$DTpgto))) +
    ylim(c(min(df_tel.2015.rec$acions.dia),max(df_tel.2015.rec$acions.dia)))

# plotando time series de total de chamadas 
pl_tel_atvm <- ggplot(df_tel.2015.atv.a, aes(Data, acions.dia)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("acionamentos") + ggtitle("Ligação Ativa Manual/Dia") +
    xlim(c(min(df_pg.primpg_dia$DTpgto),max(df_pg.primpg_dia$DTpgto))) +
    ylim(c(min(df_tel.2015.atv.a$acions.dia),max(df_tel.2015.atv.a$acions.dia)))

# plotando time series de total de chamadas ativas manuais
pl_tel_atva <- ggplot(df_tel.2015.atv.m, aes(Data, acions.dia)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("acionamentos") + ggtitle("Ligação Ativa Automática/Dia") +
    xlim(c(min(df_pg.primpg_dia$DTpgto),max(df_pg.primpg_dia$DTpgto))) +
    ylim(c(min(df_tel.2015.atv.m$acions.dia),max(df_tel.2015.atv.m$acions.dia)))

# plotando time series de total de chats realizados
pl_chat <- ggplot(df_chat.2015, aes(Data, nchats)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("acionamentos") + ggtitle("Chats/Dia") +
    xlim(c(min(df_pg.primpg_dia$DTpgto),max(df_pg.primpg_dia$DTpgto))) +
    ylim(c(min(df_chat.2015$nchats),max(df_chat.2015$nchats)))

# plots combinados
pushViewport(viewport(layout = grid.layout(2, 1)))
print(pl_tel_atvm, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
#print(pl_sms_conf, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
#print(pl_chat, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(pl_prim_npg, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
#print(pl_prim_vlpg, vp = viewport(layout.pos.row = 5, layout.pos.col = 1))

pushViewport(viewport(layout = grid.layout(4, 1)))
print(pl_tel_atva, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_tel_atvm, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(pl_tel_rec, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(pl_prim_npg, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))

# CORRELAÇÕES
#----------------------------------------------
# ABAIXO correlacionar time series
# correlacionar nro de pgtos e velor de pgtos com
# 1. sms enviados com confirmação
# 2. sms enviados sem confirmação
# 3. sms totais enviados
# 4. ativos
# 5. receptivos
# -----------------------------------------------
# mostra lag entre acionamento e pgto
# antes fazer merge por dia para colocar os valores no mesmo dia
# correlação positiva significa que quando uma cresce a outra tb e vice-versa
# o valor do eixo y indica a correlação (max = 1)
# o fato de existir lag (pico fora do zero no eixo x) significa que uma variável conduz a outra,
# ou seja, não podemos prever o movimento de uma variável olhando para a outra

# 1. Correlação sms enviado com confirmação x qtde de pgtos
#my.corr.npg <- df_pg_dia
#my.corr.vlpg <- df_vlpg_dia
#my.corr.npg$DTpgto <- as.character(my.corr.npg$DTpgto)
#my.corr.vlpg$DTpgto <- as.character(my.corr.vlpg$DTpgto)
# considerando apenas primeiro pgto
my.corr.prpg <- df_pg.primpg_dia
my.corr.prpg$DTpgto <- as.character(my.corr.prpg$DTpgto) # mudando para character para funcionar ccf()
#y <- df_acion_atv_dia

#y$DIA.ACION <- as.character(y$DIA.ACION)
#z <- full_join(my.corr.pg,y, by=c("DTpgto" = "DIA.ACION"))
#z <- na.omit(z)
#ccf(z$acions.dia ,z$pgto.dia)
#ccf(z$pgto.dia ,z$acions.dia)
# correlacionar sms confirmado com nro pgto
#############################
my.corr.sms.conf <- df_sms.2015.conf
my.corr.sms.conf$Enviado.em <- as.character(my.corr.sms.conf$Enviado.em) # mudando para character para funcionar ccf()
# correlação com nro de pgtos dia
#z2 <- full_join(my.corr.npg,my.corr.sms.conf, by=c("DTpgto" = "Enviado.em"))
#z2 <- na.omit(z2)
# x sendo o preditor de y
# lag negativo significa que x está -n dias atrás de Y, ou seja x leads y, x predict y
#ccf(z2$acions.dia ,z2$pgto.dia, na.action = na.pass)

# There are a lot of models that we could try based on the CCF and lagged scatterplots for
# these data. For demonstration purposes, we’ll first try a multiple regression in which yt,
# the recruit variable, is a linear function of (past) lags 5, 6, 7, 8, 9, and 10 of the
# SOI variable. That model works fairly well. Following is some R output. All coefficients 
# are statistically significant and the R-squared is about 62%.
# The residuals, however, have an AR(2) structure, as seen in the graph following the
# regression output. We might try the method described in Lesson 8.1 to adjust for that, 
# but we’ll take a different approach that we’ll describe after the output display.
#z2 <- na.omit(z2)
#ts1 <- ts(z2$acions.dia)
#ts2 <- ts(z2$pgto.dia)
#ccfvalues <- ccf(ts1, ts2)
#ccfvalues
#library("astsa")
#lag2.plot (ts1, ts2, 15)
# correlação com nro de primeiros pgtos dia
PGxSMS_Conf <- full_join(my.corr.prpg,my.corr.sms.conf, by=c("DTpgto" = "Enviado.em"))
# TESTE: transformar NA em 0 (MELHOROU!!!)
NPGxSMS_Conf <-
    PGxSMS_Conf %>%
    mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia),
           pgto.dia = ifelse(is.na(pgto.dia), 0, pgto.dia),
           vlpg.dia = ifelse(is.na(vlpg.dia), 0, vlpg.dia))
#z22 <- na.omit(z22)
#par(mfrow=c(2,2))
#ccf(NPGxSMS_Conf$acions.dia ,NPGxSMS_Conf$pgto.dia, na.action = na.pass, lag.max = 30)

# teste de lag2.plot
#NPGxSMS_Conf <- na.omit(NPGxSMS_Conf)
#ts1 <- ts(PGxSMS_Conf$acions.dia)
#ts2 <- ts(PGxSMS_Conf$pgto.dia)
#ccfvalues <- ccf(ts1, ts2)
#ccfvalues
#library("astsa")
#lag2.plot (ts1, ts2, 15)

# 1. sms enviado com confirmação x valor de pgtos
#z3 <- full_join(my.corr.vlpg,my.corr.sms.conf, by=c("DTpgto" = "Enviado.em"))
#z3 <- na.omit(z3)
#ccf(z3$acions.dia ,z3$pgto.dia, na.action = na.pass)
# correlação com nro de primeiros pgtos dia
#VPGxSMS_Conf <- full_join(my.corr.prpg,my.corr.sms.conf, by=c("DTpgto" = "Enviado.em"))
VPGxSMS_Conf <-
    PGxSMS_Conf %>%
    mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia),
           pgto.dia = ifelse(is.na(pgto.dia), 0, pgto.dia),
           vlpg.dia = ifelse(is.na(vlpg.dia), 0, vlpg.dia))

#ccf(VPGxSMS_Conf$acions.dia ,VPGxSMS_Conf$vlpg.dia, na.action = na.pass, lag.max = 30)

# 2. sms enviado SEM confirmação x qtde de pgtos
################################################
my.corr.sms.nconf <- df_sms.2015.nconf
my.corr.sms.nconf$Enviado.em <- as.character(my.corr.sms.nconf$Enviado.em)
#z4 <- full_join(my.corr.npg,my.corr.sms.nconf, by=c("DTpgto" = "Enviado.em"))
#z4 <- na.omit(z4)
# correlação com nro de pgtos dia
#ccf(z4$acions.dia ,z4$pgto.dia, na.action = na.pass)
# com primeiro pgto (qtde)
PGxSMS_NConf <- full_join(my.corr.prpg,my.corr.sms.nconf, by=c("DTpgto" = "Enviado.em"))
NPGxSMS_NConf <-
    PGxSMS_NConf %>%
    mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia),
           pgto.dia = ifelse(is.na(pgto.dia), 0, pgto.dia),
           vlpg.dia = ifelse(is.na(vlpg.dia), 0, vlpg.dia))

#ccf(NPGxSMS_NConf$acions.dia ,NPGxSMS_NConf$pgto.dia, na.action = na.pass, lag.max = 30)


# 2. sms enviado SEM confirmação x valor de pgtos
#z5 <- full_join(my.corr.vlpg,my.corr.sms.nconf, by=c("DTpgto" = "Enviado.em"))
#z5 <- na.omit(z5)
#ccf(z5$acions.dia ,z5$pgto.dia, na.action = na.pass)

# com primeiro pgto (valor)
#VPGxSMS_NConf <- full_join(my.corr.prpg,my.corr.sms.nconf, by=c("DTpgto" = "Enviado.em"))
VPGxSMS_NConf <-
    PGxSMS_NConf %>%
    mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia),
           pgto.dia = ifelse(is.na(pgto.dia), 0, pgto.dia),
           vlpg.dia = ifelse(is.na(vlpg.dia), 0, vlpg.dia))

#ccf(VPGxSMS_NConf$acions.dia ,VPGxSMS_NConf$vlpg.dia, na.action = na.pass, lag.max = 30)

# 3. sms totais enviados
my.corr.sms.tot <- df_sms.2015.tot
my.corr.sms.tot$Enviado.em <- as.character(my.corr.sms.tot$Enviado.em)
#z6 <- full_join(my.corr.npg,my.corr.sms.tot, by=c("DTpgto" = "Enviado.em"))
#z6 <- na.omit(z6)
# correlação com nro de pgtos dia
#ccf(z6$acions.dia ,z6$pgto.dia, na.action = na.pass)
# com primeiro pgto (qtde)
PGxSMS_TOT <- full_join(my.corr.prpg,my.corr.sms.tot, by=c("DTpgto" = "Enviado.em"))
NPGxSMS_TOT <-
    PGxSMS_TOT %>%
    mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia),
           pgto.dia = ifelse(is.na(pgto.dia), 0, pgto.dia),
           vlpg.dia = ifelse(is.na(vlpg.dia), 0, vlpg.dia))

#ccf(NPGxSMS_TOT$acions.dia ,NPGxSMS_TOT$pgto.dia, na.action = na.pass, lag.max = 30)

# 2. sms enviado SEM confirmação x valor de pgtos
#z7 <- full_join(my.corr.vlpg,my.corr.sms.tot, by=c("DTpgto" = "Enviado.em"))
#z7 <- na.omit(z7)
#ccf(z7$acions.dia ,z7$pgto.dia, na.action = na.pass)
# com primeiro pgto (valor)
#VPGxSMS_TOT <- full_join(my.corr.prpg,my.corr.sms.tot, by=c("DTpgto" = "Enviado.em"))
VPGxSMS_TOT <-
    PGxSMS_TOT %>%
    mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia),
           pgto.dia = ifelse(is.na(pgto.dia), 0, pgto.dia),
           vlpg.dia = ifelse(is.na(vlpg.dia), 0, vlpg.dia))

#ccf(VPGxSMS_TOT$acions.dia ,VPGxSMS_TOT$vlpg.dia, na.action = na.pass, lag.max = 30)

# 4. ativos x pgtos
# pegar dos arquivos, somente ligaçoes completadas
# nro pgtos x ativas manuais
my.corr.atvm <- df_tel.2015.atv.m
my.corr.atvm$Data <- as.character(my.corr.atvm$Data)
#a2 <- full_join(my.corr.npg,my.corr.atvm, by=c("DTpgto" = "Data"))
#a2 <- na.omit(a2)
# correlação com nro de pgtos dia
#ccf(a2$acions.dia ,a2$pgto.dia, na.action = na.pass)
# com primeiro pgto (qtde) USAR ESTE (PRIM PGTO de TODAS CARTEIRAS x ATIVAS MANUAIS)
# OTIMO PLOT!!!!!!
PGxATV_MAN <- full_join(my.corr.prpg,my.corr.atvm, by=c("DTpgto" = "Data"))
NPGxATV_MAN <-
    PGxATV_MAN %>%
    mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia),
           pgto.dia = ifelse(is.na(pgto.dia), 0, pgto.dia),
           vlpg.dia = ifelse(is.na(vlpg.dia), 0, vlpg.dia))

#ccf(NPGxATV_MAN$acions.dia ,NPGxATV_MAN$pgto.dia, na.action = na.pass, lag.max = 30)

#ts1 <- ts(NPGxATV_MAN$acions.dia)
#ts2 <- ts(NPGxATV_MAN$pgto.dia)

#ccfvalues <- ccf(ts1, ts2)
#ccfvalues
# obs: em caso de erro, detach package astsa e install again
#library("astsa")
#lag2.plot (ts1, ts2, 15)

# nro pgtos x ativas auto
my.corr.atva <- df_tel.2015.atv.a
my.corr.atva$Data <- as.character(my.corr.atva$Data)
#a3 <- full_join(my.corr.npg,my.corr.atva, by=c("DTpgto" = "Data"))
#a3 <- na.omit(a3)
# correlação com nro de pgtos dia
#ccf(a3$acions.dia ,a3$pgto.dia)
# com primeiro pgto (qtde) USAR ESTE (PRIM PGTO de TODAS CARTEIRAS x ATIVAS AUTO)
PGxATV_AUT <- full_join(my.corr.prpg,my.corr.atva, by=c("DTpgto" = "Data"))
NPGxATV_AUT <-
    PGxATV_AUT %>%
    mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia),
           pgto.dia = ifelse(is.na(pgto.dia), 0, pgto.dia),
           vlpg.dia = ifelse(is.na(vlpg.dia), 0, vlpg.dia))

#ccf(PGxATV_AUT$acions.dia ,PGxATV_AUT$pgto.dia, na.action = na.pass, lag.max = 30)
# correlação fraca mas negativa (sugestão: não gastar com ativo automatico)

# nro pgtos x receptivas
my.corr.rec <- df_tel.2015.rec
my.corr.rec$Data <- as.character(my.corr.rec$Data)

# CONFIRMADO: CORRELAÇÃO Aumentou de .3 para .5!!!!!
#+++++++++++++
#a4 <- full_join(my.corr.npg,my.corr.rec, by=c("DTpgto" = "Data"))
#a4 <- na.omit(a4)
# correlação com nro de pgtos dia
#ccf(a4$acions.dia ,a4$pgto.dia, na.action = na.pass)
# com primeiro pgto (qtde)
# OTIMO PLOT. 20% de corr. Deveria ser maior!!! TEntar somar SMS e FONE
PGxREC <- full_join(my.corr.prpg,my.corr.rec, by=c("DTpgto" = "Data"))
NPGxREC <-
    PGxREC %>%
    mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia),
           pgto.dia = ifelse(is.na(pgto.dia), 0, pgto.dia),
           vlpg.dia = ifelse(is.na(vlpg.dia), 0, vlpg.dia))

#ccf(NPGxREC$acions.dia ,NPGxREC$pgto.dia, na.action = na.pass, lag.max = 30)
# interessante: correlação mais alta de todas e próximo de lag 0!!!!
# confirma que existe correlação entre pgto e receptivo
# conclusão: não perder receptivo!!

# Obtendo acionamento somados: ATIVO MANUAL e SMS CONF
my.corr.sms_atvm <- full_join(my.corr.atvm,my.corr.sms.conf, by=c("Data" = "Enviado.em"))
my.corr.sms_atvm <-
    my.corr.sms_atvm %>%
    mutate(acions.dia.y = ifelse(is.na(acions.dia.y), 0, acions.dia.y),
           sms_fone = acions.dia.x + acions.dia.y) %>%
    select(Data, sms_fone)
# correlacionando com nro pgtos
PGxSMS_FONE <- full_join(my.corr.prpg,my.corr.sms_atvm, by=c("DTpgto" = "Data"))
NPGxSMS_FONE <-
    PGxSMS_FONE %>%
    mutate(sms_fone = ifelse(is.na(sms_fone), 0, sms_fone),
           pgto.dia = ifelse(is.na(pgto.dia), 0, pgto.dia),
           vlpg.dia = ifelse(is.na(vlpg.dia), 0, vlpg.dia))

#ccf(NPGxSMS_FONE$sms_fone ,NPGxSMS_FONE$pgto.dia, na.action = na.pass, lag.max = 30)

# correlacoes chat
my.corr.chat <- df_chat.2015
my.corr.chat$Data <- as.character(my.corr.chat$Data)

# CONFIRMADO: CORRELAÇÃO Aumentou de .3 para .5!!!!!
#+++++++++++++
#a4 <- full_join(my.corr.npg,my.corr.rec, by=c("DTpgto" = "Data"))
#a4 <- na.omit(a4)
# correlação com nro de pgtos dia
#ccf(a4$acions.dia ,a4$pgto.dia, na.action = na.pass)
# com primeiro pgto (qtde)
# OTIMO PLOT. 20% de corr. Deveria ser maior!!! TEntar somar SMS e FONE
PGxCHAT <- full_join(my.corr.prpg,my.corr.chat, by=c("DTpgto" = "Data"))
NPGxCHAT <-
    PGxCHAT %>%
    mutate(nchats = ifelse(is.na(nchats), 0, nchats),
           pgto.dia = ifelse(is.na(pgto.dia), 0, pgto.dia),
           vlpg.dia = ifelse(is.na(vlpg.dia), 0, vlpg.dia))


# PLOTANDO AS CORRELAÇÕES
################################

par(mfrow=c(2,3))
ccf(NPGxSMS_Conf$acions.dia ,NPGxSMS_Conf$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado")
#ccf(VPGxSMS_Conf$acions.dia ,VPGxSMS_Conf$vlpg.dia, na.action = na.pass, lag.max = 30)
ccf(NPGxSMS_NConf$acions.dia ,NPGxSMS_NConf$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS NÃO Confirmado")
#ccf(VPGxSMS_NConf$acions.dia ,VPGxSMS_NConf$vlpg.dia, na.action = na.pass, lag.max = 30)
#ccf(NPGxSMS_TOT$acions.dia ,NPGxSMS_TOT$pgto.dia, 
#    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Total")
#ccf(VPGxSMS_TOT$acions.dia ,VPGxSMS_TOT$vlpg.dia, na.action = na.pass, lag.max = 30)
ccf(NPGxATV_MAN$acions.dia ,NPGxATV_MAN$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "Ativo Manual")
ccf(NPGxATV_AUT$acions.dia ,NPGxATV_AUT$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "Ativo Automático")
ccf(NPGxREC$ acions.dia ,NPGxREC$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "Receptivo Completado")
ccf(NPGxCHAT$nchats ,NPGxCHAT$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "Chats Realizados")
#ccf(NPGxSMS_FONE$sms_fone ,NPGxSMS_FONE$pgto.dia, 
#    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "Total SMS e Ativo Manual")

# TESTE: correlacionar chat com pgtos somente desde o inicio da time series de chat
# considerar pagtos a partir de 01/06/2015
# USAR!!!!!!!
x <-
    NPGxCHAT %>%
    filter (DTpgto >= "2015-06-01")
ccf(x$nchats ,x$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "Chats Realizados")
y <-
    NPGxATV_MAN %>%
    filter (DTpgto >= "2015-01-02")
ccf(y$acions.dia ,y$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "Ativos Manuais Realizados")
#ts1 <- ts(NPGxSMS_FONE$sms_fone)
#ts2 <- ts(x$pgto.dia)

#ccfvalues <- ccf(ts1, ts2)
#ccfvalues
# obs: em caso de erro, detach package astsa e install again
#library("astsa")
#lag2.plot (ts1, ts2, 15)






# FALTA: obter percentual de abandonos

# FALTA: mesma análise de TS prim pgto x chat
# FALTA: considerar nos telefones tb a Duração > X e Espera???

# TESTE DE REGRESSÃO MULTIPLA
# tentar regressão com formula: PGTO ~ lag1 + lag2 ...
# ideia: 
# 1. usar lag() para obter a timeserie de acionamentos para as lags maiores no plot
# obs: lag -1 maior, significa que devo trazer 1 dia para traz a time serie de pgtos
#      para concidir com dia.pgto, aplicando lag(acions.dia, 1)
# 2. criar dataframe com estas series + pgto
# 3. aplicar lm pgto ~ lag1 + lag2 ...

# EXPLICAÇÃO
# quando uso ccf() para obter os lags, a função recua a serie de nacions.dia dia a dia, mantendo pgto.dia fixo
# os valores de lag negativos me mostram a correlação de nacions.dia leading pgto.dia pra cada dia.
# ou seja, em quais intervalos de dias para trás de nacions.dia em relação a pgto.dia existe maior correlação
# Por isso, obtenho os lags negativos com maiores correlações e, a partir deles, uso a função lag para
# "atrasar no tempo" a time serie de pgto.dia para estes dias, para poder correlacionar depois com a serie de
# nacions.dia usando lm(). Crio uma nova time serie de pgto.dia para cada lag com correlação boa e depois
# aplico lm() para obter os resultados e avaliar o modelo!!
# OBS: testar acima criando dados fakes para ver se meu raciocínio está ok.



# NPG x SMS.CONF
# no caso abaixo, as lags maiores foram -7, -8, -13, -19, -24 (PG x SMS CONF)
lag.7 <- lag(NPGxSMS_Conf$pgto.dia,7)
plot(lag.7, type = "l")
lag.8 <- lag(NPGxSMS_Conf$pgto.dia,8)
lag.13 <- lag(NPGxSMS_Conf$pgto.dia,13)
lag.19 <- lag(NPGxSMS_Conf$pgto.dia,19)
lag.24 <- lag(NPGxSMS_Conf$pgto.dia,24)

# criando data.frame
my.df <- data.frame(pgto.dia = NPGxSMS_Conf$pgto.dia,
                    lag.7 = lag.7,
                    lag.8 = lag.8,
                    lag.13 = lag.13,
                    lag.19 = lag.19,
                    lag.24 = lag.24)
# aplica a regressão múltipla
regr.1 <- lm(pgto.dia ~ lag.7 + lag.8 + lag.13 + lag.19 + lag.24, my.df)
summary(regr.1)

# NPG x ATIVA MAN
z <- ccf(NPGxATV_MAN$acions.dia ,NPGxATV_MAN$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "Ativo Manual")
my.df.lag <- data.frame(lag = z$lag, acf = z$acf)
# examinando o df acima, as lags maiores foram -9, -10, -11 (PG x ATIV MAN)
lag.9 <- lag(NPGxATV_MAN$pgto.dia,9)
plot(lag.9, type = "l")
lag.10 <- lag(NPGxATV_MAN$pgto.dia,10)
lag.11 <- lag(NPGxATV_MAN$pgto.dia,11)

# criando data.frame
my.df.2 <- data.frame(pgto.dia = NPGxATV_MAN$pgto.dia,
                    lag.9 = lag.9,
                    lag.10 = lag.10,
                    lag.11 = lag.11)
# aplica a regressão múltipla
regr.2 <- lm(pgto.dia ~ lag.9 + lag.10 + lag.11, my.df.2)
summary(regr.2)


# importante: checar se existe correlação nos resíduos (não deveria!)
z <- ccf(regr.2$residuals ,regr.2$residuals, 
         na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "Regressões")
#++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++
# ATE AQUI OK
# TESTAR ABAIXO NO LUGAR DE USAR ccf() ou acf()!!!!
OP <- par(mfrow=c(3,3)) 
for(i in 1:9) { 
    CT <- cor.test(x[i:(20+i)],y,alternative="less") 
    PV <- CT$p.value 
    cat("lag =",9-i,"p-value =",PV,"\n") 
    COR <- sprintf("%1.3f",CT$estimate) 
    plot(x[i:(20+i)],y,xlab="x",main=paste("lag =",9-i,"corr =",COR)) 
} 
par(OP) 

HTH

# USANDO PACOTE TS
#ts_acion_dia <- ts(df_sms.2015.tot$acions.dia , frequency=365, start=c(2014,365))
#plot(ts_acion_dia)

# decompose time series
#f <- decompose(ts_acion_dia)
# seasonal figures
#f$figure




#++++++++++++++++++++++
# TUTORIAL ASSOCIADO AO PAPER 8.2 Cross Correlation Functions and Laged ...
library(astsa)  
soi= scan("soi.dat")
rec = scan("recruit.dat")
soi=ts (soi)
rec = ts(rec)
ccfvalues =ccf (soi, rec)
ccfvalues
lag2.plot (soi, rec, 10)
alldata=ts.intersect(rec,reclag1=lag(rec,-1), reclag2=lag(rec,-2), soilag5 = lag(soi,-5),
                     soilag6=lag(soi,-6), soilag7=lag(soi,-7), soilag8=lag(soi,-8), soilag9=lag(soi,-9),
                     soilag10=lag(soi,-10))
tryit = lm(rec~soilag5+soilag6+soilag7+soilag8+soilag9+soilag10, data = alldata)
summary (tryit)
acf2(residuals(tryit))
tryit2 = lm(rec~reclag1+reclag2+soilag5+soilag6+soilag7+soilag8+soilag9+soilag10,
            data = alldata)
summary (tryit2)
acf2(residuals(tryit2))
tryit3 = lm(rec~reclag1+reclag2+ soilag5+soilag6, data = alldata)
summary (tryit3)
acf2(residuals(tryit3))




# TESTE: testando nro de acionamentos para ativo e sms ++++++++++++++++++
# agrupa por dia e depois por celular
df_sms.2015.conf.cel <-
    df_sms.2015 %>%
    group_by(Enviado.em,Celular) %>%
    summarise(acions.dia = n())
# assim posso filtrar somente acionamentos por celular maior que X para correlacionar
# OBS: filtrar por n acionamentos por celular maior que X e repetir a correlação acima
df_sms.2015.fx.1 <-
    df_sms.2015.conf.cel %>%
    filter(acions.dia == 1)
df_sms.2015.fx.2.5 <-
    df_sms.2015.conf.cel %>%
    filter(acions.dia > 1 & acions.dia <= 5)
df_sms.2015.fx.6.10 <-
    df_sms.2015.conf.cel %>%
    filter(acions.dia > 5 & acions.dia <= 10)

# PAREI AQUIPOIS ESTA DANDO ERRO. PODE SER DO RSTUDIO. TENTAR DE NOVO E FAZER CORRELACAO
# plotando time series
#plot(df_sms.2015.conf.cel$acions.dia, type = "l")
# eliminando gaps
#dts_sms.conf <- as.data.frame(seq(as.Date("2015-04-08"), as.Date("2015-11-30"), "days"))
#names(dts_sms.conf) <- "Enviado.em"
#df_sms.2015.fx.1$Enviado.em <- as.Date(df_sms.2015.conf$Enviado.em)
df_sms.2015.fx.2.5 <- full_join(df_sms.2015.fx.2.5,dts_sms.conf, by = "Enviado.em")
#df_sms.2015.fx.1 <-
#    df_sms.2015.fx.1 %>%
#    mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia))
pl_sms_conf.fx.2.5 <- ggplot(df_sms.2015.fx.2.5, aes(Enviado.em, acions.dia)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("acionamentos") + ggtitle("SMS com Recebimento Confirmado/Dia") +
    xlim(c(min(df_pg.primpg_dia$DTpgto),max(df_pg.primpg_dia$DTpgto))) +
    ylim(c(min(df_sms.2015.conf$acions.dia),max(df_sms.2015.conf$acions.dia)))
#plot(pl_sms_conf.fx.2.5$acions.dia, type = "l")

# OBS: valores muito estranhos para alguns telefones com numeros estranhos e outros não
# ex. 551170000000 e 
#x <- df_sms.2015.conf.cel %>% mutate(Celular = as.numeric(as.character(Celular)))
#x <- df_sms.2015.conf.cel %>%
#    arrange(desc(acions.dia))
#getOption("scipen") # obtendo default para display de notação científica em R
#options(scipen=999) # removendo display de notação científica em R
#t <- as.numeric(df_sms.2015.conf.cel[1,1])
#(t)
#options(scipen=0) # restaurando opção default
#head(x) # numero para averiguar: 556292000000, 
# filtrando menores de 1000
#++++++++++++++++++++++++++++++++++++++++++++++++++++
#df_sms.2015.conf.cel <-
#    df_sms.2015.conf.cel %>%
#    filter(acions.dia < 1000)
# plotando time series
plot(df_sms.2015.conf.cel$acions.dia, type = "l")
#++++++++++++++++++++++++++++++++++++++++++++++++++++
#df_sms.2015.conf.cel <-
#    df_sms.2015.conf.cel %>%
#    filter(acions.dia < 1000)
# testando correlação entre acionamento e pagamento
# antes fazer o merge para pegar as mesmas datas
#x <- df_pg_dia
#x$DTpgto <- as.character(x$DTpgto)
#y <- df_acion_sms_dia
#y$DIA.ACION <- as.character(y$DIA.ACION)
#my.dt.acion <- merge(x, y, by.x = "DTpgto", by.y = "DIA.ACION")
#my.dt.acion <- inner_join(x, y, by = c("DTpgto" = "DIA.ACION"))
#my.cor <- cor(my.dt.acion$pgto.dia, my.dt.acion$acions.dia)
#(my.cor)
# FALTA: criar diferentes time series para: nro contratos/dia, nro pgtos/dia
# nro sms/dia, nro ativo/dia, nro passivo, dia (combinar cada um no memso plot de pgto)
# criar tb nro pgto/dia x nro/aciona/dia por cada operador