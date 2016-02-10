# análises exploratórias de time series
require("gridExtra")
require("ggplot2")
source("~/Documents/MyGit/COBRA/R/f_leRawCobra.R")
valor_divida = 1.0
l_raw <- f_leRawCobra(valor_divida)
# obtém dataframe
df_acion <- l_raw[[1]]
df_carteira <- l_raw[[2]]
df_pg <- l_raw[[3]]

# ACIONAMENTOS
##################
# filtrando por Ativo
df_acion.atv <-
    df_acion %>%
    filter(grepl("Ativo",TIPO.ACION))
# filtrando por Receptivo
df_acion.rec <-
    df_acion %>%
    filter(grepl("Receptivo",TIPO.ACION))
# filtrando por SMS
df_acion.sms <-
    df_acion %>%
    filter(grepl("Envio de Sms",OCORRENCIA))

# filtrando por Ativo, Receptivo
df_acion.fone <-
    df_acion %>%
    filter(grepl("Ativo|Receptivo",TIPO.ACION))

# somando os Ativo, c, SMS
df_acion.sms_fone <-
    df_acion %>%
    filter(grepl("Ativo|Receptivo",TIPO.ACION) |
               grepl("Envio de Sms",OCORRENCIA))

# ACIONAMENTOS DE SMS E FONE
##################

#  obs: tem que converter para POSIXct para funcionar com dplyr!!!!
df_acion.sms_fone$DIA.ACION <- as.POSIXct(trunc.POSIXt(df_acion.sms_fone$DATA.ACION, units = "days"))
# agrupando por dia 
df_acion_dia <-
    df_acion.sms_fone %>%
    group_by(DIA.ACION) %>%
    summarise(acions.dia = n())
# POR DIA
#ts_acion_dia <- ts(df_acion_dia$acions.dia, frequency=365, start=c(2014,365))
#plot(ts_acion_dia)
# ggplot 
pl_ac <- ggplot(df_acion_dia, aes(DIA.ACION, acions.dia)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("acionamentos") + ggtitle("Acionamentos (SMS e Fone)/Dia") +
    xlim(c(min(df_acion_dia$DIA.ACION),max(df_acion_dia$DIA.ACION)))


# PGTO
##############

# agrupando por dia 
df_pg_dia <-
    df_pg %>%
    group_by(DTpgto) %>%
    summarise(pgto.dia = n())

pl_pg <- ggplot(df_pg_dia, aes(DTpgto, pgto.dia)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("pagamentos") + ggtitle("Pagamentos/Dia") + 
    xlim(c(min(df_acion_dia$DIA.ACION),max(df_acion_dia$DIA.ACION)))

#grid.arrange(pl_ac,pl_pg, nrow=2, ncol=1)
# outra alternativa a grid
#library(grid)
#pushViewport(viewport(layout = grid.layout(2, 1)))
#print(pl_ac, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
#print(pl_pg, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))

# ACIONAMENTOS DE SMS
##################
#  obs: tem que converter para POSIXct para funcionar com dplyr!!!!
df_acion.sms$DIA.ACION <- as.POSIXct(trunc.POSIXt(df_acion.sms$DATA.ACION, units = "days"))
# agrupando por dia 
df_acion_sms_dia <-
    df_acion.sms %>%
    group_by(DIA.ACION) %>%
    summarise(acions.dia = n())
# POR DIA
#ts_acion_dia <- ts(df_acion_dia$acions.dia, frequency=365, start=c(2014,365))
#plot(ts_acion_dia)
# ggplot 
pl_ac.sms <- ggplot(df_acion_sms_dia, aes(DIA.ACION, acions.dia)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("acionamentos") + ggtitle("Acionamentos (SMS)/Dia") +
    xlim(c(min(df_acion_sms_dia$DIA.ACION),max(df_acion_sms_dia$DIA.ACION)))

# ACIONAMENTOS DE ATIVO
##################
#  obs: tem que converter para POSIXct para funcionar com dplyr!!!!
df_acion.atv$DIA.ACION <- as.POSIXct(trunc.POSIXt(df_acion.atv$DATA.ACION, units = "days"))
# agrupando por dia 
df_acion_atv_dia <-
    df_acion.atv %>%
    group_by(DIA.ACION) %>%
    summarise(acions.dia = n())
# POR DIA
#ts_acion_dia <- ts(df_acion_dia$acions.dia, frequency=365, start=c(2014,365))
#plot(ts_acion_dia)
# ggplot 
pl_ac.atv <- ggplot(df_acion_atv_dia, aes(DIA.ACION, acions.dia)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("acionamentos") + ggtitle("Acionamentos (Ativo)/Dia") +
    xlim(c(min(df_acion_atv_dia$DIA.ACION),max(df_acion_atv_dia$DIA.ACION)))

# ACIONAMENTOS DE RECEPTIVO
##################
#  obs: tem que converter para POSIXct para funcionar com dplyr!!!!
df_acion.rec$DIA.ACION <- as.POSIXct(trunc.POSIXt(df_acion.rec$DATA.ACION, units = "days"))
# agrupando por dia 
df_acion_rec_dia <-
    df_acion.rec %>%
    group_by(DIA.ACION) %>%
    summarise(acions.dia = n())
# POR DIA
#ts_acion_dia <- ts(df_acion_dia$acions.dia, frequency=365, start=c(2014,365))
#plot(ts_acion_dia)
# ggplot 
pl_ac.rec <- ggplot(df_acion_rec_dia, aes(DIA.ACION, acions.dia)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("acionamentos") + ggtitle("Acionamentos (Receptivo)/Dia") +
    xlim(c(min(df_acion_rec_dia$DIA.ACION),max(df_acion_rec_dia$DIA.ACION)))
#grid.arrange(pl_ac,pl_pg, nrow=2, ncol=1)
# outra alternativa a grid
# plot por acionamento
library(grid)
pushViewport(viewport(layout = grid.layout(4, 1)))
#print(pl_ac, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_pg, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_ac.sms, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(pl_ac.atv, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(pl_ac.rec, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))

# Mesmos plots por contrato
#-----------------------------------------
df_pg.contr <-
    df_pg %>%
    arrange(CONTRATO,DTpgto) %>%
    group_by(CONTRATO) %>%
    mutate(VALOR.PAGO = sum(VlPag),
           NPARCELAS.PAGAS = n()) %>% # aqui cuidado, pois algumas parcelas se repetiram (??)
    distinct(CONTRATO) %>%
    select(-VlPag)
# número de parcelas pagas por dia
pl_pg.rparc <- ggplot(df_pg.contr, aes(DTpgto, NPARCELAS.PAGAS)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("pagamentos") + ggtitle("Número de Parcelas/Dia") + 
    xlim(c(min(df_acion_dia$DIA.ACION),max(df_acion_dia$DIA.ACION)))
# valor total pago por dia
pl_pg.ttpg <- ggplot(df_pg.contr, aes(DTpgto, VALOR.PAGO)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("pagamentos") + ggtitle("Valor Total Pago/Dia") + 
    xlim(c(min(df_acion_dia$DIA.ACION),max(df_acion_dia$DIA.ACION)))

# TESTANDO OS ARQUIVOS RECEBIDOS DE SMS E TELEFONIA DO RAFAEL
# 1. obter a quantidade de SMS enviados, agrupados por: com ou sem confirmação
# 2. obter o dia do envio para comparar com time serie de pgtos

# SMS de 04 2015
df_sms.04.08 <- read.csv2("./data/SMS/04 - Abril/08-04.csv", encoding="latin1",
                    stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.04.13 <- read.csv2("./data/SMS/04 - Abril/13-04.csv", encoding="latin1",
                             stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.04.15 <- read.csv2("./data/SMS/04 - Abril/15-04.csv", encoding="latin1",
                             stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.04.16 <- read.csv2("./data/SMS/04 - Abril/16-04.csv", encoding="latin1",
                             stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.04.17 <- read.csv2("./data/SMS/04 - Abril/17-04.csv", encoding="latin1",
                             stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.04.22 <- read.csv2("./data/SMS/04 - Abril/22-04.csv", encoding="latin1",
                             stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.04.23 <- read.csv2("./data/SMS/04 - Abril/23-04.csv", encoding="latin1",
                             stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.04.27 <- read.csv2("./data/SMS/04 - Abril/27-04.csv", encoding="latin1",
                             stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.04.28 <- read.csv2("./data/SMS/04 - Abril/28-04.csv", encoding="latin1",
                             stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")

# agrupando por dia
df_sms.04.2015 <- bind_rows(list(df_sms.04.08,df_sms.04.13,df_sms.04.15,df_sms.04.16,
                                df_sms.04.17,df_sms.04.22,df_sms.04.23,df_sms.04.27,df_sms.04.28))
# SMS de 05 2015
df_sms.05.04 <- read.csv2("./data/SMS/05 - Maio/04-05.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.05.05 <- read.csv2("./data/SMS/05 - Maio/05-05.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.05.07 <- read.csv2("./data/SMS/05 - Maio/07-05.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.05.12 <- read.csv2("./data/SMS/05 - Maio/12-05.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.05.14 <- read.csv2("./data/SMS/05 - Maio/14-05.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.05.19 <- read.csv2("./data/SMS/05 - Maio/19-05.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.05.22 <- read.csv2("./data/SMS/05 - Maio/22-05.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.05.26 <- read.csv2("./data/SMS/05 - Maio/26-05.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.05.27 <- read.csv2("./data/SMS/05 - Maio/27-05.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")

# agrupando por dia
df_sms.05.2015 <- bind_rows(list(df_sms.05.04,df_sms.05.05,df_sms.05.07,df_sms.05.12,
                                 df_sms.05.14,df_sms.05.19,df_sms.05.22,df_sms.05.26,df_sms.05.27))

# SMS de 06 2015
df_sms.06.01 <- read.csv2("./data/SMS/06 - Junho/01-06.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.06.02 <- read.csv2("./data/SMS/06 - Junho/02-06.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.06.05 <- read.csv2("./data/SMS/06 - Junho/05-06.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.06.08 <- read.csv2("./data/SMS/06 - Junho/08-06.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.06.09 <- read.csv2("./data/SMS/06 - Junho/09-06.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.06.10 <- read.csv2("./data/SMS/06 - Junho/10-06.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.06.11 <- read.csv2("./data/SMS/06 - Junho/11-06.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.06.12 <- read.csv2("./data/SMS/06 - Junho/12-06.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.06.15 <- read.csv2("./data/SMS/06 - Junho/15-06.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.06.16 <- read.csv2("./data/SMS/06 - Junho/16-06.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.06.17 <- read.csv2("./data/SMS/06 - Junho/17-06.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.06.19 <- read.csv2("./data/SMS/06 - Junho/19-06.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.06.23 <- read.csv2("./data/SMS/06 - Junho/23-06.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.06.24 <- read.csv2("./data/SMS/06 - Junho/24-06.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.06.25 <- read.csv2("./data/SMS/06 - Junho/25-06.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")

# SMS de 07 2015
df_sms.07.01 <- read.csv2("./data/SMS/07 - Julho/01-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.02 <- read.csv2("./data/SMS/07 - Julho/02-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.03 <- read.csv2("./data/SMS/07 - Julho/03-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.04 <- read.csv2("./data/SMS/07 - Julho/04-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.06 <- read.csv2("./data/SMS/07 - Julho/06-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.07 <- read.csv2("./data/SMS/07 - Julho/07-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.08 <- read.csv2("./data/SMS/07 - Julho/08-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.09 <- read.csv2("./data/SMS/07 - Julho/09-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.10 <- read.csv2("./data/SMS/07 - Julho/10-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.11 <- read.csv2("./data/SMS/07 - Julho/11-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.13 <- read.csv2("./data/SMS/07 - Julho/13-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.14 <- read.csv2("./data/SMS/07 - Julho/14-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.15 <- read.csv2("./data/SMS/07 - Julho/15-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.16 <- read.csv2("./data/SMS/07 - Julho/16-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.17 <- read.csv2("./data/SMS/07 - Julho/17-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.18 <- read.csv2("./data/SMS/07 - Julho/18-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.20 <- read.csv2("./data/SMS/07 - Julho/20-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.21 <- read.csv2("./data/SMS/07 - Julho/21-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.22 <- read.csv2("./data/SMS/07 - Julho/22-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.23 <- read.csv2("./data/SMS/07 - Julho/23-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.24 <- read.csv2("./data/SMS/07 - Julho/24-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.25 <- read.csv2("./data/SMS/07 - Julho/25-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.27 <- read.csv2("./data/SMS/07 - Julho/27-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.28 <- read.csv2("./data/SMS/07 - Julho/28-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.29 <- read.csv2("./data/SMS/07 - Julho/29-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.07.30 <- read.csv2("./data/SMS/07 - Julho/30-07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")


# agrupando por dia
df_sms.07.2015 <- bind_rows(list(df_sms.07.01,
                                 df_sms.07.02,
                                 df_sms.07.03,
                                 df_sms.07.04,
                                 df_sms.07.06,
                                 df_sms.07.07,
                                 df_sms.07.08,
                                 df_sms.07.09,
                                 df_sms.07.11,
                                 df_sms.07.13, 
                                 df_sms.07.14, 
                                 df_sms.07.15, 
                                 df_sms.07.16, 
                                 df_sms.07.17,
                                 df_sms.07.18,
                                 df_sms.07.20,
                                 df_sms.07.21,
                                 df_sms.07.22,
                                 df_sms.07.23,
                                 df_sms.07.24,
                                 df_sms.07.25,
                                 df_sms.07.27,
                                 df_sms.07.28,
                                 df_sms.07.29,
                                 df_sms.07.30
                                 ))

# SMS de 08 2015
df_sms.08.04 <- read.csv2("./data/SMS/08 - Agosto/04-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.05 <- read.csv2("./data/SMS/08 - Agosto/05-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.06 <- read.csv2("./data/SMS/08 - Agosto/06-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.07 <- read.csv2("./data/SMS/08 - Agosto/07-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.10 <- read.csv2("./data/SMS/08 - Agosto/10-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.11 <- read.csv2("./data/SMS/08 - Agosto/11-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.12 <- read.csv2("./data/SMS/08 - Agosto/12-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.13 <- read.csv2("./data/SMS/08 - Agosto/13-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.14 <- read.csv2("./data/SMS/08 - Agosto/14-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.15 <- read.csv2("./data/SMS/08 - Agosto/15-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.17 <- read.csv2("./data/SMS/08 - Agosto/17-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.18 <- read.csv2("./data/SMS/08 - Agosto/18-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.19 <- read.csv2("./data/SMS/08 - Agosto/19-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.20 <- read.csv2("./data/SMS/08 - Agosto/20-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.21 <- read.csv2("./data/SMS/08 - Agosto/21-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.24 <- read.csv2("./data/SMS/08 - Agosto/24-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.25 <- read.csv2("./data/SMS/08 - Agosto/25-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.26 <- read.csv2("./data/SMS/08 - Agosto/26-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.27 <- read.csv2("./data/SMS/08 - Agosto/27-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.28 <- read.csv2("./data/SMS/08 - Agosto/28-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.29 <- read.csv2("./data/SMS/08 - Agosto/29-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.30 <- read.csv2("./data/SMS/08 - Agosto/30-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.08.31 <- read.csv2("./data/SMS/08 - Agosto/31-08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")

# agrupando por dia
df_sms.08.2015 <- bind_rows(list(df_sms.08.04,
                                 df_sms.08.05,
                                 df_sms.08.06,
                                 df_sms.08.07,
                                 df_sms.08.10,
                                 df_sms.08.11,
                                 df_sms.08.12,
                                 df_sms.08.13,
                                 df_sms.08.14,
                                 df_sms.08.15, 
                                 df_sms.08.17, 
                                 df_sms.08.18, 
                                 df_sms.08.19, 
                                 df_sms.08.20,
                                 df_sms.08.21,
                                 df_sms.08.24,
                                 df_sms.08.25,
                                 df_sms.08.26,
                                 df_sms.08.27,
                                 df_sms.08.28,
                                 df_sms.08.29,
                                 df_sms.08.30,
                                 df_sms.08.31
))

# SMS de 09 2015
df_sms.09.01 <- read.csv2("./data/SMS/09 - Setembro/01-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.02 <- read.csv2("./data/SMS/09 - Setembro/02-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.03 <- read.csv2("./data/SMS/09 - Setembro/03-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.04 <- read.csv2("./data/SMS/09 - Setembro/04-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.08 <- read.csv2("./data/SMS/09 - Setembro/08-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.09 <- read.csv2("./data/SMS/09 - Setembro/09-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.10 <- read.csv2("./data/SMS/09 - Setembro/10-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.11 <- read.csv2("./data/SMS/09 - Setembro/11-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.12 <- read.csv2("./data/SMS/09 - Setembro/12-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.14 <- read.csv2("./data/SMS/09 - Setembro/14-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.15 <- read.csv2("./data/SMS/09 - Setembro/15-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.16 <- read.csv2("./data/SMS/09 - Setembro/16-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.17 <- read.csv2("./data/SMS/09 - Setembro/17-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.18 <- read.csv2("./data/SMS/09 - Setembro/18-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.19 <- read.csv2("./data/SMS/09 - Setembro/19-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.21 <- read.csv2("./data/SMS/09 - Setembro/21-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.22 <- read.csv2("./data/SMS/09 - Setembro/22-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.23 <- read.csv2("./data/SMS/09 - Setembro/23-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.24 <- read.csv2("./data/SMS/09 - Setembro/24-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.25 <- read.csv2("./data/SMS/09 - Setembro/25-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.26 <- read.csv2("./data/SMS/09 - Setembro/26-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.28 <- read.csv2("./data/SMS/09 - Setembro/28-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.29 <- read.csv2("./data/SMS/09 - Setembro/29-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.09.30 <- read.csv2("./data/SMS/09 - Setembro/30-09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")

# agrupando por dia
df_sms.09.2015 <- bind_rows(list(df_sms.09.01,
                                 df_sms.09.02,
                                 df_sms.09.03,
                                 df_sms.09.04,
                                 df_sms.09.08,
                                 df_sms.09.09,
                                 df_sms.09.10,
                                 df_sms.09.11,
                                 df_sms.09.14,
                                 df_sms.09.15, 
                                 df_sms.09.16, 
                                 df_sms.09.17, 
                                 df_sms.09.18, 
                                 df_sms.09.19,
                                 df_sms.09.21,
                                 df_sms.09.22,
                                 df_sms.09.23,
                                 df_sms.09.24,
                                 df_sms.09.25,
                                 df_sms.09.26,
                                 df_sms.09.28,
                                 df_sms.09.29,
                                 df_sms.09.30
))

# SMS de 10 2015
df_sms.10.01 <- read.csv2("./data/SMS/10 - Outubro/01.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.10.02 <- read.csv2("./data/SMS/10 - Outubro/02.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.10.03 <- read.csv2("./data/SMS/10 - Outubro/03.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.10.05 <- read.csv2("./data/SMS/10 - Outubro/05.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.10.06 <- read.csv2("./data/SMS/10 - Outubro/06.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.10.07 <- read.csv2("./data/SMS/10 - Outubro/07.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.10.08 <- read.csv2("./data/SMS/10 - Outubro/08.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.10.09 <- read.csv2("./data/SMS/10 - Outubro/09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.10.13 <- read.csv2("./data/SMS/10 - Outubro/13.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.10.14 <- read.csv2("./data/SMS/10 - Outubro/14.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.10.15 <- read.csv2("./data/SMS/10 - Outubro/15.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.10.16 <- read.csv2("./data/SMS/10 - Outubro/16.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.10.19 <- read.csv2("./data/SMS/10 - Outubro/19.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.10.23 <- read.csv2("./data/SMS/10 - Outubro/23.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.10.26 <- read.csv2("./data/SMS/10 - Outubro/26.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.10.28 <- read.csv2("./data/SMS/10 - Outubro/28.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.10.29 <- read.csv2("./data/SMS/10 - Outubro/29.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.10.30 <- read.csv2("./data/SMS/10 - Outubro/30.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")

# agrupando por dia
df_sms.10.2015 <- bind_rows(list(df_sms.10.01,
                                 df_sms.10.02,
                                 df_sms.10.03,
                                 df_sms.10.05,
                                 df_sms.10.06,
                                 df_sms.10.07,
                                 df_sms.10.08,
                                 df_sms.10.09,
                                 df_sms.10.13,
                                 df_sms.10.14, 
                                 df_sms.10.15, 
                                 df_sms.10.16, 
                                 df_sms.10.19, 
                                 df_sms.10.23,
                                 df_sms.10.26,
                                 df_sms.10.28,
                                 df_sms.10.29,
                                 df_sms.10.30
))

# SMS de 11 2015
df_sms.11.03 <- read.csv2("./data/SMS/11 - Novembro/03.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.04 <- read.csv2("./data/SMS/11 - Novembro/04.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.05 <- read.csv2("./data/SMS/11 - Novembro/05.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.06 <- read.csv2("./data/SMS/11 - Novembro/06.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.09 <- read.csv2("./data/SMS/11 - Novembro/09.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.10 <- read.csv2("./data/SMS/11 - Novembro/10.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.11 <- read.csv2("./data/SMS/11 - Novembro/11.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.12 <- read.csv2("./data/SMS/11 - Novembro/12.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.13 <- read.csv2("./data/SMS/11 - Novembro/13.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.16 <- read.csv2("./data/SMS/11 - Novembro/16.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.17 <- read.csv2("./data/SMS/11 - Novembro/17.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.18 <- read.csv2("./data/SMS/11 - Novembro/18.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.19 <- read.csv2("./data/SMS/11 - Novembro/19.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.20 <- read.csv2("./data/SMS/11 - Novembro/20.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.22 <- read.csv2("./data/SMS/11 - Novembro/22.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.23 <- read.csv2("./data/SMS/11 - Novembro/23.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.24 <- read.csv2("./data/SMS/11 - Novembro/24.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.25 <- read.csv2("./data/SMS/11 - Novembro/25.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.26 <- read.csv2("./data/SMS/11 - Novembro/26.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.27 <- read.csv2("./data/SMS/11 - Novembro/27.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")
df_sms.11.30 <- read.csv2("./data/SMS/11 - Novembro/30.csv", encoding="latin1",
                          stringsAsFactors = FALSE, header = TRUE, skip = 1, sep = ";")

# agrupando por dia
df_sms.11.2015 <- bind_rows(list(df_sms.11.03,
                                 df_sms.11.04,
                                 df_sms.11.05,
                                 df_sms.11.06,
                                 df_sms.11.09,
                                 df_sms.11.10,
                                 df_sms.11.11,
                                 df_sms.11.12,
                                 df_sms.11.13,
                                 df_sms.11.16, 
                                 df_sms.11.17, 
                                 df_sms.11.18, 
                                 df_sms.11.19, 
                                 df_sms.11.20,
                                 df_sms.11.22,
                                 df_sms.11.23,
                                 df_sms.11.24,
                                 df_sms.11.25,
                                 df_sms.11.26,
                                 df_sms.11.27,
                                 df_sms.11.30
))

# concatenando todos os meses
df_sms.2015 <- bind_rows(list(df_sms.04.2015,
                              df_sms.05.2015,
                              df_sms.06.2015,
                              df_sms.07.2015,
                              df_sms.08.2015,
                              df_sms.09.2015,
                              df_sms.10.2015,
                              df_sms.11.2015))
# agrupar totais por dia, totais pro dia sem confirmação, totais por dia com confirmação
# estatísticas iniciais
#table(df_sms.2015$Status)
prop.table(table(df_sms.2015$Status))
# eliminando não recebidos e bloqueados
df_sms.2015 <-
    df_sms.2015 %>%
    filter(!(grepl("Não Recebido|Bloqueado",Status)))
# eliminando não recebidos
df_sms.2015 <-
    df_sms.2015 %>%
    filter(!(grepl("Não Recebido",Status)))

# separando não confirmados de confirmados

# ACIONAMENTOS
##################
# filtrando Confirmados
df_sms.2015.conf <-
    df_sms.2015 %>%
    filter(grepl("Entregue com Confirmação",Status))

# preparando para plot de time serie
library(lubridate)
# primeiro cnvertendo coluna em formato Date
#as.Date(d, "%d/%m/%Y")
#  obs: tem que converter para POSIXct para funcionar com dplyr!!!!
df_sms.2015.conf$Enviado.em <- as.Date(df_sms.2015.conf$Enviado.em, "%d/%m/%Y")
# transformando em PosixCt
df_sms.2015.conf$Enviado.em <- as.POSIXct(df_sms.2015.conf$Enviado.em)
# agrupando por dia 
df_sms.2015.conf <-
    df_sms.2015.conf %>%
    group_by(Enviado.em) %>%
    summarise(acions.dia = n())

# filtrando Não Confirmados
df_sms.2015.nconf <-
    df_sms.2015 %>%
    filter(grepl("Entregue sem Confirmação",Status))
library(ggplot2)
# plotando time series
pl_sms <- ggplot(df_sms.2015.conf, aes(Enviado.em, acions.dia)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("acionamentos") + ggtitle("SMS com Recebimento Confirmado/Dia") +
    xlim(c(min(df_acion_dia$DIA.ACION),max(df_acion_dia$DIA.ACION)))










# confirmado: no memso dia não se repetiram envios ao mesmo celular
#x <-
#    df_sms.04.2015 %>%
#    group_by(Celular)



#++++++++++++++++++++++++++++++++++++++++++++++++++
# ATE AQUI OK

# testando correlação entre acionamento e pagamento
# antes fazer o merge para pegar as mesmas datas
x <- df_pg_dia
x$DTpgto <- as.character(x$DTpgto)
y <- df_acion_sms_dia
y$DIA.ACION <- as.character(y$DIA.ACION)
my.dt.acion <- merge(x, y, by.x = "DTpgto", by.y = "DIA.ACION")
#my.dt.acion <- inner_join(x, y, by = c("DTpgto" = "DIA.ACION"))
my.cor <- cor(my.dt.acion$pgto.dia, my.dt.acion$acions.dia)
(my.cor)
# FALTA: criar diferentes time series para: nro contratos/dia, nro pgtos/dia
# nro sms/dia, nro ativo/dia, nro passivo, dia (combinar cada um no memso plot de pgto)
# criar tb nro pgto/dia x nro/aciona/dia por cada operador