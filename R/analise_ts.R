# análises exploratórias de time series
require("gridExtra")
require("ggplot2")

valor_divida = 1.0
l_raw <- f_leRawCobra(valor_divida)
# obtém dataframe
df_acion <- l_raw[[1]]
df_carteira <- l_raw[[2]]
df_pg <- l_raw[[3]]

# ACIONAMENTOS
##################

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
library(grid)
pushViewport(viewport(layout = grid.layout(2, 1)))
print(pl_ac, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_pg, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))

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
pl_ac <- ggplot(df_acion_sms_dia, aes(DIA.ACION, acions.dia)) + geom_line() + geom_smooth() +
    xlab("dia") + ylab("acionamentos") + ggtitle("Acionamentos (SMS e Fone)/Dia") +
    xlim(c(min(df_acion_sms_dia$DIA.ACION),max(df_acion_sms_dia$DIA.ACION)))
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