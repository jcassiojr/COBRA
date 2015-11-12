# análises gerais
require("xlsx")
require("dplyr")
require("lubridate")
df_acion <- read.xlsx2("./data/Acionamentos out e nov 2015-raw.xlsx", sheetIndex = 1, header = TRUE)
# acerto de datas
df_acion <-
    df_acion %>%
    mutate (Acionamento = dmy_hm(Acionamento),
            diasem.acion = wday(Acionamento, label = TRUE),
            hora.acion = hour(Acionamento),
            dia = day(Acionamento),
            mes = month(Acionamento),
            ano = year(Acionamento))
        
# totais de acionamentos por mês
df_sumar_mes <-
    df_acion %>%
    group_by(ano,mes) %>%
    summarize(tt_acion = n()) # total de chamadas no mês

# totais de acionamentos por dia
df_sumar_dia <-
    df_acion %>%
    group_by(ano,mes,dia) %>%
    summarize(tt_acion = n()) # total de chamadas no mês

# totais de operadores
df_tt_oper <-
    df_acion %>%
    group_by(Operador) %>%
    summarize(tt_acion = n()) # total de chamadas no mês

# medias de acionamentos diários
df_sumar_mn_dia <-
    df_sumar_dia %>%
    group_by(ano) %>%
    summarize(md_mes = (round(mean(tt_acion),2))) # total de chamadas no mês 

# totais por dia e operador (criar heatmap??!!)
df_sumar_dia_oper <-
    df_acion %>%
    group_by(ano,mes,dia, Operador) %>%
    summarize(tt_acion = n()) # total de chamadas no mês

# medias de acionamentos diários por operador
df_sumar_mn_oper <-
    df_sumar_dia_oper %>%
    group_by(Operador) %>%
    summarize(md_oper = (round(mean(tt_acion),2))) # total de chamadas no mês 
# média geral de acionamentos/dia por operador
mn_acion_opr <- mean(df_sumar_mn_oper$md_oper)
