# funcão de correlacionar time series de sms x pgtos realizadoa
# input: número de acionamentos de sms por celular
# output: lista com os plots gerados para acionamento total, confirmado e não confirmado,
# dataframes para acionamentos total, confirmados e não confirmados a serem aplicados a
# funcão ccf()

f_ccf_sms_pgto <- function(in.df_sms.2015, in.num.sms) {
    require("ggplot2")
    require("dplyr")
    require("xlsx")
    require("lubridate")
    require("grid")
    
    #source("~/Documents/MyGit/COBRA/R/f_le_sms.R")
    
    #registerDoMC(5) # parallel processing
    
    # PGTO 
    ##############
    # obter dados de pgto totais (não somente Avon)
    ################################################################
    
    #getOption("scipen") # obtendo default para display de notação científica em R
    options(scipen=999) # removendo display de notação científica em R
    
    df_pg <- read.csv("./data/Dados Raw-pgtos agencia 4c.csv", sep = ",", header = TRUE, 
                      stringsAsFactors = FALSE,na.strings = "NULL")
    df_pg <- na.omit(df_pg)
    # converting from chr to Date format
    df_pg$DTpgto <- as.Date(df_pg$DTpgto, "%m/%d/%y")
    
    # eliminando virgula de milhar para conversão abaixo não forçar NA neste casos
    df_pg <-
        df_pg %>%
        mutate (VlPag = as.numeric(gsub(",","", VlPag))) %>% # forçando Valor numeric
        rename(CONTRATO = CONTR_CON)
    
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
    # removendo arquivo já usados
    rm(df_pg)
    rm(df_pg.primpg)
    
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
    
    
    # sumarizando acionamento por celular para posterior filtro nos dados lidos
    df_sms.2015.cel <-
        in.df_sms.2015 %>%
        group_by(Celular) %>%
        summarise(acions.dia = n()) %>%
        filter(acions.dia == in.num.sms)
    
    # filtra dos dados originais somente estes celulares
    in.df_sms.2015 <- inner_join(in.df_sms.2015,df_sms.2015.cel, by = "Celular")
    
    # SMS total agrupado por dia 
    df_sms.2015.tot <-
        in.df_sms.2015 %>%
        group_by(Enviado.em) %>%
        summarise(acions.dia = n())
    
    # separando não confirmados de confirmados
    
    # ACIONAMENTOS SMS
    ##################
    # filtrando Confirmados
    #########################
    df_sms.2015.conf <-
        in.df_sms.2015 %>%
        filter(grepl("Entregue com Confirmação",Status))
    
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
        in.df_sms.2015 %>%
        filter(grepl("Entregue sem Confirmação",Status))
    
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
    
    # PLOTS
    pl_prim_npg <- ggplot(df_pg.primpg_dia, aes(DTpgto, pgto.dia)) + geom_line() + geom_smooth() +
        xlab("dia") + ylab("pagamentos") + ggtitle("Qtde de 1o Pagamento/Dia") +
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
    
    # CORRELAÇÕES
    #----------------------------------------------
    # ABAIXO correlacionar time series
    # correlacionar nro de pgtos e velor de pgtos com
    # 1. sms enviados com confirmação
    # 2. sms enviados sem confirmação
    # 3. sms totais enviados
    # -----------------------------------------------
    # mostra lag entre acionamento e pgto
    # antes fazer merge por dia para colocar os valores no mesmo dia
    # correlação positiva significa que quando uma cresce a outra tb e vice-versa
    # o valor do eixo y indica a correlação (max = 1)
    # o fato de existir lag (pico fora do zero no eixo x) significa que uma variável conduz a outra,
    # ou seja, não podemos prever o movimento de uma variável olhando para a outra
    
    # 1. Correlação sms enviado com confirmação x qtde de pgtos
    # considerando apenas primeiro pgto
    my.corr.prpg <- df_pg.primpg_dia
    my.corr.prpg$DTpgto <- as.character(my.corr.prpg$DTpgto) # mudando para character para funcionar ccf()
    
    # correlacionar sms confirmado com nro pgto
    #############################
    my.corr.sms.conf <- df_sms.2015.conf
    my.corr.sms.conf$Enviado.em <- as.character(my.corr.sms.conf$Enviado.em) # mudando para character para funcionar ccf()
    
    # correlação com nro de primeiros pgtos dia
    PGxSMS_Conf <- full_join(my.corr.prpg,my.corr.sms.conf, by=c("DTpgto" = "Enviado.em"))
    # TESTE: transformar NA em 0 (MELHOROU!!!)
    NPGxSMS_Conf <-
        PGxSMS_Conf %>%
        mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia),
               pgto.dia = ifelse(is.na(pgto.dia), 0, pgto.dia),
               vlpg.dia = ifelse(is.na(vlpg.dia), 0, vlpg.dia))
    
    # 2. sms enviado SEM confirmação x qtde de pgtos
    ################################################
    my.corr.sms.nconf <- df_sms.2015.nconf
    my.corr.sms.nconf$Enviado.em <- as.character(my.corr.sms.nconf$Enviado.em)

    PGxSMS_NConf <- full_join(my.corr.prpg,my.corr.sms.nconf, by=c("DTpgto" = "Enviado.em"))
    NPGxSMS_NConf <-
        PGxSMS_NConf %>%
        mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia),
               pgto.dia = ifelse(is.na(pgto.dia), 0, pgto.dia),
               vlpg.dia = ifelse(is.na(vlpg.dia), 0, vlpg.dia))
    
    # 3. sms totais enviados
    my.corr.sms.tot <- df_sms.2015.tot
    my.corr.sms.tot$Enviado.em <- as.character(my.corr.sms.tot$Enviado.em)

    PGxSMS_TOT <- full_join(my.corr.prpg,my.corr.sms.tot, by=c("DTpgto" = "Enviado.em"))
    NPGxSMS_TOT <-
        PGxSMS_TOT %>%
        mutate(acions.dia = ifelse(is.na(acions.dia), 0, acions.dia),
               pgto.dia = ifelse(is.na(pgto.dia), 0, pgto.dia),
               vlpg.dia = ifelse(is.na(vlpg.dia), 0, vlpg.dia))
    
    # PLOTANDO AS CORRELAÇÕES
    ################################
    
    #par(mfrow=c(2,3))
    #ccf(NPGxSMS_Conf$acions.dia ,NPGxSMS_Conf$pgto.dia, 
    #    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 3 acionamentos")
    
    #ccf(NPGxSMS_NConf$acions.dia ,NPGxSMS_NConf$pgto.dia, 
    #    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS NÃO Confirmado")
    
    #ccf(NPGxSMS_TOT$acions.dia ,NPGxSMS_TOT$pgto.dia, 
    #    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Total")
    
    # retorna plots e dataframes para aplicar funcao ccf
    out.list <- list(pl_prim_npg,pl_sms_tot,pl_sms_conf,pl_sms_nconf,
                     NPGxSMS_NConf, PGxSMS_Conf, PGxSMS_TOT)
}