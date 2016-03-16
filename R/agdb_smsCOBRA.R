# script que mostra resultados para SMSs
require("doMC")

source("~/Documents/MyGit/COBRA/R/f_ccf_sms_pgto.R")
source("~/Documents/MyGit/COBRA/R/f_le_sms.R")

registerDoMC(5) # parallel processing

# LE arquivos SMS
##########################################
df_sms.2015 <- f_le_sms()
# transformando em caracter o celular
df_sms.2015 <-
    df_sms.2015 %>%
    mutate(Celular = as.character(Celular))
# eliminando números esquisitos: terminados em 0000
df_sms.2015 <-
    df_sms.2015 %>%
    filter(!grepl("0000$",Celular))

# agrupar totais por dia, totais por dia sem confirmação, totais por dia com confirmação
# estatísticas iniciais
#table(in.df_sms.2015$Status)
#prop.table(table(in.df_sms.2015$Status)) # confirmados: 34%, não confirmados: 40%, bloqueados: 4%, Não recebidos: 23%

# eliminando não recebidos e bloqueados
df_sms.2015 <-
    df_sms.2015 %>%
    filter(!(grepl("Não Recebido|Bloqueado",Status)))

# preparando formato correto de datas para plot
df_sms.2015$Enviado.em <- as.Date(df_sms.2015$Enviado.em, "%d/%m/%Y")


# chama funcao passando o numero de acionamentos de sms desejado por celular
#nr.sms  <- 5 # número de acionamentos por celular para filtrar os dados
#my.list <- f_ccf_sms_pgto(df_sms.2015, nr.sms) 

# PLOTS das time series
#pushViewport(viewport(layout = grid.layout(4, 1)))
#print(my.list[[1]], vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
#print(my.list[[2]], vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
#print(my.list[[3]], vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
#print(my.list[[4]], vp = viewport(layout.pos.row = 4, layout.pos.col = 1))

# CROSS CORRELATIONS
#par(mfrow=c(1,3))
#ccf(my.list[[5]]$acions.dia ,my.list[[5]]$pgto.dia, 
#    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = paste0("SMS NÃO Confirmado - ", nr.sms, " acionamentos"))

#ccf(my.list[[6]]$acions.dia ,my.list[[6]]$pgto.dia, 
#    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = paste0("SMS Confirmado - ", nr.sms, " acionamentos"))

#ccf(my.list[[7]]$acions.dia ,my.list[[7]]$pgto.dia, 
#    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = paste0("SMS Total - ", nr.sms, " acionamentos"))

# varios acessos para confirmados
# loop para gerar lista de 1 a 15 acionamentos
max.nacion <- 15
my.list.nr <- list()
for(i in 1:max.nacion) {
    my.list.nrsms[i] <- f_ccf_sms_pgto(df_sms.2015, i)
}
nr.sms  <- 1 # número de acionamentos por celular para filtrar os dados
my.list.1 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
nr.sms  <- 2 # número de acionamentos por celular para filtrar os dados
my.list.2 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
nr.sms  <- 3 # número de acionamentos por celular para filtrar os dados
my.list.3 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
nr.sms  <- 4 # número de acionamentos por celular para filtrar os dados
my.list.4 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
nr.sms  <- 5 # número de acionamentos por celular para filtrar os dados
my.list.5 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
nr.sms  <- 6 # número de acionamentos por celular para filtrar os dados
my.list.6 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
nr.sms  <- 7 # número de acionamentos por celular para filtrar os dados
my.list.7 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
nr.sms  <- 8 # número de acionamentos por celular para filtrar os dados
my.list.8 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
nr.sms  <- 9 # número de acionamentos por celular para filtrar os dados
my.list.9 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
nr.sms  <- 10 # número de acionamentos por celular para filtrar os dados
my.list.10 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
nr.sms  <- 11 # número de acionamentos por celular para filtrar os dados
my.list.11 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
nr.sms  <- 12 # número de acionamentos por celular para filtrar os dados
my.list.12 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
nr.sms  <- 13 # número de acionamentos por celular para filtrar os dados
my.list.13 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
nr.sms  <- 14 # número de acionamentos por celular para filtrar os dados
my.list.14 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
nr.sms  <- 15 # número de acionamentos por celular para filtrar os dados
my.list.15 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)

# correlacoes para acionamentos de 1 a 10, confirmados
par(mfrow=c(2,3))

my.ccf.1 <- ccf(my.list.1[[6]]$acions.dia ,my.list.1[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 1 acionamento")
my.ccf.2 <- ccf(my.list.2[[6]]$acions.dia ,my.list.2[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 2 acionamentos")
my.ccf.3 <- ccf(my.list.3[[6]]$acions.dia ,my.list.3[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 3 acionamentos")
my.ccf.4 <- ccf(my.list.4[[6]]$acions.dia ,my.list.4[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 4 acionamentos")
my.ccf.5 <- ccf(my.list.5[[6]]$acions.dia ,my.list.5[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 5 acionamentos")
my.ccf.6 <- ccf(my.list.6[[6]]$acions.dia ,my.list.6[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 6 acionamentos")

par(mfrow=c(2,3))
my.ccf.7 <- ccf(my.list.7[[6]]$acions.dia ,my.list.7[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "7 confirmed SMS")
my.ccf.8 <- ccf(my.list.8[[6]]$acions.dia ,my.list.8[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "8 confirmed SMS")
my.ccf.9 <- ccf(my.list.9[[6]]$acions.dia ,my.list.9[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "9 confirmed SMS")
my.ccf.10 <- ccf(my.list.10[[6]]$acions.dia ,my.list.10[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "10 confirmed SMS")

par(mfrow=c(2,3))
my.ccf.11 <- ccf(my.list.11[[6]]$acions.dia ,my.list.11[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 11 acionamentos")
my.ccf.12 <- ccf(my.list.12[[6]]$acions.dia ,my.list.12[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 12 acionamentos")
my.ccf.13 <- ccf(my.list.13[[6]]$acions.dia ,my.list.13[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 13 acionamentos")
my.ccf.14 <- ccf(my.list.14[[6]]$acions.dia ,my.list.14[[6]]$pgto.dia, 
                 na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 14 acionamentos")
my.ccf.15 <- ccf(my.list.15[[6]]$acions.dia ,my.list.15[[6]]$pgto.dia, 
                 na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 15 acionamentos")

my.df_max_corr <- data.frame()

# criando data frame de lag x max(corr) 1 acionamento
my.v_corr <- as.numeric(my.ccf.1$acf)
my.v_lag <- as.numeric(my.ccf.1$lag)
my.df_corr <- as.data.frame(cbind(lag = my.v_lag, corr = my.v_corr)) #data.frame com lags
my.df_corr <-
    my.df_corr %>%
    filter(lag < 0)
#my.df_corr [which.max(my.df_corr[,2]),]

my.df_max_corr <- rbind(my.df_max_corr,my.df_corr [which.max(my.df_corr[,2]),])

# criando data frame de lag x max(corr) 2 acionamento
my.v_corr <- as.numeric(my.ccf.2$acf)
my.v_lag <- as.numeric(my.ccf.2$lag)
my.df_corr <- as.data.frame(cbind(lag = my.v_lag, corr = my.v_corr)) #data.frame com lags
my.df_corr <-
    my.df_corr %>%
    filter(lag < 0)
#my.df_corr [which.max(my.df_corr[,2]),]

my.df_max_corr <- rbind(my.df_max_corr,my.df_corr [which.max(my.df_corr[,2]),])

# criando data frame de lag x max(corr) 3 acionamento
my.v_corr <- as.numeric(my.ccf.3$acf)
my.v_lag <- as.numeric(my.ccf.3$lag)
my.df_corr <- as.data.frame(cbind(lag = my.v_lag, corr = my.v_corr)) #data.frame com lags
my.df_corr <-
    my.df_corr %>%
    filter(lag < 0)
#my.df_corr [which.max(my.df_corr[,2]),]

my.df_max_corr <- rbind(my.df_max_corr,my.df_corr [which.max(my.df_corr[,2]),])

# criando data frame de lag x max(corr) 3 acionamento
my.v_corr <- as.numeric(my.ccf.4$acf)
my.v_lag <- as.numeric(my.ccf.4$lag)
my.df_corr <- as.data.frame(cbind(lag = my.v_lag, corr = my.v_corr)) #data.frame com lags
my.df_corr <-
    my.df_corr %>%
    filter(lag < 0)
#my.df_corr [which.max(my.df_corr[,2]),]

my.df_max_corr <- rbind(my.df_max_corr,my.df_corr [which.max(my.df_corr[,2]),])

# criando data frame de lag x max(corr) 3 acionamento
my.v_corr <- as.numeric(my.ccf.5$acf)
my.v_lag <- as.numeric(my.ccf.5$lag)
my.df_corr <- as.data.frame(cbind(lag = my.v_lag, corr = my.v_corr)) #data.frame com lags
my.df_corr <-
    my.df_corr %>%
    filter(lag < 0)
#my.df_corr [which.max(my.df_corr[,2]),]

my.df_max_corr <- rbind(my.df_max_corr,my.df_corr [which.max(my.df_corr[,2]),])

# criando data frame de lag x max(corr) 3 acionamento
my.v_corr <- as.numeric(my.ccf.6$acf)
my.v_lag <- as.numeric(my.ccf.6$lag)
my.df_corr <- as.data.frame(cbind(lag = my.v_lag, corr = my.v_corr)) #data.frame com lags
my.df_corr <-
    my.df_corr %>%
    filter(lag < 0)
#my.df_corr [which.max(my.df_corr[,2]),]

my.df_max_corr <- rbind(my.df_max_corr,my.df_corr [which.max(my.df_corr[,2]),])

# criando data frame de lag x max(corr) 3 acionamento
my.v_corr <- as.numeric(my.ccf.7$acf)
my.v_lag <- as.numeric(my.ccf.7$lag)
my.df_corr <- as.data.frame(cbind(lag = my.v_lag, corr = my.v_corr)) #data.frame com lags
my.df_corr <-
    my.df_corr %>%
    filter(lag < 0)
#my.df_corr [which.max(my.df_corr[,2]),]

my.df_max_corr <- rbind(my.df_max_corr,my.df_corr [which.max(my.df_corr[,2]),])

# criando data frame de lag x max(corr) 3 acionamento
my.v_corr <- as.numeric(my.ccf.8$acf)
my.v_lag <- as.numeric(my.ccf.8$lag)
my.df_corr <- as.data.frame(cbind(lag = my.v_lag, corr = my.v_corr)) #data.frame com lags
my.df_corr <-
    my.df_corr %>%
    filter(lag < 0)
#my.df_corr [which.max(my.df_corr[,2]),]

my.df_max_corr <- rbind(my.df_max_corr,my.df_corr [which.max(my.df_corr[,2]),])

# criando data frame de lag x max(corr) 3 acionamento
my.v_corr <- as.numeric(my.ccf.9$acf)
my.v_lag <- as.numeric(my.ccf.9$lag)
my.df_corr <- as.data.frame(cbind(lag = my.v_lag, corr = my.v_corr)) #data.frame com lags
my.df_corr <-
    my.df_corr %>%
    filter(lag < 0)
#my.df_corr [which.max(my.df_corr[,2]),]

my.df_max_corr <- rbind(my.df_max_corr,my.df_corr [which.max(my.df_corr[,2]),])

# criando data frame de lag x max(corr) 3 acionamento
my.v_corr <- as.numeric(my.ccf.10$acf)
my.v_lag <- as.numeric(my.ccf.10$lag)
my.df_corr <- as.data.frame(cbind(lag = my.v_lag, corr = my.v_corr)) #data.frame com lags
my.df_corr <-
    my.df_corr %>%
    filter(lag < 0)
#my.df_corr [which.max(my.df_corr[,2]),]

my.df_max_corr <- rbind(my.df_max_corr,my.df_corr [which.max(my.df_corr[,2]),])

# criando data frame de lag x max(corr) 3 acionamento
my.v_corr <- as.numeric(my.ccf.11$acf)
my.v_lag <- as.numeric(my.ccf.11$lag)
my.df_corr <- as.data.frame(cbind(lag = my.v_lag, corr = my.v_corr)) #data.frame com lags
my.df_corr <-
    my.df_corr %>%
    filter(lag < 0)
#my.df_corr [which.max(my.df_corr[,2]),]

my.df_max_corr <- rbind(my.df_max_corr,my.df_corr [which.max(my.df_corr[,2]),])

# criando data frame de lag x max(corr) 3 acionamento
my.v_corr <- as.numeric(my.ccf.12$acf)
my.v_lag <- as.numeric(my.ccf.12$lag)
my.df_corr <- as.data.frame(cbind(lag = my.v_lag, corr = my.v_corr)) #data.frame com lags
my.df_corr <-
    my.df_corr %>%
    filter(lag < 0)
#my.df_corr [which.max(my.df_corr[,2]),]

my.df_max_corr <- rbind(my.df_max_corr,my.df_corr [which.max(my.df_corr[,2]),])

# criando data frame de lag x max(corr) 3 acionamento
my.v_corr <- as.numeric(my.ccf.13$acf)
my.v_lag <- as.numeric(my.ccf.13$lag)
my.df_corr <- as.data.frame(cbind(lag = my.v_lag, corr = my.v_corr)) #data.frame com lags
my.df_corr <-
    my.df_corr %>%
    filter(lag < 0)
#my.df_corr [which.max(my.df_corr[,2]),]

my.df_max_corr <- rbind(my.df_max_corr,my.df_corr [which.max(my.df_corr[,2]),])

# criando data frame de lag x max(corr) 3 acionamento
my.v_corr <- as.numeric(my.ccf.14$acf)
my.v_lag <- as.numeric(my.ccf.14$lag)
my.df_corr <- as.data.frame(cbind(lag = my.v_lag, corr = my.v_corr)) #data.frame com lags
my.df_corr <-
    my.df_corr %>%
    filter(lag < 0)
#my.df_corr [which.max(my.df_corr[,2]),]

my.df_max_corr <- rbind(my.df_max_corr,my.df_corr [which.max(my.df_corr[,2]),])

# criando data frame de lag x max(corr) 3 acionamento
my.v_corr <- as.numeric(my.ccf.15$acf)
my.v_lag <- as.numeric(my.ccf.15$lag)
my.df_corr <- as.data.frame(cbind(lag = my.v_lag, corr = my.v_corr)) #data.frame com lags
my.df_corr <-
    my.df_corr %>%
    filter(lag < 0)
#my.df_corr [which.max(my.df_corr[,2]),]

my.df_max_corr <- rbind(my.df_max_corr,my.df_corr [which.max(my.df_corr[,2]),])

my.df_max_corr <-
    my.df_max_corr %>%
    mutate(n.acion = seq(1:15))

# plot de maiores correlações por lag

pl_max_lag <- ggplot(my.df_max_corr, aes(lag, corr)) + geom_line() + geom_smooth() +
    xlab("lag") + ylab("correlação") + 
    ggtitle("Máximo de Pgtos: 10-13 dias após SMS confirmados") 
    #ylim(c(min(my.df_max_corr$corr),max(my.df_max_corr$corr)))

# plot de maiores correlações x nro de acionamentos

pl_max_corr <- ggplot(my.df_max_corr, aes(n.acion, corr)) + geom_line() + geom_smooth() +
    xlab("# acionamentos") + ylab("correlação") + 
    ggtitle("Máximo de Pgtos: 8 acionamentos SMS confirmados") 
#xlim(c(min(df_pg.primpg_dia$DTpgto),max(df_pg.primpg_dia$DTpgto)))

pushViewport(viewport(layout = grid.layout(1, 2)))
print(pl_max_lag, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_max_corr, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# tabela de maiores corelações e em que lag (montar)
# salvar cada objeto retornado de ccf acima e selecionar o lag com maior correlação
# para cada nro de acionamentos, montando data frame nr.acion.por x max(corr)
# fazer plot dos máximos par amostra pico em 8 acions!!



# TESTE++++++++ DAQUI P BAIXO OK +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# usar abaixo para filtrar por DDD - SP e depois agrupar para análise
#################################
df_sms.2015.SP <-
    df_sms.2015 %>%
    filter(grepl("^551",Celular))

# loop para gerar lista de 1 a 15 acionamentos
max.nacion <- 15
my.df_max_corr <- data.frame()

for(i in 1:max.nacion) {
    my.list <- f_ccf_sms_pgto(df_sms.2015.SP, i)
    my.ccf <- ccf(my.list[[6]]$acions.dia ,my.list[[6]]$pgto.dia, 
                    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), 
                    plot = FALSE)

    my.v_corr <- as.numeric(my.ccf$acf)
    my.v_lag <- as.numeric(my.ccf$lag)
    my.df_corr <- as.data.frame(cbind(lag = my.v_lag, corr = my.v_corr)) #data.frame com lags
    my.df_corr <-
        my.df_corr %>%
        filter(lag < 0)
    my.df_max_corr <- rbind(my.df_max_corr,my.df_corr [which.max(my.df_corr[,2]),])    
}

my.df_max_corr <-
    my.df_max_corr %>%
    mutate(n.acion = seq(1:15))

# plot de maiores correlações por lag
# FALTE DEFINIR O CONCEITO IMPORTANTE AQUI
# é importante saber quais os nro acionamentos tem maior correlacao com pgto (plot2)
# e quais lags tem maior correlaçao com pgto. Portanto, plot lag x corr
# IMPORTANTE: DESCOBRI CORRELACAO ENTRE corr e n.acions!!! tem sentido (ver abaixo!!!)
# a correlaçao mede o quanto o pagto está relacionado ao acionamento. Se a corr está correlacionada ao
# nro de acionamentos, podemos confirmar que quanto mais mandamos sms conformado mais forte a
# correlacao com pgto efetuado!!!!!
# FALTA OBTER O R-Square desta correlação
x <- lm(my.df_max_corr$corr ~ my.df_max_corr$n.acion)
summary(x) # r-squared = 78% !!!!!

plot(my.df_max_corr)
# E LAG x CORR mostra clusterização entre -15 e -20 dias!!!!!
# Elaborar acima na apresentação e no post˜˜˜

pl_max_lag <- ggplot(my.df_max_corr, aes(lag, corr)) + geom_line() + 
    #geom_smooth() +
    xlab("lag") + ylab("correlação") + 
    ggtitle("Máximo de Pgtos - SMS confirmados-SP") 
#ylim(c(min(my.df_max_corr$corr),max(my.df_max_corr$corr)))

# plot de maiores correlações x nro de acionamentos

pl_max_corr <- ggplot(my.df_max_corr, aes(n.acion, corr)) + geom_line() + geom_smooth() +
    xlab("# acionamentos") + ylab("correlação") + 
    ggtitle("Máximo de Pgtos - SMS confirmados-SP") 

pushViewport(viewport(layout = grid.layout(1, 2)))
print(pl_max_lag, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_max_corr, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# +++++++++++ DAQUI PARA CIMA OK +++++++++++++


# varios acessos para confirmados
nr.sms  <- 1 # número de acionamentos por celular para filtrar os dados
my.list.1 <- f_ccf_sms_pgto(df_sms.2015.SP, nr.sms)
nr.sms  <- 2 # número de acionamentos por celular para filtrar os dados
my.list.2 <- f_ccf_sms_pgto(df_sms.2015.SP, nr.sms)
nr.sms  <- 3 # número de acionamentos por celular para filtrar os dados
my.list.3 <- f_ccf_sms_pgto(df_sms.2015.SP, nr.sms)
nr.sms  <- 4 # número de acionamentos por celular para filtrar os dados
my.list.4 <- f_ccf_sms_pgto(df_sms.2015.SP, nr.sms)
nr.sms  <- 5 # número de acionamentos por celular para filtrar os dados
my.list.5 <- f_ccf_sms_pgto(df_sms.2015.SP, nr.sms)
nr.sms  <- 6 # número de acionamentos por celular para filtrar os dados
my.list.6 <- f_ccf_sms_pgto(df_sms.2015.SP, nr.sms)
nr.sms  <- 7 # número de acionamentos por celular para filtrar os dados
my.list.7 <- f_ccf_sms_pgto(df_sms.2015.SP, nr.sms)
nr.sms  <- 8 # número de acionamentos por celular para filtrar os dados
my.list.8 <- f_ccf_sms_pgto(df_sms.2015.SP, nr.sms)
nr.sms  <- 9 # número de acionamentos por celular para filtrar os dados
my.list.9 <- f_ccf_sms_pgto(df_sms.2015.SP, nr.sms)
nr.sms  <- 10 # número de acionamentos por celular para filtrar os dados
my.list.10 <- f_ccf_sms_pgto(df_sms.2015.SP, nr.sms)
nr.sms  <- 11 # número de acionamentos por celular para filtrar os dados
my.list.11 <- f_ccf_sms_pgto(df_sms.2015.SP, nr.sms)
nr.sms  <- 12 # número de acionamentos por celular para filtrar os dados
my.list.12 <- f_ccf_sms_pgto(df_sms.2015.SP, nr.sms)
nr.sms  <- 13 # número de acionamentos por celular para filtrar os dados
my.list.13 <- f_ccf_sms_pgto(df_sms.2015.SP, nr.sms)
nr.sms  <- 14 # número de acionamentos por celular para filtrar os dados
my.list.14 <- f_ccf_sms_pgto(df_sms.2015.SP, nr.sms)
nr.sms  <- 15 # número de acionamentos por celular para filtrar os dados
my.list.15 <- f_ccf_sms_pgto(df_sms.2015.SP, nr.sms)
# correlacoes para acionamentos de 1 a 10, confirmados
par(mfrow=c(2,3))

my.ccf.1 <- ccf(my.list.1[[6]]$acions.dia ,my.list.1[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 1 acionamento - SP")
my.ccf.2 <- ccf(my.list.2[[6]]$acions.dia ,my.list.2[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 2 acionamentos - SP")
my.ccf.3 <- ccf(my.list.3[[6]]$acions.dia ,my.list.3[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 3 acionamentos - SP")
my.ccf.4 <- ccf(my.list.4[[6]]$acions.dia ,my.list.4[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 4 acionamentos - SP")
my.ccf.5 <- ccf(my.list.5[[6]]$acions.dia ,my.list.5[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 5 acionamentos - SP")
my.ccf.6 <- ccf(my.list.6[[6]]$acions.dia ,my.list.6[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 6 acionamentos - SP")

par(mfrow=c(2,3))
my.ccf.7 <- ccf(my.list.7[[6]]$acions.dia ,my.list.7[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "7 confirmed SMS - SP")
my.ccf.8 <- ccf(my.list.8[[6]]$acions.dia ,my.list.8[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "8 confirmed SMS - SP")
my.ccf.9 <- ccf(my.list.9[[6]]$acions.dia ,my.list.9[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "9 confirmed SMS - SP")
my.ccf.10 <- ccf(my.list.10[[6]]$acions.dia ,my.list.10[[6]]$pgto.dia, 
                 na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "10 confirmed SMS - SP")

par(mfrow=c(2,3))
my.ccf.11 <- ccf(my.list.11[[6]]$acions.dia ,my.list.11[[6]]$pgto.dia, 
                 na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 11 acionamentos - SP")
my.ccf.12 <- ccf(my.list.12[[6]]$acions.dia ,my.list.12[[6]]$pgto.dia, 
                 na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 12 acionamentos - SP")
my.ccf.13 <- ccf(my.list.13[[6]]$acions.dia ,my.list.13[[6]]$pgto.dia, 
                 na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 13 acionamentos - SP")
my.ccf.14 <- ccf(my.list.14[[6]]$acions.dia ,my.list.14[[6]]$pgto.dia, 
                 na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 14 acionamentos - SP")
my.ccf.15 <- ccf(my.list.15[[6]]$acions.dia ,my.list.15[[6]]$pgto.dia, 
                 na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 15 acionamentos - SP")

# usar abaixo para filtrar por DDD - BA e depois agrupar para análise
#####################################################################

df_sms.2015.BA <-
    df_sms.2015 %>%
    filter(grepl("^557",Celular))
# varios acessos para confirmados
nr.sms  <- 1 # número de acionamentos por celular para filtrar os dados
my.list.1 <- f_ccf_sms_pgto(df_sms.2015.BA, nr.sms)
nr.sms  <- 2 # número de acionamentos por celular para filtrar os dados
my.list.2 <- f_ccf_sms_pgto(df_sms.2015.BA, nr.sms)
nr.sms  <- 3 # número de acionamentos por celular para filtrar os dados
my.list.3 <- f_ccf_sms_pgto(df_sms.2015.BA, nr.sms)
nr.sms  <- 4 # número de acionamentos por celular para filtrar os dados
my.list.4 <- f_ccf_sms_pgto(df_sms.2015.BA, nr.sms)
nr.sms  <- 5 # número de acionamentos por celular para filtrar os dados
my.list.5 <- f_ccf_sms_pgto(df_sms.2015.BA, nr.sms)
nr.sms  <- 6 # número de acionamentos por celular para filtrar os dados
my.list.6 <- f_ccf_sms_pgto(df_sms.2015.BA, nr.sms)
nr.sms  <- 7 # número de acionamentos por celular para filtrar os dados
my.list.7 <- f_ccf_sms_pgto(df_sms.2015.BA, nr.sms)
nr.sms  <- 8 # número de acionamentos por celular para filtrar os dados
my.list.8 <- f_ccf_sms_pgto(df_sms.2015.BA, nr.sms)
nr.sms  <- 9 # número de acionamentos por celular para filtrar os dados
my.list.9 <- f_ccf_sms_pgto(df_sms.2015.BA, nr.sms)
nr.sms  <- 10 # número de acionamentos por celular para filtrar os dados
my.list.10 <- f_ccf_sms_pgto(df_sms.2015.BA, nr.sms)
nr.sms  <- 11 # número de acionamentos por celular para filtrar os dados
my.list.11 <- f_ccf_sms_pgto(df_sms.2015.BA, nr.sms)
nr.sms  <- 12 # número de acionamentos por celular para filtrar os dados
my.list.12 <- f_ccf_sms_pgto(df_sms.2015.BA, nr.sms)
nr.sms  <- 13 # número de acionamentos por celular para filtrar os dados
my.list.13 <- f_ccf_sms_pgto(df_sms.2015.BA, nr.sms)
nr.sms  <- 14 # número de acionamentos por celular para filtrar os dados
my.list.14 <- f_ccf_sms_pgto(df_sms.2015.BA, nr.sms)
nr.sms  <- 15 # número de acionamentos por celular para filtrar os dados
my.list.15 <- f_ccf_sms_pgto(df_sms.2015.BA, nr.sms)
# correlacoes para acionamentos de 1 a 10, confirmados
par(mfrow=c(2,3))

my.ccf.1 <- ccf(my.list.1[[6]]$acions.dia ,my.list.1[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 1 acionamento - BA")
my.ccf.2 <- ccf(my.list.2[[6]]$acions.dia ,my.list.2[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 2 acionamentos - BA")
my.ccf.3 <- ccf(my.list.3[[6]]$acions.dia ,my.list.3[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 3 acionamentos - BA")
my.ccf.4 <- ccf(my.list.4[[6]]$acions.dia ,my.list.4[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 4 acionamentos - BA")
my.ccf.5 <- ccf(my.list.5[[6]]$acions.dia ,my.list.5[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 5 acionamentos - BA")
my.ccf.6 <- ccf(my.list.6[[6]]$acions.dia ,my.list.6[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 6 acionamentos - BA")

par(mfrow=c(2,3))
my.ccf.7 <- ccf(my.list.7[[6]]$acions.dia ,my.list.7[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "7 confirmed SMS - BA")
my.ccf.8 <- ccf(my.list.8[[6]]$acions.dia ,my.list.8[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "8 confirmed SMS - BA")
my.ccf.9 <- ccf(my.list.9[[6]]$acions.dia ,my.list.9[[6]]$pgto.dia, 
                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "9 confirmed SMS - BA")
my.ccf.10 <- ccf(my.list.10[[6]]$acions.dia ,my.list.10[[6]]$pgto.dia, 
                 na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "10 confirmed SMS - BA")

par(mfrow=c(2,3))
my.ccf.11 <- ccf(my.list.11[[6]]$acions.dia ,my.list.11[[6]]$pgto.dia, 
                 na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 11 acionamentos - BA")
my.ccf.12 <- ccf(my.list.12[[6]]$acions.dia ,my.list.12[[6]]$pgto.dia, 
                 na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 12 acionamentos - BA")
my.ccf.13 <- ccf(my.list.13[[6]]$acions.dia ,my.list.13[[6]]$pgto.dia, 
                 na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 13 acionamentos - BA")
my.ccf.14 <- ccf(my.list.14[[6]]$acions.dia ,my.list.14[[6]]$pgto.dia, 
                 na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 14 acionamentos - BA")
my.ccf.15 <- ccf(my.list.15[[6]]$acions.dia ,my.list.15[[6]]$pgto.dia, 
                 na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 15 acionamentos - BA")
