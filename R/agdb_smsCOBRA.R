# script que mostra resultados para SMSs
require("doMC")

source("~/Documents/MyGit/COBRA/R/f_ccf_sms_pgto.R")

registerDoMC(5) # parallel processing
# chama funcao passando o numero de acionamentos de sms desejado por celular
nr.sms  <- 5 # número de acionamentos por celular para filtrar os dados
my.list. <- f_ccf_sms_pgto(nr.sms) 

# PLOTS das time series
pushViewport(viewport(layout = grid.layout(4, 1)))
print(my.list[[1]], vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(my.list[[2]], vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(my.list[[3]], vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(my.list[[4]], vp = viewport(layout.pos.row = 4, layout.pos.col = 1))

# CROSS CORRELATIONS
par(mfrow=c(1,3))
ccf(my.list[[5]]$acions.dia ,my.list[[5]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = paste0("SMS NÃO Confirmado - ", nr.sms, " acionamentos"))

ccf(my.list[[6]]$acions.dia ,my.list[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = paste0("SMS Confirmado - ", nr.sms, " acionamentos"))

ccf(my.list[[7]]$acions.dia ,my.list[[7]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = paste0("SMS Total - ", nr.sms, " acionamentos"))

# varios acessos para confirmados
nr.sms  <- 1 # número de acionamentos por celular para filtrar os dados
my.list.1 <- f_ccf_sms_pgto(nr.sms)
nr.sms  <- 2 # número de acionamentos por celular para filtrar os dados
my.list.2 <- f_ccf_sms_pgto(nr.sms)
nr.sms  <- 3 # número de acionamentos por celular para filtrar os dados
my.list.3 <- f_ccf_sms_pgto(nr.sms)
nr.sms  <- 4 # número de acionamentos por celular para filtrar os dados
my.list.4 <- f_ccf_sms_pgto(nr.sms)
nr.sms  <- 5 # número de acionamentos por celular para filtrar os dados
my.list.5 <- f_ccf_sms_pgto(nr.sms)
nr.sms  <- 6 # número de acionamentos por celular para filtrar os dados
my.list.6 <- f_ccf_sms_pgto(nr.sms)
nr.sms  <- 7 # número de acionamentos por celular para filtrar os dados
my.list.7 <- f_ccf_sms_pgto(nr.sms)
nr.sms  <- 8 # número de acionamentos por celular para filtrar os dados
my.list.8 <- f_ccf_sms_pgto(nr.sms)
nr.sms  <- 9 # número de acionamentos por celular para filtrar os dados
my.list.9 <- f_ccf_sms_pgto(nr.sms)
nr.sms  <- 10 # número de acionamentos por celular para filtrar os dados
my.list.10 <- f_ccf_sms_pgto(nr.sms)

# correlacoes para acionamentos de 1 a 10, confirmados
par(mfrow=c(4,4))

ccf(my.list.1[[6]]$acions.dia ,my.list.1[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 1 acionamento")
ccf(my.list.2[[6]]$acions.dia ,my.list.2[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 2 acionamentos")
ccf(my.list.3[[6]]$acions.dia ,my.list.3[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 3 acionamentos")
ccf(my.list.4[[6]]$acions.dia ,my.list.4[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 4 acionamentos")
ccf(my.list.5[[6]]$acions.dia ,my.list.5[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 5 acionamentos")
ccf(my.list.6[[6]]$acions.dia ,my.list.6[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 6 acionamentos")
ccf(my.list.7[[6]]$acions.dia ,my.list.7[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 7 acionamentos")
ccf(my.list.8[[6]]$acions.dia ,my.list.8[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 8 acionamentos")
ccf(my.list.9[[6]]$acions.dia ,my.list.9[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 9 acionamentos")
ccf(my.list.10[[6]]$acions.dia ,my.list.10[[6]]$pgto.dia, 
    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 10 acionamentos")

# tabela de maiores corelações e em que lag (montar)
# salvar cada objeto retornado de ccf acima e selecionar o lag com maior correlação
# para cada nro de acionamentos, montando data frame nr.acion.por x max(corr)
# fazer plot dos máximos par amostra pico em 