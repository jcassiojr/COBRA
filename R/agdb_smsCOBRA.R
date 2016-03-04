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

