# script que mostra resultados para SMSs
require("doMC")
require("dplyr")

source("~/Documents/MyGit/COBRA/R/f_ccf_sms_pgto.R")
source("~/Documents/MyGit/COBRA/R/f_le_sms.R")
source("~/Documents/MyGit/COBRA/R/f_nacion_reg.R")

registerDoMC(5) # parallel processing
options(scipen=999) # removendo display de notação científica em R
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
#df_sms.2015 <-
#    df_sms.2015 %>%
#    filter(!(grepl("Não Recebido|Bloqueado",Status)))

# filtrando somente confirmados
df_sms.2015 <-
    df_sms.2015 %>%
    filter(grepl("Entregue com Confirmação",Status))
# obs: depois repetir analise para status (Entregue com Confirmação!!!!)

# obtendo somente SMS onde consta o codigo de barra
#df_sms.2015 <-
#    df_sms.2015 %>%
#    filter(grepl("cod de barra",Conteúdo.do.SMS))

# preparando formato correto de datas para plot
df_sms.2015$Enviado.em <- as.Date(df_sms.2015$Enviado.em, "%d/%m/%Y")

# obtendo média total de sms por celular
#df_sms.2015.ncel <-
#    df_sms.2015 %>%
#    group_by(Celular) %>%
#    summarise(acions.cel = n())

#summary(df_sms.2015.ncel$acions.cel)

# COMENTADOS ABAIXO PARA ANALISE SEM DIVIDIR POR REGIAO DE DDD
################################################################

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
# PULAR DAQUI ATE.....

#max.nacion <- 15
#my.list.nr <- list()
#for(i in 1:max.nacion) {
#    my.list.nrsms[i] <- f_ccf_sms_pgto(df_sms.2015, i)
#}
#nr.sms  <- 1 # número de acionamentos por celular para filtrar os dados
#my.list.1 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
#nr.sms  <- 2 # número de acionamentos por celular para filtrar os dados
#my.list.2 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
#nr.sms  <- 3 # número de acionamentos por celular para filtrar os dados
#my.list.3 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
#nr.sms  <- 4 # número de acionamentos por celular para filtrar os dados
#my.list.4 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
#nr.sms  <- 5 # número de acionamentos por celular para filtrar os dados
#my.list.5 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
#nr.sms  <- 6 # número de acionamentos por celular para filtrar os dados
#my.list.6 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
#nr.sms  <- 7 # número de acionamentos por celular para filtrar os dados
#my.list.7 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
#nr.sms  <- 8 # número de acionamentos por celular para filtrar os dados
#my.list.8 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
#nr.sms  <- 9 # número de acionamentos por celular para filtrar os dados
#my.list.9 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
#nr.sms  <- 10 # número de acionamentos por celular para filtrar os dados
#my.list.10 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
#nr.sms  <- 11 # número de acionamentos por celular para filtrar os dados
#my.list.11 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
#nr.sms  <- 12 # número de acionamentos por celular para filtrar os dados
#my.list.12 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
#nr.sms  <- 13 # número de acionamentos por celular para filtrar os dados
#my.list.13 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
#nr.sms  <- 14 # número de acionamentos por celular para filtrar os dados
#my.list.14 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)
#nr.sms  <- 15 # número de acionamentos por celular para filtrar os dados
#my.list.15 <- f_ccf_sms_pgto(df_sms.2015, nr.sms)

# correlacoes para acionamentos de 1 a 10, confirmados
#par(mfrow=c(2,3))

#my.ccf.1 <- ccf(my.list.1[[6]]$acions.dia ,my.list.1[[6]]$pgto.dia, 
#    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 1 acionamento")
#my.ccf.2 <- ccf(my.list.2[[6]]$acions.dia ,my.list.2[[6]]$pgto.dia, 
#    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 2 acionamentos")
#my.ccf.3 <- ccf(my.list.3[[6]]$acions.dia ,my.list.3[[6]]$pgto.dia, 
#    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 3 acionamentos")
#my.ccf.4 <- ccf(my.list.4[[6]]$acions.dia ,my.list.4[[6]]$pgto.dia, 
#    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 4 acionamentos")
#my.ccf.5 <- ccf(my.list.5[[6]]$acions.dia ,my.list.5[[6]]$pgto.dia, 
#    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 5 acionamentos")
#my.ccf.6 <- ccf(my.list.6[[6]]$acions.dia ,my.list.6[[6]]$pgto.dia, 
#    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 6 acionamentos")#

#par(mfrow=c(2,3))
#my.ccf.7 <- ccf(my.list.7[[6]]$acions.dia ,my.list.7[[6]]$pgto.dia, 
#    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "7 confirmed SMS")
#my.ccf.8 <- ccf(my.list.8[[6]]$acions.dia ,my.list.8[[6]]$pgto.dia, 
#    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "8 confirmed SMS")
#my.ccf.9 <- ccf(my.list.9[[6]]$acions.dia ,my.list.9[[6]]$pgto.dia, 
#    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "9 confirmed SMS")
#my.ccf.10 <- ccf(my.list.10[[6]]$acions.dia ,my.list.10[[6]]$pgto.dia, 
#    na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "10 confirmed SMS")

#par(mfrow=c(2,3))
#my.ccf.11 <- ccf(my.list.11[[6]]$acions.dia ,my.list.11[[6]]$pgto.dia, 
#                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 11 acionamentos")
#my.ccf.12 <- ccf(my.list.12[[6]]$acions.dia ,my.list.12[[6]]$pgto.dia, 
#                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 12 acionamentos")
#my.ccf.13 <- ccf(my.list.13[[6]]$acions.dia ,my.list.13[[6]]$pgto.dia, 
#                na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 13 acionamentos")
#my.ccf.14 <- ccf(my.list.14[[6]]$acions.dia ,my.list.14[[6]]$pgto.dia, 
#                 na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 14 acionamentos")
#my.ccf.15 <- ccf(my.list.15[[6]]$acions.dia ,my.list.15[[6]]$pgto.dia, 
#                 na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), main = "SMS Confirmado - 15 acionamentos")


# TESTE DE AGRUPAMENTO POR REGIAO ++++++++ DAQUI P BAIXO OK +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# usar abaixo para filtrar por DDD - SP e depois agrupar para análise
#################################
# DDD TODOS
########
ddd <- "^55"
nacion.max <- 15
l_nacion <- f_nacion_reg(df_sms.2015, ddd,nacion.max)
pl_max_lag.SP <- l_nacion$plot.lag
pl_max_corr.SP <- l_nacion$plot.acion
my.df_max_corr.SP <- l_nacion$df.max.corr
my.lm.n.acion.SP <- l_nacion$reg.lin
my.df_nacion.cel <- l_nacion$nacion.cel

# plot das correlações máximas por lag e por número de acionamentos
pushViewport(viewport(layout = grid.layout(1, 2)))
print(pl_max_lag.SP, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_max_corr.SP, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# plot da correlação e valor de R squared

# IMPORTANTE: O R Squared da correlação entre:
#   valor de máximas correlações entre nro de sms x pagto, distribuídos por número de acionamentos e
#   distribuição de número d eacionamentos
# comprova relação direta e forte (> 70%) entre número de acionamentos x pagamentos efetuados
my.lm.descr <-  summary(my.lm.n.acion.SP)
sprintf("R Squared: %.3f",  my.lm.descr$r.squared)

# plot das correlações permite ver agrupamento de lags para maior correlação nro acion x pgto
# o plot permite visualmente identificar os lags com maior correlação com pgto, permitindo
# planejar e entrada dos pagamentos no tempo após os acionamentos, por região
plot(my.df_max_corr.SP)

# DDD SP
########
ddd <- "^551"
nacion.max <- 15
l_nacion <- f_nacion_reg(df_sms.2015, ddd,nacion.max)
pl_max_lag.SP <- l_nacion$plot.lag
pl_max_corr.SP <- l_nacion$plot.acion
my.df_max_corr.SP <- l_nacion$df.max.corr
my.lm.n.acion.SP <- l_nacion$reg.lin
my.df_nacion.cel <- l_nacion$nacion.cel

# plot das correlações máximas por lag e por número de acionamentos
pushViewport(viewport(layout = grid.layout(1, 2)))
print(pl_max_lag.SP, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_max_corr.SP, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# plot da correlação e valor de R squared

# IMPORTANTE: O R Squared da correlação entre:
#   valor de máximas correlações entre nro de sms x pagto, distribuídos por número de acionamentos e
#   distribuição de número d eacionamentos
# comprova relação direta e forte (> 70%) entre número de acionamentos x pagamentos efetuados
my.lm.descr <-  summary(my.lm.n.acion.SP)
sprintf("R Squared: %.3f",  my.lm.descr$r.squared)

# plot das correlações permite ver agrupamento de lags para maior correlação nro acion x pgto
# o plot permite visualmente identificar os lags com maior correlação com pgto, permitindo
# planejar e entrada dos pagamentos no tempo após os acionamentos, por região
plot(my.df_max_corr.SP)

# DDD CIDADE DE SP
########
ddd <- "^5511"
nacion.max <- 15
l_nacion <- f_nacion_reg(df_sms.2015, ddd,nacion.max)
pl_max_lag.SPC <- l_nacion$plot.lag
pl_max_corr.SPC <- l_nacion$plot.acion
my.df_max_corr.SPC <- l_nacion$df.max.corr
my.lm.n.acion.SPC <- l_nacion$reg.lin

# plot das correlações máximas por lag e por número de acionamentos
pushViewport(viewport(layout = grid.layout(1, 2)))
print(pl_max_lag.SPC, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_max_corr.SPC, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# plot da correlação e valor de R squared
my.lm.descr <-  summary(my.lm.n.acion.SPC)
sprintf("R Squared: %.3f",  my.lm.descr$r.squared)

# plot das correlações permite ver agrupamento de lags para maior correlação nro acion x pgto
plot(my.df_max_corr.SPC)

# DDD SP (INTERIOR)
########
ddd <- "^551[23456789]"
nacion.max <- 15
l_nacion <- f_nacion_reg(df_sms.2015, ddd,nacion.max)
pl_max_lag.SPI <- l_nacion$plot.lag
pl_max_corr.SPI <- l_nacion$plot.acion
my.df_max_corr.SPI <- l_nacion$df.max.corr
my.lm.n.acion.SPI <- l_nacion$reg.lin

# plot das correlações máximas por lag e por número de acionamentos
pushViewport(viewport(layout = grid.layout(1, 2)))
print(pl_max_lag.SPI, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_max_corr.SPI, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# plot da correlação e valor de R squared
my.lm.descr <-  summary(my.lm.n.acion.SPI)
sprintf("R Squared: %.3f",  my.lm.descr$r.squared)

# plot das correlações permite ver agrupamento de lags para maior correlação nro acion x pgto
plot(my.df_max_corr.SPI)

# DDD RJ, ES
########
ddd <- "^552"
nacion.max <- 15
l_nacion <- f_nacion_reg(df_sms.2015, ddd,nacion.max)
pl_max_lag.RJ.ES <- l_nacion$plot.lag
pl_max_corr.RJ.ES <- l_nacion$plot.acion
my.df_max_corr.RJ.ES <- l_nacion$df.max.corr
my.lm.n.acion.RJ.ES <- l_nacion$reg.lin

# plot das correlações máximas por lag e por número de acionamentos
pushViewport(viewport(layout = grid.layout(1, 2)))
print(pl_max_lag.RJ.ES, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_max_corr.RJ.ES, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# plot da correlação e valor de R squared
my.lm.descr <-  summary(my.lm.n.acion.RJ.ES)
sprintf("R Squared: %.3f",  my.lm.descr$r.squared)

# plot das correlações permite ver agrupamento de lags para maior correlação nro acion x pgto
plot(my.df_max_corr.RJ.ES)

# DDD MG
########
ddd <- "^553"
nacion.max <- 15
l_nacion <- f_nacion_reg(df_sms.2015, ddd,nacion.max)
pl_max_lag.MG <- l_nacion$plot.lag
pl_max_corr.MG <- l_nacion$plot.acion
my.df_max_corr.MG <- l_nacion$df.max.corr
my.lm.n.acion.MG <- l_nacion$reg.lin

# plot das correlações máximas por lag e por número de acionamentos
pushViewport(viewport(layout = grid.layout(1, 2)))
print(pl_max_lag.MG, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_max_corr.MG, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# plot da correlação e valor de R squared

# IMPORTANTE: O R Squared da correlação entre:
#   valor de máximas correlações entre nro de sms x pagto, distribuídos por número de acionamentos e
#   distribuição de número d eacionamentos
# comprova relação direta e forte (> 70%) entre número de acionamentos x pagamentos efetuados
my.lm.descr <-  summary(my.lm.n.acion.MG)
sprintf("R Squared: %.3f",  my.lm.descr$r.squared)

# plot das correlações permite ver agrupamento de lags para maior correlação nro acion x pgto
plot(my.df_max_corr.MG)

# DDD SC, PR
########
ddd <- "^554"
nacion.max <- 15
l_nacion <- f_nacion_reg(df_sms.2015, ddd,nacion.max)
pl_max_lag.SC.PR <- l_nacion$plot.lag
pl_max_corr.SC.PR <- l_nacion$plot.acion
my.df_max_corr.SC.PR <- l_nacion$df.max.corr
my.lm.n.acion.SC.PR <- l_nacion$reg.lin

# plot das correlações máximas por lag e por número de acionamentos
pushViewport(viewport(layout = grid.layout(1, 2)))
print(pl_max_lag.SC.PR, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_max_corr.SC.PR, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# plot da correlação e valor de R squared
my.lm.descr <-  summary(my.lm.n.acion.SC.PR)
sprintf("R Squared: %.3f",  my.lm.descr$r.squared)

# plot das correlações permite ver agrupamento de lags para maior correlação nro acion x pgto
plot(my.df_max_corr.SC.PR)

# DDD RS
########
ddd <- "^555"
nacion.max <- 15
l_nacion <- f_nacion_reg(df_sms.2015, ddd,nacion.max)
pl_max_lag.RS <- l_nacion$plot.lag
pl_max_corr.RS <- l_nacion$plot.acion
my.df_max_corr.RS <- l_nacion$df.max.corr
my.lm.n.acion.RS <- l_nacion$reg.lin

# plot das correlações máximas por lag e por número de acionamentos
pushViewport(viewport(layout = grid.layout(1, 2)))
print(pl_max_lag.RS, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_max_corr.RS, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# plot da correlação e valor de R squared
my.lm.descr <-  summary(my.lm.n.acion.RS)
sprintf("R Squared: %.3f",  my.lm.descr$r.squared)

# plot das correlações permite ver agrupamento de lags para maior correlação nro acion x pgto
plot(my.df_max_corr.RS)

# DDD AC, RD, MS, TO, GO
########
ddd <- "^556"
nacion.max <- 15
l_nacion <- f_nacion_reg(df_sms.2015, ddd,nacion.max)
pl_max_lag.CO <- l_nacion$plot.lag
pl_max_corr.CO <- l_nacion$plot.acion
my.df_max_corr.CO <- l_nacion$df.max.corr
my.lm.n.acion.CO <- l_nacion$reg.lin

# plot das correlações máximas por lag e por número de acionamentos
pushViewport(viewport(layout = grid.layout(1, 2)))
print(pl_max_lag.CO, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_max_corr.CO, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# plot da correlação e valor de R squared
my.lm.descr <-  summary(my.lm.n.acion.CO)
sprintf("R Squared: %.3f",  my.lm.descr$r.squared)

# plot das correlações permite ver agrupamento de lags para maior correlação nro acion x pgto
plot(my.df_max_corr.CO)

# DDD BA, SE
########
ddd <- "^557"
nacion.max <- 15
l_nacion <- f_nacion_reg(df_sms.2015, ddd,nacion.max)
pl_max_lag.BA.SE <- l_nacion$plot.lag
pl_max_corr.BA.SE <- l_nacion$plot.acion
my.df_max_corr.BA.SE <- l_nacion$df.max.corr
my.lm.n.acion.BA.SE <- l_nacion$reg.lin

# plot das correlações máximas por lag e por número de acionamentos
pushViewport(viewport(layout = grid.layout(1, 2)))
print(pl_max_lag.BA.SE, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_max_corr.BA.SE, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# plot da correlação e valor de R squared

# IMPORTANTE: O R Squared da correlação entre:
#   valor de máximas correlações entre nro de sms x pagto, distribuídos por número de acionamentos e
#   distribuição de número d eacionamentos
# comprova relação direta e forte (> 70%) entre número de acionamentos x pagamentos efetuados
my.lm.descr <-  summary(my.lm.n.acion.BA.SE)
sprintf("R Squared: %.3f",  my.lm.descr$r.squared)

# plot das correlações permite ver agrupamento de lags para maior correlação nro acion x pgto
# o plot permite visualmente identificar os lags com maior correlação com pgto, permitindo
# planejar e entrada dos pagamentos no tempo após os acionamentos, por região
plot(my.df_max_corr.BA.SE)

# DDD PI, RN, CE, PB, PE, AL
########
ddd <- "^558"
nacion.max <- 15
l_nacion <- f_nacion_reg(df_sms.2015, ddd,nacion.max)
pl_max_lag.NE <- l_nacion$plot.lag
pl_max_corr.NE <- l_nacion$plot.acion
my.df_max_corr.NE <- l_nacion$df.max.corr
my.lm.n.acion.NE <- l_nacion$reg.lin

# plot das correlações máximas por lag e por número de acionamentos
pushViewport(viewport(layout = grid.layout(1, 2)))
print(pl_max_lag.NE, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_max_corr.NE, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# plot da correlação e valor de R squared

# IMPORTANTE: O R Squared da correlação entre:
#   valor de máximas correlações entre nro de sms x pagto, distribuídos por número de acionamentos e
#   distribuição de número d eacionamentos
# comprova relação direta e forte (> 70%) entre número de acionamentos x pagamentos efetuados
my.lm.descr <-  summary(my.lm.n.acion.NE)
sprintf("R Squared: %.3f",  my.lm.descr$r.squared)

# plot das correlações permite ver agrupamento de lags para maior correlação nro acion x pgto
# o plot permite visualmente identificar os lags com maior correlação com pgto, permitindo
# planejar e entrada dos pagamentos no tempo após os acionamentos, por região
plot(my.df_max_corr.NE)

# DDD AM, RR, PA, AM, MA
########
ddd <- "^559"
nacion.max <- 15
l_nacion <- f_nacion_reg(df_sms.2015, ddd,nacion.max)
pl_max_lag.NO <- l_nacion$plot.lag
pl_max_corr.NO <- l_nacion$plot.acion
my.df_max_corr.NO <- l_nacion$df.max.corr
my.lm.n.acion.NO <- l_nacion$reg.lin

# plot das correlações máximas por lag e por número de acionamentos
pushViewport(viewport(layout = grid.layout(1, 2)))
print(pl_max_lag.NO, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pl_max_corr.NO, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# plot da correlação e valor de R squared

# IMPORTANTE: O R Squared da correlação entre:
#   valor de máximas correlações entre nro de sms x pagto, distribuídos por número de acionamentos e
#   distribuição de número d eacionamentos
# comprova relação direta e forte (> 70%) entre número de acionamentos x pagamentos efetuados
my.lm.descr <-  summary(my.lm.n.acion)
sprintf("R Squared: %.3f",  my.lm.descr$r.squared)

# plot das correlações permite ver agrupamento de lags para maior correlação nro acion x pgto
# o plot permite visualmente identificar os lags com maior correlação com pgto, permitindo
# planejar e entrada dos pagamentos no tempo após os acionamentos, por região
plot(my.df_max_corr)


#### AQUI FAZER ANALISE DA CONCLUSAO ####
# 1. plots por região de max numero de acionamentos 
# 2. tabela mostrando quantos sms por região seriam ideal
# 3. comparação com número médio de sms/celular por região
# 4. calcular economia, se usar os números acima




# +++++++++++ DAQUI PARA CIMA OK +++++++++++++
# FALTA FAZER ACIMA PARA CADA REGIAO DO BRASIL

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
