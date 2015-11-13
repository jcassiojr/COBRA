# agbd_COBRA
require("caret")
require("ROCR")
require("doMC")
require("lubridate")
require("MASS")
require("dplyr")

#############################################
## DATA PREPARATION
## agrupar coluna hora.acion truncando por hora
## carregar como dados de uso para previsão dados novos ainda não rodados
## para dados de uso para previsão precisamos ter atributos: 
## Valor (da carteira), 
## Cobrável: Se é cobravel, incobravel , 3a onda,(da carteira)
## Cidade do cliente (da carteira)
## Operador (previsto para acionamento)
## Dia da semana (previsto para acionamento)
## Hora (prevista para acionamento)
## e se quiser prever melhor dia da semana?
## R: montar tabela com dados previstos de acionamento. Obter as probabilidades,
##    ordenar por Operador/dia da semana e pegar os de maior probabilidade.
##    ver quantos conseguem por dia para corte
## outra abordagem: considerar modelo somente com dados carteira. Depois com um 
## atributo de acionamento por vez
#############################################
source("~/Documents/MyGit/COBRA/R/f_leRawCobra.R")
source("~/Documents/MyGit/COBRA/R/f_train_model.R")
source("~/Documents/MyGit/COBRA/R/f_opt_cut.R")

registerDoMC(5) # parallel processing

# lê dados para treino e teste
l_pagos <- f_leRawCobra()
# obtém dataframe
df_cobr <- l_pagos[[1]]
df_incobr <- l_pagos[[2]]
df_3fase <- l_pagos[[3]]

# prior probs das tabelas lidas
# obs: se a diferença entre cobr e incobr é pequena, concatenar
prop.table(table(df_cobr$pago))   # sucesso de 3.7 %
prop.table(table(df_incobr$pago)) # sucesso de 4.8 %
prop.table(table(df_3fase$pago))  # sem números suficientes

# treina o modelo com dataframe passado (cobráveis)
#---------------------------------------------------
l_cobr_model <- f_train_model(df_cobr)
# Obtem objetos retornados
models <-  l_cobr_model[[1]] # modelo trans de caret
aic <-  l_cobr_model[[2]] # Valor de AIC do modelo
cf <-  l_cobr_model[[3]] # objeto confusion matrix
roc.perf <-  l_cobr_model[[4]] # objeto performance de caret
roc.auc <-  l_cobr_model[[5]] # valor de AUC de curva ROC
pred <-  l_cobr_model[[6]] # valor de cutoff calculado (best balance)
df_rank <-  l_cobr_model[[7]] # dataframe com pobabilidades de teste rankeadas

resampleHist(models$nb)
# objeto confusion Matrix (devo diminuir false positive)
print(cf$table)
print(cf$byClass)
print(cf$overall)
# Plot roc. objects (para cada modelo)
plot(roc.perf)
abline(a=0, b= 1)
# lift plot
roc.perf.lift = performance(pred, measure = "lift", x.measure = "rpp")
plot(roc.perf.lift)

# cutoff best balance
valor_cutoff <- f_opt_cut(roc.perf, pred)[3]

# cuttof over the ranked dataframe
df_cutoff <-
    df_rank %>%
    filter (df_rank >= valor_cutoff)

# prints
sprintf("AIC Cobravel: %.2f",aic$aic)
sprintf("Cobravel - %s : %.4f",roc.auc@y.name,roc.auc@y.values)
sprintf("Cobravel - Valor de Cutoff-ROC Best Balance : %.4f",valor_cutoff)

# treina o modelo com dataframe passado (incobráveis)
#---------------------------------------------------
l_incobr_model <- f_train_model(df_incobr)
# Obtem objetos retornados
models <-  l_incobr_model[[1]] # modelo trans de caret
aic <-  l_incobr_model[[2]] # Valor de AIC do modelo
cf <-  l_incobr_model[[3]] # objeto confusion matrix
roc.perf <-  l_incobr_model[[4]] # objeto performance de caret
roc.auc <-  l_incobr_model[[5]] # valor de AUC de curva ROC
pred <-  l_incobr_model[[6]] # valor de cutoff calculado (best balance)
df_rank <-  l_incobr_model[[7]] # dataframe com pobabilidades de teste rankeadas

resampleHist(models$nb)
# objeto confusion Matrix (devo diminuir false positive)
print(cf$table)
print(cf$byClass)
print(cf$overall)
# Plot roc. objects (para cada modelo)
plot(roc.perf)
abline(a=0, b= 1)
# lift plot
roc.perf.lift = performance(pred, measure = "lift", x.measure = "rpp")
plot(roc.perf.lift)

# cutoff best balance
valor_cutoff <- f_opt_cut(roc.perf, pred)[3]

# cuttof over the ranked dataframe
df_cutoff <-
    df_rank %>%
    filter (df_rank >= valor_cutoff)

# prints
sprintf("AIC Incobravel: %.2f",aic$aic)
sprintf("Incobravel - %s : %.4f",roc.auc@y.name,roc.auc@y.values)
sprintf("Incobravel - Valor de Cutoff-ROC Best Balance : %.4f",valor_cutoff)

#--------------------------------------------------
# Previsão
# separa dados para previsão (usar os dados de cliente que não aparecem nos acionamentos)

