# agbd_COBRA
require("xlsx")
require("lubridate")
require("caret")
require("ROCR")
require("doMC")
require("MASS")
require("dplyr")

source("~/Documents/MyGit/COBRA/R/f_leRawCobra.R")
source("~/Documents/MyGit/COBRA/R/f_geraTidyCobra.R")
source("~/Documents/MyGit/COBRA/R/f_train_model.R")
source("~/Documents/MyGit/COBRA/R/f_opt_cut.R")
source("~/Documents/MyGit/COBRA/R/f_prep_uso.R")
source("~/Documents/MyGit/COBRA/R/f_previsao.R")

registerDoMC(5) # parallel processing

# lê dados raw em 3 data.frames: acionamentos, carteira Avon e pagamentos
##########################################################################
l_raw <- f_leRawCobra()
 # obtém dataframe
df_acion <- l_raw[[1]]
df_carteira <- l_raw[[2]]
df_pg <- l_raw[[3]]

# prepara dados tidy para analise a partir dos dataframes raw
# retorna o dataframe pronto para análise
#########################################################################
df_tidy <- f_geraTidyCobra(df_acion, df_carteira, df_pg)

# prior probs das tabelas lidas
prop.table(table(df_tidy$PAGO))   # sucesso de 59 % de obter primeiro pgto quando acionado

# prepara dados para uso na previsao
# retorna lista com 1 dataframe de ID da carteira e outro de Features de carteira sem pagamento detectado
# para ser usado na criação do renking a partir das previsões
########################################################################
l_uso <- f_prep_uso(df_carteira, df_pg)

# treina o modelo separando os dados tidy em dados de teste e treino
########################################################################
l_model <- f_train_model(df_tidy)

# avalia o resultado do modelo implementado, para ver se se adequa a previsão desejada
########################################################################################
models <-  l_model[[1]] # modelo trans de caret
aic <-  l_model[[2]] # Valor de AIC do modelo
cf <-  l_model[[3]] # objeto confusion matrix
roc.perf <-  l_model[[4]] # objeto performance de caret
roc.auc <-  l_model[[5]] # valor de AUC de curva ROC
pred <-  l_model[[6]] # valor de cutoff calculado (best balance)

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

# obtém o valor de cutoff de best balance na curva ROC
valor_cutoff <- f_opt_cut(roc.perf, pred)[3]

# prints dos resultados da avaliação do modelo
print(aic$formula)
sprintf("AIC Cobravel: %.2f",aic$aic)
sprintf("Avon - %s : %.4f",roc.auc@y.name,roc.auc@y.values)
sprintf("Avon - Valor de Cutoff-ROC Best Balance : %.4f",valor_cutoff)

# Previsão
# separa dados para previsão (usando os dados de cliente que não aparecem nos acionamentos)

# f_previsao, usando dados de Avon que não aparecem no acionamento
# retorna o data.frame com a carteira prevista
####################################################################
df_rank.prev <- f_previsao(l_model, l_uso)

# filtrando por valor de cutoff
df_rank.cutoff <-
    df_rank.prev %>%
    filter(probClass >= valor_cutoff)

# filtrando para valor de % dado
df_rank.50 <-
    df_rank.prev %>%
    filter(probClass >= 0.5)
#df_rank.40 <-
#    df_rank.prev %>%
#    filter(probClass >= 0.4)
#df_rank.30 <-
#    df_rank.prev %>%
#    filter(probClass >= 0.3)
#df_rank.20 <-
#    df_rank.prev %>%
#    filter(probClass >= 0.2)

# salvando em planilhas os rankings obtidos
#############################################
write.xlsx(df_rank.50, "./data/Ranking-50.xlsx")
#write.xlsx(df_rank.40, "./data/Ranking-40.xlsx")
#write.xlsx(df_rank.30, "./data/Ranking-30.xlsx")
#write.xlsx(df_rank.20, "./data/Ranking-20.xlsx")
write.xlsx(df_rank.cutoff, "./data/Ranking-cutoff.xlsx")

# obs: para ver resultado, obter performnce do memso mês de 2014 (dez(2014), jan(2015))

