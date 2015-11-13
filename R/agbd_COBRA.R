# agbd_COBRA
require("caret")
#require("corrplot")
#library(devtools)
#install_version("colorspace", "1.2-4") # para não carregar X11 no ggplot2 (não funcionou!)
#require("ggplot2")
#require("pROC")
require("ROCR")
#require("rpart")
#require("rattle")					# Fancy tree plot
#require("rpart.plot")
library("lubridate")
require("doMC")

registerDoMC(5) # parallel processing

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

# ----- carrega dados simulados de Human Guide
#source("./R/f_leRawCobra.R")
source("~/Documents/MyGit/COBRA/R/f_leRawCobra.R")
l_pagos <- f_leRawCobra()

# obtem os data.frames preparados
df_cobr <- l_pagos[[1]]
df_incobr <- l_pagos[[2]]
df_3fase <- l_pagos[[3]]

# prior probs das tabelas lidas
prop.table(table(df_acion_cobr_tidy$acordo))   # sucesso de 0.9 %
prop.table(table(df_acion_incobr_tidy$acordo)) # sucesso de 2.6 %
prop.table(table(df_acion_3Fase_tidy$acordo))  # suceso de 26.6 %

# Fazer a análise abaixo sobre duas premissas:
#--------------------------------------------------------------------------------
# 1. considerar acordos sobre todos os acionamentos por contrato (sucesso ou não)
#--------------------------------------------------------------------------------
df_acordo_cart <-l_acordo[[1]]

# confere prior probabilities
prop.table(table(df_acordo_cart$acordo))
#selecionando as features e target em arquivos diferentes
class <- as.factor(df_acordo_cart[,"acordo"]) # transformando em vetor de fatores de target
descr <- df_acordo_cart[,-c(1,5,6,7,8,9,10,11)] # transformando em dataframe de features
# ----- cria datasets de treino, teste e uso em previsão
set.seed(1)
inTrain <- createDataPartition(class, p = 3/4, list = FALSE)

trainDescr <- descr[inTrain,]
testDescr  <- descr[-inTrain,]
#useDescr  <- df_scores_hg_use[,-2] # transformando em dataframe de uso sem coluna target

trainClass <- class[inTrain]
testClass  <- class[-inTrain]
#-------------------------------------------------
# REMOVENDO NEAR ZERO VARIANCE AND CORRELATIONS 
# (FOR CORRELATION, NUMERIC FEATURES ONLY)
#-------------------------------------------------
trn_nzvar <- nearZeroVar(trainDescr, freqCut = 20, uniqueCut = 20)
tst_nzvar <- nearZeroVar(testDescr, freqCut = 20, uniqueCut = 20)
# removendo colunas que não passaram no teste
if(length(trn_nzvar) != 0  || length(tst_nzvar) != 0) {
    trainDescr <- trainDescr[,-(trn_nzvar)]
    testDescr <- testDescr[,-(tst_nzvar)]
}
# testar com somente dados de carteira (Muito boa curva ROC!!)
trainDescr <- trainDescr[,c(2,3,6)]
testDescr <- testDescr[,c(2,3,6)]
# repetir a análise separadamente somente dados de acionamento  (Curva ROC boa também)
trainDescr <- trainDescr[,c(1,4,5)]
testDescr <- testDescr[,c(1,4,5)]
# repetir a análise com dados de carteira e acionamento  (Curva ROC melhor de todas)
#trainDescr <- trainDescr[,c(1,4,5)]
#testDescr <- testDescr[,c(1,4,5)]
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combinar os dois casos, testando dados de previsão para ambos separados,
# combinando as maiores probabilidades de carteira e acionamento (como?)
# Ex. cliente X tem 90%, o mesmo cliente tem 70% com operador/data/hora
# Ex. cliente Y tem 20%, o mesmo cliente tem 90% com operador/dia semana/hora
# colocar como peso a multiplicaão da duas, e alimentar
# matriz de ligações por operador/ dia semana/hora
# ex. cliente A, para Operador B, no dia de semana X, hora Y
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#----- eliminando features com menor importância (funciona com features numéricas ou categóricas)
# outra importante técnica de separar features importantes
require(MASS)
#trainTotal <- cbind(sexo = trainClass,trainDescr)
initial <- glm(acordo ~ ., data = cbind(acordo = trainClass,trainDescr), family = "binomial")
stepAIC(initial, direction = "both")


#######################################
## BUILDING AND TUNING MODELS
#######################################
control <- trainControl(method="repeatedcv", number=10, repeats=3,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary # comentar para uso com iris
)
nb_model <- train(trainDescr, trainClass, 
                  #nbagg = 50,
                  metric = "ROC",
                  preProcess=c("center", "scale"),
                  trControl=control,  
                  method="nb")
models <- list(nb = nb_model)
#plot(models$nb, plotType = "level")
resampleHist(models$nb)

# obtém probabilidades dos modelos
probValues <- extractProb(
    models,
    testX = testDescr,
    testY = testClass)
# confusion matrix
svm_cf <- confusionMatrix(probValues$pred, probValues$obs)
print(svm_cf$table)
print(svm_cf$byClass)
print(svm_cf$overall)
# making a prediction object
pred <- prediction(probValues$S, probValues$obs)
# ROC curve
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
# AUC
roc.auc = performance(pred, measure = "auc")
auc <- roc.auc@y.values
# Plot roc. objects (para cada modelo)
#-----------------
plot(roc.perf)
abline(a=0, b= 1)
# getting optimal cut-point (melhor balanço entre TPR = max and FPR = min)
opt.cut = function(perf, pred){
    cut.ind = mapply(FUN=function(x, y, p){
        d = (x - 0)^2 + (y-1)^2
        ind = which(d == min(d))
        c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
          cutoff = p[[ind]])
    }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))
# Criar um data.frame com as probabilidades até o índice obtido no objeto performance
# obtenho o valor de cutoff obtido da função acima
valor_cutoff <- opt.cut(roc.perf, pred)[3]
# crio data frame com as probabilidades do preditor (já ordenado)
df_otpm <- data.frame (my_pred = roc.perf@alpha.values[[1]])
# obtém dataframe final apenas com as probabilidades acima do cutoff escolhido
df_otpm <-
    df_otpm %>%
    filter (df_otpm >= valor_cutoff)

# outrso plots ROC
# lift plot
roc.perf = performance(pred, measure = "lift", x.measure = "rpp")
# Plot roc. objects (para cada modelo)
#-----------------
plot(roc.perf)
abline(a=0, b= 1)
# sens/spec
roc.perf = performance(pred, measure = "sens", x.measure = "spec")
# Plot roc. objects (para cada modelo)
#-----------------
plot(roc.perf)
abline(a=0, b= 1)
# ---------------- A tree induction to observe best features
#form <- as.formula(acordo ~ .)
#tree.2 <- rpart(form,cbind(acordo = trainClass,trainDescr[,-c(2,4)]))		# A more reasonable tree
#prp(tree.2)   
# A fast plot													
#fancyRpartPlot(tree.2)				# A fancy plot from rattle
#--------------------------------------------------------------------------------
# 2. considerar acordos somente sobre os acionamentos de sucesso por contrato
#--------------------------------------------------------------------------------
df_acordo_suces <-l_acordo[[2]]
# confere prior probabilities
prop.table(table(df_acordo_suces$acordo))
#ncol(trainDescr)
#selecionando as features e target em arquivos diferentes
class <- as.factor(df_acion[,"acordo"]) # transformando em vetor de fatores de target
descr <- df_acion[,-c(1,5,6,7,8,9,10,11)] # transformando em dataframe de features

