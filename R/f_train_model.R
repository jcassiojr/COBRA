#'função que treina modelo de data.frame passado

#require("caret")
#require("MASS")
#require("ROCR")
#library("lubridate")
#require("doMC")
f_train_model <- function(df_in) {
    
    registerDoMC(5) # parallel processing
 
    #selecionando as features e target em arquivos diferentes
    class <- as.factor(df_in[,"PAGO"]) # transformando em vetor de fatores de target
    class <- factor(class, levels = c("S","N")) # ordenando levels para "S" se ro primeiro
    # usando Operador, Cidade, diasem.acion, hora.acion, valGroups
    # OBS: nenhuma cobinação abaixo pareceu melhorar o modelo
    # ABAIXO USANDO VALOR DEVIDO AGRUPADO
    #descr <- df_in[,c(8,9,11,12,19)] # transformando em dataframe de features
    # ABAIXO USANDO VALOR, SETOR e DIAS ATRASO AGRUPADOS
    #descr <- df_in[,c(11,12,19,20,21)] # transformando em dataframe de features
    # ABAIXO USANDO VALOR DEVIDO SEM AGRUPAR E USANDO SOMENTE FEATURES DO ARQUIVO AVON PARA PODER PREVER DEPOIS
    descr <- df_in[,c(8,9,10)] # Obs: depois trocar Valor Devido por Faixa para ver se melhora o modelo!!
    # transformando N. Parcelas NA em média dos valores e depois em zero para ver se melhora o modelo 
    # MELHOROU O MODELO! FICOU MELHOR QUE CONSIDERANDO A MEDIA ABAIXO
    #descr <-
    #    descr %>%
    #    mutate(NPARCELAS.PAGAS = ifelse(is.na(NPARCELAS.PAGAS), 0,NPARCELAS.PAGAS),
    #           DESC.PERC = ifelse(is.na(DESC.PERC), 0,DESC.PERC))
    # Tentar com NA igual ao valor médio
    # calculo da média
    #aux <- na.omit(descr)
    #mean.nparc <- mean(aux$NPARCELAS.PAGAS)
    #descr <-
    #    descr %>%
    #    mutate(NPARCELAS.PAGAS = ifelse(is.na(NPARCELAS.PAGAS), mean.nparc,NPARCELAS.PAGAS))
    # por enquanto considerar somente VALOR.DEVIDO (VALOR PAGO E PARCELAS TEM NA. NÃO PODEM SER FEATURES???)
    # ----- cria datasets de treino, teste e uso em previsão
    
    # TESTE PARA CRIAR DESCONTO CONCEDIDO PARA VER SE MELHORA O MODELO
    
    #+++++++++++++++++++++++++++++++++++
    
    set.seed(1)
    inTrain <- createDataPartition(class, p = 3/4, list = FALSE)
    
    trainDescr <- descr[inTrain,]
    testDescr  <- descr[-inTrain,]
    # aqui gerar dados de uso
    # contratos Avon que não constam de pagamento
    useDescr <- anti_join(df_carteira, df_pg,by=c("CONTRATO"))
    useDescr <- useDescr[,3:5]
    useID <- useDescr[,1:2]
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

    # eliminando features com menor importância 
    
    #trainTotal <- cbind(sexo = trainClass,trainDescr)
    initial <- glm(pago ~ ., data = cbind(pago = trainClass,trainDescr), family = "binomial")
    aic_o <- stepAIC(initial, direction = "both", trace = FALSE)
    
    #######################################
    ## BUILDING AND TUNING MODELS
    # O NAIVE BAYES PARECE Se MOSTROU MELHOR COMPARADO COM OS TESTADOS ABAIXO
    # GBM, TBG, CTREE2, GLM
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
                      na.action=na.omit,
                      method="nb")
    
    models_o <- list(nb = nb_model)
    #plot(models$nb, plotType = "level")
    #resampleHist(models$nb)
    # RETORNAR objeto models
    # obtém probabilidades dos modelos
    probValues <- extractProb(
        models_o,
        testX = testDescr,
        testY = testClass)
    # confusion matrix
    cf_o <- confusionMatrix(probValues$pred, probValues$obs)
    # RETORNAR objeto confusion Matrix
    #print(cf_o$table)
    #print(cf_o$byClass)
    #print(cf_o$overall)
    # making a prediction object
    pred_o <- prediction(probValues$S, probValues$obs)
    # ROC curve
    roc.perf_o = performance(pred_o, measure = "tpr", x.measure = "fpr")
    # Plot roc. objects (para cada modelo)
    #-----------------
    # RETORNAR objeto performance
    plot(roc.perf_o)
    abline(a=0, b= 1)
    # # RETORNAR AUC
    roc.auc_o = performance(pred_o, measure = "auc")
    auc <- roc.auc_o@y.values
    #print(auc)
    
    # getting optimal cut-point (melhor balanço entre TPR = max and FPR = min)
    #opt.cut = function(perf, pred_o){
    #    cut.ind = mapply(FUN=function(x, y, p){
    #        d = (x - 0)^2 + (y-1)^2
    #        ind = which(d == min(d))
    #        c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
    #          cutoff = p[[ind]])
    #    }, perf@x.values, perf@y.values, pred@cutoffs)
    #}
    #print(opt.cut(roc.perf, pred))
    # Criar um data.frame com as probabilidades até o índice obtido no objeto performance
    # obtenho o valor de cutoff obtido da função acima
    #valor_cutoff_o <- opt.cut(roc.perf_o, pred)[3]
    #print(valor_cutoff)
    # RETORNAR valor de cutt-off
    # crio data frame com as probabilidades do preditor (já ordenado)
    df_rank_o <- data.frame (my_pred = roc.perf_o@alpha.values[[1]])
    # RETORNAR data frame de probabilidades 
    # obtém dataframe final apenas com as probabilidades acima do cutoff escolhido
    #df_cutoff <-
    #    df_otpm %>%
    #    filter (df_otpm >= valor_cutoff)
    
    # outros plots ROC
    # lift plot
    #roc.perf = performance(pred, measure = "lift", x.measure = "rpp")
    # Plot roc. objects (para cada modelo)
    #-----------------
    #plot(roc.perf)
    #abline(a=0, b= 1)
    # sens/spec
    #roc.perf = performance(pred, measure = "sens", x.measure = "spec")
    # Plot roc. objects (para cada modelo)
    #-----------------
    #plot(roc.perf)
    #abline(a=0, b= 1)
    # ---------------- A tree induction to observe best features
    #form <- as.formula(acordo ~ .)
    #tree.2 <- rpart(form,cbind(acordo = trainClass,trainDescr[,-c(2,4)]))		# A more reasonable tree
    #prp(tree.2)   
    # A fast plot													
    #fancyRpartPlot(tree.2)				# A fancy plot from rattle
    #--------------------------------------------------------------------------------
    # 2. considerar acordos somente sobre os acionamentos de sucesso por contrato
    #--------------------------------------------------------------------------------
    #df_acordo_suces <-l_acordo[[2]]
    # confere prior probabilities
    #prop.table(table(df_acordo_suces$acordo))
    #ncol(trainDescr)
    #selecionando as features e target em arquivos diferentes
    #class <- as.factor(df_acion[,"acordo"]) # transformando em vetor de fatores de target
    #descr <- df_acion[,-c(1,5,6,7,8,9,10,11)] # transformando em dataframe de features
    l_o <- list(models_o,aic_o,cf_o, roc.perf_o, roc.auc_o, pred_o, df_rank_o)
    return(l_o)
    
    
    
    # ABAIXO TESTE DE OUTROS MODELOS
    #--------------------------------
    # STOCHASTIC GRADIENT BOOST MODEL (GBM)
    #---------------
    # At each step of the GBM algorithm, a new decision tree is constructed. 
    # The question when growing a decision tree is 'when to stop?'. The 
    # furthest you can go is to split each node until there is only 1 
    # observation in each terminal node. This would correspond to 
    # n.minobsinnode=1. Alternatively, the splitting of nodes can cease 
    # when a certain number of observations are in each node. The default 
    # for the R GBM package is 10.
    # usando abaixo tunning mais sofisticado do grid
    #gbmGrid <- expand.grid(
    #    .interaction.depth = (1:5) * 2,
    #    .n.trees = (1:10)*25,
    #    .shrinkage = .1,
    #    .n.minobsinnode = 1)
    #set.seed(2)
    #gbm_model <- train(
    #    trainDescr, trainClass,
    #    method = "gbm",
    #    trControl = control,
    ##    verbose = FALSE,
     #   metric = "ROC",
    #    preProcess=c("center", "scale"),
    #    bag.fraction = 0.5,                
    #    tuneGrid = gbmGrid)
    
    #    gbm_model
    #    gbm_model$finalModel
    ##    ggplot(gbm_model) + theme(legend.position = "top")
    
    # estimating feature importance
    #    gbm_importance <- varImp(gbm_model, scale=FALSE)
    # summarize importance
    #    print(gbm_importance)
    # plot importance
    #    plot(gbm_importance)
    
    # TREE BAG MODEL
    #---------------
    #tbg_model <- train(trainDescr, trainClass, 
    #                   nbagg = 50,
    #                   metric = "ROC",
    #                   preProcess=c("center", "scale"),
    #                   trControl=control, 
    #                   method="treebag")
    
    #    tbg_model
    ##    tbg_model$finalModel
    
    # estimating feature importance
    #    tbg_importance <- varImp(tbg_model, scale=FALSE)
    # summarize importance
    #    print(tbg_importance)
    #    plot(tbg_importance)
    
    # CONDITIONAL INFERENCE TREE MODEL
    #-------
    #ctree2_model <- train(trainDescr, trainClass, 
    #                      metric = "ROC",
    #                      preProcess=c("center", "scale"),
    #                      trControl=control, 
    #                      method="ctree2")
    #    ctree2_model
    #    ctree2_model$finalModel
    #    ggplot(ctree2_model) + theme(legend.position = "top")
    
    # estimating feature importance
    #    ctree2_importance <- varImp(ctree2_model, scale=FALSE)
    # summarize importance
    #    print(ctree2_importance)
    # plot importance
    #    plot(ctree2_importance)
    
    # BAYESIAN GENERALIZING LINEAR MODEL
    #----------
    #bglm_model <- train(trainDescr, trainClass, 
    #                    metric = "ROC",
    #                    preProcess=c("center", "scale"),
    #                    trControl=control,
    #                    method="bayesglm")
    #    bglm_model
    #    bglm_model$finalModel
    
    # estimating feature importance
    #    bglm_importance <- varImp(bglm_model, scale=FALSE)
    # summarize importance
    #    print(bglm_importance)
    # plot importance
    #    plot(bglm_importance)
    
    # GENERALIZING LINEAR MODEL
    #----------
    #glm_model <- train(trainDescr, trainClass, 
    #                   metric = "ROC",
    #                   preProcess=c("center", "scale"),
    #                   trControl=control, 
    #                   method="glm")
    #    glm_model
    #    glm_model$finalModel
    
    # estimating feature importance
    #    glm_importance <- varImp(glm_model, scale=FALSE)
    # summarize importance
    #    print(glm_importance)
    # plot importance
    #    plot(glm_importance)
    
    # BOOSTED LOGISTIC REGRESSION MODEL
    #---------
    #logb_model <- train(trainDescr, trainClass, 
    #                    #nbagg = 50,
    #                    metric = "ROC",
    ##                    preProcess=c("center", "scale"),
    #                    trControl=control,  
    #                    method="LogitBoost")
    #    logb_model
    #    logb_model$finalModel
    #    ggplot(logb_model) + theme(legend.position = "top")
    
    # estimating feature importance
    #    logb_importance <- varImp(logb_model, scale=FALSE)
    # summarize importance
    #    print(logb_importance)
    # plot importance
    #    plot(logb_importance)
    
    # NAIVE BAYES MODEL
    #---------
    #nb_model <- train(trainDescr, trainClass, 
    #                  #nbagg = 50,
    #                  metric = "ROC",
    #                  preProcess=c("center", "scale"),
    #                  trControl=control,  
    #                  method="nb")
    
    #    nb_model
    #    nb_model$finalModel
    #    ggplot(nb_model) + theme(legend.position = "top")
    
    # estimating feature importance
    #    nb_importance <- varImp(nb_model, scale=FALSE)
    # summarize importance
    #    print(nb_importance)
    # plot importance
    #    plot(nb_importance)
    
    # consolidando previsões de diversos modelos eu um output
    #models <- list(
    #                #svm = svm_model,
    #               gbm = gbm_model,#
    #               tbg = tbg_model,#
    #               ctree2 = ctree2_model,#
    #               #bglm = bglm_model,
    #               glm = glm_model,#
    #               #logb = logb_model,
    #               nb = nb_model) #
    
    
    #probValues <- extractProb(
    #    models,
    #    testX = testDescr,
    #    testY = testClass)
    # obtém somente o subset de dados de teste
    #testProbs <- subset(
    #    probValues,
    #    dataType == "Test")
    #table(testProbs$model)
    # ESTE PARECE SER O MELHOR RESULTADO
    #tipo_mod = "gbm"
    #gbmProbs <- subset(testProbs, model == tipo_mod)
    #gbm_cf <- confusionMatrix(gbmProbs$pred, gbmProbs$obs)
    #gbm_pred <- prediction(gbmProbs$S, gbmProbs$obs)
    ##roc.perf = performance(gbm_pred, measure = "tpr", x.measure = "fpr")
#    plot(roc.perf)
    #abline(a=0, b= 1)
    
    #tipo_mod = "ctree2"
    #ctree2Probs <- subset(testProbs, model == tipo_mod)
    #ctree2_cf <- confusionMatrix(ctree2Probs$pred, ctree2Probs$obs)
    #ctree2_pred <- prediction(ctree2Probs$S, ctree2Probs$obs)
    #roc.perf = performance(ctree2_pred, measure = "tpr", x.measure = "fpr")
    #plot(roc.perf)
    #abline(a=0, b= 1)
    
    #tipo_mod = "glm"
    ##glmProbs <- subset(testProbs, model == tipo_mod)
    #glm_cf <- confusionMatrix(glmProbs$pred, glmProbs$obs)
    #glm_pred <- prediction(glmProbs$S, glmProbs$obs)
    #roc.perf = performance(glm_pred, measure = "tpr", x.measure = "fpr")
    #plot(roc.perf)
    #abline(a=0, b= 1)
    
    #tipo_mod = "nb"
    #nbProbs <- subset(testProbs, model == tipo_mod)
    #nb_cf <- confusionMatrix(nbProbs$pred, nbProbs$obs)
    #nb_pred <- prediction(nbProbs$S, nbProbs$obs)
    #roc.perf = performance(nb_pred, measure = "tpr", x.measure = "fpr")
    #plot(roc.perf)
    #abline(a=0, b= 1)
    
    #tipo_mod = "treebag"
    #treebagProbs <- subset(testProbs, model == tipo_mod)
    #treebag_cf <- confusionMatrix(treebagProbs$pred, treebagProbs$obs)
    ##treebag_pred <- prediction(treebagProbs$S, treebagProbs$obs)
    #roc.perf = performance(treebag_pred, measure = "tpr", x.measure = "fpr")
    #plot(roc.perf)
    #abline(a=0, b= 1)
    
    
    
    
    
    
    
    
    
    
    #++++++++++++++++++++++++++++++++++++++
    
    
    
    
    
    
    
    
    
    
    
    
    

    
}