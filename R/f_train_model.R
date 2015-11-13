#'função que treina modelo de data.frame passado

#require("caret")
#require("MASS")
#require("ROCR")
#library("lubridate")
#require("doMC")
f_train_model <- function(df_in) {
    
    registerDoMC(5) # parallel processing
 
    #selecionando as features e target em arquivos diferentes
    class <- as.factor(df_in[,"pago"]) # transformando em vetor de fatores de target
    # usando Operador, Cidade, diasem.acion, hora.acion, valGroups
    #descr <- df_in[,c(3,5,10,11,12)] # transformando em dataframe de features
    descr <- df_in[,c(3,10,11,12)] # tirando cidade para teste enquanto não tem
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

    # eliminando features com menor importância 
    
    #trainTotal <- cbind(sexo = trainClass,trainDescr)
    initial <- glm(pago ~ ., data = cbind(pago = trainClass,trainDescr), family = "binomial")
    aic_o <- stepAIC(initial, direction = "both", trace = FALSE)
    
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
    #plot(roc.perf)
    #abline(a=0, b= 1)
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
    
}