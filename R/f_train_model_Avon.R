#'função que treina modelo de data.frame passado

#require("caret")
#require("MASS")
#require("ROCR")
#library("lubridate")
#require("doMC")
f_train_model_Avon <- function(df_in) {
    
    #registerDoMC(5) # parallel processing
 
    #selecionando as features e target em arquivos diferentes
    class <- as.factor(df_in$pago) # transformando em vetor de fatores de target
    # reordenando para considerar como o evento = S e não evento = N
    class <- factor(class, levels = c("S","N"))
    # usando atributos:
    # Cidade, Numero.Contatos, Diasem.Acion, Hora.Acion, Faixa.Valores
    #descr <- df_in[,c(4,10,11,12,13)] # tirando cidade para teste enquanto não tem
    # TESTE só com dados do cadastro: Cidade, faixa.Valores
    descr <- df_in[,c(4,13)] # tirando cidade para teste enquanto não tem
    
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
    
    # obtém probabilidades dos modelos
    probValues <- extractProb(
        models_o,
        testX = testDescr,
        testY = testClass)
    # confusion matrix
    cf_o <- confusionMatrix(probValues$pred, probValues$obs)

    # making a prediction object
    pred_o <- prediction(probValues$S, probValues$obs)
    # ROC curve
    roc.perf_o = performance(pred_o, measure = "tpr", x.measure = "fpr")


    # AUC
    roc.auc_o = performance(pred_o, measure = "auc")
    auc <- roc.auc_o@y.values
   
    # crio data frame com as probabilidades do preditor (já ordenado)
    df_rank_o <- data.frame (my_pred = roc.perf_o@alpha.values[[1]])

    l_o <- list(models_o,aic_o,cf_o, roc.perf_o, roc.auc_o, pred_o, df_rank_o)
    return(l_o)
}