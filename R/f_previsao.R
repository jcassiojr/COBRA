# função de previsão
#require("dplyr")
#require("caret")
f_previsao <- function(l_model_in, l_uso_in) {
    # i_ID: contem os colunas que identificam o cliente: CONTRATO E TIPO (tipo: cobrável, incobrável) obtidos da
    # planilha clientes Avon que não constam da planilha de pagamento
    # i_useDescr: semelhante ao TestDescr, mas montado a partir dos dados da Avon: colunas: 
    # DIAS ATRASO, SETOR, VALOR DEVIDO
    
    # obtem os dados de identificação do cliente e de features
    useID <- l_uso_in[[1]]
    useDescr <- l_uso_in[[2]]
    # obtendo probabulidades da classificação
    l_probs <- predict(l_model_in[[1]], useDescr, type = "prob")
    # concatennando probabilidades de PAGO = "S"com identificação do cliente Avon
    df_probs <- data.frame(ID = useID, probClass = l_probs[[1]]$S)
    # ordenando pro maior probabilidade
    df_kank_out <-
        df_probs %>%
        arrange(desc(probClass))
    #x <- df_probs %>% filter(grepl("Cobraveis", df_probs$ID.CARTEIRA))
    # seleciona a probabilidade do ID desejado para cada modelo
    #l_prv_id [[i]]<- 
    #    l_id_probs %>%
    #    filter (ID == i_ID)
    
    # libera library usada na funcao para evitar conflitos em outras funcoes
    #detach("package:dplyr", unload=TRUE)
    #detach("package:caret", unload=TRUE)
    
    return(df_kank_out)
}