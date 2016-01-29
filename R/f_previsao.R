# função de previsão 
# recebe lista com modelo escolhido e lista com data frames de uso (um para ID do cliente e outro
# para suas features)
# i_ID: contem os colunas que identificam o cliente: CONTRATO E TIPO (tipo: cobrável, incobrável) obtidos da
# planilha clientes Avon que não constam da planilha de pagamento
# i_useDescr: semelhante ao TestDescr, mas montado a partir dos dados da Avon: colunas: 
# DIAS ATRASO, SETOR, VALOR DEVIDO

f_previsao <- function(l_model_in, l_uso_in) {
   
    # obtem os dados de identificação do cliente e de features
    ##########################################################
    useID <- l_uso_in[[1]]
    useDescr <- l_uso_in[[2]]
    
    # obtendo probabilidades da classificação para pago = S
    l_probs <- predict(l_model_in[[1]], useDescr, type = "prob")
    
    # concatenando probabilidades com identificação do cliente Avon
    # alteracao: incluindo valor da divida para usar como corte
    df_probs <- data.frame(ID = useID, divida = useDescr$VALOR.DEVIDO, probClass = l_probs[[1]]$S)
    
    # ordenando por maior probabilidade
    df_kank_out <-
        df_probs %>%
        arrange(desc(probClass))
    
    # retorna dataframe com ranking de probabilidades de pago S, identificação do cliente (contrato)
    # e tipo de carteira (cobrável, incobrável)
    return(df_kank_out)
}