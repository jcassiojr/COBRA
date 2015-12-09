#' funcao para preparar dados de uso a partir de dados de cliente Avon
#'  que não estão na base de acionamentos
#require("dplyr")
f_prep_uso <- function(df_carteira_in, df_pg_in) {
    ##################################
    # ARQUIVO DE USO PARA PREVISÃO (DEPOIS SEPARAR EM FUNCAO SEPARADA)
    #---------------------------------------------------------------
    # criar arquivo para previsão a partir de clientes Avon que não
    # aparecem em pagamentos
    # aqui gerar dados de uso
    # contratos Avon que não constam de pagamento
    useDescr <- anti_join(df_carteira_in, df_pg_in,by=c("CONTRATO"))
    useID <- useDescr[,1:2]
    useDescr <- useDescr[,3:5]
    
    # separando tb por cobraveis e incobraveis
    #useDescr <- anti_join(df_carteira_in, df_pg_in,by=c("CONTRATO"))
    
    # prepara cobraveis
    #useDescr.cobr <- 
    #    useDescr %>%
    #    filter(grepl("Cobraveis", useDescr$CARTEIRA))
    #useID.cobr <- useDescr.cobr[,1:2]
    #useDescr.cobr <- useDescr.cobr[,3:5]
    # prepara incobraveis
    #useDescr.incobr <- 
    #    useDescr %>%
    #    filter(grepl("Incobraveis", useDescr$CARTEIRA))
    #useID.incobr <- useDescr.incobr[,1:2]
    #useDescr.incobr <- useDescr.incobr[,3:5]
    
    # libera library usada na funcao para evitar conflitos em outras funcoes
    #detach("package:dplyr", unload=TRUE)
    
    l <- list(id = useID, descr = useDescr)
    return(l)
}