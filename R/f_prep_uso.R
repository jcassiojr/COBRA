#' funcao para preparar dados de uso a partir de dados de cliente Avon
#'  que não estão na base de acionamentos
#require("dplyr")
f_prep_uso <- function(df_carteira_in, df_pg_in) {
    
    # ARQUIVO DE USO PARA PREVISÃO 
    ###########################################################
    # criar arquivo para previsão a partir de clientes Avon que não
    # aparecem em pagamentos
    useDescr <- anti_join(df_carteira_in, df_pg_in,by=c("CONTRATO"))
    useID <- useDescr[,1:2]
    useDescr <- useDescr[,3:5]
    
    # retorna lista com:
    # dataframe para identificar cliente por nro Contrato e tipo (cobrável, incobrável)
    # dataframe com features de clientes que não pagaram para uso em previsão
    l <- list(id = useID, descr = useDescr)
    return(l)
}