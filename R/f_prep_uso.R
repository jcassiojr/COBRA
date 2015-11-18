#' funcao para preparar dados de uso a partir de dados de cliente Avon
#'  que não estão na base de acionamentos
f_prep_uso <- function(df_cli_i, df_acion_i) {
    ##################################
    # ARQUIVO DE USO PARA PREVISÃO (DEPOIS SEPARAR EM FUNCAO SEPARADA)
    
    #---------------------------------------------------------------
    # criar arquivo para previsão a partir de clientes Avon que não
    # aparecem em acionamentos
    # gerar um arquivo para cobráveise e um para incobráveis
    # o arquivo dever ter as colunas:
    # CPF - não usado no modelo, para identificar cliente. Origem: arquivo clientes Avon
    # Nome - não usado no modelo, para identificar cliente. Origem: arquivo clientes Avon
    # Contrato  - não usado no modelo, para identificar cliente. Origem: arquivo clientes Avon
    # valGroups - usado no modelo via quantile gerado do valor do arquivo Avon
    # Cidade - usado no modelo. Origem: arquivo clientes Avon (por enquanto SEM)
    # Operador - usado no modelo, Origem: produto cartesiano com arquivo de uso
    # diasem.acion - usado no modelo, Origem: produto cartesiano com arquivo de uso 
    # hora.acion - usado no modelo. Origem: produto cartesiano com arquivo de uso 
    
    # obs: se não tiver Cidade do Cliente tirar do modelo
    #---------------------------------------------------------------
    # obter clientes do arquivo Avon que não aparecem no acionamento
    # usar merge que traga todos os cliav mas somente os acion que 
    
    
    # Aparentemente, o arquivo de clientes não tem aqueles que pagaram,
    # mas existem algumas duplicações (clientes em ambos os arquivos)
    #  que fazer nesta situação? A principio vou considerar pago, se aparece
    # em ambos
    
    
    # TESTAR PARA INCOBRAVEIS
    # eliminar duplicidade de contratos em acionamentos
    # tirar duplicidade de contratos 
    df_acion_i <- 
        df_acion_i %>%
        distinct(Contrato)
    
    df_incobr_use <- merge(df_cli_i, df_acion_i,by=c("Contrato"), all.x = TRUE)
    
    # criando dataframe de clientes sem acionamento cobraveis
    df_incobr_use <-
        df_incobr_use %>%
        filter(is.na(Operador)) %>%
        select(Contrato, Nome, CPF = CGC...CPF, Valor) %>%
        mutate(Valor = as.numeric(gsub(",","", Valor))) # forçando Valor numeric
    
    # agrupar coluna Valor por quantile
    df_incobr_use$valGroups = cut(df_incobr_use$Valor,breaks=quantile(df_incobr_use$Valor))
    # eliminando a coluna de Valor
    df_incobr_use <- 
        df_incobr_use %>%
        select (-Valor)
    
    # fazer produto cartesiano com dados do acionamento
    #--------------------------------------------------
    # Operador -> criando vetor
    df_oper <-
        df_acion_i %>%
        distinct(Operador) 
    v_oper <- as.character(df_oper$Operador)
    
    # diasem.acion -> criando vetor
    df_acion_i <-
        df_acion_i %>%
        mutate (Acionamento = dmy_hm(Acionamento),
                diasem.acion = wday(Acionamento),
                hora.acion = hour(Acionamento)) 
    df_wday <-
        df_acion_i %>%
        distinct(diasem.acion)
    v_wday <- as.character(df_wday$diasem.acion)
    #class(v_wday)    
    # hora.acion -> criando vetor 
    df_hora <-
        df_acion_i %>%
        distinct(hora.acion)
    v_hora <- as.numeric(df_hora$hora.acion)
    #class(v_hora)
    
    # fazer produto cartesiano com dados do cliente (par Contrato + Valgroups)
    #--------------------------------------------------
    # Contratos -> criando vetor
    df_contr_valgrp <-
        df_incobr_use %>%
        #select(Contrato, valGroups) %>%
        mutate (contr_valgrp = paste(Contrato,valGroups,sep = "#")) # concatena colunas
    # cria vetor contrato + valgroups
    v_contr_valgrp <- as.character(df_contr_valgrp$contr_valgrp)
    
    #criando produto cartesiano dos vetores
    df_use_final <- CJ(Operador = v_oper, diasem.acion = v_wday, 
                       hora.acion = v_hora, contr_valgrp = v_contr_valgrp)
    
    # separando novamente as colunas de Contrato e valGroups
    require (reshape)
    df_use_final <- transform(df_use_final, contr_valgrp = colsplit(contr_valgrp, 
                                    split = "\\#", names = c('Contrato', 'valGroups')))
    
    return(df_use_final)
}