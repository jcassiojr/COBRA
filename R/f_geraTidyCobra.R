# funcao para pegar dados brutos e preparar para treinar modelo

f_geraTidyCobra <- function(df_acion_in, df_carteira_in, df_pg_in) {

    # obtendo somente os acionamentos de clientes Avon
    df_acion_avon <- inner_join(df_acion_in, df_carteira_in,by=c("CONTRATO"))
    
    df_acion_avon <-
        df_acion_avon %>%
        mutate (DATA.ACION = ymd_hms(DATA.ACION),
                DIASEM.ACION = wday(DATA.ACION, label = TRUE),
                HORA.ACION = hour(DATA.ACION)) 
    # ATENÇÃO: hora de acionamento não é muito confiável. sistema gera zerado!!
    # por isso, considerar somente acionamentos das 8 as 20 horas
    df_acion_avon <-
        df_acion_avon %>%
        filter (HORA.ACION %in% c(8:20))
    
    # marcando os acionamentos que resultaram em pagamento
    # obtendo apenas o primeiro pagamento de cada contrato
    # ordena por contrato e Recebimento, salvando o valor total da dívida paga
    # (somando as parcelas)
    df_pg_in <-
        df_pg_in %>%
        arrange(CONTRATO,DTpgto) %>%
        group_by(CONTRATO) %>%
        mutate(VALOR.PAGO = sum(VlPag),
               NPARCELAS.PAGAS = n()) %>% # aqui cuidado, pois algumas parcelas se repetiram (??)
        distinct(CONTRATO) %>%
        select(-VlPag)
    
    # primeiro marcando todos como PAGO = N
    df_acion_avon <-  
        df_acion_avon %>%
        mutate( PAGO = "N") # inicializa nova coluna pago como N
    
    # marcando como PAGO = S apenas os acionamentos que aparecem no arquivo de pgtos
    # e data de pagamento menor que 10 dias de algum acionamento
    # obs: ver se dá muita diferença nos totais para este ultimo caso
    df_tidy_pg <- left_join(df_acion_avon, df_pg_in,by=c("CONTRATO"))
    
    # Associando pagamento a acionamentos do contrato
    # obs: pode acontecer de data acion ser depois de dt pgto pois entrou em contato após
    # pgto. Portanto, devo pegar a data de acionamento em df_acion mais próxima e anterior a menos de
    # 10 dias de algum pagamento. MARCAR COMO PAGO os acionamentos de contratos dentro deste intervalo
    #################################################################
    
    # 1. filtrar somente DATA.ACION menor ou igual a 10 dias de DtPag somente para os que tem pgto
    df_tidy_pg <-
        df_tidy_pg %>%
        filter((difftime(DTpgto ,DATA.ACION , units = c("days")) < 10) &
                   (difftime(DTpgto ,DATA.ACION , units = c("days")) >= 0) |
                   is.na(PAGAM_PAG))
    
    # 2. marcar como PAGO (OK!)
    # 3. tirar contratos duplicados
    df_tidy_pg <-
        df_tidy_pg %>%
        mutate(PAGO = ifelse(!is.na(PAGAM_PAG), "S", PAGO)) %>%
        distinct(CONTRATO)
        
    # Desligado por enquanto, salvar primeira e última data de acionamento por contrato
    #df_tidy_pg <-
    #    df_tidy_pg %>%
    #    group_by(CONTRATO) %>%
    #    mutate(ULT.ACION = max(DATA.ACION),
    #              PRIM.ACION = min(DATA.ACION)) %>%
    #    distinct(CONTRATO) %>%
    #    select(-OCORRENCIA, -DATA.ACION)
    

    # Criar DESCONTO, para testar como analise para correlacionar com pgto
    df_tidy_pg <-
        df_tidy_pg %>%
        mutate(DESCONTO = VALOR.DEVIDO - VALOR.PAGO,
               DESC.PERC = DESCONTO/VALOR.DEVIDO)
    
    # retorna dataframe pronto para analises
    return(df_tidy_pg)
}