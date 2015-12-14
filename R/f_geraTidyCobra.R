# funcao para pegar dados brutos e preparar para treinar modelo
#library("dplyr")
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
        #arrange(CONTRATO,desc(Acionamento)) %>%
        mutate( PAGO = "N") # inicializa nova coluna pago como N
    
    # marcando como PAGO = S apenas os acionamentos que aparecem no arquivo de pgtos
    # e data de pagamento menor que 10 dias do acionamento
    # obs: ver se dá muita diferença nos totais para este ultimo caso
    df_tidy_pg <- left_join(df_acion_avon, df_pg_in,by=c("CONTRATO"))
    
    # obs: pode acontecer de data acion ser depois de dt pgto pois entrou em contato após
    # pgto. Portanto, devo pegar a primeira data de acionamento em df_acion, tirar contratos
    # duplicados para manter apenas a primeira data e depois marcar os pagamentos como "S"
    # 1. pegar somente a primeira dt acion de cada contrato e tirar contratos em duplicidade
    
    # TESTE para criar coluna com min e max data de acionamento
    # MARCAR COMO PAGO os acionamentos de contratos dentro de intervalo de menos de 10 dias do pgto!!! 
    # FORMA DE ASSOCIAR ACIONAMENTO A PGTO!!! TESTAR
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
        
    
    
    
    
    
    
    
    #df_tidy_pg <-
    #    df_tidy_pg %>%
    #    group_by(CONTRATO) %>%
    #    mutate(ULT.ACION = max(DATA.ACION),
    #              PRIM.ACION = min(DATA.ACION)) %>%
    #    distinct(CONTRATO) %>%
    #    select(-OCORRENCIA, -DATA.ACION)
    
    # filtrar somente acionamentos com prim.acion < dt.pgto 
    # ATENÇÃO: último acionamento pode ser depois que já pagou algumas parcelas
    # Portanto, não deve ser usado para filtrar os dados se o objetivo é
    # detectar o primeiro pagamento. Para isso, usar somente pgto após primeiro acionamento
    # e não muito distante no tempo (dt pgto < 10 dias? ou 30 dias? do prim acionamento)
    #df_tidy_pg <-
    #    df_tidy_pg %>%
    #    mutate(PAGO = ifelse(!is.na(PAGAM_PAG) & 
    #                             (difftime(DTpgto ,PRIM.ACION , units = c("days")) >= 0), "S", PAGO))
    # filtrar somente acionamentos com ult.acion < 10 dias
    #++++++++++++++++++
    #df_tidy_pg <-
    #    df_tidy_pg %>%
    #    arrange(CONTRATO, DATA.ACION) %>%
    #    distinct(CONTRATO)
    # 3. marcar somente os dt acion antes de dt pgto < 10 dias para os casos que existe pagamento
    #df_tidy_pg <-
    #    df_tidy_pg %>%
    #    mutate(PAGO = ifelse(!is.na(PAGAM_PAG) & 
    #                             (difftime(DTpgto ,DATA.ACION , units = c("days")) < 10) &
    #                             (difftime(DTpgto ,DATA.ACION , units = c("days")) >= 0), "S", PAGO))
    
    #PAREI AQUI: criar colunas: PRIM ACIONAMENTO, ULT ACIONAMENTO
    # filtrar data pgto maior que data do primeiro acionamento e depois data de pgto menor que 10 dias da 
    # ultima data de acionamento
    # TESTE: Criar DESCONTO, para testar como analise para correlacionar com pgto
    df_tidy_pg <-
        df_tidy_pg %>%
        mutate(DESCONTO = VALOR.DEVIDO - VALOR.PAGO,
               DESC.PERC = DESCONTO/VALOR.DEVIDO)
    
    # criar faixas de Valores (por enquanto não para Setores e Dias Atraso. Testar depois se melhora o modelo!!) NÃO!!!
    # criando colunas de quantiles para valor para ser usada como classificador
    # criar coluna de desconto concedido como feature!!!!
    #df_tidy_pg$Faixa.Setor = cut(df_tidy_pg$SETOR,breaks=quantile(df_tidy_pg$SETOR), include.lowest = TRUE)
    #df_tidy_pg$Faixa.DiasAtraso = cut(df_tidy_pg$DIAS_ATRASO,breaks=quantile(df_tidy_pg$DIAS_ATRASO), include.lowest = TRUE)
    #df_tidy_pg$Faixa.Valor.Devido = cut(df_tidy_pg$VALOR.DEVIDO,breaks=quantile(df_tidy_pg$VALOR.DEVIDO), include.lowest = TRUE)
    
    # eliminando a coluna de Valor
    #df_tidy_pg <- 
    #    df_tidy_pg %>%
    #    select (-VALOR.DEVIDO)
    
    
    
    # proporcao de pagos
    #prop.table(table(df_tidy_pg$PAGO))
    
    # libera library usada na funcao para evitar conflitos em outras funcoes
    #detach("package:dplyr", unload=TRUE)
    
    return(df_tidy_pg)
    
    # FALTA
    #TB, Calcular percentual pago em relação ao devido (FEATURE? OU PARA CORREL E ANALISAR % PAGO POR ALGUMA DIMENSAO?)
    
    #+++++++++++++++++++++++++++++++++++++++++++++++
    
    
    # 4. selecionar somente os dt acion antes de dt pgto para os casos que existe pagamento
    
    #df_tidy_pg <-
    #    df_tidy_pg %>%
    #    filter(difftime(DTpgto ,DATA.ACION , units = c("days")) >= 0)
    # 3. proceder a marcar PAGO = "S como abaixo
    #df_tidy_pg <-
    #    df_tidy_pg %>%
    #    filter(difftime(DTpgto ,DATA.ACION , units = c("days")) >= 0)
    # CONSIDERAR SOMENTE PAGO SE DT RECEB < 10 dias do acionamento
    # OBS: sem considerar os 10 dias, o máximo que retornou foram 24 dias (perguntar se considera)
    # ANTES: ordenar os acionamentos e pegar o último para subtrair as datas
    
    

    
    #df_tidy_pg <-
    #    df_tidy_pg %>%
    #    mutate(PAGO = ifelse(!is.na(PAGAM_PAG) & 
    #                             (difftime(DTpgto ,DATA.ACION , units = c("days")) < 10) &
    #                             (difftime(DTpgto ,DATA.ACION , units = c("days")) >= 0), "S", PAGO)) %>%
    #    mutate(DIFDT = difftime(DTpgto ,DATA.ACION , units = c("days")))
    
    
    
    # tirando contratos diplicados, mas contando número de contatos
    # quais considerar como contatos para contar?
    # R: por enquanto deixar em aberto. Responder para qd for usar a contagem 
    #df_tidy_pg <-
    #    df_tidy_pg %>%
    #    distinct(CONTRATO)
    
   
    
}