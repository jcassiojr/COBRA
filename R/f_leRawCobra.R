#'função de preparação dos dados tidy a partir dos dados raw
#'# estratégia de manipulação
# 1. obter o arauivo total de acionamentos no período
# 2. obter o arquivo de pgtos efetuados
# 3. mesclar dados de pgto com de acionamento por Contrato, eliminando os que não existem
#    no dataframe acionamento e os contratos duplicados
# 3.b alternativa: mesclar acionamentos com arquivo de acordos por CPF 
#    (neste caso, não tem Contrato. primeiro mesclar acion + clientes AVon
#    por Contrato e depois mesclar resultado com arquivo Acordo por CPF)
# 4. criar variavel target pgto=S/N
# 5. preparar as features para serem usadas

# features a usar de clientes Avon e Pgto: CPF, Valor/Valor.Acordo (de pgtos), Contrato

require("xlsx")
require("data.table")
#require("dplyr")
#require("doMC")
#require("lubridate")

f_leRawCobra <- function() {
    registerDoMC(5) # parallel processing
    
    # ler planilha com dados de acordos
    #df_acor <- read.xlsx2("./data/Acordos-cass.xls", sheetIndex = 1, header = TRUE)
    
    # passo 1: ler planilha com dados de clientes Avon. 
    # clientes ativos, sem acordo ativo, tipo: cobráveis, incobráveis e terceira fase
    # a ser usada para aplicar modelo preditivo
    # obs: salvar os dados da planilha original Avon em csv para performance
    
    #-----------------------------------------------------------
    # obter dados do cliente (para o caso de usar arquivo de Acordos ao invés de Pagto)
    #-----------------------------------------------------------
    df_cliav_cobr <- read.csv("./data/Clientes Avon-cass-cobr.csv", header = TRUE)
    # tirar duplicidade de contratos 
    df_cliav_cobr <- 
        df_cliav_cobr %>%
        distinct(Contrato)
    #df_cliav_cobr <-
        #df_cliav_cobr
        #mutate (tipo.cobranca = "cobravel") # criando coluna para classificar tipo cobranca
    df_cliav_incobr <- read.csv("./data/Clientes Avon-cass-incobr.csv",header = TRUE)
    
    # tirar duplicidade de contratos 
    df_cliav_incobr <- 
        df_cliav_incobr %>%
        distinct(Contrato)
    #df_cliav_incobr <-
        #df_cliav_incobr
        #mutate (tipo.cobranca = "incobravel") # criando coluna para classificar tipo cobranca
    
    # concatenando incobraveis e cobraveis
    df_cliav <- rbind(df_cliav_cobr,df_cliav_incobr)
    
    # obtendo somente as features: CPF..CGC, Contrato, Valor
    # obs: para converter coluna valor em numérica
    # primeiro tirando a vírgula da coluna
    df_cliav <-
        df_cliav %>%
        mutate (CGC...CPF = as.character(CGC...CPF), # forçando CPF character
                             Valor = as.numeric(gsub(",","", Valor))) %>% # forçando Valor numeric
        select (CPF = CGC...CPF, Contrato, Valor)
    
    #-----------------------------------------------------------
    # obter acionamentos que geraram pagamentos para considerar sucesso
    #-----------------------------------------------------------
    
    # ler planilha com dados de acionamentos
    df_acion <- read.xlsx2("./data/Acionamentos out e nov 2015-raw.xlsx", sheetIndex = 1, header = TRUE)

    #-----------------------------------------------------------
    # obter dados de pgto
    #-----------------------------------------------------------
    # obs: campos de valor já vêem sem vírgula como separador de milhar
    # obs: cuidado para limpar colunas de totais do xlsx
    df_pg <- read.xlsx2("./data/PGTO 2015-cass.xls", sheetIndex = 1, header = TRUE)
 
    # mudando nome da coluna Nosso.Número para Contrato e
    # inserindo hífen no número do contrato
    # obtendo somente as features: CPF..CGC, Contrato, Valor
    df_pg <-
        df_pg %>%
        mutate(Contrato = Nosso.Número) %>%
        mutate(Contrato = paste0(substr(Nosso.Número, 1,5),"-", substr(Nosso.Número, 6,8))) %>%
        select(CPF, Contrato, Valor = Valor.Acordo) # ou usar Valor.Principal?
    
    # NÃO FUNCIONA NO dplyr! x <- df_pg %>% mutate (Valor = as.numeric(as.charcter(Valor)))
    # tem que fazer passo a passo ocmo abaixo
    df_pg <- df_pg %>% mutate (Valor = as.character(Valor)) # para acerto do merge com cliav
    df_pg <- df_pg %>% mutate (Valor = as.numeric(Valor)) # para acerto do merge com cliav
    df_pg <- df_pg %>% mutate (CPF = as.character(CPF)) # para acerto do merge com cliav
    
    # combina este dataframe com o de acionamentos para obter
    # acionamentos que obtiveram sucesso no pgto. pgto = S
    # merge com arquivo de pgtos para ver quais se 
    # encontram nos acionamentos, para trazer poder gerar target
    # pgto = S/N
    # escopo: merge com todas as ocorrências de df_acion, 
    # somente as ocorrencias de df_pg que aparecem em ambos e com duplicações mantidas
    df_acion_pg <- merge(df_acion, df_pg,by=c("Contrato"), all.x = TRUE)
    
    # criar variavel target pago = S para os casos obtidos (obter somente estas linhas)
    df_acion_pg <-
        df_acion_pg %>%
        mutate(pago = ifelse(!is.na(CPF), "S", CPF))
    # deixando as colunas como character
    #df_acion_pg <- df_acion_pg %>% mutate (CPF = as.character(CPF))

    # Aparentemente, o arquivo de clientes não tem aqueles que pagaram,
    # mas existem algumas duplicações (clientes em ambos os arquivos)
    #  que fazer nesta situação? A principio vou considerar pago, se aparece
    # em ambos
    df_acion_pg_cli <- merge(df_acion_pg, df_cliav,by=c("Contrato"), all.x = TRUE)
    
    df_acion_pg_cli <- 
        df_acion_pg_cli %>% 
        mutate (CPF.x = ifelse(!is.na(CPF.y),CPF.y, CPF.x), # mesclando CPFs
                pago = ifelse(is.na(pago),"N", pago)) %>% # marcando o que não é pago
        filter(!is.na(CPF.x)) # tirando as linhas sem informação em pago e clientes Avon

    # consolidando valores na coluna Valor.x
    # Se tem valor nas duas, mantém o Valor.x (negociado)
    df_acion_pg_cli <- 
        df_acion_pg_cli %>% 
        mutate(Valor.x = ifelse(pago == "N", Valor.y,Valor.x))
    
    # eliminar registros que não tem informações de clientes
    df_acion_pg_cli <-
        df_acion_pg_cli %>%
        filter(!is.na(CPF.y))
    
    # tirar os contratos duplicados e colunas não usadas
    df_acion_pg_cli <- 
        df_acion_pg_cli %>%
        distinct(Contrato)
    # elimina colunas desnecessárias
    df_acion_pg_cli <- 
        df_acion_pg_cli %>%
        select(Valor = Valor.x, CPF = CPF.x,  everything()) %>%
        select(-Valor.y, -CPF.y)
    
    # probabilidade prior para acordos conseguidos por acionamento de toda a carteira
    prop.table(table(df_acion_pg_cli$pago))
    # 3.9 % de pagamentos entre todos os acionamentos (sucesso ou não)
    
    # criar coluna de dia da semana e de hora do dia para
    # usar como feature
    # os dados na coluna Acionamento tem o formato DD/MM/AAA HH:MM
    df_acion_pg_cli <-
        df_acion_pg_cli %>%
        mutate (Acionamento = dmy_hm(Acionamento),
                diasem.acion = wday(Acionamento, label = TRUE),
                hora.acion = hour(Acionamento))
    
    # criar coluna pago S/N
    #df_acion_pg
    #    df_acion_pg %>%
    #        mutate(pago = ifelse(Ocorrência %in% c("Acordo", "Acordo - Manutenção", "Acordo - Realizado",
    #                                              "Boleto Gerado", "Inclusão de Forma de Pagamento",
    #                                              "Alteração de Forma de Pagamento", "Promessa"), "S", "N"))
    #-----------------------------------------------------------
    # obtem acordos conseguidos para toda a carteira cobravel
    #-----------------------------------------------------------
    #df_acordo_cart <-
    #    df_acion %>%
    #    mutate (acordo = ifelse(Ocorrência %in% c("Acordo", "Acordo - Manutenção", "Acordo - Realizado",
    #                                              "Boleto Gerado", "Inclusão de Forma de Pagamento",
    #                                              "Alteração de Forma de Pagamento", "Promessa"), "S", "N"))
    #
    
    # tirar os contratos duplicados
    #df_acordo_cart <- 
    #    df_acordo_cart %>%
    #    distinct(Contrato)
    
    # merge com arquivo de clientes Avon para ver quais se 
    # encontram nos acionamentos, para trazer CPF e valor da dívida
    # escopo: merge com todas as ocorrências de df_acordo_cart, 
    # somente as ocorrencias de df_cliav que aparecem em ambos e com duplicações mantidas
    # df_acion_cart_total <- merge(df_acordo_cart, df_cliav,by=c("Contrato"), all.x = TRUE)
    
    # criar coluna de dia da semana e de hora do dia para
    # usar como feature
    # os dados na coluna Acionamento tem o formato DD/MM/AAA HH:MM
    #df_acion_cart_total <-
    #    df_acion_cart_total %>%
    #    mutate (Acionamento = dmy_hm(Acionamento),
    #            diasem.acion = wday(Acionamento, label = TRUE),
    #            hora.acion = hour(Acionamento))
                #hora.acion = substr(Acionamento, 12,16))
                
    # convertendo coluna valor em numérica
    # primeiro tirando a vírgula da coluna
    ##df_acion_cart_total <-
     #   df_acion_cart_total %>%
     #   mutate(Valor = as.numeric(gsub(",","", Valor)))
                
    
    # eliminar registros sem estas informações de acionamentos
    # se der muito poucos valores, pegar acionamentos anteriores
    #df_acion_cart_tidy <-
    #    df_acion_cart_total %>%
    #    filter(!is.na(CGC...CPF))
    
    ## agrupar coluna Valor por quantile
    #restData$zipGroups = cut(restData$zipCode,breaks=quantile(restData$zipCode))
    df_acion_pg_cli$valGroups = cut(df_acion_pg_cli$Valor,breaks=quantile(df_acion_pg_cli$Valor))
    # eliminando a coluna de Valor
    df_acion_pg_cli <- 
        df_acion_pg_cli %>%
        select (-Valor)

    # separar acionamentos em duas bases de acordo com ocorrência: 
    # acordo e não acordo, tirando os demais
    
    # lendo separadamente arquivos de cobráveis
    df_cobr_tidy <- 
        df_acion_pg_cli %>%
        filter(Carteira == "Cobraveis")
    
    # lendo separadamente arquivos de incobráveis e Avon 3a.Fase
    df_incobr_tidy <- 
        df_acion_pg_cli %>%
        filter(Carteira == "Incobraveis")
    
    # lendo separadamente arquivos de Avon 3a.Fase
    df_3Fase_tidy <- 
        df_acion_pg_cli %>%
        filter(Carteira == "Avon 3a.Fase")
    
    
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
    df_acion <- 
        df_acion %>%
        distinct(Contrato)
    
    df_incobr_use <- merge(df_cliav_incobr, df_acion,by=c("Contrato"), all.x = TRUE)
    
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
        df_acion %>%
        distinct(Operador) 
    v_oper <- as.character(df_oper$Operador)
    
    # diasem.acion -> criando vetor
    df_acion <-
        df_acion %>%
        mutate (Acionamento = dmy_hm(Acionamento),
                diasem.acion = wday(Acionamento, label = TRUE),
                hora.acion = hour(Acionamento)) 
    df_wday <-
        df_acion %>%
        distinct(diasem.acion)
    v_wday <- as.character(df_wday$diasem.acion)
    #class(v_wday)    
    # hora.acion -> criando vetor 
    df_hora <-
        df_acion %>%
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
    df_use_final <- transform(df_use_final, contr_valgrp = colsplit(contr_valgrp, split = "\\#", names = c('Contrato', 'valGroups')))
    # retorna lista com os data.frames para uso no treino e teste do modelo
    # separados para os tipos de carteira:
    # total de acionamantos cobráveis
    # total de acionamentos incobráveis
    # total de acionamentos Avon 3a.Fase
    
    l <- list(df_cobr_tidy,df_incobr_tidy,df_3Fase_tidy)
    
    return (l) 
}