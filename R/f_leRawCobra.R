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
#require("data.table")
#require("dplyr")
#require("doMC")
require("lubridate")

f_leRawCobra <- function() {
    # constantes
    vl_dívida_minimo = 1.0
    
    registerDoMC(5) # parallel processing
    
    # ler planilha com dados de acordos
    #df_acor <- read.xlsx2("./data/Acordos-cass.xls", sheetIndex = 1, header = TRUE)
    
    # passo 1: ler planilha com dados de clientes Avon. 
    # clientes ativos, sem acordo ativo, tipo: cobráveis, incobráveis e terceira fase
    # a ser usada para aplicar modelo preditivo
    # obs: salvar os dados da planilha original Avon em csv para performance
    
    #-----------------------------------------------------------
    # obter dados do cliente Avon
    #-----------------------------------------------------------
   
    # FALTA MEMÓRIA: df_carteira <- read.xlsx2("./data/Dados Raw-04-12-2015.xlsx", sheetIndex = "CARTEIRA", colIndex = c(1,2,5:8), header = TRUE)
    df_carteira <- read.csv("./data/Dados Raw-04-12-2015-CARTEIRA.csv", header = TRUE)
    # tirar duplicidade de contratos 
    df_carteira <- 
        df_carteira %>%
        distinct(CONTRATO)

    # somando valores para obter valor total da dívida 
    # antes, dois passos para transformar coluna JUROS de fator -> character -> numeric
    df_carteira <- df_carteira %>% mutate (JUROS = as.character(JUROS)) # para acerto do merge com cliav
    df_carteira <- df_carteira %>% mutate (JUROS = as.numeric(JUROS)) # para acerto do merge com cliav
    
    ind <-is.na(df_carteira$JUROS)
    df_carteira[ind,]
    # eliminando valores de JUROS igual a NA (nao tem valores de divida tb)
    df_carteira <- na.omit(df_carteira)
    
    df_carteira <-
        df_carteira %>%
        mutate(VALOR.DEVIDO = VLSAL_CON + JUROS + VLORI_TRA)
    
    # elimina clientes com valor abaixo do mínimo definido (R$ 1,00 ?)
    df_carteira <-
        df_carteira %>%
        filter (VALOR.DEVIDO >= vl_dívida_minimo)
    
    # eliminando colunas desnecessárias
    df_carteira <-
        df_carteira %>%
        select (-TITUL_TRA, -NOME, -VLSAL_CON, -JUROS, -VLORI_TRA)
    
    #df_cliav_incobr <- read.csv("./data/Clientes Avon-cass-incobr.csv",header = TRUE)
    
    # tirar duplicidade de contratos 
    #df_cliav_incobr <- 
    #    df_cliav_incobr %>%
    #    distinct(Contrato)
    #df_cliav_incobr <-
        #df_cliav_incobr
        #mutate (tipo.cobranca = "incobravel") # criando coluna para classificar tipo cobranca
    
    # concatenando incobraveis e cobraveis
    #df_cliav <- rbind(df_cliav_cobr,df_cliav_incobr)
    
    # obtendo somente as features: CPF..CGC, Contrato, Valor
    # obs: para converter coluna valor em numérica
    # primeiro tirando a vírgula da coluna
    #df_cliav <-
    #    df_cliav %>%
    #    mutate (CGC...CPF = as.character(CGC...CPF), # forçando CPF character
    #                         Valor = as.numeric(gsub(",","", Valor))) %>% # forçando Valor numeric
    #    select (CPF = CGC...CPF, Contrato, Valor)
    
    
    #-----------------------------------------------------------
    # obter acionamentos que geraram pagamentos para considerar sucesso
    #-----------------------------------------------------------
    
    # ler planilha com dados de acionamentos
    #df_acion <- read.xlsx2("./data/Acionamentos2.xlsx", sheetIndex = 1, header = TRUE)
    # FALTA MEMORIA: df_acion <- read.xlsx2("./data/Dados Raw-04-12-2015.xlsx", sheetIndex = "ACIONAMENTO", header = TRUE)
    # ATENÇÃO: salvar do excel em formato texto (txt) com UTF-16
    df_acion <- read.csv2("./data/Acionamentos2.csv", header = TRUE, sep = ",")
   
    # mudando nomes dos headers
    #colnames (df_acion) <- c("CONTRATO", "OCORRENCIA", "DATA.ACION", "SUCESSO", "COD.ACION", "TIPO.ACION")
    df_acion <-
        df_acion %>%
        rename(CONTRATO = REG_ASK,
               OCORRENCIA = DESCR_OCO,
               DATA.ACION = DTAND_AND,
               SUCESSO = SUCES_OCO,
               COD.ACION = ORIGE_AND,
               TIPO.ACION = Tipo_Acionamento)
    # tirando os caracteres não printáveis
    #lapply(df_acion$V2, sub,"<8d>","ç",df_acion$V2)
    #df_acion$V2 <- sub("\x8d","ç", df_acion$V2)
    #lapply(df$a, sub, "-","", df$a)
    
    # acertando a coluna data que no excel está numérica
    #df_acion <-
    #    df_acion %>%
    #    mutate (DATA.ACION = as.Date(as.numeric(paste(Data.Agendamento)), origin="1899-12-30") )   
    
    # transformando coluna DATA.ACION em formato data e criando colunas dia.semana e hora.acion
    #df_acion <-
    #    df_acion %>%
    #    mutate (DATA.ACION = ymd_hms(DATA.ACION),
    #            DIASEM.ACION = wday(DATA.ACION, label = TRUE),
    #            HORA.ACION = hour(DATA.ACION)) # ATENÇÃO: hora de acionamento não é muito confiável. sistema gera zerado!!
    
    #--------------------------------------
    # prepara dados para uso em previsão (testando para incobraveis)
    #--------------------------------------
    #df_uso <- f_prep_uso(df_acion, df_cliav_incobr)
    
    #-----------------------------------------------------------
    # obter dados de pgto
    #-----------------------------------------------------------
    # obs: campos de valor já vêem sem vírgula como separador de milhar
    # obs: cuidado para limpar colunas de totais do xlsx
    df_pg <- read.xlsx2("./data/Dados Raw-04-12-2015-Pgtos Avon.xlsx", sheetIndex = 1, header = TRUE)
    #df_pg <- read.csv("./data/Dados Raw-04-12-2015-PGTO.csv", header = TRUE)
    
    # acertando a coluna data que no excel está numérica
    df_pg <-
        df_pg %>%
        mutate(DTpgto = ymd(as.Date(as.numeric(paste(DTpgto)), origin="1899-12-30")) ) %>%
        rename(CONTRATO = CONTR_CON)

    # mudando nome da coluna Nosso.Número para Contrato e
    # inserindo hífen no número do contrato
    # obtendo somente as features: CPF..CGC, Contrato, Valor
    #df_pg <-
    #    df_pg %>%
    #    mutate(Contrato = Nosso.Número) %>%
    #    mutate(Contrato = paste0(substr(Nosso.Número, 1,5),"-", substr(Nosso.Número, 6,8))) %>%
    #    select(CPF, Contrato, Recebimento, Valor = Valor.Acordo) # ou usar Valor.Principal?
    
    # NÃO FUNCIONA NO dplyr! x <- df_pg %>% mutate (Valor = as.numeric(as.charcter(Valor)))
    # tem que fazer passo a passo ocmo abaixo
    df_pg <- df_pg %>% mutate (VlPag = as.character(VlPag)) # para acerto do merge com cliav
    df_pg <- df_pg %>% mutate (VlPag = as.numeric(VlPag)) # para acerto do merge com cliav

    # combina este dataframe com o de acionamentos para obter
    # acionamentos que obtiveram sucesso no pgto. pgto = S
    # merge com arquivo de pgtos para ver quais se 
    # encontram nos acionamentos, para trazer poder gerar target
    # pgto = S/N
    # escopo: merge com todas as ocorrências de df_acion, 
    # somente as ocorrencias de df_pg que aparecem em ambos e com duplicações mantidas
    #df_acion_pg <- left_join(df_acion, df_pg,by=c("Contrato"))
    
    # TEMP: selecionando pagamentos com Recebimento maior ou igual ao primeiro mês de 
    # acionamento considerado para os treinos. Para garantir causa/efeito
    
    # criar variavel target pago = S para os casos obtidos (obter somente estas linhas)
    #df_acion_pg <-
    #    df_acion_pg %>%
    #    mutate(pago = ifelse(!is.na(CPF), "S", CPF))
    # deixando as colunas como character
    #df_acion_pg <- df_acion_pg %>% mutate (CPF = as.character(CPF))

    # Aparentemente, o arquivo de clientes não tem aqueles que pagaram,
    # mas existem algumas duplicações (clientes em ambos os arquivos)
    #  que fazer nesta situação? A principio vou considerar pago, se aparece
    # em ambos
    #df_acion_pg_cli <- left_join(df_acion_pg, df_cliav,by=c("Contrato"))
    
    #df_acion_pg_cli <- 
    #    df_acion_pg_cli %>% 
    #    mutate (CPF.x = ifelse(!is.na(CPF.y),CPF.y, CPF.x), # mesclando CPFs
    #            pago = ifelse(is.na(pago),"N", pago)) %>% # marcando o que não é pago
    #    filter(!is.na(CPF.x)) # tirando as linhas sem informação em pago e clientes Avon

    # consolidando valores na coluna Valor.x
    # Se tem valor nas duas, mantém o Valor.x (negociado)
    #df_acion_pg_cli <- 
    #    df_acion_pg_cli %>% 
    #    mutate(Valor.x = ifelse(pago == "N", Valor.y,Valor.x))
    
    # eliminar registros que não tem informações de clientes
    #df_acion_pg_cli <-
    #    df_acion_pg_cli %>%
    #    filter(!is.na(CPF.y))
    
    # tirar os contratos duplicados e colunas não usadas
    #df_acion_pg_cli <- 
    ##    df_acion_pg_cli %>%
     #   distinct(Contrato)
    
    # elimina colunas desnecessárias
    #df_acion_pg_cli <- 
    #    df_acion_pg_cli %>%
    #    select(Valor = Valor.x, CPF = CPF.x,  everything()) %>%
    #    select(-Valor.y, -CPF.y)
    
    # probabilidade prior para acordos conseguidos por acionamento de toda a carteira
    #prop.table(table(df_acion_pg_cli$pago))
    # 3.2 % de pagamentos entre todos os acionamentos (sucesso ou não)
    
    # criar coluna de dia da semana e de hora do dia para
    # usar como feature
    # os dados na coluna Acionamento tem o formato DD/MM/AAA HH:MM
    #df_acion_pg_cli <-
    #    df_acion_pg_cli %>%
    #    mutate (Acionamento = dmy_hm(Acionamento),
    #            diasem.acion = wday(Acionamento, label = TRUE),
    #            hora.acion = hour(Acionamento))
    
    # COLOCAR AQUI Filtro para pegar Recebimento > Acionamento (datas)
    #x <-
    #    df_acion_pg_cli %>%
    #    filter (Recebimento > Acionamento)
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
    #df_acion_pg_cli$valGroups = cut(df_acion_pg_cli$Valor,breaks=quantile(df_acion_pg_cli$Valor))
    # eliminando a coluna de Valor
    #df_acion_pg_cli <- 
    #    df_acion_pg_cli %>%
    #    select (-Valor)

    # separar acionamentos em duas bases de acordo com ocorrência: 
    # acordo e não acordo, tirando os demais
    
    # lendo separadamente arquivos de cobráveis
    #df_cobr_tidy <- 
    #    df_acion_pg_cli %>%
    #    filter(Carteira == "Cobraveis")
    
    # lendo separadamente arquivos de incobráveis e Avon 3a.Fase
    #df_incobr_tidy <- 
    #    df_acion_pg_cli %>%
    #    filter(Carteira == "Incobraveis")
    
    # lendo separadamente arquivos de Avon 3a.Fase
    #df_3Fase_tidy <- 
    #    df_acion_pg_cli %>%
    #    filter(Carteira == "Avon 3a.Fase")
    
    # retorna lista com os data.frames para uso no treino e teste do modelo
    # separados para os tipos de carteira:
    # total de acionamantos cobráveis
    # total de acionamentos incobráveis
    # total de acionamentos Avon 3a.Fase
    
    # retorna lista com os 3 arquivos raw lidos
    
    l <- list(df_acion,df_carteira,df_pg)
    
    return (l) 
}