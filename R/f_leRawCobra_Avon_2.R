#'função de preparação dos dados tidy a partir dos dados raw para clientes Avon
#' parâmetros:
#' mês do arquivo de acionamentos raw. Formato: AAAAMM
#' retorno: datafranmes lidos e preparados de incobráveis, cobráveis e 3a fase

# rodar o comando abaixo se tiver problema de memoria java no read.xlsx2()
options(java.parameters = "-Xmx2000m")
# features a usar de clientes Avon e Pgto: CPF, Valor/Valor.Acordo (de pgtos), Contrato

require("xlsx")
#require("data.table")
#require("dplyr")
#require("doMC")
#require("lubridate")
# esta funcao pega todos os acionamentso (sucessos ou não) e sem discriminar
# por incobráveis ou cobráveis
# também desconsidera cidades não informadas
f_leRawCobra_Avon_2 <- function(in_aaaamm) {
    # constantes
    vl_dívida_minimo = 1.0
    
    registerDoMC(5) # parallel processing
    
    #-----------------------------------------------------------
    # obter dados do cliente (para o caso de usar arquivo de Acordos ao invés de Pagto)
    #-----------------------------------------------------------
    df_cliav_cobr <- read.csv("./data/Clientes Avon-cass-cobr.csv", header = TRUE)
    # tirar duplicidade de contratos 
    df_cliav_cobr <- 
        df_cliav_cobr %>%
        distinct(Contrato)
    
    df_cliav_incobr <- read.csv("./data/Clientes Avon-cass-incobr.csv",header = TRUE)
    # tirar duplicidade de contratos (pode eliminar mais d euma dívida por contrato) 
    df_cliav_incobr <- 
        df_cliav_incobr %>%
        distinct(Contrato)
    
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
    
    # elimina clientes com valor abaixo do mínimo definido (R$ 1,00 ?)
    df_cliav <-
        df_cliav %>%
        filter (Valor >= vl_dívida_minimo)
    
    #-----------------------------------------------------------
    # obter acionamentos que geraram pagamentos para considerar sucesso
    #-----------------------------------------------------------
    
    # ler planilha com dados de acionamentosm de Janeiro 2015
    
    # COBRAVEIS
    #-----------------
    
    #df_acion_cobr <- read.xlsx2("./data/acion-jan15-raw.xlsx", sheetIndex = "COBR",  header = TRUE)
    df_acion_cobr <- read.xlsx2(paste0("./data/acion-", in_aaaamm, "-raw.xlsx"), sheetIndex = "COBR",  header = TRUE)
    # acertando a coluna data que no excel está numérica
    df_acion_cobr <-
        df_acion_cobr %>%
        mutate (Data.Agendamento = ymd(as.Date(as.numeric(paste(Data.Agendamento)), origin="1899-12-30")),
                Acionamento = dmy_hm(Acionamento))
    
    df_acion_cobr_sort <- 
        df_acion_cobr %>%
        arrange(Contrato,desc(Acionamento)) %>%
        mutate( pago = "N") # inicializa nova coluna pago como N
    
    # salvando numero de contatos com sucesso e eliminando repeticoes de contrato
    df_acion_cobr_sort <-
        df_acion_cobr_sort %>%
        group_by(Contrato) %>%
        mutate(Numero.Contatos = n()) %>%   # salvando qtde de contatos por contrato
        distinct(Contrato)              # eliminando contratos duplicados
        
    
    # seelcionar somente o que está pago e é cliente Avon para fechar o escopo e 
    # obter o valor da dívida
    # em outro momento ou se ficarem poucos dados para treinar o modelo
    # nõa usar cliente Avon e colocar no valor NA o ticket médio (R$ 150)
    df_acion_cobr_avon <- left_join(df_acion_cobr_sort, df_cliav,by=c("Contrato"))
    
    df_acion_cobr_avon <-
        df_acion_cobr_avon %>%
        filter(!is.na(CPF.y)) %>% # deixando somente acionamentos de clientes Avon
        select(CPF = CPF.x,  everything()) %>%
        select(-CPF.y)
    
    # INCOBRAVEIS
    #-----------------
    df_acion_incobr <- read.xlsx2(paste0("./data/acion-", in_aaaamm, "-raw.xlsx"), sheetIndex = "INCOBR",  header = TRUE)
    
    df_acion_incobr <-
        df_acion_incobr %>%
        mutate (Data.Agendamento = ymd(as.Date(as.numeric(paste(Data.Agendamento)), origin="1899-12-30")),
                Acionamento = dmy_hm(Acionamento))
    
    df_acion_incobr_sort <- 
        df_acion_incobr %>%
        arrange(Contrato, desc(Acionamento)) %>%
        mutate( pago = "N") # inicializa nova coluna pago como N
    
    # salvando numero de contatos com sucesso e eliminando repeticoes de contrato
    df_acion_incobr_sort <-
        df_acion_incobr_sort %>%
        group_by(Contrato) %>%
        mutate(Numero.Contatos = n()) %>%   # salvando qtde de contatos por contrato
        distinct(Contrato)              # eliminando contratos duplicados
       
    
    # selecionar somente o que está pago e é cliente Avon para fechar o escopo
    df_acion_incobr_avon <- left_join(df_acion_incobr_sort, df_cliav,by=c("Contrato"))
    
    df_acion_incobr_avon <-
        df_acion_incobr_avon %>%
        filter(!is.na(CPF.y)) %>% # deixando somente acionamentos de clientes Avon
        select(CPF = CPF.x,  everything()) %>%
        select(-CPF.y)
    
    # 3a FASE
    #-----------------
    df_acion_3fase <- read.xlsx2(paste0("./data/acion-", in_aaaamm, "-raw.xlsx"), sheetIndex = "3FASE",  header = TRUE)
    
    df_acion_3fase <-
        df_acion_3fase %>%
        mutate (Data.Agendamento = ymd(as.Date(as.numeric(paste(Data.Agendamento)), origin="1899-12-30")),
                Acionamento = dmy_hm(Acionamento))
    
    # ordena por Contrato e ordem de data de Recebimento
    # e cria coluna target inicializada com pago = N
    df_acion_3fase_sort <- 
        df_acion_3fase %>%
        arrange(Contrato, desc(Acionamento)) %>%
        mutate( pago = "N") # inicializa nova coluna pago como N
    
    # salvando numero de contatos com sucesso e eliminando repeticoes de contrato
    df_acion_3fase_sort <-
        df_acion_3fase_sort %>%
        group_by(Contrato) %>%
        mutate(Numero.Contatos = n()) %>%   # salvando qtde de contatos por contrato
        distinct(Contrato)              # eliminando contratos duplicados
       
    
    # selecionar somente o que está pago e é cliente Avon para fechar o escopo
    df_acion_3fase_avon <- left_join(df_acion_3fase_sort, df_cliav,by=c("Contrato"))
    
    df_acion_3fase_avon <-
        df_acion_3fase_avon %>%
        filter(!is.na(CPF.y)) %>% # deixando somente acionamentos de clientes Avon
        select(CPF = CPF.x,  everything()) %>%
        select(-CPF.y)
    
    
    # concatenando incobraveis e cobraveis
    df_acion_avon <- rbind(df_acion_incobr_avon,df_acion_cobr_avon, df_acion_3fase_avon)
    
    #--------------------------------------
    # prepara dados para uso em previsão (testando para incobraveis)
    #--------------------------------------
    #df_uso <- f_prep_uso(df_acion, df_cliav_incobr)
    
    #-----------------------------------------------------------
    # obter dados de pgto
    #-----------------------------------------------------------
    # obs: campos de valor já vêem sem vírgula como separador de milhar
    # obs: cuidado para limpar colunas de totais do xlsx
    df_pg <- read.xlsx2("./data/PGTO 2015-cass.xls", sheetIndex = 1, header = TRUE)
    # acertando a coluna data que no excel está numérica
    df_pg <-
        df_pg %>%
        mutate (Recebimento = ymd(as.Date(as.numeric(paste(Recebimento)), origin="1899-12-30")) )
    
    # mudando nome da coluna Nosso.Número para Contrato e
    # inserindo hífen no número do contrato
    # obtendo somente as features: CPF..CGC, Contrato, Valor
    df_pg <-
        df_pg %>%
        mutate(Contrato = Nosso.Número) %>%
        mutate(Contrato = paste0(substr(Nosso.Número, 1,5),"-", substr(Nosso.Número, 6,8))) %>%
        select(CPF, Contrato, Recebimento, Valor = Valor.Acordo) # ou usar Valor.Principal?
    
    # NÃO FUNCIONA NO dplyr! x <- df_pg %>% mutate (Valor = as.numeric(as.charcter(Valor)))
    # tem que fazer passo a passo ocmo abaixo
    df_pg <- df_pg %>% mutate (Valor = as.character(Valor)) # para acerto do merge com cliav
    df_pg <- df_pg %>% mutate (Valor = as.numeric(Valor)) # para acerto do merge com cliav
    df_pg <- df_pg %>% mutate (CPF = as.character(CPF)) # para acerto do merge com cliav
    
    # IMPORTANTE: o certo seria ler cada obs de PGTO do Recebimento mais recente para trás
    # procurando nos acionamentos concatenados de todo 2015 o Acionamento mais recente
    # e então marcar como pago = S
    
    # ordena por contrato e Recebimento, salvando o valor total da dívida paga
    # (somando as parcelas)
    df_pg_sort <-
        df_pg %>%
        arrange(Contrato,Recebimento) %>%
        group_by(Contrato) %>%
        mutate(Valor.Pago = sum(Valor))
    
    #seleciona somente a primeira data de recebimento de cada contrato 
    # (pois podem ser parcelas)
    df_pg_sort <- # ordena por ordem decrescente de data de Recebimento
        df_pg_sort %>%
        distinct(Contrato) %>%
        select(-Valor)
    
    # identificar quais acionamentos resultaram em pgto
    # combina este dataframe com o de acionamentos para obter
    # acionamentos que obtiveram sucesso no pgto. pgto = S
    # merge com arquivo de pgtos para ver quais se 
    # encontram nos acionamentos, para trazer poder gerar target
    # pgto = S/N
    # escopo: merge com todas as ocorrências de df_acion, 
    # somente as ocorrencias de df_pg que aparecem em ambos e com duplicações mantidas
    
    # COBRAVEIS
    #------------
    df_acion_pg <- left_join(df_acion_avon, df_pg_sort,by=c("Contrato"))
    
    df_acion_pg <-
        df_acion_pg %>%
        mutate(pago = ifelse(!is.na(Recebimento), "S", pago)) %>%
        select (CPF = CPF.x, everything()) %>%
        select (-CPF.y, -Recebimento) %>%
        mutate(Valor = ifelse(pago == "S", Valor.Pago,Valor)) %>% # se pago, considera valor pgto sobre devido
        select (-Valor.Pago) # elimina coluna antiga
    
    df_acion_pg <-
        df_acion_pg %>%
        mutate (Diasem.Acion = wday(Acionamento, label = TRUE),
                Hora.Acion = hour(Acionamento))
    
    # criando colunas de quantiles para valor para ser usada como classificador
    #df_acion_pg$Faixa.Valores = cut(df_acion_pg$Valor,breaks=quantile(df_acion_pg$Valor), include.lowest = TRUE)
    # eliminando a coluna de Valor
    #df_acion_pg <- 
    #    df_acion_pg %>%
    #    select (-Valor)
    
    # retorna  data.frame para uso no treino e teste do modelo
    
    return (df_acion_pg) 
}