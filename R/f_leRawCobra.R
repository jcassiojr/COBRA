#'função de preparação dos dados tidy a partir dos dados raw
#'# estratégia de manipulação
# 1. obter o arquivo total de acionamentos no período desde 07/10/2014 até 11/12/2015
# 2. obter o arquivo de pgtos efetuados de 15/10/2014 a 01/12/2015
# 3. obter dados da carteira avon

f_leRawCobra <- function() {
    # constantes
    vl_dívida_minimo = 1.0
    
    # obter dados do cliente Avon
    ###############################################################
   
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
    
    # eliminando valores de JUROS igual a NA (nao tem valores de divida tb)
    df_carteira <- na.omit(df_carteira)
    
    # obtém valor devido total
    df_carteira <-
        df_carteira %>%
        mutate(VALOR.DEVIDO = VLSAL_CON + JUROS + VLORI_TRA)
    
    # elimina clientes com valor abaixo do mínimo definido (R$ 1,00)
    df_carteira <-
        df_carteira %>%
        filter (VALOR.DEVIDO >= vl_dívida_minimo)
    
    # eliminando colunas desnecessárias
    df_carteira <-
        df_carteira %>%
        select (-TITUL_TRA, -NOME, -VLSAL_CON, -JUROS, -VLORI_TRA)
    
    # obter acionamentos que geraram pagamentos para considerar sucesso
    ########################################################################
    
    # ler planilha com dados de acionamentos
    df_acion <- read.csv("./data/Acionamentos Avon-14-12-2015.rpt", skip = 2, sep = "|", header = TRUE)
    # elimina última linha de totalização
    df_acion <- na.omit(df_acion)
    
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
    
    
    # obter dados de pgto
    ################################################################
    # obs: campos de valor já vêem sem vírgula como separador de milhar
    # obs: cuidado para limpar colunas de totais do xlsx
    df_pg <- read.xlsx2("./data/Dados Raw-04-12-2015-Pgtos Avon.xlsx", sheetIndex = 1, header = TRUE)
    
    # acertando a coluna data que no excel está numérica
    df_pg <-
        df_pg %>%
        mutate(DTpgto = ymd(as.Date(as.numeric(paste(DTpgto)), origin="1899-12-30")) ) %>%
        rename(CONTRATO = CONTR_CON)

    # NÃO FUNCIONA NO dplyr! x <- df_pg %>% mutate (Valor = as.numeric(as.charcter(Valor)))
    # tem que fazer passo a passo como abaixo
    df_pg <- df_pg %>% mutate (VlPag = as.character(VlPag)) # para acerto do merge com cliav
    df_pg <- df_pg %>% mutate (VlPag = as.numeric(VlPag)) # para acerto do merge com cliav
    
    # retorna lista com os 3 arquivos raw lidos
    l <- list(df_acion,df_carteira,df_pg)
    
    return (l) 
}