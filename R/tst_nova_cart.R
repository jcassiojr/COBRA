# testes nova base carteira Avon 274
df_cart.incobr <- read.csv("./data/Carteira Avon-274-Incobravel.csv", stringsAsFactors=FALSE, fileEncoding="latin1", header = TRUE)
df_cart.incobr.sd <- read.csv("./data/Carteira Avon-274-Incobravel-SemDUPLI.csv", stringsAsFactors=FALSE, fileEncoding="latin1", header = TRUE)
df_cart.cobr <- read.csv("./data/Carteira Avon-274-Cobravel.csv", stringsAsFactors=FALSE, fileEncoding="latin1", header = TRUE)
df_cart.cobr.sd <- read.csv("./data/Carteira Avon-274-Cobravel-SemDUPLI.csv", stringsAsFactors=FALSE, fileEncoding="latin1", header = TRUE)

# ideias
# 1. tentar identificar telefones da carteira com os arquivos de SMS e TELEFONE (ao inves de qrq acion)
# 2. tentar identificar por contrato se pgto efetuado
# 3. com os valores acima rodar o modelo, acrescentando features novas (faixa de idade, cep, bairro)
# obs. tb testar o modelo usando campo Dt de Envio para Cobranca (esclarecer que tipo de envio)  
#      ao inves de usar arquivos de acionamento (SMS e TELEFONE)

# 1
df_pg <- read.xlsx2("./data/Dados Raw-04-12-2015-Pgtos Avon.xlsx", sheetIndex = 1, header = TRUE)

# acertando a coluna data que no excel está numérica
df_pg <-
    df_pg %>%
    mutate(DTpgto = ymd(as.Date(as.numeric(paste(DTpgto)), origin="1899-12-30")) ) %>%
    rename(CONTRATO = CONTR_CON)
# pesquisando contrato em PG x CARTEIRA