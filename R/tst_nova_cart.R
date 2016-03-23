# testes nova base carteira Avon 274
# obs: usando carteiras sem duplicidade de contrato
#df_cart.incobr <- read.csv("./data/Carteira Avon-274-Incobravel.csv", stringsAsFactors=FALSE, fileEncoding="latin1", header = TRUE)
df_cart.incobr <- read.csv("./data/Carteira Avon-274-Incobravel-SemDUPLI.csv", stringsAsFactors=FALSE, fileEncoding="latin1", header = TRUE)
#df_cart.cobr <- read.csv("./data/Carteira Avon-274-Cobravel.csv", stringsAsFactors=FALSE, fileEncoding="latin1", header = TRUE)
df_cart.cobr <- read.csv("./data/Carteira Avon-274-Cobravel-SemDUPLI.csv", stringsAsFactors=FALSE, fileEncoding="latin1", header = TRUE)

# removendo NA
df_cart.cobr <- na.omit(df_cart.cobr)
df_cart.incobr <- na.omit(df_cart.incobr)

# eliminando features desnecessárias
df_cart.cobr <- 
    df_cart.cobr %>%
    select(contrato = RegAsc, # contrato (sem hifen)
           setor = Setor, 
           idade = Idade,
           fx.idade = Faixa.de.Idade,
           cep = CEP,
           dt.abertcad = Dt.Estabelecimento.Abertura.Cadastro, # em formato int. Passar a character (talvez usar gsub para barras) e data 
           equipe.vendas = Equipe.de.Vendas, 
           nro.camplos = N.mero.de.Campanhas..LOS., # numero de campanhas que participou. 19/ano -> tempo de atividade
           fx.los = Faixa.de.LOS,
           dt.enviocobr = Data.de.Envio.para.Cobran.a,
           valor.pend = ValorPendencia
           ) %>%
    mutate(contrato = paste0(substr(as.character(contrato),1,5),"-", substr(as.character(contrato),6,8))) # inserindo hifen no numero do contrato

# acertando datas
x <-
    df_cart.cobr %>%
    mutate(dt.abertcad = strtrim(dt.abertcad,8)) %>%
    mutate(dt.abertcad = ifelse(nchar(dt.abertcad) == 7, paste0("0", dt.abertcad), dt.abertcad)) # primeiro força 8 caracteres

# OK acima. FALTA: transformar em formato data!!
y <-
    x %>%
    mutate(dt.abertcad = paste0(substr(dt.abertcad,1,2), substr(dt.abertcad,3,4), substr(dt.abertcad,5,8))) # primeiro força 8 caracteres
           

df_cart.incobr <- 
    df_cart.incobr %>%
    select(contrato = revendedora_id, # contrato (sem hifen)
           setor = tb_setor_id,
           cep = rev_cep,
           ddd.rec = rev_ddd_recados,
           ddd.res = rev_ddd_residencial,
           ddd.com = rev_ddd_comercial,
           ddd.cel = rev_ddd_celular,
           tel.rec = rev_telefone_recado,
           tel.res = rev_telefone_residencial,
           tel.com = rev_telefone_comercial,
           cel = rev_telefone_celular,
           cpf = rev_cpf,
           idade = Idade,
           fx.idade = Faixa.de.Idade,
           dt.abertcad = rev_dt_estabelecimento,
           los = Los,
           fx.los = Faixa.de.LOS,
           est.civil = Estado.Civil,
           email = rev_email,
           valor.pend = valor_pendencia,
           dt.envio = dt_envio
    ) %>%
    mutate(contrato = paste0(substr(as.character(contrato),1,5),"-", substr(as.character(contrato),6,8))) # inserindo hifen no numero do contrato

# ideias
# 1. tentar identificar telefones da carteira com os arquivos de SMS e TELEFONE (ao inves de qrq acion)
# 2. tentar identificar por contrato se pgto efetuado
# 3. com os valores acima rodar o modelo, acrescentando features novas (faixa de idade, cep, bairro, estado civil)
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