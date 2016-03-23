# identificacao de sms com celular da carteira Avon nova Incobraveis
require("doMC")
require("dplyr")

source("~/Documents/MyGit/COBRA/R/f_ccf_sms_pgto.R")
source("~/Documents/MyGit/COBRA/R/f_le_sms.R")
source("~/Documents/MyGit/COBRA/R/f_nacion_reg.R")

registerDoMC(5) # parallel processing
options(scipen=999) # removendo display de notação científica em R
# LE arquivos SMS
##########################################
df_sms.2015 <- f_le_sms()
# transformando em caracter o celular
df_sms.2015 <-
    df_sms.2015 %>%
    mutate(Celular = as.character(Celular))
# eliminando números esquisitos: terminados em 0000
df_sms.2015 <-
    df_sms.2015 %>%
    filter(!grepl("0000$",Celular))
#eliminando Enviado.Em com "-"
df_sms.2015 <-
    df_sms.2015 %>%
    filter(!(grepl("-", Enviado.em)))

df_cart.incobr <- read.csv("./data/Carteira Avon-274-Incobravel-SemDUPLI.csv", stringsAsFactors=FALSE, fileEncoding="latin1", header = TRUE)

# removendo NA
df_cart.incobr <- na.omit(df_cart.incobr)

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

# eliminando celulares nulos
df_cart.incobr <-
    df_cart.incobr %>%
    filter(cel != "NULL")
# padronizando formato do celular
df_cart.incobr <-
    df_cart.incobr %>%
    mutate(Celular = paste0("55", ddd.cel, cel)) %>%
    mutate (valor.pend = as.numeric(gsub(",","", valor.pend))) %>% # forçando Valor numerico
    select (-ddd.cel, -cel)

# cria data frame com sms identificados na carteira
df_sms.cart <- inner_join(df_cart.incobr,df_sms.2015, by = "Celular")

# conferir manualmente acima para ver se fez ok. Se sim, combinar, via contrato, com pgto
# para modelo preditivo!!!!!!

# FALTA: TESTAR COM ARQUIVO ABAIXO
# df_pg <- read.xlsx2("./data/Dados Raw-04-12-2015-Pgtos Avon.xlsx", sheetIndex = 1, header = TRUE)

df_pg <- read.csv("./data/Dados Raw-pgtos agencia 4c.csv", sep = ",", header = TRUE, 
                  stringsAsFactors = FALSE,na.strings = "NULL")
df_pg <- na.omit(df_pg)
# converting from chr to Date format
df_pg$DTpgto <- as.Date(df_pg$DTpgto, "%m/%d/%y")

# eliminando virgula de milhar para conversão abaixo não forçar NA neste casos
df_pg <-
    df_pg %>%
    mutate (VlPag = as.numeric(gsub(",","", VlPag))) %>% # forçando Valor numeric
    rename(contrato = CONTR_CON)

# IMPORTANTE ABAIXO: considerando somente o primeiro pgto de cada contrato/dia
# considerando somente o primiro pagamento de cada contrato
df_pg.primpg <- 
    df_pg %>%
    group_by(contrato) %>%
    filter(DTpgto == min(DTpgto))
# agrupa por dia
#df_pg.primpg_dia <- 
#    df_pg.primpg %>%
#    group_by(DTpgto) %>%
#    summarise(vlpg.dia = sum(VlPag),
#              pgto.dia = n())
# removendo arquivo já usados
rm(df_pg)
#rm(df_pg.primpg)

# incorpora ao dataframe cart o pgto
# cria data frame com sms identificados na carteira
df_sms.cart.pg <- inner_join(df_sms.cart,df_pg.primpg, by = "contrato")

# agora aplicar o modelo nos dados acima (1021 instâncias)
# 1. criando atributo pgto = S/N
# 2. aplicnaod funcao de treino do modelo e avaliando resultado


