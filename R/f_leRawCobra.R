#'função de preparação dos dados tidy a partir dos dados raw
require("xlsx")
require("dplyr")
require("doMC")

f_leRawCobra <- function() {
    registerDoMC(5) # parallel processing
    # estratégia de manipulação
    # 1. agrupar por contrato para examinar os tipos de ocorrências
    #    ordenando por contrato/data e hora de acionamento
    # 3. eliminar duplicidades de contatos, deixando somente o último
    # 2. criar critério para identificar classificação binária para acordo
    #    na coluna Ocorrências, de acionamentos, d eforma a considerar somente
    # os casos onde teve sucesso ou não, eliminando contatos que não se 
    # encaixam em nenhum dos dois casos
    #  critério: considerar apenas ligações onde contato foi efetivo, com o titular.
    #  destes, considerar como acordo o que gerou algum tipo de acordo, como
    #  não acordo onde o cliente negou explicitamente e desprezar os que ainda
    #  estão em andamento, ou seja, não se encaixam em nenhum dos casos acima
    #    Ocorrências / status
    #    Acordo                           / acordo
    #    Acordo - Manutenção              / acordo
    #    Acordo - Realizado               / acordo
    #    Alega pagamento                  / desprezar ou acordo?
    #    Alteração Cadastral de Email     / desprezar
    #    Alteração Cadastral de Endereço  / desprezar
    #    Alteração Cadastral de Telefone  / desprezar
    #    Alteração de Forma de Pagamento  / desprezar
    #    Boleto Gerado                    / acordo
    #    Cliente Não Revende              / desprezar (qualidade da base)
    #    Desempregado                     / não acordo
    #    Falecido                         / desprezar
    #    Inclusão de Forma de Pagamento   / acordo
    #    Ligação Descartada               / deprezar 
    #    Mensagem da operadora            / desprezar (pendente)
    #    Não Atende                       / desprezar (pendente)
    #    Não Conhece o Cliente            / desprezar (qualidade da base) ou não acordo (mentira)
    #    Não Localizado                   / desprezar (qualidade da base)
    #    Nega-se a Pagar                  / não acordo
    #    Ocupado                          / desprezar (pendente)
    #    Promessa                         / acordo               
    #    Recado com Terceiros             / desprezar (pendente)
    #    Recado na Secretária             / desprezar (pendente)
    #    Retornar Ligação                 / desprezar (pendente)
    #    Sem Condições de Pagamento       / não acordo
    #    Tts - Não Atende                 / desprezar (pendente)
    # obs: quando ocorrência é boleto gerado, na coluna complemento é possível obter 
    # o valor da parcela e número de parcelas se necessário
    # obs2: dos dados de acionamento tenho o contrato que pode ser usado como chave para 
    # obter cpf e nome dos dados de clientes. e valores?
    
    # ler planilha com dados de acordos
    #df_acor <- read.xlsx2("./data/Acordos-cass.xls", sheetIndex = 1, header = TRUE)
    
    # passo 1: ler planilha com dados de clientes Avon. 
    # clientes ativos, sem acordo ativo, tipo: cobráveis, incobráveis e terceira fase
    # a ser usada para aplicar modelo preditivo
    # obs: salvar os dados da planilha original Avon em csv para preformance
    df_cliav_cobr <- read.csv("./data/Clientes Avon-cass-cobr.csv", header = TRUE)
    #df_cliav_cobr <-
    #    df_cliav_cobr %>%
    #    mutate (tipo.cobranca = "cobravel") # criando coluna para classificar tipo cobranca
    
    df_cliav_incobr <- read.csv("./data/Clientes Avon-cass-incobr.csv",header = TRUE)
    #df_cliav_incobr <-
    #    df_cliav_incobr %>%
    #    mutate (tipo.cobranca = "incobravel") # criando coluna para classificar tipo cobranca
    
    # concatenando incobraveis e cobraveis
    df_cliav <- rbind(df_cliav_cobr,df_cliav_incobr)
    # tirar duplicidade de contratos 
    df_cliav <- 
        df_cliav %>%
        distinct(Contrato)
    # ler planilha com dados de pagamentos 2015
    #df_pg_train <- read.xlsx2("./data/PGTO 2015-cass.xls", sheetIndex = 1, header = TRUE)
    
    # passo 2: ler planilha com dados de acionamentos
    df_acion <- read.xlsx2("./data/Acionamentos out e nov 2015-raw.xlsx", sheetIndex = 1, header = TRUE)
    
    #-----------------------------------------------------------
    # ABORDAGEM 1: obtem acordos conseguidos para toda a carteira
    #-----------------------------------------------------------
    df_acordo_cart <-
        df_acion %>%
        mutate (acordo = ifelse(Ocorrência %in% c("Acordo", "Acordo - Manutenção", "Acordo - Realizado",
                                                  "Boleto Gerado", "Inclusão de Forma de Pagamento",
                                                  "Alteração de Forma de Pagamento", "Promessa"), "S", "N"))
    
    # probabilidade prior para acordos conseguidos por acionamento de toda a carteira
    prop.table(table(df_acordo_cart$acordo))
    # 9.1 % de acordos entre todos os acionamentos (sucesso ou não)
    # tirar os contratos duplicados
    df_acordo_cart <- 
        df_acordo_cart %>%
        distinct(Contrato)
    # probabilidade prior para acordos conseguidos por contrato para toda a carteira 
    prop.table(table(df_acordo_cart$acordo))
    # 5.5 % de acordos entre todos os acionamentos agrupados por contrato (sucesso ou não)
    # tirar os contratos duplicados
    
    # merge com arquivo de clientes Avon para ver quais se 
    # encontram nos acionamentos, para trazer CPF e valor da dívida
    # escopo: merge com todas as ocorrências de df_acion_S_N, 
    # somente as ocorrencias de df_cliav que aparecem em ambos e com duplicações mantidas
    df_acion_cart_total <- merge(df_acordo_cart, df_cliav,by=c("Contrato"), all.x = TRUE)
    
    # criar coluna de dia da semana e de hora do dia para
    # usar como feature
    # os dados na coluna Acionamento tem o formato DD/MM/AAA HH:MM
    df_acion_cart_total <-
        df_acion_cart_total %>%
        mutate (Acionamento = dmy_hm(Acionamento),
                diasem.acion = wday(Acionamento, label = TRUE),
                hora.acion = hour(Acionamento))
                #hora.acion = substr(Acionamento, 12,16))
                
    # convertendo coluna valor em numérica
    # primeiro tirando a vírgula da coluna
    df_acion_cart_total <-
        df_acion_cart_total %>%
        mutate(Valor = as.numeric(gsub(",","", Valor)))
                
    
    # eliminar registros sem estas informações de acionamentos
    # se der muito poucos valores, pegar acionamentos anteriores
    df_acion_cart_tidy <-
        df_acion_cart_total %>%
        filter(!is.na(CGC...CPF))
    
    ## agrupar coluna Valor por quantile
    #restData$zipGroups = cut(restData$zipCode,breaks=quantile(restData$zipCode))
    df_acion_cart_tidy$valGroups = cut(df_acion_cart_tidy$Valor,breaks=quantile(df_acion_cart_tidy$Valor))
    # eliminando a coluna de Valor
    df_acion_cart_tidy <-df_acion_cart_tidy[,-12]
    # passo 3: separar acionamentos em duas bases de acordo com ocorrência: 
    # acordo e não acordo, tirando os demais
    
    #-----------------------------------------------------------
    # ABORDAGEM 2: obtem acordos conseguidos para acionamentos com sucesso
    #-----------------------------------------------------------
    
    df_acion_acordo_S <- 
        df_acion %>%
        filter (Ocorrência %in% c("Acordo", "Acordo - Manutenção", "Acordo - Realizado",
                                  "Boleto Gerado", "Inclusão de Forma de Pagamento",
                                  "Alteração de Forma de Pagamento", "promessa")) %>%
        mutate(acordo = "S")
    df_acion_acordo_N <- 
        df_acion %>%
        filter (Ocorrência %in% c("Desempregado", "Nega-se A Pagar", 
                                  "Sem Condições de Pagamento")) %>%                                    
        mutate(acordo = "N")
    
    # passo 4: concatenar os arquivos
    df_acion_S_N <- rbind(df_acion_acordo_S,df_acion_acordo_N)
    
    # passo 5: tirar duplicidade de contratos 
    df_acion_S_N <- 
        df_acion_S_N %>%
        distinct(Contrato)
    
    # obtem prior probabilities para somente contatos de sucesso
    prop.table(table(df_acion_S_N$acordo))
    # entre os contatos efetivamente realizados o percentual de acordo 
    # é de 85%
    
    # entre todos os trabalhados (mesmo sem contato) realizados o percentual de acordo 
    # é de X %
    
    # passo 6: merge com arquivo de clientes Avon para ver quais se 
    # encontram nos acionamentos, para trazer CPF e valor da dívida
    # escopo: merge com todas as ocorrências de df_acion_S_N, 
    # somente as ocorrencias de df_cliav que aparecem em ambos e com duplicações mantidas
    df_acion_total <- merge(df_acion_S_N, df_cliav,by=c("Contrato"), all.x = TRUE)
    
    # passo 6: falta criar coluna de dia da semana e de hora do dia para
    # usar como feature
    # os dados na coluna Acionamento tem o formato DD/MM/AAA HH:MM
    df_acion_total <-
        df_acion_total %>%
    mutate (Acionamento = dmy_hm(Acionamento),
            diasem.acion = wday(Acionamento, label = TRUE),
            hora.acion = substr(Acionamento, 12,16))
    
    # convertendo coluna valor em numérica
    # primeiro tirando a vírgula da coluna
    df_acion_total <-
        df_acion_total %>%
        mutate(Valor = as.numeric(gsub(",","", Valor)))
    
    # passo 7: eliminar registros sem estas informações de acionamentos
    # se der muito poucos valores, pegar acionamentos anteriores
    df_acion_suces_tidy <-
        df_acion_total %>%
        filter(!is.na(CGC...CPF))
    
    ## agrupar coluna Valor por quantile
    #restData$zipGroups = cut(restData$zipCode,breaks=quantile(restData$zipCode))
    df_acion_suces_tidy$valGroups = cut(df_acion_suces_tidy$Valor,breaks=quantile(df_acion_suces_tidy$Valor))
    # eliminando a coluna de Valor
    df_acion_suces_tidy <-df_acion_suces_tidy[,-12]
    
    l <- list(df_acion_cart_tidy,df_acion_suces_tidy)
    return (l) # retorna dados tidy para análise por carteira total e acionamento com sucesso
    # a partir daqui, gerar dados de tran e test para testar modelos
    
    # passo 7: criar coluna target = acordo: S/N
    #df_acion_acor <-
    #    df_acion_acor %>%
    #    mutate (Acordo = ifelse(is.na(Nome), "N", "S"))
    # eliminar colunas duplicadas de contrato em acionamentos
    #df_acion <- 
    #    df_acion %>%
    #    distinct(Contrato)
    # eliminar colunas duplicadas de contrato em acordos
    #df_acor <- 
    #    df_acor %>%
    #    distinct(Contrato)
    # merge dos dados de acionamento e acordos, usando mesmo acionamentos que não deram acordo
    # criar coluna acordo (s/n) e colocar 'n' naquelas que ficaram com contrato vazio
    # outra opção é criar a coluna acordo de acordo com ocorrência
    
    # eliminando linhas que não se encaixam em acordo e não acordo segundo
    # critério acima, para tornar independente da qualidade dos dados
    
    # opção 1: merge
    # escopo: merge com todas as ocorrências de df_acion, 
    # somente as ocorrencias de df_acor que aparecem em ambos e com duplicações mantidas
    # PAREI AQUI: concatenar cobr e incobr transformando em linha nos acionamentos dos
    # clientes Avon
    #df_acion_acor <- merge(df_acion, df_acor,by=c("Contrato"), all.x = TRUE)
    # criar coluna acordo
    #df_acion_acor <-
    #    df_acion_acor %>%
    #    mutate (Acordo = ifelse(is.na(Nome), "N", "S"))
    # verificaca quantos acordos e quantos não
    #table(df_acion_acor$Acordo)
    
    # opção 2: usando o critério de ocorrências 
    # na planilha, sem duplicaçoes sobraram 7329, dos quais 2289 são acordo
    # e 209 são não acordo
}