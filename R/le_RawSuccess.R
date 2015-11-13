# abordagem de considerar somente contatos com sucesso
# a partir do dataframe df_acion

#-----------------------------------------------------------
# ABORDAGEM 2: obtem acordos conseguidos para acionamentos com sucesso
#-----------------------------------------------------------
# passo 2: ler planilha com dados de acionamentos
df_acion <- read.xlsx2("./data/Acionamentos out e nov 2015-raw.xlsx", sheetIndex = 1, header = TRUE)

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
