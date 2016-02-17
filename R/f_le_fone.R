# le arquivos de acionamentos via telefone ativos e receptivos
f_le_fone <- function() {
    # TELEFONIA ATIVA E RECEPTIVA de 2015
    # janeiro
    df_tel.1.01.2015 <- read.csv2("./data/TELEFONIA/01 - Janeiro/01-10.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE, sep = ";")
    df_tel.2.01.2015 <- read.csv2("./data/TELEFONIA/01 - Janeiro/11-20.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE, sep = ";")
    df_tel.3.01.2015 <- read.csv2("./data/TELEFONIA/01 - Janeiro/21-31.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE, sep = ";")
    # fevereiro
    df_tel.1.02.2015 <- read.csv2("./data/TELEFONIA/02 - Fevereiro/01-20.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE, sep = ";")
    df_tel.2.02.2015 <- read.csv2("./data/TELEFONIA/02 - Fevereiro/21-28.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    #março
    df_tel.1.03.2015 <- read.csv2("./data/TELEFONIA/03 - Março/01-20.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.2.03.2015 <- read.csv2("./data/TELEFONIA/03 - Março/21-27.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.3.03.2015 <- read.csv2("./data/TELEFONIA/03 - Março/28-31.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    # abril
    df_tel.1.04.2015 <- read.csv2("./data/TELEFONIA/04 - Abril/01-08.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.2.04.2015 <- read.csv2("./data/TELEFONIA/04 - Abril/09-14.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.3.04.2015 <- read.csv2("./data/TELEFONIA/04 - Abril/15-22.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.4.04.2015 <- read.csv2("./data/TELEFONIA/04 - Abril/23-29.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.5.04.2015 <- read.csv2("./data/TELEFONIA/04 - Abril/30.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    # maio
    df_tel.1.05.2015 <- read.csv2("./data/TELEFONIA/05 - Maio/01-07.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.2.05.2015 <- read.csv2("./data/TELEFONIA/05 - Maio/08-14.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.3.05.2015 <- read.csv2("./data/TELEFONIA/05 - Maio/15-22.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.4.05.2015 <- read.csv2("./data/TELEFONIA/05 - Maio/23-31.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    # junho
    df_tel.1.06.2015 <- read.csv2("./data/TELEFONIA/06 - Junho/01-10.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.2.06.2015 <- read.csv2("./data/TELEFONIA/06 - Junho/11-18.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.3.06.2015 <- read.csv2("./data/TELEFONIA/06 - Junho/19-24.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.4.06.2015 <- read.csv2("./data/TELEFONIA/06 - Junho/25-30.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    # julho
    df_tel.1.07.2015 <- read.csv2("./data/TELEFONIA/07 - Julho/01-06.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.2.07.2015 <- read.csv2("./data/TELEFONIA/07 - Julho/07-12.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.3.07.2015 <- read.csv2("./data/TELEFONIA/07 - Julho/13-19.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.4.07.2015 <- read.csv2("./data/TELEFONIA/07 - Julho/20-27.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.5.07.2015 <- read.csv2("./data/TELEFONIA/07 - Julho/28-31.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    # agosto
    df_tel.1.08.2015 <- read.csv2("./data/TELEFONIA/08 - Agosto/01-20.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.2.08.2015 <- read.csv2("./data/TELEFONIA/08 - Agosto/21-31.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    # setembro
    df_tel.1.09.2015 <- read.csv2("./data/TELEFONIA/09 - Setembro/01-20.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.2.09.2015 <- read.csv2("./data/TELEFONIA/09 - Setembro/21-30.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    # outubro
    df_tel.1.10.2015 <- read.csv2("./data/TELEFONIA/10 - Outubro/01-20.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.2.10.2015 <- read.csv2("./data/TELEFONIA/10 - Outubro/21-31.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    # novembro
    df_tel.1.11.2015 <- read.csv2("./data/TELEFONIA/11 - Novembro/01-20.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    df_tel.2.11.2015 <- read.csv2("./data/TELEFONIA/11 - Novembro/21-30.csv", encoding="latin1",
                                  stringsAsFactors = FALSE, header = TRUE,  sep = ";")
    
    # concatenando todos os meses
    df_fone.out <- bind_rows(list(df_tel.1.01.2015,
                                  df_tel.2.01.2015,
                                  df_tel.3.01.2015,
                                  df_tel.1.02.2015,
                                  df_tel.2.02.2015,
                                  df_tel.1.03.2015,
                                  df_tel.2.03.2015,
                                  df_tel.3.03.2015,
                                  df_tel.1.04.2015,
                                  df_tel.2.04.2015,
                                  df_tel.3.04.2015,
                                  df_tel.4.04.2015,
                                  df_tel.5.04.2015,
                                  df_tel.1.05.2015,
                                  df_tel.2.05.2015,
                                  df_tel.3.05.2015,
                                  df_tel.4.05.2015,
                                  df_tel.1.06.2015,
                                  df_tel.2.06.2015,
                                  df_tel.3.06.2015,
                                  df_tel.4.06.2015,
                                  df_tel.1.07.2015,
                                  df_tel.2.07.2015,
                                  df_tel.3.07.2015,
                                  df_tel.4.07.2015,
                                  df_tel.5.07.2015,
                                  df_tel.1.08.2015,
                                  df_tel.2.08.2015,
                                  df_tel.1.09.2015,
                                  df_tel.2.09.2015,
                                  df_tel.1.10.2015,
                                  df_tel.2.10.2015,
                                  df_tel.1.11.2015,
                                  df_tel.2.11.2015
    ))
    
    rm(list = c("df_tel.1.01.2015",
                "df_tel.2.01.2015",
                "df_tel.3.01.2015",
                "df_tel.1.02.2015",
                "df_tel.2.02.2015",
                "df_tel.1.03.2015",
                "df_tel.2.03.2015",
                "df_tel.3.03.2015",
                "df_tel.1.04.2015",
                "df_tel.2.04.2015",
                "df_tel.3.04.2015",
                "df_tel.4.04.2015",
                "df_tel.5.04.2015",
                "df_tel.1.05.2015",
                "df_tel.2.05.2015",
                "df_tel.3.05.2015",
                "df_tel.4.05.2015",
                "df_tel.1.06.2015",
                "df_tel.2.06.2015",
                "df_tel.3.06.2015",
                "df_tel.4.06.2015",
                "df_tel.1.07.2015",
                "df_tel.2.07.2015",
                "df_tel.3.07.2015",
                "df_tel.4.07.2015",
                "df_tel.5.07.2015",
                "df_tel.1.08.2015",
                "df_tel.2.08.2015",
                "df_tel.1.09.2015",
                "df_tel.2.09.2015",
                "df_tel.1.10.2015",
                "df_tel.2.10.2015",
                "df_tel.1.11.2015",
                "df_tel.2.11.2015"))
    return(df_fone.out)
}