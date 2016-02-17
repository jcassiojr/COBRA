# Chats XML
########################
## reading XML files
########################
library(XML)
#########################
## reading XML files
# FALTA: carregar reconhecendo cedilha e acentos (UTF-8?)
# Porque ler?
# identificar o cpf da pessoa (no quadro ou na transcrição), associar ao dia
# e pelo cpf identificar se pagou ou não (primeiro somente para dt pgto > chat, depois ir
# experimentando com 10 dias pos chat, 20, 30, etc)
# funcao que le cada xml, obtem cpf e email e grava em data frame com data de contato
# segundo passo: identificar palavras termos usados para quem pagou ao menos uma vez
# terceiro passo: identifica termos usados para quem pagou tudo
# quarto passo: identificar palavras chave que indiquem que vale a pena novo contato (ex. quero pagar, etc)
# quinto passo: obter e-mail e tentar encontrar na rede social
########################
#doc <- xmlParse("./data/CHAT/052015/2015-05-22.xml")

# EXEMPLO
#fileName <- system.file("exampleData", "mtcars.xml", package="XML") 
#doc <- xmlTreeParse(fileName)
#doc$doc$children$dataset
#xmlToDataFrame(
#    getNodeSet(doc, "//Message timestamp"),
#    colClasses=c("character")
#)
#fileUrl <- "http://www.w3schools.com/xml/simple.xml"
#fileXml <- "./data/CHAT/052015/2015-05-01.xml"
#fileXml <- "./data/CHAT/052015/2015-05-22.xml"
#chat.doc <- xmlTreeParse(fileXml, useInternal = TRUE)
#chat.rootNode <- xmlRoot(chat.doc)  # get the root node
#xmlName(chat.rootNode) # name of the file root node
#names(chat.rootNode) # names de inclosing nodes of root node

#xmlToDataFrame(nodes = xmlChildren(xmlRoot(chat.doc)[["SupportChat"]]))

## accessing parts of the XML documents
#chat.rootNode[[1]] # like the first element in a list object (Support Chat)
#chat.rootNode[[1]][[1]] # like the first element of the inclosing element in a list object (Support Chat/Timestamp)
#chat.rootNode[[1]][[2]] # like the second element of the inclosing element in a list object
#chat.rootNode[[1]][[3]] # like the third element of the inclosing element in a list object
#chat.rootNode[[1]][[3]][[1]] # like the first element of the third elemente of the inclosing element in a list object
#chat.rootNode[[1]][[3]][[2]] # like the first element of the inclosing element in a list object
#chat.rootNode[[1]][[3]][[3]] # like the first element of the inclosing element in a list object

## programactically extract parts of the file
#xmlSApply(chat.rootNode, xmlValue) # extrai os conteúdos de todos os subgrupos
#xmlSApply(chat.rootNode, xmlValue)[[1]] # extrai os conteúdos do primeiro subgrupo

## getting the itens of the menu and prices using XPath
#xpathSApply(chat.rootNode, "//CountryCode", xmlValue)
#xpathSApply(chat.rootNode, "//CountryName", xmlValue)
#xpathSApply(chat.rootNode, "//IP", xmlValue)
#xpathSApply(chat.rootNode, "//Visitor", xmlValue)

# lndo recursivamente os xmls
#my.dirs <- list.dirs("data/CHAT")
#my.chat.dirs <- list.dirs(my.dirs, recursive=FALSE)[-1]
#x <- list.dirs(my.chat.dirs, recursive=FALSE)
#r.scripts <- file.path(my.chat.dirs)


#l_df1 <- list()
#j <- 1
#for (k in my.chat.dirs){
#    l_df2 <- list()
#    i <- 1
    #print(k)
#    file.name.v <- list.files(my.chat.dirs[2], pattern="*.xml")
#    for (f in file.name.v) {
#        file.xml <- paste(my.chat.dirs[1],f, sep = "/")
#        chat.doc <- xmlTreeParse(file.xml, useInternal = TRUE)
#        chat.rootNode <- xmlRoot(chat.doc)
        # 2. criar data frame com coluna data
#        df_tmstp <- as.data.frame(xpathSApply(chat.rootNode, "//Timestamp", xmlValue))
        #print(paste(file.xml,dim(df_tmstp)[1]))
#        if (dim(df_tmstp)[1]) {
#            names(df_tmstp) <- c("Data")
#        
#            # 3. sumarizar por data quantidade de chats
#            df_tmstp$Data<- as.Date(df_tmstp$Data, "%Y-%m-%d")
#            df_tmstp_sum <-
#                df_tmstp %>%
#                group_by(Data) %>%
#                summarise(nchats = n())
#        
#            l_df2[i] <- as.data.frame(df_tmstp_sum)
#            i+1
##        }
 #       #print(file.xml)
 #   }
#    l_df1[j] <- l_df2
#    j <- j + 1
#}


# 1. obter a data do chat (ao menos alguém quis pagar, por isso tentou no chat) para todos os dias
f_le_xml <- function(file_xml) {
    #fileXml <- "./data/CHAT/052015/2015-05-22.xml"
    chat.doc <- xmlTreeParse(file_xml, useInternal = TRUE)
    chat.rootNode <- xmlRoot(chat.doc)  # get the root node
    # 2. criar data frame com coluna data
    df_tmstp <- as.data.frame(xpathSApply(chat.rootNode, "//Timestamp", xmlValue))
    if (dim(df_tmstp)[1]) {
        names(df_tmstp) <- c("Data")
        # 3. sumarizar por data quantidade de chats
        df_tmstp$Data<- as.Date(df_tmstp$Data, "%Y-%m-%d")
        df_tmstp_sum <-
            df_tmstp %>%
            group_by(Data) %>%
            summarise(nchats = n())
    } else {
        df_tmstp_sum <- data.frame(Data = substr(file_xml, 20,29), nchats = 0)
    }
    return(df_tmstp_sum) 
}

# le e concatena todos os arquivos (obs: 2015 05 somente tem dados de teste)
# 2015 06
df1 <- f_le_xml("./data/CHAT/062015/2015-06-01.xml")
df2 <- f_le_xml("./data/CHAT/062015/2015-06-02.xml")
df3 <- f_le_xml("./data/CHAT/062015/2015-06-03.xml")
df4 <- f_le_xml("./data/CHAT/062015/2015-06-04.xml")
df5 <- f_le_xml("./data/CHAT/062015/2015-06-05.xml")
df6 <- f_le_xml("./data/CHAT/062015/2015-06-06.xml")
df7 <- f_le_xml("./data/CHAT/062015/2015-06-07.xml")
df8 <- f_le_xml("./data/CHAT/062015/2015-06-08.xml")
df9 <- f_le_xml("./data/CHAT/062015/2015-06-09.xml")
df10 <- f_le_xml("./data/CHAT/062015/2015-06-10.xml")
df11 <- f_le_xml("./data/CHAT/062015/2015-06-11.xml")
df12 <- f_le_xml("./data/CHAT/062015/2015-06-12.xml")
df13 <- f_le_xml("./data/CHAT/062015/2015-06-13.xml")
df14 <- f_le_xml("./data/CHAT/062015/2015-06-14.xml")
df15 <- f_le_xml("./data/CHAT/062015/2015-06-15.xml")
df16 <- f_le_xml("./data/CHAT/062015/2015-06-16.xml")
df17 <- f_le_xml("./data/CHAT/062015/2015-06-17.xml")
df18 <- f_le_xml("./data/CHAT/062015/2015-06-18.xml")
df19 <- f_le_xml("./data/CHAT/062015/2015-06-19.xml")
df20 <- f_le_xml("./data/CHAT/062015/2015-06-20.xml")
df21 <- f_le_xml("./data/CHAT/062015/2015-06-21.xml")
df22 <- f_le_xml("./data/CHAT/062015/2015-06-22.xml")
df23 <- f_le_xml("./data/CHAT/062015/2015-06-23.xml")
df24 <- f_le_xml("./data/CHAT/062015/2015-06-24.xml")
df25 <- f_le_xml("./data/CHAT/062015/2015-06-25.xml")
df26 <- f_le_xml("./data/CHAT/062015/2015-06-26.xml")
df27 <- f_le_xml("./data/CHAT/062015/2015-06-27.xml")
df28 <- f_le_xml("./data/CHAT/062015/2015-06-28.xml")
df29 <- f_le_xml("./data/CHAT/062015/2015-06-29.xml")
df30 <- f_le_xml("./data/CHAT/062015/2015-06-30.xml")

df_chat.2015.06 <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10,
                          df11, df12, df13, df14, df15, df16, df17, df18, df19, df20,
                          df21, df22, df23, df24, df25, df26, df27, df28, df29, df30)
rm(list = c("df1", "df2", "df3", "df4", "df5", "df6", "df7", "df8", "df9", "df10",
       "df11", "df12", "df13", "df14","df15", "df16", "df17", "df18", "df19", "df20",
       "df21", "df22", "df23", "df24", "df25", "df26", "df27", "df28", "df29", "df30"))

# 2015 07
df1 <- f_le_xml("./data/CHAT/072015/2015-07-01.xml")
df2 <- f_le_xml("./data/CHAT/072015/2015-07-02.xml")
df3 <- f_le_xml("./data/CHAT/072015/2015-07-03.xml")
df4 <- f_le_xml("./data/CHAT/072015/2015-07-04.xml")
df5 <- f_le_xml("./data/CHAT/072015/2015-07-05.xml")
df6 <- f_le_xml("./data/CHAT/072015/2015-07-06.xml")
df7 <- f_le_xml("./data/CHAT/072015/2015-07-07.xml")
df8 <- f_le_xml("./data/CHAT/072015/2015-07-08.xml")
df9 <- f_le_xml("./data/CHAT/072015/2015-07-09.xml")
df10 <- f_le_xml("./data/CHAT/072015/2015-07-10.xml")
df11 <- f_le_xml("./data/CHAT/072015/2015-07-11.xml")
df12 <- f_le_xml("./data/CHAT/072015/2015-07-12.xml")
df13 <- f_le_xml("./data/CHAT/072015/2015-07-13.xml")
df14 <- f_le_xml("./data/CHAT/072015/2015-07-14.xml")
df15 <- f_le_xml("./data/CHAT/072015/2015-07-15.xml")
df16 <- f_le_xml("./data/CHAT/072015/2015-07-16.xml")
df17 <- f_le_xml("./data/CHAT/072015/2015-07-17.xml")
df18 <- f_le_xml("./data/CHAT/072015/2015-07-18.xml")
df19 <- f_le_xml("./data/CHAT/072015/2015-07-19.xml")
df20 <- f_le_xml("./data/CHAT/072015/2015-07-20.xml")
df21 <- f_le_xml("./data/CHAT/072015/2015-07-21.xml")
df22 <- f_le_xml("./data/CHAT/072015/2015-07-22.xml")
df23 <- f_le_xml("./data/CHAT/072015/2015-07-23.xml")
df24 <- f_le_xml("./data/CHAT/072015/2015-07-24.xml")
df25 <- f_le_xml("./data/CHAT/072015/2015-07-25.xml")
df26 <- f_le_xml("./data/CHAT/072015/2015-07-26.xml")
df27 <- f_le_xml("./data/CHAT/072015/2015-07-27.xml")
df28 <- f_le_xml("./data/CHAT/072015/2015-07-28.xml")
df29 <- f_le_xml("./data/CHAT/072015/2015-07-29.xml")
df30 <- f_le_xml("./data/CHAT/072015/2015-07-30.xml")
df31 <- f_le_xml("./data/CHAT/072015/2015-07-31.xml")


df_chat.2015.07 <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10,
                         df11, df12, df13, df14, df15, df16, df17, df18, df19, df20,
                         df21, df22, df23, df24, df25, df26, df27, df28, df29, df30, df31)
rm(list = c("df1", "df2", "df3", "df4", "df5", "df6", "df7", "df8", "df9", "df10",
            "df11", "df12", "df13", "df14","df15", "df16", "df17", "df18", "df19", "df20",
            "df21", "df22", "df23", "df24", "df25", "df26", "df27", "df28", "df29", "df30", "df31"))

# 2015 08
df1 <- f_le_xml("./data/CHAT/082015/2015-08-01.xml")
df2 <- f_le_xml("./data/CHAT/082015/2015-08-02.xml")
df3 <- f_le_xml("./data/CHAT/082015/2015-08-03.xml")
df4 <- f_le_xml("./data/CHAT/082015/2015-08-04.xml")
df5 <- f_le_xml("./data/CHAT/082015/2015-08-05.xml")
df6 <- f_le_xml("./data/CHAT/082015/2015-08-06.xml")
df7 <- f_le_xml("./data/CHAT/082015/2015-08-07.xml")
df8 <- f_le_xml("./data/CHAT/082015/2015-08-08.xml")
df9 <- f_le_xml("./data/CHAT/082015/2015-08-09.xml")
df10 <- f_le_xml("./data/CHAT/082015/2015-08-10.xml")
df11 <- f_le_xml("./data/CHAT/082015/2015-08-11.xml")
df12 <- f_le_xml("./data/CHAT/082015/2015-08-12.xml")
df13 <- f_le_xml("./data/CHAT/082015/2015-08-13.xml")
df14 <- f_le_xml("./data/CHAT/082015/2015-08-14.xml")
df15 <- f_le_xml("./data/CHAT/082015/2015-08-15.xml")
df16 <- f_le_xml("./data/CHAT/082015/2015-08-16.xml")
df17 <- f_le_xml("./data/CHAT/082015/2015-08-17.xml")
df18 <- f_le_xml("./data/CHAT/082015/2015-08-18.xml")
df19 <- f_le_xml("./data/CHAT/082015/2015-08-19.xml")
df20 <- f_le_xml("./data/CHAT/082015/2015-08-20.xml")
df21 <- f_le_xml("./data/CHAT/082015/2015-08-21.xml")
df22 <- f_le_xml("./data/CHAT/082015/2015-08-22.xml")
df23 <- f_le_xml("./data/CHAT/082015/2015-08-23.xml")
df24 <- f_le_xml("./data/CHAT/082015/2015-08-24.xml")
df25 <- f_le_xml("./data/CHAT/082015/2015-08-25.xml")
df26 <- f_le_xml("./data/CHAT/082015/2015-08-26.xml")
df27 <- f_le_xml("./data/CHAT/082015/2015-08-27.xml")
df28 <- f_le_xml("./data/CHAT/082015/2015-08-28.xml")
df29 <- f_le_xml("./data/CHAT/082015/2015-08-29.xml")
df30 <- f_le_xml("./data/CHAT/082015/2015-08-30.xml")
df31 <- f_le_xml("./data/CHAT/082015/2015-08-31.xml")

df_chat.2015.08 <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10,
                         df11, df12, df13, df14, df15, df16, df17, df18, df19, df20,
                         df21, df22, df23, df24, df25, df26, df27, df28, df29, df30, df31)
rm(list = c("df1", "df2", "df3", "df4", "df5", "df6", "df7", "df8", "df9", "df10",
            "df11", "df12", "df13", "df14","df15", "df16", "df17", "df18", "df19", "df20",
            "df21", "df22", "df23", "df24", "df25", "df26", "df27", "df28", "df29", "df30", "df31"))

# 2015 09
df1 <- f_le_xml("./data/CHAT/092015/2015-09-01.xml")
df2 <- f_le_xml("./data/CHAT/092015/2015-09-02.xml")
df3 <- f_le_xml("./data/CHAT/092015/2015-09-03.xml")
df4 <- f_le_xml("./data/CHAT/092015/2015-09-04.xml")
df5 <- f_le_xml("./data/CHAT/092015/2015-09-05.xml")
df6 <- f_le_xml("./data/CHAT/092015/2015-09-06.xml")
df7 <- f_le_xml("./data/CHAT/092015/2015-09-07.xml")
df8 <- f_le_xml("./data/CHAT/092015/2015-09-08.xml")
df9 <- f_le_xml("./data/CHAT/092015/2015-09-09.xml")
df10 <- f_le_xml("./data/CHAT/092015/2015-09-10.xml")
df11 <- f_le_xml("./data/CHAT/092015/2015-09-11.xml")
df12 <- f_le_xml("./data/CHAT/092015/2015-09-12.xml")
df13 <- f_le_xml("./data/CHAT/092015/2015-09-13.xml")
df14 <- f_le_xml("./data/CHAT/092015/2015-09-14.xml")
df15 <- f_le_xml("./data/CHAT/092015/2015-09-15.xml")
df16 <- f_le_xml("./data/CHAT/092015/2015-09-16.xml")
df17 <- f_le_xml("./data/CHAT/092015/2015-09-17.xml")
df18 <- f_le_xml("./data/CHAT/092015/2015-09-18.xml")
df19 <- f_le_xml("./data/CHAT/092015/2015-09-19.xml")
df20 <- f_le_xml("./data/CHAT/092015/2015-09-20.xml")
df21 <- f_le_xml("./data/CHAT/092015/2015-09-21.xml")
df22 <- f_le_xml("./data/CHAT/092015/2015-09-22.xml")
df23 <- f_le_xml("./data/CHAT/092015/2015-09-23.xml")
df24 <- f_le_xml("./data/CHAT/092015/2015-09-24.xml")
df25 <- f_le_xml("./data/CHAT/092015/2015-09-25.xml")
df26 <- f_le_xml("./data/CHAT/092015/2015-09-26.xml")
df27 <- f_le_xml("./data/CHAT/092015/2015-09-27.xml")
df28 <- f_le_xml("./data/CHAT/092015/2015-09-28.xml")
df29 <- f_le_xml("./data/CHAT/092015/2015-09-29.xml")
df30 <- f_le_xml("./data/CHAT/092015/2015-09-30.xml")

df_chat.2015.09 <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10,
                         df11, df12, df13, df14, df15, df16, df17, df18, df19, df20,
                         df21, df22, df23, df24, df25, df26, df27, df28, df29, df30)
rm(list = c("df1", "df2", "df3", "df4", "df5", "df6", "df7", "df8", "df9", "df10",
            "df11", "df12", "df13", "df14","df15", "df16", "df17", "df18", "df19", "df20",
            "df21", "df22", "df23", "df24", "df25", "df26", "df27", "df28", "df29", "df30"))

# 2015 10
df1 <- f_le_xml("./data/CHAT/102015/2015-10-01.xml")
df2 <- f_le_xml("./data/CHAT/102015/2015-10-02.xml")
df3 <- f_le_xml("./data/CHAT/102015/2015-10-03.xml")
df4 <- f_le_xml("./data/CHAT/102015/2015-10-04.xml")
df5 <- f_le_xml("./data/CHAT/102015/2015-10-05.xml")
df6 <- f_le_xml("./data/CHAT/102015/2015-10-06.xml")
df7 <- f_le_xml("./data/CHAT/102015/2015-10-07.xml")
df8 <- f_le_xml("./data/CHAT/102015/2015-10-08.xml")
df9 <- f_le_xml("./data/CHAT/102015/2015-10-09.xml")
df10 <- f_le_xml("./data/CHAT/102015/2015-10-10.xml")
df11 <- f_le_xml("./data/CHAT/102015/2015-10-11.xml")
df12 <- f_le_xml("./data/CHAT/102015/2015-10-12.xml")
df13 <- f_le_xml("./data/CHAT/102015/2015-10-13.xml")
df14 <- f_le_xml("./data/CHAT/102015/2015-10-14.xml")
df15 <- f_le_xml("./data/CHAT/102015/2015-10-15.xml")
df16 <- f_le_xml("./data/CHAT/102015/2015-10-16.xml")
df17 <- f_le_xml("./data/CHAT/102015/2015-10-17.xml")
df18 <- f_le_xml("./data/CHAT/102015/2015-10-18.xml")
df19 <- f_le_xml("./data/CHAT/102015/2015-10-19.xml")
df20 <- f_le_xml("./data/CHAT/102015/2015-10-20.xml")
df21 <- f_le_xml("./data/CHAT/102015/2015-10-21.xml")
df22 <- f_le_xml("./data/CHAT/102015/2015-10-22.xml")
df23 <- f_le_xml("./data/CHAT/102015/2015-10-23.xml")
df24 <- f_le_xml("./data/CHAT/102015/2015-10-24.xml")
df25 <- f_le_xml("./data/CHAT/102015/2015-10-25.xml")
df26 <- f_le_xml("./data/CHAT/102015/2015-10-26.xml")
df27 <- f_le_xml("./data/CHAT/102015/2015-10-27.xml")
df28 <- f_le_xml("./data/CHAT/102015/2015-10-28.xml")
df29 <- f_le_xml("./data/CHAT/102015/2015-10-29.xml")
df30 <- f_le_xml("./data/CHAT/102015/2015-10-30.xml")
df31 <- f_le_xml("./data/CHAT/102015/2015-10-31.xml")

df_chat.2015.10 <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10,
                         df11, df12, df13, df14, df15, df16, df17, df18, df19, df20,
                         df21, df22, df23, df24, df25, df26, df27, df28, df29, df30, df31)
rm(list = c("df1", "df2", "df3", "df4", "df5", "df6", "df7", "df8", "df9", "df10",
            "df11", "df12", "df13", "df14","df15", "df16", "df17", "df18", "df19", "df20",
            "df21", "df22", "df23", "df24", "df25", "df26", "df27", "df28", "df29", "df30", "df31"))

# 2015 11
df1 <- f_le_xml("./data/CHAT/112015/2015-11-01.xml")
df2 <- f_le_xml("./data/CHAT/112015/2015-11-02.xml")
df3 <- f_le_xml("./data/CHAT/112015/2015-11-03.xml")
df4 <- f_le_xml("./data/CHAT/112015/2015-11-04.xml")
df5 <- f_le_xml("./data/CHAT/112015/2015-11-05.xml")
df6 <- f_le_xml("./data/CHAT/112015/2015-11-06.xml")
df7 <- f_le_xml("./data/CHAT/112015/2015-11-07.xml")
df8 <- f_le_xml("./data/CHAT/112015/2015-11-08.xml")
df9 <- f_le_xml("./data/CHAT/112015/2015-11-09.xml")
df10 <- f_le_xml("./data/CHAT/112015/2015-11-10.xml")
df11 <- f_le_xml("./data/CHAT/112015/2015-11-11.xml")
df12 <- f_le_xml("./data/CHAT/112015/2015-11-12.xml")
df13 <- f_le_xml("./data/CHAT/112015/2015-11-13.xml")
df14 <- f_le_xml("./data/CHAT/112015/2015-11-14.xml")
df15 <- f_le_xml("./data/CHAT/112015/2015-11-15.xml")
df16 <- f_le_xml("./data/CHAT/112015/2015-11-16.xml")
df17 <- f_le_xml("./data/CHAT/112015/2015-11-17.xml")
df18 <- f_le_xml("./data/CHAT/112015/2015-11-18.xml")
df19 <- f_le_xml("./data/CHAT/112015/2015-11-19.xml")
df20 <- f_le_xml("./data/CHAT/112015/2015-11-20.xml")
df21 <- f_le_xml("./data/CHAT/112015/2015-11-21.xml")
df22 <- f_le_xml("./data/CHAT/112015/2015-11-22.xml")
df23 <- f_le_xml("./data/CHAT/112015/2015-11-23.xml")
df24 <- f_le_xml("./data/CHAT/112015/2015-11-24.xml")
df25 <- f_le_xml("./data/CHAT/112015/2015-11-25.xml")
df26 <- f_le_xml("./data/CHAT/112015/2015-11-26.xml")
df27 <- f_le_xml("./data/CHAT/112015/2015-11-27.xml")
df28 <- f_le_xml("./data/CHAT/112015/2015-11-28.xml")
df29 <- f_le_xml("./data/CHAT/112015/2015-11-29.xml")
df30 <- f_le_xml("./data/CHAT/112015/2015-11-30.xml")


df_chat.2015.11 <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10,
                         df11, df12, df13, df14, df15, df16, df17, df18, df19, df20,
                         df21, df22, df23, df24, df25, df26, df27, df28, df29, df30)
rm(list = c("df1", "df2", "df3", "df4", "df5", "df6", "df7", "df8", "df9", "df10",
            "df11", "df12", "df13", "df14","df15", "df16", "df17", "df18", "df19", "df20",
            "df21", "df22", "df23", "df24", "df25", "df26", "df27", "df28", "df29", "df30"))

# 2015 12
df1 <- f_le_xml("./data/CHAT/122015/2015-12-01.xml")
df2 <- f_le_xml("./data/CHAT/122015/2015-12-02.xml")
df3 <- f_le_xml("./data/CHAT/122015/2015-12-03.xml")
df4 <- f_le_xml("./data/CHAT/122015/2015-12-04.xml")
df5 <- f_le_xml("./data/CHAT/122015/2015-12-05.xml")
df6 <- f_le_xml("./data/CHAT/122015/2015-12-06.xml")
df7 <- f_le_xml("./data/CHAT/122015/2015-12-07.xml")
df8 <- f_le_xml("./data/CHAT/122015/2015-12-08.xml")
df9 <- f_le_xml("./data/CHAT/122015/2015-12-09.xml")
df10 <- f_le_xml("./data/CHAT/122015/2015-12-10.xml")
df11 <- f_le_xml("./data/CHAT/122015/2015-12-11.xml")
df12 <- f_le_xml("./data/CHAT/122015/2015-12-12.xml")
df13 <- f_le_xml("./data/CHAT/122015/2015-12-13.xml")
df14 <- f_le_xml("./data/CHAT/122015/2015-12-14.xml")
df15 <- f_le_xml("./data/CHAT/122015/2015-12-15.xml")
df16 <- f_le_xml("./data/CHAT/122015/2015-12-16.xml")
df17 <- f_le_xml("./data/CHAT/122015/2015-12-17.xml")
df18 <- f_le_xml("./data/CHAT/122015/2015-12-18.xml")
df19 <- f_le_xml("./data/CHAT/122015/2015-12-19.xml")
df20 <- f_le_xml("./data/CHAT/122015/2015-12-20.xml")
df21 <- f_le_xml("./data/CHAT/122015/2015-12-21.xml")
df22 <- f_le_xml("./data/CHAT/122015/2015-12-22.xml")
df23 <- f_le_xml("./data/CHAT/122015/2015-12-23.xml")
df24 <- f_le_xml("./data/CHAT/122015/2015-12-24.xml")
df25 <- f_le_xml("./data/CHAT/122015/2015-12-25.xml")
df26 <- f_le_xml("./data/CHAT/122015/2015-12-26.xml")
df27 <- f_le_xml("./data/CHAT/122015/2015-12-27.xml")
df28 <- f_le_xml("./data/CHAT/122015/2015-12-28.xml")
df29 <- f_le_xml("./data/CHAT/122015/2015-12-29.xml")
df30 <- f_le_xml("./data/CHAT/122015/2015-12-30.xml")
df31 <- f_le_xml("./data/CHAT/122015/2015-12-31.xml")

df_chat.2015.12 <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10,
                         df11, df12, df13, df14, df15, df16, df17, df18, df19, df20,
                         df21, df22, df23, df24, df25, df26, df27, df28, df29, df30, df31)
rm(list = c("df1", "df2", "df3", "df4", "df5", "df6", "df7", "df8", "df9", "df10",
            "df11", "df12", "df13", "df14","df15", "df16", "df17", "df18", "df19", "df20",
            "df21", "df22", "df23", "df24", "df25", "df26", "df27", "df28", "df29", "df30", "df31"))

# 2016 01
df1 <- f_le_xml("./data/CHAT/012016/2016-01-01.xml")
df2 <- f_le_xml("./data/CHAT/012016/2016-01-02.xml")
df3 <- f_le_xml("./data/CHAT/012016/2016-01-03.xml")
df4 <- f_le_xml("./data/CHAT/012016/2016-01-04.xml")
df5 <- f_le_xml("./data/CHAT/012016/2016-01-05.xml")
df6 <- f_le_xml("./data/CHAT/012016/2016-01-06.xml")
df7 <- f_le_xml("./data/CHAT/012016/2016-01-07.xml")
df8 <- f_le_xml("./data/CHAT/012016/2016-01-08.xml")
df9 <- f_le_xml("./data/CHAT/012016/2016-01-09.xml")
df10 <- f_le_xml("./data/CHAT/012016/2016-01-10.xml")
df11 <- f_le_xml("./data/CHAT/012016/2016-01-11.xml")
df12 <- f_le_xml("./data/CHAT/012016/2016-01-12.xml")
df13 <- f_le_xml("./data/CHAT/012016/2016-01-13.xml")
df14 <- f_le_xml("./data/CHAT/012016/2016-01-14.xml")
df15 <- f_le_xml("./data/CHAT/012016/2016-01-15.xml")
df16 <- f_le_xml("./data/CHAT/012016/2016-01-16.xml")
df17 <- f_le_xml("./data/CHAT/012016/2016-01-17.xml")
df18 <- f_le_xml("./data/CHAT/012016/2016-01-18.xml")
df19 <- f_le_xml("./data/CHAT/012016/2016-01-19.xml")
df20 <- f_le_xml("./data/CHAT/012016/2016-01-20.xml")
df21 <- f_le_xml("./data/CHAT/012016/2016-01-21.xml")
df22 <- f_le_xml("./data/CHAT/012016/2016-01-22.xml")
df23 <- f_le_xml("./data/CHAT/012016/2016-01-23.xml")
df24 <- f_le_xml("./data/CHAT/012016/2016-01-24.xml")
df25 <- f_le_xml("./data/CHAT/012016/2016-01-25.xml")
df26 <- f_le_xml("./data/CHAT/012016/2016-01-26.xml")
df27 <- f_le_xml("./data/CHAT/012016/2016-01-27.xml")
df28 <- f_le_xml("./data/CHAT/012016/2016-01-28.xml")
df29 <- f_le_xml("./data/CHAT/012016/2016-01-29.xml")
df30 <- f_le_xml("./data/CHAT/012016/2016-01-30.xml")
df31 <- f_le_xml("./data/CHAT/012016/2016-01-31.xml")

df_chat.2016.01 <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10,
                         df11, df12, df13, df14, df15, df16, df17, df18, df19, df20,
                         df21, df22, df23, df24, df25, df26, df27, df28, df29, df30, df31)
rm(list = c("df1", "df2", "df3", "df4", "df5", "df6", "df7", "df8", "df9", "df10",
            "df11", "df12", "df13", "df14","df15", "df16", "df17", "df18", "df19", "df20",
            "df21", "df22", "df23", "df24", "df25", "df26", "df27", "df28", "df29", "df30", "df31"))

# 2016 02
df1 <- f_le_xml("./data/CHAT/022016/2016-02-01.xml")
df2 <- f_le_xml("./data/CHAT/022016/2016-02-02.xml")
df3 <- f_le_xml("./data/CHAT/022016/2016-02-03.xml")
df4 <- f_le_xml("./data/CHAT/022016/2016-02-04.xml")
df5 <- f_le_xml("./data/CHAT/022016/2016-02-05.xml")
df6 <- f_le_xml("./data/CHAT/022016/2016-02-06.xml")
df7 <- f_le_xml("./data/CHAT/022016/2016-02-07.xml")
df8 <- f_le_xml("./data/CHAT/022016/2016-02-08.xml")
df9 <- f_le_xml("./data/CHAT/022016/2016-02-09.xml")
df10 <- f_le_xml("./data/CHAT/022016/2016-02-10.xml")
df11 <- f_le_xml("./data/CHAT/022016/2016-02-11.xml")
df12 <- f_le_xml("./data/CHAT/022016/2016-02-12.xml")
df13 <- f_le_xml("./data/CHAT/022016/2016-02-13.xml")
df14 <- f_le_xml("./data/CHAT/022016/2016-02-14.xml")
df15 <- f_le_xml("./data/CHAT/022016/2016-02-15.xml")
df16 <- f_le_xml("./data/CHAT/022016/2016-02-16.xml")
df17 <- f_le_xml("./data/CHAT/022016/2016-02-17.xml")
df18 <- f_le_xml("./data/CHAT/022016/2016-02-18.xml")
df19 <- f_le_xml("./data/CHAT/022016/2016-02-19.xml")
df20 <- f_le_xml("./data/CHAT/022016/2016-02-20.xml")
df21 <- f_le_xml("./data/CHAT/022016/2016-02-21.xml")
df22 <- f_le_xml("./data/CHAT/022016/2016-02-22.xml")
df23 <- f_le_xml("./data/CHAT/022016/2016-02-23.xml")
df24 <- f_le_xml("./data/CHAT/022016/2016-02-24.xml")
df25 <- f_le_xml("./data/CHAT/022016/2016-02-25.xml")
df26 <- f_le_xml("./data/CHAT/022016/2016-02-26.xml")
df27 <- f_le_xml("./data/CHAT/022016/2016-02-27.xml")
df28 <- f_le_xml("./data/CHAT/022016/2016-02-28.xml")
df29 <- f_le_xml("./data/CHAT/022016/2016-02-29.xml")

df_chat.2016.02 <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10,
                         df11, df12, df13, df14, df15, df16, df17, df18, df19, df20,
                         df21, df22, df23, df24, df25, df26, df27, df28, df29)
rm(list = c("df1", "df2", "df3", "df4", "df5", "df6", "df7", "df8", "df9", "df10",
            "df11", "df12", "df13", "df14","df15", "df16", "df17", "df18", "df19", "df20",
            "df21", "df22", "df23", "df24", "df25", "df26", "df27", "df28", "df29"))

# concatenar os dataframes (somente os de 2015)
df_chat.2015 <- bind_rows(list(df_chat.2015.06,
                               df_chat.2015.07,
                               df_chat.2015.08,
                               df_chat.2015.09,
                               df_chat.2015.10,
                               df_chat.2015.11,
                               df_chat.2015.12))

# 4. fazer plot time serie e correlacão com pgtos



# PAREI AQUI. PARA CIMA OK

xpathSApply(chat.rootNode, "//li[@id='1095865.Urefgfh9Px4TwQ']", xmlValue)
library(xml2)
baz3 <- xml_find_all(x, ".//Visitor") # obter conteudo de determinada tag
xml_attr(baz3, "id") # ver atributo de determinada tag

## extract content by attributes
fileUrl <- "http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
doc <- htmlTreeParse(fileUrl, useInternal = TRUE)
scores <- xpathSApply(doc, "//li[@class='score']", xmlValue)
teams <- xpathSApply(doc, "//li[@class='team-name']", xmlValue)
scores
teams
#ver slide do curso para pegar link "An outstanding guide to the XML guide"

doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
rootNode <- xmlRoot(doc)  # get the root node
xmlName(rootNode) # name of the file root node
names(rootNode) # names de inclosing nodes of root node
## accessing parts of the XML documents
rootNode[[1]] # like the first element in a list object
rootNode[[1]][[1]] # like the first element of the inclosing element in a list object
## programactically extract parts of the file
xmlSApply(rootNode, xmlValue)
## getting the itens of the menu and prices using XPath
xpathSApply(rootNode, "//name", xmlValue)
xpathSApply(rootNode, "//price", xmlValue)
xpathSApply(rootNode, "//li[@id='1095865.Urefgfh9Px4TwQ']", xmlValue)
## extract content by attributes
fileUrl <- "http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
doc <- htmlTreeParse(fileUrl, useInternal = TRUE)
scores <- xpathSApply(doc, "//li[@class='score']", xmlValue)
teams <- xpathSApply(doc, "//li[@class='team-name']", xmlValue)
scores
teams



# OUTRO PACOTE
#ver slide do curso para pegar link "An outstanding guide to the XML guide"
library(xml2)
x <- read_xml(fileXml)
xml_name(x) # obtem primeiro nivel do html
xml_children(x) # obtém todos os filhos hierarquicos
baz1 <- xml_find_all(x, ".//Timestamp")
baz2 <- xml_find_all(x, ".//Unread")
baz3 <- xml_find_all(x, ".//Visitor") # obter conteudo de determinada tag
xml_path(baz3) # ver caminho na hierarquia XMl de determinada tag
xml_attr(baz3, "id") # ver atributo de determinada tag
