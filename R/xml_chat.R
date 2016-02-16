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
        df_tmstp_sum <- data.frame(Data = "1999-01-01", nchats = 0)
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

df_chat.2015.05 <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10,
                          df11, df12, df13, df14, df15, df16, df17, df18, df19, df20,
                          df21, df22, df23, df24, df25, df26, df27, df28, df29, df30)
rm(list = c("df1", "df2", "df3", "df4", "df5", "df6", "df7", "df8", "df9", "df10",
       "df11", "df12", "df13", "df14","df15", "df16", "df17", "df18", "df19", "df20",
       "df21", "df22", "df23", "df24", "df25", "df26", "df27", "df28", "df29", "df30"))

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

df_chat.2015.05 <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10,
                         df11, df12, df13, df14, df15, df16, df17, df18, df19, df20,
                         df21, df22, df23, df24, df25, df26, df27, df28, df29, df30)
rm(list = c("df1", "df2", "df3", "df4", "df5", "df6", "df7", "df8", "df9", "df10",
            "df11", "df12", "df13", "df14","df15", "df16", "df17", "df18", "df19", "df20",
            "df21", "df22", "df23", "df24", "df25", "df26", "df27", "df28", "df29", "df30"))





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
