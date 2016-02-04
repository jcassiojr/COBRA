# Chats XML
########################
## reading XML files
########################
library(XML)
#########################
## reading XML files
# FALTA: carregar reconhecendo cedilha e acentos (UTF-8?)
# Porque ler?
# quantos contatos/dia (rodrigo já faz)
# identificar o cpf da pessoa (no quadro ou na transcrição), associar ao dia
# e pelo cpf identificar se pagou ou não (primeiro somente para dt pgto > chat, depois ir
# experimentando com 10 dias pos chat, 20, 30, etc)
# segundo passo: identificar palavras termos usados para quem pagou ao menos uma vez
# terceiro passo: identifica termos usados para quem pagou tudo
# quarto passo: identificar palavras chave que indiquem que vale a pena novo contato (ex. quero pagar, etc)
# quinto passo: obter e-mail e tentar encontrar na rede social
########################
library(XML)
#fileUrl <- "http://www.w3schools.com/xml/simple.xml"
#fileXml <- "./data/CHAT/052015/2015-05-01.xml"
fileXml <- "./data/CHAT/052015/2015-05-22.xml"

chat.doc <- xmlTreeParse(fileXml, useInternal = TRUE)
chat.rootNode <- xmlRoot(chat.doc)  # get the root node
xmlName(chat.rootNode) # name of the file root node
names(chat.rootNode) # names de inclosing nodes of root node
## accessing parts of the XML documents
chat.rootNode[[1]] # like the first element in a list object (Support Chat)
chat.rootNode[[1]][[1]] # like the first element of the inclosing element in a list object (Support Chat/Timestamp)
chat.rootNode[[1]][[2]] # like the second element of the inclosing element in a list object
chat.rootNode[[1]][[3]] # like the third element of the inclosing element in a list object
chat.rootNode[[1]][[3]][[1]] # like the first element of the third elemente of the inclosing element in a list object
chat.rootNode[[1]][[3]][[2]] # like the first element of the inclosing element in a list object
chat.rootNode[[1]][[3]][[3]] # like the first element of the inclosing element in a list object

## programactically extract parts of the file
xmlSApply(chat.rootNode, xmlValue) # extrai os conteúdos de todos os subgrupos
xmlSApply(chat.rootNode, xmlValue)[[1]] # extrai os conteúdos do primeiro subgrupo

## getting the itens of the menu and prices using XPath
xpathSApply(chat.rootNode, "//CountryCode", xmlValue)
xpathSApply(chat.rootNode, "//CountryName", xmlValue)
xpathSApply(chat.rootNode, "//IP", xmlValue)
xpathSApply(chat.rootNode, "//Visitor", xmlValue)
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
