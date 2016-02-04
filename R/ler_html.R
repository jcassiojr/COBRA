# teste de ler conteúdo do site Asserth
library(XML)

# Read and parse HTML file
doc.html = htmlTreeParse('http://www.grupoasserth.com.br/consultoria-empresarial/bpo/gestao-de-recebiveis.html',
                         useInternal = TRUE)

# Extract all the paragraphs (HTML tag is p, starting at
# the root of the document). Unlist flattens the list to
# create a character vector.
doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))

# Replace all \n by spaces
doc.text = gsub('\\n', ' ', doc.text)

# Join all the elements of the character vector into a single
# character string, separated by spaces
doc.text = paste(doc.text, collapse = ' ')

# outro modo de ler
thepage = readLines('http://www.grupoasserth.com.br/consultoria-empresarial/bpo/gestao-de-recebiveis.html')
grep('Assessoria',thepage)
# separando linhas de acordo com padrão
mypattern = '<td class="row-text">([^<]*)</td>'
datalines = grep(mypattern,thepage[536:length(thepage)],value=TRUE)

# ainda outro modo
# parse the document for R representation: 
mps <- "http://www.grupoasserth.com.br/consultoria-empresarial/bpo/gestao-de-recebiveis.html" 
mps.doc <- htmlParse(mps)
# get all the tables in mps.doc as data frames
mps.tabs <- readHTMLTable(mps.doc)
length(mps.tabs)
head(mps.tabs[[1]])  #and
tail(mps.tabs[[1]])  #for 1 to 7


# EXEMPLO DE ACESSAR PESQUISA NO YAHOO






# outro exemplo
require(RCurl)
require(XML)
webpage <- getURL("http://www.haaretz.com/")
webpage <- readLines(tc <- textConnection(webpage)); close(tc)
pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
# parse the tree by tables
x <- xpathSApply(pagetree, "//*/table", xmlValue)  
# do some clean up with regular expressions
x <- unlist(strsplit(x, "\n"))
x <- gsub("\t","",x)
x <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
x <- x[!(x %in% c("", "|"))]
x

# outro ainda
library(RCurl)
library(XML)

# Download page using RCurl
# You may need to set proxy details, etc.,  in the call to getURL
theurl <- "http://en.wikipedia.org/wiki/Brazil_national_football_team"
webpage <- getURL(theurl)
# Process escape characters
webpage <- readLines(tc <- textConnection(webpage)); close(tc)

# Parse the html tree, ignoring errors on the page
pagetree <- htmlTreeParse(webpage, error=function(...){})

# Navigate your way through the tree. It may be possible to do this more efficiently using getNodeSet
body <- pagetree$children$html$children$body 
divbodyContent <- body$children$div$children[[1]]$children$div$children[[4]]
tables <- divbodyContent$children[names(divbodyContent)=="table"]

#In this case, the required table is the only one with class "wikitable sortable"  
tableclasses <- sapply(tables, function(x) x$attributes["class"])
thetable  <- tables[which(tableclasses=="wikitable sortable")]$table

#Get columns headers
headers <- thetable$children[[1]]$children
columnnames <- unname(sapply(headers, function(x) x$children$text$value))

# Get rows from table
content <- c()
for(i in 2:length(thetable$children))
{
    tablerow <- thetable$children[[i]]$children
    opponent <- tablerow[[1]]$children[[2]]$children$text$value
    others <- unname(sapply(tablerow[-1], function(x) x$children$text$value)) 
    content <- rbind(content, c(opponent, others))
}

# Convert to data frame
colnames(content) <- columnnames
as.data.frame(content)

# OUTRO
library(RCurl)
library(XML)

theurl <- "http://en.wikipedia.org/wiki/Brazil_national_football_team"
webpage <- getURL(theurl)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)

pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)

# Extract table header and contents
tablehead <- xpathSApply(pagetree, "//*/table[@class='wikitable sortable']/tr/th", xmlValue)
results <- xpathSApply(pagetree, "//*/table[@class='wikitable sortable']/tr/td", xmlValue)

# Convert character vector to dataframe
content <- as.data.frame(matrix(results, ncol = 8, byrow = TRUE))

# Clean up the results
content[,1] <- gsub("Â ", "", content[,1])
tablehead <- gsub("Â ", "", tablehead)
names(content) <- tablehead

#-----------------------------
#-----------------------------
# LENDO DE PESQUISA YAHOO (ERRO!)
#-----------------------------
#-----------------------------

# load packages
library(RCurl)
library(XML)

get_yahoo_search_df <- function(u) {
    # I hacked my own version of xpathSApply to deal with cases that return NULL which were causing me problems
    xpathSNullApply <- function(doc, path.base, path, FUN, FUN2 = NULL) {
        nodes.len <- length(xpathSApply(doc, path.base))
        paths <- sapply(1:nodes.len, function(i) gsub( path.base, paste(path.base, "[", i, "]", sep = ""), path, fixed = TRUE))
        xx <- lapply(paths, function(xpath) xpathSApply(doc, xpath, FUN))
        if(!is.null(FUN2)) xx <- FUN2(xx)
        xx[sapply(xx, length)<1] <- NA
        xx <- as.vector(unlist(xx))
        return(xx)
    }
    
    # download html and parse into tree structure
    html <- getURL(u, followlocation = TRUE)
    doc <- htmlParse(html)
    
    # path to nodes of interest
    path.base <- "/html/body/div[@id='doc']/div[@id='bd-wrap']/div[@id='bd']/div[@id='results']/div[@id='cols']/div[@id='left']/div[@id='main']/div[@id='web']/ol/li"
    
    # construct data frame
    df <- data.frame(
        title = xpathSNullApply(doc, path.base, "/html/body/div[@id='doc']/div[@id='bd-wrap']/div[@id='bd']/div[@id='results']/div[@id='cols']/div[@id='left']/div[@id='main']/div[@id='web']/ol/li/div/div/h3/a", xmlValue),
        url = xpathSNullApply(doc, path.base, "/html/body/div[@id='doc']/div[@id='bd-wrap']/div[@id='bd']/div[@id='results']/div[@id='cols']/div[@id='left']/div[@id='main']/div[@id='web']/ol/li/div/div/h3/a[@href]", xmlAttrs, FUN2 = function(xx) sapply(xx, function(x) x[2])),
        description = xpathSNullApply(doc, path.base, "/html/body/div[@id='doc']/div[@id='bd-wrap']/div[@id='bd']/div[@id='results']/div[@id='cols']/div[@id='left']/div[@id='main']/div[@id='web']/ol/li/div/div", xmlValue),
        cached = xpathSNullApply(doc, path.base, "/html/body/div[@id='doc']/div[@id='bd-wrap']/div[@id='bd']/div[@id='results']/div[@id='cols']/div[@id='left']/div[@id='main']/div[@id='web']/ol/li/div/a[@href]", xmlAttrs, FUN2 = function(xx) sapply(xx, function(x) x[1])),
        recorded = xpathSNullApply(doc, path.base, "/html/body/div[@id='doc']/div[@id='bd-wrap']/div[@id='bd']/div[@id='results']/div[@id='cols']/div[@id='left']/div[@id='main']/div[@id='web']/ol/li/div/div/span[@id='resultTime']", xmlValue),
        stringsAsFactors = FALSE)
    
    # free doc from memory
    free(doc)
    
    # return data frame
    return(df)
}

u <- "http://uk.search.yahoo.com/search;_ylt=A7x9QV6rWrxOYTsAHNFLBQx.?fr2=time&rd=r1&fr=yfp-t-702&p=Wil%20Wheaton&btf=w"
df <- get_yahoo_search_df(u)
t(df[1:5, ])

# user provides url and the function extracts relevant information into a data frame as follows
u <- "http://uk.search.yahoo.com/search;_ylt=A7x9QV6rWrxOYTsAHNFLBQx.?fr2=time&rd=r1&fr=yfp-t-702&p=Wil%20Wheaton&btf=w"
df <- get_yahoo_search_df(u)
t(df[1, ])

#             1
# title       "Wil Wheaton - Google+"
# url         "https://plus.google.com/108176814619778619437"
# description "Wil Wheaton - Google+6 days ago"
# cached      "http://87.248.112.8/search/srpcache?ei=UTF-8&p=Wil+Wheaton&rd=r1&fr=yfp-t-702&u=http://cc.bingj.com/cache.aspx?q=Wil+Wheaton&d=4592664708059042&mkt=en-GB&setlang=en-GB&w=48d4b732,65b6306b&icp=1&.intl=uk&sig=6lwcOA8_4oGClQam_5I0cA--"
# recorded    "6 days ago"
#             1
# title       "Wil Wheaton - Google+"
# url         "https://plus.google.com/108176814619778619437"
# description "Wil Wheaton - Google+6 days ago"
# cached      "http://87.248.112.8/search/srpcache?ei=UTF-8&p=Wil+Wheaton&rd=r1&fr=yfp-t-702&u=http://cc.bingj.com/cache.aspx?q=Wil+Wheaton&d=4592664708059042&mkt=en-GB&setlang=en-GB&w=48d4b732,65b6306b&icp=1&.intl=uk&sig=6lwcOA8_4oGClQam_5I0cA--"
# recorded    "6 days ago"
#             2
# title       "WIL WHEATON DOT NET"
# url         "http://www.wilwheaton.net/coollinks.php"
# description "Wil Wheaton - Don't be a dick! - Writer and Actor - Your Mom - I'm Wil Wheaton. I'm an author (that's why I'm wilwheatonbooks), an actor, and a lifelong geek."
# cached      "http://87.248.112.8/search/srpcache?ei=UTF-8&p=Wil+Wheaton&rd=r1&fr=yfp-t-702&u=http://cc.bingj.com/cache.aspx?q=Wil+Wheaton&d=4592836504520824&mkt=en-GB&setlang=en-GB&w=eaeb9364,4a4e7c54&icp=1&.intl=uk&sig=VC7eV8GUMXVuu9apHagYNg--"
# recorded    "2 days ago"
#             3
# title       "this is one hell of a geeky weekend - WWdN: In Exile"
# url         "http://wilwheaton.typepad.com/wwdnbackup/2008/05/this-is-one-hel.html"
# description "WIL WHEATON DOT NET2 days ago"
# cached      "http://87.248.112.8/search/srpcache?ei=UTF-8&p=Wil+Wheaton&rd=r1&fr=yfp-t-702&u=http://cc.bingj.com/cache.aspx?q=Wil+Wheaton&d=4559391600545150&mkt=en-GB&setlang=en-GB&w=90d3ee39,34d4424b&icp=1&.intl=uk&sig=ZN.UpexVV4pm3yn7XiEURw--"
# recorded    "2 days ago"
#             4
# title       "Wil Wheaton - Google+ - I realized today that when someone ..."
# url         "https://plus.google.com/108176814619778619437/posts/ENTkBMZKeGY"
# description ">Cool Sites. Okay, I'm talking to the guys here: do you ever get \"the sigh\"? You know what I'm talking about...you're really into some cool website, and your ..."
# cached      "http://87.248.112.8/search/srpcache?ei=UTF-8&p=Wil+Wheaton&rd=r1&fr=yfp-t-702&u=http://cc.bingj.com/cache.aspx?q=Wil+Wheaton&d=4718764947541872&mkt=en-GB&setlang=en-GB&w=9bca6e9a,dba19826&icp=1&.intl=uk&sig=jGaKkuIFOINEBBfBwarrgg--"
# recorded    "6 days ago"
#             5
# title       "The Hot List: Dwight Slade, Back Fence PDX, Wil Wheaton vs ..."
# url         "http://www.oregonlive.com/movies/index.ssf/2011/11/the_hot_list_dwight_slade_back.html"
# description "this is one hell of a geeky weekend - WWdN: In Exile2 days ago"
# cached      "http://87.248.112.8/search/srpcache?ei=UTF-8&p=Wil+Wheaton&rd=r1&fr=yfp-t-702&u=http://cc.bingj.com/cache.aspx?q=Wil+Wheaton&d=414191857143&mkt=en-GB&setlang=en-GB&w=3081364,e585aa21&icp=1&.intl=uk&sig=KufdBZ_Thr1Mm8.SnjpMUQ--"
# recorded    "4 hours ago"


# TENTANDO ASSERTH NO YAHOO
thepage = readLines('http://uk.search.yahoo.com/search;_ylt=A7x9QV6rWrxOYTsAHNFLBQx.?fr2=time&rd=r1&fr=yfp-t-702&p=Grupo%20Asserth&btf=w')
grep('Asserth',thepage)
