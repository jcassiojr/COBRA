# le_xml
require("XML")
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
