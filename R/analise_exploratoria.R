# análise exploratória
require("caret")
require("ROCR")
require("doMC")
require("lubridate")
require("MASS")
require("dplyr")

registerDoMC(5) # parallel processing

source("~/Documents/MyGit/COBRA/R/f_leRawCobra_Avon_2.R")
# abaixo para ler todos os acionamentos Avon, por contrato, sem filtors adicionais

# lê dados para treino e teste de Acionamentos Janeiro 2015
ct_Jan15 <- "201501"
df_jan15 <- suppressWarnings(f_leRawCobra_Avon_2(ct_Jan15))

# Fevereiro 2015
ct_Feb15 <- "201502"
df_feb15 <- suppressWarnings(f_leRawCobra_Avon_2(ct_Feb15))

# Março 2015
ct_Mar15 <- "201503"
df_mar15 <- suppressWarnings(f_leRawCobra_Avon_2(ct_Mar15))

# Abril 2015
ct_Apr15 <- "201504"
df_apr15 <- suppressWarnings(f_leRawCobra_Avon_2(ct_Apr15))

df_acion_pg <- as.data.frame(rbind(df_jan15, df_feb15, df_mar15, df_apr15))

# criando colunas de quantiles para valor para ser usada como classificador
df_acion_pg$Faixa.Valores = cut(df_acion_pg$Valor,breaks=quantile(df_acion_pg$Valor), include.lowest = TRUE)
# eliminando a coluna de Valor
df_acion_pg <- 
    df_acion_pg %>%
    select (-Valor)
# prior probs das tabelas lidas
prop.table(table(df_acion_pg$pago))

#df_x <- as.data.frame(df_acion_pg)
#df_x <-
#    df_x %>%
#    mutate (pago = ifelse (pago == "S", 1,0))
#df_y <-
##    df_x %>%
#    group_by(Faixa.Valores) %>%
#    summarise(x = n())
hist(df_acion_pg$Hora.Acion, breaks=10, prob=T)
hist(df_acion_pg$Numero.Contatos, breaks=10, prob=T)
# sumarizar pgtos por Cidade, etc e plotar
df_y <-
    df_acion_pg %>%
    group_by(Cidade) %>%
    summarise(NPG_Cid = n()) %>%
    select (Cidade, NPG_Cid)
plot(df_y)
df_z <-
    df_acion_pg %>%
    group_by(Faixa.Valores) %>%
    summarise(NPG_FV = n()) %>%
    select (Faixa.Valores, NPG_FV)
plot(df_z)
df_w <-
    df_acion_pg %>%
    group_by(Diasem.Acion) %>%
    summarise(NPG_DS = n()) %>%
    select (Diasem.Acion, NPG_DS)
plot(df_w)
df_t <-
    df_acion_pg %>%
    group_by(Numero.Contatos) %>%
    summarise(NPG_NC = n()) %>%
    select (Numero.Contatos, NPG_NC)
plot(df_t)
df_v <-
    df_acion_pg %>%
    group_by(Hora.Acion) %>%
    summarise(NPG_HA = n()) %>%
    select (Hora.Acion, NPG_HA)
plot(df_v)
# correlações
#pairs(df_acion_pg[,c(4,10,11,12,13)], main = "Acionamentos Avon", 
#      pch=21, bg=c("red","green3")[unclass(df_acion_pg$pago)])
#pairs(df_x[,c(10,11,12,13,9)], main = "Acionamentos Avon")
#plot(CPF~Faixa.Valores, data=df_x)
#model <- lm(Hora.Acion~Numero.Contatos, data=df_x)
# Plot the regression line
#abline(model)
# Now learn the local linear model
#model2 <- lowess(df_x$Hora.Acion~df_x$Numero.Contatos)
#lines(model, col="red")


# teste 3D colorido (PARA DEPOIS FAZER COM PREVISÃO)
library(scatterplot3d)
# create column indicating point color
mtcars$pcolor[mtcars$cyl==4] <- "red"
mtcars$pcolor[mtcars$cyl==6] <- "blue"
mtcars$pcolor[mtcars$cyl==8] <- "darkgreen"
with(mtcars, {
    s3d <- scatterplot3d(disp, wt, mpg,
                         color=pcolor, pch=19,
                         # x y and z axis
                         # circle color indicates no. of cylinders
                         # lines to the horizontal plane
                         # scale y axis (reduce by 25%)
                         type="h", lty.hplot=2,
                         scale.y=.75,
                         main="3-D Scatterplot Example 4",
                         xlab="Displacement (cu. in.)",
                         ylab="Weight (lb/1000)",
                         zlab="Miles/(US) Gallon")
    s3d.coords <- s3d$xyz.convert(disp, wt, mpg)
    text(s3d.coords$x, s3d.coords$y,
         labels=row.names(mtcars),
         pos=4, cex=.5)
    # x and y coordinates
    # text to plot
    # shrink text 50% and place to right of points)
    # add the legend
    legend("topleft", inset=.05,
           bty="n", cex=.5,
           title="Number of Cylinders",
           c("4", "6", "8"), fill=c("red", "blue", "darkgreen"))
})

# aplicando a HG
library(scatterplot3d)
# create column indicating point color
my.prev.cor$pcolor[my.prev.cor$TIPOUSER=="FUNC PUBLICO"] <- "red"
my.prev.cor$pcolor[my.prev.cor$TIPOUSER=="AMBEV"] <- "blue"
my.prev.cor$pcolor[my.prev.cor$TIPOUSER=="KROTON"] <- "darkgreen"
                         
with(my.prev.cor, {
    s3d <- scatterplot3d(PC1, PC2, PC3,
                         color=pcolor, pch=19,
                         type="h", lty.hplot=2,
                         scale.y=.75,
                         main="3-D Scatterplot Example 4",
                         xlab="Displacement (cu. in.)",
                         ylab="Weight (lb/1000)",
                         zlab="Miles/(US) Gallon")
    s3d.coords <- s3d$xyz.convert(PC1, PC2, PC3)
    text(s3d.coords$x, s3d.coords$y,
         labels=row.names(my.prev.cor),
         pos=4, cex=.5)
    legend("topright", inset=.05,
           bty="n", cex=.5,
           title="Tipo do Usuário",
           c("FUNC PUBLICO", "AMBEV", "KROTON"), fill=c("red", "blue", "darkgreen"))
})

