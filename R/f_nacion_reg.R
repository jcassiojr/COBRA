# funcao que correlaciona numero de acionamentos pos região de DDD
# input: string de DDD da regiao, numero maximo de acionamentos
# output: lista com df com lag, corr, nacion para cada numero de acionamento
#         objeto retorno da regressao linear para obter plot e summary com R-squared
#         objeto plot de corr.nacion com smooth para ver pico
#         data frame sumarizado de numero de sms por celular para a região
f_nacion_reg <- function(df_sms.2015.in, ddd.in, nacion.max.in) {
    # ddd.in <- "^551"
    # nacion.max.in <- 15
    df_sms.regiao <-
        df_sms.2015.in %>%
        filter(grepl(ddd.in,Celular))
    
    # numero de sms por celular
    df_sms.regiao.mn <-
        df_sms.regiao %>%
        group_by(Celular) %>%
        summarise(acions.cel = n())
    
    #boxplot(df_sms.regiao.mn$acions.cel)
    #hist(df_sms.regiao.mn$acions.cel)
    #summary(df_sms.regiao.mn)
    # loop para gerar lista de 1 a 15 acionamentos
    #max.nacion <- 15
    my.df_max_corr <- data.frame()
    
    for(i in 1:nacion.max.in) {
        my.list <- f_ccf_sms_pgto(df_sms.regiao, i)
        my.ccf <- ccf(my.list[[6]]$acions.dia ,my.list[[6]]$pgto.dia, 
                      na.action = na.pass, lag.max = 30, ylim = c(-0.1, 0.5), 
                      plot = FALSE)
        
        my.v_corr <- as.numeric(my.ccf$acf)
        my.v_lag <- as.numeric(my.ccf$lag)
        my.df_corr <- as.data.frame(cbind(lag = my.v_lag, corr = my.v_corr)) #data.frame com lags
        my.df_corr <-
            my.df_corr %>%
            filter(lag < 0)
        my.df_max_corr <- rbind(my.df_max_corr,my.df_corr [which.max(my.df_corr[,2]),])    
    }
    
    my.df_max_corr <-
        my.df_max_corr %>%
        mutate(n.acion = seq(1:dim(my.df_max_corr)[1]))
    
    # plot de maiores correlações por lag
    # FALTE DEFINIR O CONCEITO IMPORTANTE AQUI
    # é importante saber quais os nro acionamentos tem maior correlacao com pgto (plot2)
    # e quais lags tem maior correlaçao com pgto. Portanto, plot lag x corr
    # IMPORTANTE: DESCOBRI CORRELACAO ENTRE corr e n.acions!!! tem sentido (ver abaixo!!!)
    # a correlaçao mede o quanto o pagto está relacionado ao acionamento. Se a corr está correlacionada ao
    # nro de acionamentos, podemos confirmar que quanto mais mandamos sms conformado mais forte a
    # correlacao com pgto efetuado!!!!!
    my.lm.n.acion <- lm(my.df_max_corr$corr ~ my.df_max_corr$n.acion)
    #summary(my.lm.n.acion) # r-squared = 78% !!!!!
    #plot da correlação
    #plot(my.df_max_corr)
    #sprintf("R Squared: %f", my.lm.n.acion$qr)
    
    # do plot acima, a corr.lag mostra onde lag se concentra para maior correlação n.acion x prim. pgto
    
    # E LAG x CORR mostra clusterização entre -15 e -20 dias!!!!!
    # Elaborar acima na apresentação e no post˜˜˜
    
    pl_max_lag <- ggplot(my.df_max_corr, aes(lag, corr)) +
        geom_line() + 
        #geom_smooth() +
        xlab("lag") + ylab("correlação") + 
        ggtitle("Máximo de Pgtos - SMS confirmados-SP") 
    #ylim(c(min(my.df_max_corr$corr),max(my.df_max_corr$corr)))
    #heatmap.2(as.matrix(my.df_max_corr[,1:2]))
    # plot de maiores correlações x nro de acionamentos

    pl_max_acion <- ggplot(my.df_max_corr, aes(n.acion, corr)) + geom_line() + geom_smooth() +
        xlab("# acionamentos") + ylab("correlação") + 
        ggtitle("Máximo de Pgtos - SMS confirmados-SP") 
    
    #pushViewport(viewport(layout = grid.layout(1, 2)))
    #print(pl_max_lag, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    #print(pl_max_corr, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
    
    l.out <- list(plot.lag = pl_max_lag, plot.acion = pl_max_acion, 
                  df.max.corr = my.df_max_corr, reg.lin = my.lm.n.acion, nacion.cel = df_sms.regiao.mn)
    
    return(l.out)
}