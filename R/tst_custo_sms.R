# custos de sms
df_sms.2015.cel <-
    in.df_sms.2015 %>%
    group_by(Celular) %>%
    summarise(acions.dia = n())

x <-
    df_sms.2015.cel %>%
    group_by(acions.dia) %>%
    summarise(rep.nr.acion = sum(acions.dia))
total.sms <-  sum(x$rep.nr.acion)

x.sub <- subset(x, acions.dia > 8)
total.sms.maior8 <-  sum(x.sub$rep.nr.acion)
sprintf("Período avaliado: 01/07/2015 a 31/08/2015")
sprintf("Total de SMS enviados ao mesmo celular: %d, Custo: R$ %.2f", 
        total.sms, total.sms * 0.05)
sprintf("Total de SMS enviados mais de 8 vezes ao mesmo celular: %d, Custo: R$ %.2f", 
        total.sms.maior8, total.sms.maior8 * 0.05)
sprintf("Economia estimada -> %.2f %%", 
        (total.sms.maior8/total.sms)*100)
