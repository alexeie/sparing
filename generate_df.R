
rentetall = seq(0.03, 0.14, by = 0.01)
# rentetall = seq(5000, 10000, by= 50)    # Used for test to see if large numbers or long sequences affected performance. It didn't really affect it at all
maaneder_q = quote(set_aar*12)
sparing = 5000

#tictoc::tic()
result <-
    purrr::map_dfr(
        rentetall,
        ~ data.frame(
            mnd = 1:eval(maaneder_q),
            aar = round(1:eval(maaneder_q)/12,2),
            rente_pros = .
            # ,sparing_mnd = 1.01,
            # kum_sparing = 1.01,
            # renteInntekt_mnd = 1.01,
            # kum_renteInntekt = 1.01
            # sparing_tot = 1.01
            )
    )

result %<>% 
    group_by(rente_pros) %>% 
        mutate(sparing_mnd = sparing) %>% 
        mutate(kum_sparing = cumsum(sparing_mnd)) %>% 
        # mutate(renteInntekt_mnd = ifelse(row_number() == 1 , 
        #                                  0, 
        #                                  1+(rente_pros/12) * kum_sparing)) %>% 
        mutate(renteInntekt_mnd = 1+(rente_pros/12) * kum_sparing) %>% 
        mutate(kum_renteInntekt = round(cumsum(renteInntekt_mnd),0)) %>% 
        mutate(sparing_tot = kum_sparing + kum_renteInntekt)

#tictoc::toc()

library(ggplot2)

ggplot(result, aes(x=aar, y=sparing_tot))+
    geom_line(aes(y = sparing_tot, color = rente_pros))

library(scales)
ggplot(result, aes(x=aar, y=kum_renteInntekt))+
    geom_line(aes(color=as.factor(rente_pros)))+
    scale_y_continuous(labels = comma)+
    labs(color = "Rente")
