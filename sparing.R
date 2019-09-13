set_aar = 100
input_mndsparing = 5000   # Maa ta input fra slider
maaneder_q = quote(set_aar*12)
# rente = 0.03       # Superflous?

calc_inntjening = function(aar = set_aar, 
                           mnd_sparing = input_mndsparing,      # Sorg for at denne oppdaterer seg ved input-oppdatering
                           maaneder = eval(maaneder_q),     # Test for at denne blir korrekt
                           rente) {
    result <- 
        data.frame(mnd = 1:eval(maaneder)) %>% 
            mutate(aar = round(mnd / 12, 3),
                   rente_pros = rente*100) %>% 
            mutate(sparing_mnd = mnd_sparing) %>% 
            mutate(kumSparing = cumsum(sparing_mnd)) %>% 
            mutate(renteInntekt_mnd = 1+(rente/12) * kumSparing) %>% 
            mutate(kum_renteInntekt = round(cumsum(renteInntekt_mnd),0),
                   sparing_tot = kumSparing + kum_renteInntekt) %>% 
            # filter(renteInntekt_mnd >= sparing_mnd) %>%                      # Mulig maal 1
            filter(sparing_tot >= 3000000) %>%                                # Mulig maal 2
            #filter(aar %in% c(1:35))                                           # Bor kanskje fjernes. Ikke nodvendig?
            head(1) 
    return(result)
}

# IDE: Inkorporer horisontal linje der målet er ((kan være egen kryss-av-boks for om man vil ha linje))
# IDE: Inkorporere highlight av ønsket rente

rentetall = seq(0.03, 0.2, by = 0.01)

datalist = list()

for (i in seq(rentetall)) {
    rente_seq = rentetall[i]
    data_holder <- calc_inntjening(rente = rente_seq)
    datalist[[i]] <- data_holder # add it to your list
}
df <- do.call(rbind, datalist)
tail(df)

ggplot(df, aes(x=aar, y=sparing_tot))+
    geom_line(aes(y = sparing_tot, color = rente_pros))

ggplot(df, aes(x=aar, y=kum_renteInntekt))+
    geom_point(aes(color=rente_pros))+
    geom_line(aes(x=aar, y=kumSparing))

