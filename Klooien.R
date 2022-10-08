aap <- ps %>% 
  rows_update(
    tibble (organisatie = "V.o.f. Westersypen", wnMedew = 5),
    by = "organisatie"
    )


hist (ps$wVerzuimPerc,50)
aap <- ps$wVerzuimPerc / (1-ps$wVerzuimPerc)
hist(aap, 50)


ps %>% 
  filter (wVerzuimPerc < 10) %>% 
  mutate (logitVerzuim = wVerzuimPerc / (1-wVerzuimPerc)) %>% 
  ggplot (aes(logitVerzuim)) + geom_histogram(bins = 50)
