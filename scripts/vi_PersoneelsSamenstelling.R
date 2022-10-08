# Long to wide
ps <- ps %>% 
  # Eerst zorgen we dat alle organisaties per jaar en per indicator maar 1x voorkomen
  # Haal ind op voor de kolomnaam
  filter (!(okvk %in% DoubleOrgs$okvk & verslagjaar %in% DoubleOrgs$verslagjaar)) %>% 
  left_join(
    select (indicatoren, icode, ind),
    by = "icode"
  ) %>% 
  # Haal organisatienaam op
  left_join(
    select (organisaties, okvk, organisatie),
    by = "okvk"
  ) %>% 
  # waardes (+ tellers en noemers) gaan naar de kolommen
  rename (
    w = iwaarde,
    t = teller,
    n = noemer
  ) %>% 
  pivot_wider(
    id_cols = c(verslagjaar, okvk, organisatie),
    names_from = ind, 
    values_from = c(w,t,n), 
    names_sep = "", 
  ) %>% 
  # Alle aantallen hebben geen teller of noemer
  select (!(starts_with("tn", F)|starts_with("nn", F))) %>% 
  mutate_at (c(4:64),as.numeric) 
  

write.csv(ps, "ps.csv")
