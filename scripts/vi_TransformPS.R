# Long to wide
ps2 <- ps1 %>% 
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
  # Dit zijn allemaal numerieke waarden
  mutate_at (c(4:64),as.numeric) %>% 
  # Zorg dat de variabelnamen geen w-prefix hebben
  # De tellers en de noemers houden hun t en n prefix
  rename_with(.fn = str_remove, .cols = starts_with("w"), "^w") %>% 
  # Haal niet gebruikte variabelen eruit
  select (
    # Gemiddeld contract is te berekenen door nFTE / nMedew (gecheckt)
    -GemContr,
    -tGemContr,
    -nGemContr,
    # De teller en de noemer van het verzuimpercentage worden heel verschillend
    # ingevuld. eruit.
    -tVerzuimPerc, 
    -nVerzuimPerc,
    # Waarden die overeenkomen met nFTE
    -nPnil,
    -tFTEperCt,
    -nNiv1,
    -nNiv2,
    -nNiv3,
    -nNiv4,
    -nNiv5,
    -nNiv6,
    -nBdnst,
    -nOvPers,
    -nLling
  ) 
