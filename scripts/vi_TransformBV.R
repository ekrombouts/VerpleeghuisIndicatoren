load(file="data/bv1.Rda")
library (tidyverse)
library (fedmatch)

# Long to wide
bv2 <- bv1 %>% 
  # Eerst zorgen we dat alle organisaties per jaar en per indicator maar 1x voorkomen
  # Haal ind op voor de kolomnaam
  filter (!(okvk %in% DoubleLocs$okvk & verslagjaar %in% DoubleLocs$verslagjaar)) %>% 
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
    id_cols = c(verslagjaar, okvk, locatie, lpostcode),
    names_from = ind, 
    values_from = c(w,t,n), 
    names_sep = "", 
  ) %>% 
  # Tekst, numeriek en ja/nee hebben geen teller of noemer
  select (!(starts_with("tjn", F) |
              starts_with("njn", F) |
              starts_with("tgek", F) |
              starts_with("ngek", F) |
              starts_with("tn", F) |
              starts_with("nn", F) |
              starts_with("ttx", F) |
              starts_with("ntx"))) %>% 
  # Kolommen met percentages (tellers en noemers zijn al goed)
  mutate (across(starts_with("wp"), as.numeric)) %>% 
  mutate (across(starts_with("wjn")|starts_with("wgek"), 
              ~ if_else(.x == "ja" | .x == "Ja", 
                       "TRUE", 
                       "FALSE"))) %>% 
  mutate (across(starts_with("wjn")|starts_with("wgek"), 
                as.logical)) %>% 
  # # Zorg dat de variabelnamen geen w-prefix hebben
  # # De tellers en de noemers houden hun t en n prefix
  rename_with(.fn = str_remove, .cols = starts_with("w"), "^w") 
