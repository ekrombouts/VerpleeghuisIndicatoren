library (tidyverse)

# Get data ----------------------------------------------------------------
load (file = "data/ps.Rda")
ps <- ps %>%
  mutate (
    # Verzuimfrequenties van 0 waar geen enkele andere info is gegeven vertrouw
    # ik niet
    VerzuimFreq = if_else(VerzuimFreq == 0 & is.na(VerzuimPerc),
                          NA_real_,
                          VerzuimFreq),
    # Verzuimpercentages van >60% kloppen niet
    VerzuimPerc = if_else(VerzuimPerc > 60,
                          NA_real_,
                          VerzuimPerc),
    # nFTEs klopt niet als veel groter dan aantal medewerkers
    nFTE = if_else(nFTE > 1.5 * nMedew,
                   NA_real_,
                   nFTE)
  )


# Organisatienamen --------------------------------------------------------
# Controleer of organisaties vaker dan 1x per jaar hebben gerapporteerd
ps %>%
  count (okvk, verslagjaar) %>%
  filter (n > 1)
# Nee

# Controleer of organisaties per kvk verschillende namen hebben
ps %>%
  group_by (okvk) %>%
  summarise (UniqueName = n_distinct(organisatie)) %>%
  filter (UniqueName > 1)
# Nee


# Beschrijving variabelen -------------------------------------------------
#| GemContr: Gemiddeld contract. Is aantal FTE / aantal medewerkers
#|
#| VerzuimPerc: Verzuimpercentage vlg definitie CBS / Vernet. De tellers en de
#| noemers waren echter zodanig verschillend ingevuld, dat ik deze twee
#| variabelen heb verwijderd.
