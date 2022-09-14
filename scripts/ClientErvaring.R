
# Clientervaring ----------------------------------------------------------

# Datum: 7 september 2022
# Auteur: Eva Rombouts
# Oefenproject in het kader van leren R en R Studio

# source ('scripts/LadenEnOpschonen.R')

# https://www.zorginzicht.nl/binaries/content/assets/zorginzicht/kwaliteitsinstrumenten/handboek-indicatoren-basisveiligheid-verslagjaar-2020.pdf

# Maak tabel --------------------------------------------------------------

clientervaring <- bv %>%
  left_join(select(indicatoren, thema_ID, indicator_ID),
            "indicator_ID") %>%
  filter (thema_ID %in% c(12:16)) %>% #Thema clientervaring
  mutate (ind = recode(indicator_ID,
                       `41` = "Perc8910",
                       `42` = "PercJa",
                       `43` = "TotaalScore",
                       `44` = "nBevraagd",
                       `45` = "oClientErvaring")) %>%
  select (-verslagjaar,
          -bron,
          -begindat,
          -einddat,
          -thema_ID,
          -indicator_ID) %>%
  pivot_wider(
    names_from = ind,
    values_from = c(iwaarde, teller, noemer, opmerking, invt)
  ) %>% 
  select (
    lokatie_ID, 
    iwaarde_Perc8910, 
    teller_Perc8910, 
    noemer_Perc8910, 
    iwaarde_PercJa, 
    teller_PercJa, 
    noemer_PercJa, 
    iwaarde_TotaalScore, 
    iwaarde_nBevraagd, 
    iwaarde_oClientErvaring, 
    opmerking_Perc8910, 
    opmerking_PercJa, 
    opmerking_TotaalScore, 
    opmerking_nBevraagd
  ) %>% 
  rename(
    Perc8910 = iwaarde_Perc8910, 
    PercJa = iwaarde_PercJa, 
    TotaalScore = iwaarde_TotaalScore, 
    nBevraagd = iwaarde_nBevraagd, 
    oClientErvaring = iwaarde_oClientErvaring, 
    tPerc8910 = teller_Perc8910, 
    tPercJa = teller_PercJa, 
    nPerc8910 = noemer_Perc8910, 
    nPercJa = noemer_PercJa, 
    oPerc8910 = opmerking_Perc8910, 
    oPercJa = opmerking_PercJa, 
    oTotaalScore = opmerking_TotaalScore, 
    onBevraagd = opmerking_nBevraagd
  ) %>%
  mutate(
    Perc8910 = as.numeric(Perc8910), 
    PercJa = as.numeric(PercJa), 
    TotaalScore = as.numeric(TotaalScore), 
    nBevraagd = as.numeric(nBevraagd)
  )

# Exploreer ---------------------------------------------------------------

# Eyeball gegevens:
skim (clientervaring)
View (clientervaring)

#' lokaties moesten kiezen tussen 
#' 1. NPS of aanbevelingsvraag (NB moet door een extern bureau worden gevraagd)
#' 2. Aanbevelingsvraag ZorgkaartNederland
#' 3. Totaalscore ZorgkaartNederland

clientervaring %>% 
  select (Perc8910, PercJa, TotaalScore) %>% 
  md.pattern()
#' --> 216 lokaties hebben geen score ingevuld (foei)
#' De andere scores zijn redelijk leuk verdeeld over de andere groepen
#' Geen enkele lokatie heeft meerdere scores ingevuld (moest ook niet)

# Kijken wat die 216 voor redenen opgeven...
aap <- clientervaring %>% 
  filter (
    is.na (Perc8910) &
    is.na (PercJa) &
    is.na (TotaalScore)
  )

# Analyseer ---------------------------------------------------------------
noot <- clientervaring %>% 
  filter(!is.na(Perc8910)) %>% 
  select(
    lokatie_ID,
    Perc8910,
    tPerc8910,
    nPerc8910,
    oClientErvaring,
    oPerc8910,
  ) %>% 
  ggplot(aes(Perc8910)) +
  geom_histogram(bins = 15) +
  theme_eva()
  
