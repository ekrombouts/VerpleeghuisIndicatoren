# Verpleeghuisindicatoren -----------------------------------------------------
# Datum: Augustus 2022
# Auteur: Eva Rombouts
# Oefenproject in het kader van leren R en R Studio
# Voornaamste gebruikte bronnen voor het leren van R: 
# Coursera cursus Johns Hopkins, R Ladies Sidney en R Programming 101
# Vanaf sep 2022 EQI postacademische opleiding Data Science & Business Analytics

# Bron data: https://www.zorginzicht.nl/openbare-data/open-data-verpleeghuiszorg  
# Dit betreft de kwaliteitsindicatoren die zorginstellingen jaarlijks moeten 
# verzamelen en opsturen. Het doel van deze actie is niet zozeer om te 
# vergelijken of een organisatie "het wel goed doet", maar eerder als een 
# middel om te kunnen reflecteren. 
# Data van 2020 genomen (inmiddels zijn data van 2021 bekend, dit volgt...)

# Projectvraag: Presenteer deze gegevens op een overzichtelijke manier. 
# Waar mogelijk, geef de mogelijkheid om zorgorganisatie A te vergelijken met 
# andere - vergelijkbare - zorgorganisaties. 

# Packages ----------------------------------------------------------------

library (tidyverse) 
library (here)
library (skimr)
library (janitor)
library (readxl)
library (lubridate)
library (mice)
library (todor)
library (clipr)
# library(dm)

# Data laden --------------------------------------------------------------
# https://www.zorginzicht.nl/openbare-data/open-data-verpleeghuiszorg#verslagjaar-2020
df <- 
  read_excel(here("data", 
                  "openbaar-databestand-verpleeghuiszorg-verslagjaar-2020.xlsx"),
             sheet = "verpleeghuiszorg VJ2020")

# https://www.cbs.nl/nl-nl/longread/diversen/2021/statistische-gegevens-per-vierkant-en-postcode-2020-2019-2018?onepage=true#c-4--Beschrijving-cijfers
# tbv stedelijkheid.
cbs_postcode <- 
  read_excel(here("data",
                  "cbs_pc4_2020_v1.xlsx"),
             sheet = "PC4_2020_v1", 
             na = "-99997", # Deze data is met opzet door CBS verborgen gehouden
             # ivm privacy
             skip = 8)
# Eerste rij bevat uitleg, eruit
cbs_postcode <- cbs_postcode[-1, ]

# Opschonen ---------------------------------------------------------------

df <- df %>%
  # Aanpassen kolomnamen
  clean_names() %>% # Uit de janitor package
  rename(indicatorset = indicatorset_naam,
         organisatie = organisatie_naam,
         okvk = kvk_nummer,
         oagb = organisatie_agb_code,
         locatie = locatie_naam,
         lvestigingsnummer = vestigingsnummer,
         lpostcode = locatie_postcode,
         lhuisnummer = locatie_huisnummer,
         lplaats = locatie_plaats,
         lagb = locatie_ag_bcode,
         icode = indicator_code,
         isorteer = indicator_sorteernummer,
         inummer = indicator_nummer,
         inaam = indicator_naam,
         ieenheid = indicator_eenheid,
         iwaarde = indicator_waarde,
         invt = indicator_nvt,
         itype = indicator_type,
         begindat = meetperiode_begin_datum,
         einddat = meetperiode_eind_datum) %>%
  # Aanpassen datatypes
  mutate(begindat = as_date(begindat),
         einddat = as_date(einddat),
         verslagjaar = as.Date(ISOdate(verslagjaar,12,31)),
         indicatorset = factor(case_when(
           indicatorset == 'Verpleeghuiszorg Basisveiligheid' ~
             "Basisveiligheid", 
           indicatorset == 'Verpleeghuiszorg Personeelssamenstelling' ~ 
             "Personeelssamenstelling")),
         indicatorset_code = as.factor(indicatorset_code),
         type_zorgaanbieder = as.factor(type_zorgaanbieder),
         # thema = as.factor(thema)   Tegen besloten
         ieenheid = as.factor(ieenheid),
         invt = as.logical(invt), 
         itype = as.factor(itype),
         # Sommige variabelen moeten juist wel als tekst
         bron = as.character(bron),
         okvk = as.character(okvk),
         oagb = as.character(oagb),
         lvestigingsnummer = as.character(lvestigingsnummer),
         lagb = as.character(lagb)
  ) 

# DIM en Fact tabellen ----------------------------------------------------
## Organisaties ------------------------------------------------------------
organisaties <- df %>%
  distinct (
    organisatie,
    okvk,
    oagb, 
    type_zorgaanbieder
  ) %>%
  rowid_to_column("organisatie_ID")
# organisatie is uniek gedefinieerd door zijn okvk
# organisaties %>% count (okvk) %>% filter (n>1)

## Lokaties ----------------------------------------------------------------
# De variabele locaties in de df is rommelig. 
# Door de platte structuur van de tabel, waarin indicatoren zijn opgenomen
# op zowel locatieniveau als organisatieniveau is het soms niet helemaal 
# duidelijk wat een daadwerkelijke locatie is. Voor de indcatorset 
# Personeelssamenstelling zijn namelijk dummie locaties aangemaakt, meestal 
# (maar niet altijd) aangeduid met "Concernniveau: "

# Uiteindelijk besloten om zowel lokaties als organisaties in de tabel te houden.
# te differentieren met de variabele IsOrg
lokaties <- df %>%
  #filter (indicatorset == "Basisveiligheid") %>% 
  mutate (IsOrg = (indicatorset == "Personeelssamenstelling")) %>% 
  distinct(
    locatie, lvestigingsnummer, lpostcode, lhuisnummer, 
    lplaats, lagb, okvk, IsOrg
  ) %>%
  rowid_to_column("lokatie_ID") %>%
  # Maak foreign key naar organisaties
  left_join(
    select (organisaties, organisatie_ID, okvk),
    by = "okvk"
  ) %>%
  select (-okvk) 
# lokatie is uniek gedefinieerd door locatie, lvestigingsnummer, lagb
# lokaties %>% count (locatie, lvestigingsnummer, lagb) %>% filter (n>1) 

## Indicatorsets -----------------------------------------------------------
indicatorsets <- df %>%
  distinct(
    indicatorset_code, 
    indicatorset
  ) %>%
  rowid_to_column("indicatorset_ID")

## Themas ------------------------------------------------------------------
themas <- df %>%
  distinct(thema, indicatorset_code) %>%
  rowid_to_column("thema_ID") %>%
  # Maak foreign key naar indicatorsets
  left_join(
    select(indicatorsets, indicatorset_ID, indicatorset_code),
    by = "indicatorset_code"
  ) %>%
  select (
    -indicatorset_code
  )

## Indicatoren -------------------------------------------------------------
indicatoren <- df %>%
  distinct (
    icode, 
    isorteer,
    inummer,
    inaam,
    ieenheid,
    itype,
    thema
  ) %>%
  rowid_to_column("indicator_ID") %>% 
  # Maak foreign key naar themas
  left_join(
    select (themas, thema_ID, thema),
    by = "thema"
  ) %>%
  select (-thema)
# indicator is uniek gedefinieerd door icode
# indicatoren %>% count (icode) %>% filter (n>1) 

## Fact tabel --------------------------------------------------------------
# Dit is de oorspronkelijke tabel - in long format - gestript van de DIM 
# kolommen
dfact <- df %>%
  # Maak foreign key naar lokaties
  left_join(
    select (lokaties, lokatie_ID, locatie, lvestigingsnummer, lagb, organisatie_ID),
    by = c ("locatie", "lvestigingsnummer", "lagb")
  ) %>%
  # FIXME door deze join worden de lokatie/organisatiegegevens verwijderd
  select (
    - locatie,
    - lvestigingsnummer, 
    - lpostcode, 
    - lhuisnummer, 
    - lplaats, 
    - lagb,
    # En de organisatiegegevens hangen aan de lokatie
    - organisatie,
    - okvk,
    - oagb,
    - type_zorgaanbieder
  ) %>% 
  # Maak foreign key naar indicatoren
  left_join(
    select (indicatoren, indicator_ID, icode),
    by = "icode"
  ) %>%
  select (
    # Indicatorsets hangen aan de themas
    - indicatorset,
    - indicatorset_code,
    # themas hangen aan indicatoren
    - thema, 
    - icode,
    - isorteer, 
    - inummer, 
    - inaam, 
    - ieenheid, 
    - itype,
    # aanleverfrequentie is altijd jaarlijks...
    - aanlever_frequentie
  )

## Aparte tabellen Personeelssamenstelling en Basisveiligheid ---------------
# Toch ook weer thema en indicatorset ID erbij, dat joint makkelijker later
bv <- dfact %>% 
  left_join(
    select (indicatoren, indicator_ID, thema_ID),
    by = "indicator_ID"
  ) %>% 
  left_join(
    select (themas, thema_ID, indicatorset_ID),
    by = "thema_ID"
  ) 

ps <- bv %>%
  filter (indicatorset_ID == 2) %>% 
  select (
    - thema_ID,
    - indicatorset_ID
  )

bv <- bv %>%
  filter (indicatorset_ID == 1) %>% 
  select (
    - thema_ID,
    - indicatorset_ID
  )

## Lokaties: aanvullen en opschonen -------------------------------------------
# Om de grootte van de locatie in te schatten zijn twee soorten gegevens bekend. 
# Medicatieveiligheid en voedselvoorkeuren zijn verplichte indicatoren en 
# wordt ingevuld op respectievelijk afdelings- en op clientniveau. 
# Indicator_ID 6 = "Percentage afdelingen ... medicatiefouten"
# Indicator_ID 32 = "Percentage cliënten op de afdeling ... voedselvoorkeuren"
lokaties <- lokaties %>% 
  # koppel met indicator 6 tbv aantal afdelingen
  left_join(
    filter (select(bv, noemer, lokatie_ID, indicator_ID), indicator_ID == 6),
    by = "lokatie_ID"
  ) %>% 
  rename (nafdelingen = noemer) %>% 
  select (-indicator_ID) %>% 
  # koppel met indicator 32 tbv aantal clienten
  left_join(
    filter (select(bv, noemer, lokatie_ID, indicator_ID), indicator_ID == 32),
    by = "lokatie_ID"
  ) %>% 
  rename (nclienten = noemer) %>% 
  select (-indicator_ID) %>% 
  # en bereken het aantal clienten per afdeling
  mutate (cltPerAfd = round(nclienten/nafdelingen)) %>% 
  # grootte van de lokaties gecontroleerd middels grafiek en eyeballing tabel.
  # Allereerst lokaties met een abnormaal hoog aantal afdelingen. 
  # Organisatie 16 heeft niet goed gerapporteerd (heeft totaal aantal afdelingen
  # van organisatie genomen), dus deze waarde wordt verwijderd. 
  # De daarna hoogste aantal afdelingen hebben een reeel aantal clienten per
  # afdeling, dus dat zal kloppen
  mutate(nafdelingen = replace(nafdelingen, organisatie_ID==16, NA)) %>% 
  # Vervolgens abnormaal hoog aantal clienten per afdeling
  # > 75 is niet reeel, hieronder zie je dat er af en toe wel meerdere afdelingen
  # zijn. 
  mutate(nafdelingen = replace(nafdelingen, cltPerAfd>75, NA)) %>% 
  # herbereken cltPerAfd
  mutate (cltPerAfd = round(nclienten/nafdelingen)) %>% 
  # Omdat het toch twijfelachtig blijft maak ik hier maar een level van
  mutate (fafdelingen = factor(case_when(
    nafdelingen == 1 ~ '1',
    nafdelingen == 2 ~ '2',
    nafdelingen >= 3 & nafdelingen <= 5 ~ "3-5",
    nafdelingen >= 6 & nafdelingen <= 8 ~ "6-8",
    nafdelingen > 8 ~ ">8"))) %>% 
  mutate(fafdelingen = ordered (fafdelingen, levels = 
                           c( "1",   "2",   "3-5", "6-8", ">8" ))) %>% 
  # Controleer aantal clienten: Gecontroleerd met histogram en eyeballing. 
  # Minimaal aantal clienten is 10, lijkt me reeel. 
  # 
  # Voeg stedelijkheid toe, koppel met CBS data
  mutate (lpostcode = substr(lpostcode, 1,4)) %>% 
  left_join(
    select(cbs_postcode, PC4, STED),
    by = c("lpostcode" = "PC4")
  ) %>% 
  rename (stedelijk = STED) %>% 
  mutate (stedelijk = as.numeric(stedelijk)) %>% 
  mutate (fstedelijk = factor(case_when(
    stedelijk == 1 ~ "Zeer sterk",
    stedelijk == 2 ~ "Sterk",
    stedelijk == 3 ~ "Matig",
    stedelijk == 4 ~ "Weinig", 
    stedelijk == 5 ~ "Niet" ))) %>% 
  mutate(fstedelijk = ordered (fstedelijk, levels =
                                c("Zeer sterk", 
                                  "Sterk",
                                  "Matig",
                                  "Weinig", 
                                  "Niet"
                                 )))


## Organisaties: aanvullen en opschonen ----------------------------------------
organisaties <- organisaties %>% 
# Voeg aantal locaties, aantal afdelingen en aantal clienten toe
# Mate van stedelijkheid wordt een gemiddelde van de lokaties. 
# Te overwegen: gewogen gemiddelde...
left_join(
  select(lokaties, 
         organisatie_ID, 
         lokatie_ID, 
         nafdelingen, 
         nclienten,
         stedelijk),
  by = 'organisatie_ID'
) %>% 
  group_by(across(c(
    - lokatie_ID,
    - nafdelingen, 
    - nclienten,
    - stedelijk
  ))) %>% 
  summarise(
    nLokaties = n(),
    nAfdelingen = sum(nafdelingen, na.rm = TRUE), 
    nClienten = sum (nclienten, na.rm = TRUE),
    Stedelijk = round(mean (stedelijk, na.rm = TRUE),1)
  )

