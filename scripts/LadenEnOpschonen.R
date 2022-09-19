# Verpleeghuisindicatoren -----------------------------------------------------
# Datum: Augustus 2022 - (TODO: project afronden)
# Auteur: Eva Rombouts
# Oefenproject in het kader van leren R en R Studio
# Voornaamste gebruikte bronnen voor het leren van R: 
# Coursera cursus Johns Hopkins
# R Ladies Sidney 
# R Programming 101
# Vanaf sep 2022 EQI postacademische opleiding Data Science & Business Analytics

# Bron data: https://www.zorginzicht.nl/openbare-data/open-data-verpleeghuiszorg  
# Dit betreft de kwaliteitsindicatoren die zorginstellingen jaarlijks moeten 
# verzamelen en opsturen. Het doel van de indicatoren is niet zozeer om te 
# vergelijken of een organisatie "het wel goed doet", maar eerder als een 
# middel om te kunnen reflecteren. 
# Data van 2020 genomen (inmiddels zijn data van 2021 bekend, OOIT...)

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
# TODO Van internet laden
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
             na = "-99997", 
             # Deze data is door CBS verborgen gehouden ivm privacy
             skip = 8)
# Eerste rij bevat uitleg, eruit
cbs_postcode <- cbs_postcode[-1, ]

# Opschonen ---------------------------------------------------------------
# De tabel is een platte, lange tabel, waarin twee indicatorsets door elkaar
# heen staan. Per rij wordt een indicator weergegeven, of soms een deel van 
# een indicator. Per lokatie zijn er 45 rijen, per organisatie 23. 
# Per rij staan dus verschillende type observaties en één observatie staat
# over verschillende rijen. 
# Hieronder worden eerst twee tabellen gemaakt, één voor elke indicatorset, 
# met per rij een enkele set observaties.
# Basisveiligheid: Op lokatieniveau
# Personeelssamenstelling: Op organisatieniveau.
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

# Foutieve invoer herstellen ----------------------------------------------
# Onderstaande organisatie / lokatie koppels gaven problemen
# Careyn heeft ten onrechte meerdere concerns ingevoerd. 
df <- df %>% 
  filter (!(locatie %in% c(
    "Concernniveau: Aveant B.V.", 
    "Concernniveau: Careyn DWO/NWN B.V.",
    "Concernniveau: Careyn Zuid-Hollandse eilanden B.V.",
    "Concernniveau: Zuwe Zorg B.V." 
  )))

# King Arthur Groep heeft dezelfde gegevens voor organisatie en lokatie
# opgegeven, wat problemen geeft met unieke lokaties
df <- df %>% 
  mutate (locatie = if_else(
    locatie == "Stichting King Arthur Groep" & indicatorset == "Personeelssamenstelling", 
    true = "Concernniveau: Stichting King Arthur Groep",
    false = locatie
  )) %>% 
  mutate (locatie = if_else(
    locatie == "Concernniveau: Stichting Protestants-Christelijk Zorgcentrum 't Anker" & indicatorset == "Basisveiligheid", 
    true = "Stichting Protestants-Christelijk Zorgcentrum 't Anker",
    false = locatie
  ))

# Thebe heeft apart voor concernniveaus gerapporteerd, zonder per lokatie te 
# specificeren bij welk concern ze horen. Uitgegaan van postcodes.
df <- df %>%
  mutate(organisatie = if_else(
    organisatie == "Stichting Thebe Wonen en Zorg" & str_sub(lpostcode,1,1) == "4",
    true = "Stichting Thebe Wonen en Zorg - West",
    false = organisatie
  ))%>%
  mutate(organisatie = if_else(
    organisatie == "Stichting Thebe Wonen en Zorg" & str_sub(lpostcode,1,1) == "5",
    true = "Stichting Thebe Wonen en Zorg - Midden",
    false = organisatie
  ))

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
# organisatie is uniek gedefinieerd door okvk en organisatie
# organisaties %>% count (okvk, organisatie) %>% filter (n>1)

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
    lplaats, lagb, okvk, organisatie, IsOrg
  ) %>%
  rowid_to_column("lokatie_ID") %>%
  # Maak foreign key naar organisaties
  left_join(
    select (organisaties, organisatie_ID, okvk, organisatie),
    by = c("okvk", "organisatie")
  ) %>% 
  select (
    lokatie_ID,
    organisatie_ID,
    locatie,
    lvestigingsnummer,
    lpostcode,
    lhuisnummer,
    lplaats,
    lagb,
    IsOrg
  )
# lokatie is uniek gedefinieerd door locatie, lvestigingsnummer, lagb
# hieraan is later okvk toegevoegd.
# lokaties %>% count (locatie, lvestigingsnummer, lagb, okvk) %>% filter (n>1) 

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
  # Maak foreign key naar themas en naar indicatorset_ID
  left_join(
    select (themas, thema_ID, thema, indicatorset_ID),
    by = "thema"
  ) %>%
  select (-thema) %>% 
  # Extra variabele met korte naam voor de indicator. Deze zal later als 
  # kolomnaam worden gebruikt voor pivot-wider
  mutate (ind = recode(indicator_ID,
                       `1` = "DecGek",
                       `2` = "DecPercC",
                       `3` = "DecCasGek",
                       `4` = "DecCasPercA",
                       `5` = "ACPPercC",
                       `6` = "MedFtPercA",
                       `7` = "MedRevGek",
                       `8` = "MedRevPercC",
                       `9` = "MMGekozen",
                       `10` = "MMMechanischPercC",
                       `11` = "MMFysiekPercC",
                       `12` = "MMFarmacPercC",
                       `13` = "MMPsychPercC",
                       `14` = "MMElecPercC",
                       `15` = "MM1op1PercC",
                       `16` = "MMAfzonPercC",
                       `17` = "MMOverigPercC",
                       `18` = "MMOverigTekst",
                       `19` = "VrijBepGek",
                       `20` = "VrijBepTekst",
                       `21` = "VrijBevGek",
                       `22` = "VrijBevTekst",
                       `23` = "ContGek",
                       `24` = "ContWPlanPercC",
                       `25` = "ContGPlanPercC",
                       `26` = "ContOPlanPercC",
                       `27` = "ContVoorkeurenJN",
                       `28` = "ContOndersteuningJN",
                       `29` = "ContZelfstandigJN",
                       `30` = "ContMateriaalJN",
                       `31` = "ContAndersJN",
                       `32` = "VoedWVoorkeurPercC",
                       `33` = "VoedGVoorkeurPercC",
                       `34` = "VoedOVoorkeurPercC",
                       `35` = "VoedWelkJN",
                       `36` = "VoedVormJN",
                       `37` = "VoedHulpJN",
                       `38` = "VoedTijdPlaatsJN",
                       `39` = "VoedOverigJN",
                       `40` = "KwalVerslURL",
                       `41` = "CENPS8910PercR",
                       `42` = "CENPSJaPercR",
                       `43` = "CEScoreGet",
                       `44` = "CEnRespondenten",
                       `45` = "CEOpm",
                       `46` = "PSnMedew",
                       `47` = "PSnFTE",
                       `48` = "PSTijdPerc",
                       `49` = "PSPnilPerc",
                       `50` = "PSPnilKostPerc",
                       `51` = "PSGemContr",
                       `52` = "PSNiv1",
                       `53` = "PSNiv2",
                       `54` = "PSNiv3",
                       `55` = "PSNiv4",
                       `56` = "PSNiv5",
                       `57` = "PSNiv6",
                       `58` = "PSBehandel",
                       `59` = "PSOverig",
                       `60` = "PSLeerling",
                       `61` = "PSnStag",
                       `62` = "PSnVrijw",
                       `63` = "PSVerzuimPerc",
                       `64` = "PSVerzuimFreq",
                       `65` = "PSInstroom",
                       `66` = "PSUitstroom",
                       `67` = "PSDoorstroom",
                       `68` = "PSFTEperCt",
                       
  ),
  .after = inaam) 
# indicator is uniek gedefinieerd door icode
# indicatoren %>% count (icode) %>% filter (n>1) 

## Fact tabel --------------------------------------------------------------
# Dit is de oorspronkelijke tabel - in long format - gestript van de DIM 
# kolommen
dfact <- df %>%
  # Maak foreign key naar lokaties
  left_join(
    select (lokaties, lokatie_ID, 
            locatie, lvestigingsnummer, lagb, 
            organisatie_ID),
    by = c ("locatie", "lvestigingsnummer", "lagb")
  ) %>%
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
    Stedelijk = round(mean (stedelijk, na.rm = TRUE),1),
    .groups = "drop"
  ) 

# Basisveiligheid ---------------------------------------------------------
# Long to wide. 105707 rijen van 2349 lokaties = 45 rijen/indicatoren per lokatie
# Aangezien het verschillende datatypes betreft heb ik het opgesplitst. 
# Eerst de percentages: 
bv1 <- bv %>% 
  select(
    lokatie_ID,
    organisatie_ID,
    indicator_ID,
    iwaarde,
    teller,
    noemer
  ) %>% 
  left_join(
    y = select(indicatoren, indicator_ID, ind, ieenheid),
    by = "indicator_ID"
  ) %>% 
  filter(ieenheid == "Percentage") %>% 
  select (
    -indicator_ID,
    -ieenheid
  ) %>% 
  # namen worden prefixes, dus even korter maken
  rename (
    p = iwaarde,
    t = teller,
    n = noemer
  ) %>% 
  mutate(p = as.numeric(p)) %>% 
  pivot_wider(
    names_from = ind,
    values_from = c(p, t, n), 
    names_vary = "slowest"
  ) 

bv2 <- bv %>% 
  # Overige numerieke indicatoren. De waarde wordt gewijzigd in numeric 
  # voor de pivot
  select(
    lokatie_ID,
    organisatie_ID, 
    indicator_ID,
    iwaarde
  ) %>% 
  left_join(
    y = select(indicatoren, indicator_ID, ind, ieenheid),
    by = "indicator_ID"
  ) %>% 
  filter(ieenheid == "Aantal" | ieenheid == "Getal") %>% 
  select (
    -indicator_ID,
    -ieenheid
  ) %>% 
  mutate (iwaarde = as.numeric(iwaarde)) %>% 
  pivot_wider(
    names_from = ind,
    values_from = c(iwaarde)
  ) 

bv3 <- bv %>% 
  # En tot slot de tekstindicatoren 
  select(
    lokatie_ID,
    organisatie_ID, 
    indicator_ID,
    iwaarde
  ) %>% 
  left_join(
    y = select(indicatoren, indicator_ID, ind, ieenheid),
    by = "indicator_ID"
  ) %>% 
  filter(ieenheid == "JaNee" | ieenheid == "Tekst") %>% 
  select (
    -indicator_ID,
    -ieenheid
  ) %>% 
  pivot_wider(
    names_from = ind,
    values_from = c(iwaarde)
  ) 

BasisVeiligheid <- bv1 %>% 
  full_join(
    y = bv2,
    by = c("lokatie_ID", "organisatie_ID")
  ) %>% 
  full_join(
    y = bv3,
    by = c("lokatie_ID", "organisatie_ID")
  ) %>% 
  select (
    lokatie_ID,
    organisatie_ID,
    # decubitus
    DecGek,
    p_DecPercC,
    t_DecPercC,
    n_DecPercC,
    DecCasGek,
    p_DecCasPercA,
    t_DecCasPercA,
    n_DecCasPercA,
    # beleid / Advanced care planning
    p_ACPPercC,
    t_ACPPercC,
    n_ACPPercC,
    # medicatieveiligheid
    p_MedFtPercA,
    t_MedFtPercA,
    n_MedFtPercA,
    MedRevGek,
    p_MedRevPercC,
    t_MedRevPercC,
    n_MedRevPercC,
    # onvrijwillige zorg / middelen en maatregelen
    MMGekozen,
    p_MMMechanischPercC,
    t_MMMechanischPercC,
    n_MMMechanischPercC,
    p_MMFysiekPercC,
    t_MMFysiekPercC,
    n_MMFysiekPercC,
    p_MMFarmacPercC,
    t_MMFarmacPercC,
    n_MMFarmacPercC,
    p_MMPsychPercC,
    t_MMPsychPercC,
    n_MMPsychPercC,
    p_MMElecPercC,
    t_MMElecPercC,
    n_MMElecPercC,
    p_MM1op1PercC,
    t_MM1op1PercC,
    n_MM1op1PercC,
    p_MMAfzonPercC,
    t_MMAfzonPercC,
    n_MMAfzonPercC,
    p_MMOverigPercC,
    t_MMOverigPercC,
    n_MMOverigPercC,
    MMOverigTekst,
    VrijBepGek,
    VrijBepTekst,
    VrijBevGek,
    VrijBevTekst,
    # continentie / toiletgang
    ContGek,
    p_ContWPlanPercC,
    t_ContWPlanPercC,
    n_ContWPlanPercC,
    p_ContGPlanPercC,
    t_ContGPlanPercC,
    n_ContGPlanPercC,
    p_ContOPlanPercC,
    t_ContOPlanPercC,
    n_ContOPlanPercC,
    ContVoorkeurenJN,
    ContOndersteuningJN,
    ContZelfstandigJN,
    ContMateriaalJN,
    ContAndersJN,
    # voedingsvoorkeuren
    p_VoedWVoorkeurPercC,
    t_VoedWVoorkeurPercC,
    n_VoedWVoorkeurPercC,
    p_VoedGVoorkeurPercC,
    t_VoedGVoorkeurPercC,
    n_VoedGVoorkeurPercC,
    p_VoedOVoorkeurPercC,
    t_VoedOVoorkeurPercC,
    n_VoedOVoorkeurPercC,
    VoedWelkJN,
    VoedVormJN,
    VoedHulpJN,
    VoedTijdPlaatsJN,
    VoedOverigJN,
    # kwaliteitsverslag
    KwalVerslURL,
    # Clienttevredenheid
    p_CENPS8910PercR,
    t_CENPS8910PercR,
    n_CENPS8910PercR,
    p_CENPSJaPercR,
    t_CENPSJaPercR,
    n_CENPSJaPercR,
    CEScoreGet,
    CEnRespondenten,
    CEOpm
  )

# Personeelssamenstelling -------------------------------------------------
# Long to wide: 11316 rijen / 23 rijen/indicatoren = 492. Vier van de 496
# organisaties hebben kennelijk niet gerapporteerd. 
ps1 <- ps %>% 
  # Long to wide, eerst de percentages
  select(
    lokatie_ID,
    organisatie_ID,
    indicator_ID,
    iwaarde,
    teller,
    noemer
  ) %>% 
  left_join(
    y = select(indicatoren, indicator_ID, ind, ieenheid),
    by = "indicator_ID"
  ) %>% 
  filter(ieenheid == "Percentage") %>% 
  select (
    -indicator_ID,
    -ieenheid
  ) %>% 
  rename (
    p = iwaarde,
    t = teller,
    n = noemer
  ) %>% 
  mutate(p = as.numeric(p)) %>% 
  pivot_wider(
    names_from = ind,
    values_from = c(p, t, n), 
    names_vary = "slowest"
  ) 

ps2 <- ps %>% 
  # Long to wide: Overige numerieke indicatoren 
  select(
    lokatie_ID,
    organisatie_ID,
    indicator_ID,
    iwaarde
  ) %>% 
  left_join(
    y = select(indicatoren, indicator_ID, ind, ieenheid),
    by = "indicator_ID"
  ) %>% 
  filter(ieenheid == "Aantal" | ieenheid == "Getal") %>% 
  select (
    -indicator_ID,
    -ieenheid
  ) %>% 
  mutate (iwaarde = as.numeric(iwaarde)) %>% 
  pivot_wider(
    names_from = ind,
    values_from = c(iwaarde)
  ) 

ps3 <- ps %>% 
  # Long to wide: Tekstindicatoren 
  select(
    lokatie_ID,
    organisatie_ID,
    indicator_ID,
    iwaarde
  ) %>% 
  left_join(
    y = select(indicatoren, indicator_ID, ind, ieenheid),
    by = "indicator_ID"
  ) %>% 
  filter(ieenheid == "JaNee" | ieenheid == "Tekst") %>% 
  select (
    -indicator_ID,
    -ieenheid
  ) %>% 
  pivot_wider(
    names_from = ind,
    values_from = c(iwaarde)
  ) 

Personeelssamenstelling <- ps1 %>% 
  full_join(
    y = ps2,
    by = c("lokatie_ID", "organisatie_ID")
  ) %>% 
  full_join(
    y = ps3,
    by = c("lokatie_ID", "organisatie_ID")
  ) %>% 
  # Veel PS percentages hebben dezelfde noemer (muv tijdelijk en kosten)
  rename (n_PSnoemer = n_PSNiv1) %>% 
  select(
    organisatie_ID,
    lokatie_ID,
    PSnMedew,
    PSnFTE,
    n_PSnoemer, 
    p_PSTijdPerc,
    t_PSTijdPerc,
    n_PSTijdPerc,
    p_PSPnilPerc,
    t_PSPnilPerc,
    p_PSPnilKostPerc,
    t_PSPnilKostPerc,
    n_PSPnilKostPerc,
    PSGemContr,
    p_PSNiv1,
    t_PSNiv1,
    p_PSNiv2,
    t_PSNiv2,
    p_PSNiv3,
    t_PSNiv3,
    p_PSNiv4,
    t_PSNiv4,
    p_PSNiv5,
    t_PSNiv5,
    p_PSNiv6,
    t_PSNiv6,
    p_PSBehandel,
    t_PSBehandel,
    p_PSOverig,
    t_PSOverig,
    p_PSLeerling,
    t_PSLeerling,
    PSnStag,
    PSnVrijw,
    p_PSVerzuimPerc,
    t_PSVerzuimPerc,
    n_PSVerzuimPerc,
    PSVerzuimFreq,
    p_PSInstroom,
    t_PSInstroom,
    n_PSInstroom,
    p_PSUitstroom,
    t_PSUitstroom,
    n_PSUitstroom,
    p_PSDoorstroom,
    t_PSDoorstroom,
    n_PSDoorstroom,
    PSFTEperCt
  ) %>% 
  arrange(organisatie_ID) 

# Personeelssamenstelling samenvoegen met organisatiegegevens -----------
ps <- organisaties %>% 
  left_join(
    Personeelssamenstelling,
    by = "organisatie_ID"
  ) %>% 
  ungroup() %>% 
  select (
    -okvk,
    -oagb,
    -type_zorgaanbieder
  )
Personeelssamenstelling <- ps

rm (bv, bv1, bv2, bv3, ps, ps1, ps2, ps3, df, dfact, cbs_postcode)
