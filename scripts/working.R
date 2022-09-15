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

rm (bv, bv1, bv2, bv3, ps, ps1, ps2, ps3)
