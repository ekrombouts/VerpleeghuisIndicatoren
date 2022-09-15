bv1 <- bv %>% 
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

bv2 <- bv %>% 
  # Long to wide: alle indicatoren die geen percentage zijn. 
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
  filter(!(ieenheid == "Percentage")) %>% 
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
  select (
    lokatie_ID,
    DecGek,
    p_DecPercC,
    t_DecPercC,
    n_DecPercC,
    DecCasGek,
    p_DecCasPercA,
    t_DecCasPercA,
    n_DecCasPercA,
    p_ACPPercC,
    t_ACPPercC,
    n_ACPPercC,
    p_MedFtPercA,
    t_MedFtPercA,
    n_MedFtPercA,
    MedRevGek,
    p_MedRevPercC,
    t_MedRevPercC,
    n_MedRevPercC,
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
    KwalVerslURL,
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

rm (bv, bv1, bv2)


# Personeelssamenstelling -------------------------------------------------
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
  pivot_wider(
    names_from = ind,
    values_from = c(p, t, n), 
    names_vary = "slowest"
  ) 

ps2 <- ps %>% 
  # Long to wide: alle indicatoren die geen percentage zijn. 
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
  filter(!(ieenheid == "Percentage")) %>% 
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
  select(
    lokatie_ID,
    organisatie_ID,
    PSnMedew,
    PSnFTE,
    p_PSTijdPerc,
    t_PSTijdPerc,
    n_PSTijdPerc,
    p_PSPnilPerc,
    t_PSPnilPerc,
    n_PSPnilPerc,
    p_PSPnilKostPerc,
    t_PSPnilKostPerc,
    n_PSPnilKostPerc,
    PSGemContr,
    p_PSNiv1,
    t_PSNiv1,
    n_PSNiv1,
    p_PSNiv2,
    t_PSNiv2,
    n_PSNiv2,
    p_PSNiv3,
    t_PSNiv3,
    n_PSNiv3,
    p_PSNiv4,
    t_PSNiv4,
    n_PSNiv4,
    p_PSNiv5,
    t_PSNiv5,
    n_PSNiv5,
    p_PSNiv6,
    t_PSNiv6,
    n_PSNiv6,
    p_PSBehandel,
    t_PSBehandel,
    n_PSBehandel,
    p_PSOverig,
    t_PSOverig,
    n_PSOverig,
    p_PSLeerling,
    t_PSLeerling,
    n_PSLeerling,
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
  )

rm (ps, ps1, ps2)
