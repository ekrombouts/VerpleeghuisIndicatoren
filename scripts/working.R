# WerkTabel ---------------------------------------------------------------
VHPersoneel <- Personeelssamenstelling %>%
  mutate(
    NiveauMix = 
      (p_PSNiv1 * 1 + p_PSNiv2 * 2 + p_PSNiv3 * 3 + p_PSNiv4 * 4 + 
         p_PSNiv5 * 5 + p_PSNiv6 * 6) /
      (p_PSNiv1 + p_PSNiv2 + p_PSNiv3 + p_PSNiv4 + p_PSNiv5 + p_PSNiv6)
  ) %>%
  mutate(Stedelijk = round(Stedelijk)) %>%
  mutate (Stedelijk = factor(
    case_when(
      Stedelijk == 1 ~ "Zeer sterk",
      Stedelijk == 2 ~ "Sterk",
      Stedelijk == 3 ~ "Matig",
      Stedelijk == 4 ~ "Weinig",
      Stedelijk == 5 ~ "Niet"
    )
  )) %>%
  mutate(Stedelijk = ordered (
    Stedelijk,
    levels =
      c("Zeer sterk",
        "Sterk",
        "Matig",
        "Weinig",
        "Niet")
  )) %>%
  mutate (PersGroei = p_PSInstroom > p_PSUitstroom) %>% 
  rename (
    nMedew = PSnMedew,
    nFTE = PSnFTE,
    pPNIL = p_PSPnilPerc,
    GemContr = PSGemContr,
    pVerzuim = p_PSVerzuimPerc,
    VerZuimFreq = PSVerzuimFreq,
    pDoorstroom = p_PSDoorstroom,
    FTEperClient = PSFTEperCt
  ) %>% 
    select (
      organisatie_ID,
      organisatie,
      nClienten,
      Stedelijk,
      nMedew,
      nFTE,
      pPNIL,
      GemContr,
      NiveauMix,
      FTEperClient,
      PersGroei,
      pVerzuim,
      VerZuimFreq
    ) %>% 
  left_join(
    select(lokaties, organisatie_ID, nclienten), 
    by = "organisatie_ID"
  ) %>% 
  group_by(across(c(-nclienten))) %>% 
  summarise(LocGrootte = round(mean(nclienten, na.rm = T))) %>% 
  ungroup()

save (VHPersoneel, file = "data/VHPersoneel.Rda")  
  