

# Decubitus ------------------------------------------------------------------
# Decubitus is een keuze indicator. Het thema heeft 2 indicatoren:
# Gekozen ja/nee en het percentage. 
# Allereerst maak ik een tabel waar een rij 1 observatie is. 

decubitus <- dfact %>%
  left_join(select(indicatoren, thema_ID, indicator_ID), "indicator_ID")%>%
  filter (thema_ID == 1) %>% 
  mutate (ind = recode(indicator_ID,
                       `1` = "gekozen",
                       `2` = "percentage")) %>% 
  select (
    -verslagjaar, 
    -bron, 
    -begindat, 
    -einddat, 
    -thema_ID, 
    -indicator_ID) %>% 
  pivot_wider(names_from = ind,
              values_from = c(iwaarde, teller,noemer, opmerking, invt)) %>% 
  select (-teller_gekozen, - noemer_gekozen, - invt_gekozen) %>% 
  rename(
    gekozen = iwaarde_gekozen,
    percentage = iwaarde_percentage,
    teller = teller_percentage, 
    noemer = noemer_percentage,
    invt = invt_percentage
  ) %>% 
  mutate(percentage = as.numeric(percentage))

## Vergelijk gekozen vs niet---------------------------------------------------
# Hebben 
lokaties <- lokaties %>% 
  left_join (select(dfact, noemer, lokatie_ID),
             by = "lokatie_ID") %>% 
  group_by (across(c(-noemer))) %>% 
  summarise(maxnoemer = max(noemer, na.rm = TRUE)) %>% 
  arrange(-maxnoemer)



ggplot (data = decubitus,
        mapping = aes (x = percentage)
) + 
  geom_histogram() + 
  facet_wrap(~gekozen)

