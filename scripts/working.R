source ('scripts/LadenEnOpschonen.R')
source ('scripts/Decubitus.R')

# install.packages("naniar")
library(naniar)
# install.packages("mice")
library(mice)
# install.packages("naniar")
library(naniar)

rm (df, dfact, cbs_postcode)

# Vul decubitustabel aan met relevante gegevens van lokatie en organisatie ----
LocDec <- decubitus %>%
  left_join(
    select(
      lokaties,
      lokatie_ID,
      locatie,
      nafdelingen,
      nclienten,
      fstedelijk,
      organisatie_ID
    ),
    by = 'lokatie_ID'
  ) %>%
  select (13, 1, 9:12, 2, 8, 3:7) %>%
  rename (ogekozen = opmerking_gekozen,
          operc = opmerking_percentage)

# names(LocDec)


# Bestudeer gekozen -----------------------------------------------------------
## Beslissing op lokatie of organisatieniveau? ----
aap <- LocDec %>%
  group_by(organisatie_ID, gekozen) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = gekozen,
              values_from = n,
              names_prefix = "g") %>%
  mutate (gTRUE = !is.na(gTRUE),
          gFALSE = !is.na(gFALSE)) %>%
  group_by(gTRUE, gFALSE) %>%
  summarise((n = n()))
rm (aap)
# --> Op organisatieniveau. Slechts 22 van de 495 organisaties hebben voor
# sommige lokaties wel en voor andere lokaties niet gekozen voor decubitus,
# 239 hebben wel gekozen, 234 niet.

# Vraag: Is er een verschil tussen de organisaties die decubitus wel of niet ----
# hebben gekozen?
# In: # aantal afdelingen of clienten?

# In getallen
LocDec %>%
  group_by(gekozen) %>%
  summarise (
    n = n(),
    mean_nAfd = mean (nafdelingen, na.rm = TRUE),
    med_nAfd = median (nafdelingen, na.rm = TRUE),
    mean_nCl = mean (nclienten, na.rm = TRUE),
    med_nCl = median (nclienten, na.rm = TRUE),
    lmean_nAfd = mean (log(nafdelingen), na.rm = TRUE),
    lmed_nAfd = median (log(nafdelingen), na.rm = TRUE),
    lmean_nCl = mean (log(nclienten), na.rm = TRUE),
    lmed_nCl = median (log(nclienten), na.rm = TRUE)
  )

# In plaatjes
LocDec %>%
  ggplot(aes(gekozen, nafdelingen)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean")

LocDec %>%
  ggplot (aes(gekozen, nclienten)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean")

LocDec %>%
  mutate(nclienten = log(nclienten)) %>%
  ggplot (aes(nclienten,
              color = gekozen)) +
  geom_density()

# base-R
boxplot(nafdelingen ~ gekozen, data = LocDec,
        col = 'bisque')


# t-test
LocDec %>%
  filter (gekozen == "TRUE") %>%
  select (nclienten) %>%
  mutate (nclienten = log (nclienten)) %>%
  t.test (mu = 3.75)