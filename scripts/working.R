# Onderzoek lokaties ----
# Voor statistiek. 
# Hst 3 One sample & difference of means test


#' Hypothese1: De gemiddelde lokatie-grootte is 30 clienten, met een sd van 15.
#' Type 1 fout: rejecting while true
mean (lokaties$nclienten, na.rm = T)
median(lokaties$nclienten, na.rm = T)
sd (lokaties$nclienten, na.rm = T)

lokaties %>% 
  ggplot(aes(nclienten)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_eva()
# VRAAG Origin naar 0, kan dit niet korter?

library(Rmisc)
LocGrootte <- summarySE(lokaties, "nclienten", na.rm = T)
LocGrootte

LocGrootte %>% 
  ggplot(aes(factor(""), nclienten)) +
  geom_errorbar(aes(ymin = nclienten - ci, ymax = nclienten + ci), width = 1) +
  geom_point() +
  scale_x_discrete("") + 
  scale_y_continuous(name = "Gemiddeld aantal clienten per locatie",
                     breaks = scales::pretty_breaks(n=8))
# HIER ben ik voor statistiek ------

#' Hypothese2: Stedelijke gebieden hebben grotere lokaties
