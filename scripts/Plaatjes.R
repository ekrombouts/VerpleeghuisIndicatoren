# Grootte locaties in de stad vs platteland -----------------------------------
ggplot(
  data = lokaties,
  mapping = aes(
    x = fstedelijk,
    y = nclienten
  )
) +
  geom_violin() +
  geom_point() +
  labs (title = "Grootte locaties in de stad vs platteland",
        x = "Mate van stedelijkheid",
        y = "Aantal clienten")

# Highlight  -------------------------------------------------------------
# Lokaties
  # De Werf = 485
highlight_df <- lokaties %>% 
  filter (lokatie_ID == 485) # De Werf

# Organisaties
  # Amsta = 56
  # Coloriet = 406
highlight_df <- lokaties %>%
  filter (organisatie_ID == 406) 

# Grootte lokaties: Aantal clienten per lokatie --------------------------------
# Numeriek, 1 var: histogram

lokaties %>% 
  ggplot(aes (nclienten)) +
  geom_histogram (binwidth = 8, 
                  fill = '#161071',
                  alpha = 0.8) +
  geom_dotplot(data = highlight_df,
                 aes(nclienten),
                 fill = 'orange',
                 alpha = 0.8) +
  scale_x_continuous(breaks = seq(0,500,40), 
                     minor_breaks = seq(0,500,8), 
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs (title = "Grootte lokaties: Aantal clienten per lokatie",
        x = "Aantal clienten",
        y = 'frequentie') 
      
  
# Stedelijk -------------------------------------------------------------------
# Categorie
lokaties %>%
  drop_na(stedelijk) %>% 
  ggplot(aes (x = fstedelijk)) +
  geom_bar(fill = '#061268',
           alpha = 0.8) +
  geom_dotplot(data = highlight_df,
               aes(fstedelijk),
               fill = 'orange') +
#  coord_flip() +
  theme_classic() +
  labs (title = "Stedelijkheid: Aantal lokaties per categorie",
        x = "Mate van stedelijkheid",
        y = NULL)

 # Aantal clienten per afdeling - per afdeling ---------------------------------
 lokaties %>% 
  ggplot(aes(nafdelingen, cltPerAfd)) +
  geom_point()

