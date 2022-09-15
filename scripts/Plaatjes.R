skim (Personeelssamenstelling)

Personeelssamenstelling %>% 
  ggplot(aes(nClienten, PSnMedew))+
  geom_point()

Personeelssamenstelling %>% 
  filter (PSFTEperCt < 10) %>% 
  ggplot(aes(nClienten, PSFTEperCt)) +
  geom_point()

Personeelssamenstelling %>% 
  ggplot(aes(p_PSInstroom, p_PSUitstroom)) +
  geom_point()

Personeelssamenstelling %>% 
  ggplot(aes(nClienten, p_PSUitstroom)) +
  geom_point()+
  scale_x_log10()
  
Personeelssamenstelling %>% 
  ggplot (aes(Stedelijk, p_PSPnilKostPerc))+
  geom_jitter()+
  scale_y_log10()+
  geom_smooth()

Personeelssamenstelling %>% 
  ggplot (aes(p_PSNiv1))+
  geom_histogram()

Personeelssamenstelling %>% 
  ggplot (aes(p_PSNiv2))+
  geom_histogram()

Personeelssamenstelling %>% 
  ggplot (aes(p_PSNiv3))+
  geom_histogram()

Personeelssamenstelling %>% 
  ggplot (aes(p_PSNiv4))+
  geom_histogram()

Personeelssamenstelling %>% 
  ggplot (aes(p_PSNiv5))+
  geom_histogram()+
  scale_x_log10()

Personeelssamenstelling %>% 
  ggplot (aes(p_PSNiv6))+
  geom_histogram()+
  scale_x_log10()

Personeelssamenstelling %>% 
  ggplot (aes(p_PSBehandel))+
  geom_histogram()

Personeelssamenstelling %>% 
  ggplot (aes(nClienten, p_PSVerzuimPerc))+
  geom_point()+
#  scale_x_log10()+
  geom_smooth()

Personeelssamenstelling %>% 
  ggplot (aes(p_PSNiv2, p_PSNiv5))+
  geom_point()

Personeelssamenstelling %>% 
  ggplot(aes(p_PSVerzuimPerc, p_PSUitstroom))+
  geom_point()+
  geom_smooth(method = "lm")

Personeelssamenstelling %>% 
  ggplot(aes(p_PSNiv3))+
  geom_boxplot()

par (mfrow = c(1,6))
par (mfros = c(1,1))
boxplot(Personeelssamenstelling$p_PSNiv1)
boxplot(Personeelssamenstelling$p_PSNiv2)
boxplot(Personeelssamenstelling$p_PSNiv3)
boxplot(Personeelssamenstelling$p_PSNiv4)
boxplot(Personeelssamenstelling$p_PSNiv5)
boxplot(Personeelssamenstelling$p_PSNiv6)

aap <- Personeelssamenstelling %>% 
  mutate(TotZZP = p_PSNiv1+p_PSNiv2+p_PSNiv3+p_PSNiv4+p_PSNiv5+p_PSNiv6+p_PSBehandel+p_PSLeerling+p_PSOverig) %>% 
  mutate(ZZP = (p_PSNiv1*1 + p_PSNiv2*2 + p_PSNiv3*3 + p_PSNiv4*4 + p_PSNiv5*5 + p_PSNiv6*6)/(p_PSNiv1+p_PSNiv2+p_PSNiv3+p_PSNiv4+p_PSNiv5+p_PSNiv6)) %>% 
  select (TotZZP,
          ZZP,
          n_PSNiv1,
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
          p_PSLeerling,
          p_PSOverig) %>% 
  arrange(TotZZP) 

aap%>% 
  ggplot(aes(TotZZP))+
  geom_histogram()

plot(Personeelssamenstelling$n_PSNiv1, Personeelssamenstelling$n_PSBehandel)
plot(Personeelssamenstelling$n_PSNiv1, Personeelssamenstelling$PSnFTE)

Personeelssamenstelling <- Personeelssamenstelling %>% 
  mutate(ZZP = (p_PSNiv1*1 + p_PSNiv2*2 + p_PSNiv3*3 + p_PSNiv4*4 + p_PSNiv5*5 + p_PSNiv6*6)/(p_PSNiv1+p_PSNiv2+p_PSNiv3+p_PSNiv4+p_PSNiv5+p_PSNiv6))

Personeelssamenstelling %>% 
  ggplot (aes(ZZP,p_PSUitstroom))+
  geom_point()+
  geom_smooth(method = "lm")

Personeelssamenstelling %>% 
  ggplot (aes(ZZP,PSVerzuimFreq))+
  geom_point()+
  geom_smooth(method = "lm")

Personeelssamenstelling %>% 
  ggplot (aes(p_PSLeerling, p_PSUitstroom))+
  geom_point()+
  geom_smooth(method = "lm")

Personeelssamenstelling %>% 
  ggplot (aes(Stedelijk, p_PSUitstroom))+
  geom_point()+
  geom_smooth(method = "lm")

Personeelssamenstelling %>% 
  ggplot (aes(PSVerzuimFreq, p_PSVerzuimPerc))+
  geom_point()+
  geom_smooth(method = "lm")
