clim2060 <- read.csv("D:/abla_pien_coords_13GCMs_ensemble_ssp370_2050-2060Y.csv")
clim2060 <- read.csv("D:/abla_pien_coords_13GCMs_ensemble_ssp370_2050-2060Y.csv")
clim2090 <- read.csv("D:/abla_pien_coords_13GCMs_ensemble_ssp370_2080-2090Y.csv")

clim2060 <- clim2060 %>% 
  select(PLT_CN=ID1, Year, 7:31) %>% 
  mutate(across(where(is.numeric), .fns = ~ifelse(.x==-9999,NA_real_,.x))) %>%
  group_by(PLT_CN) %>%
  summarise(across(MAT:DD1040, ~mean(.x,na.rm=T), .names="{.col}_2060ssp370_mean"),
            across(MAT:DD1040, ~max(.x,na.rm=T), .names="{.col}_2060ssp370_max"),
            across(MAT:DD1040, ~min(.x,na.rm=T), .names="{.col}_2060ssp370_min"),
            across(MAT:DD1040, ~sd(.x,na.rm=T), .names="{.col}_206ssp370_sd")) %>% 
  select(PLT_CN, MAT_2060ssp370_max, CMD_2060ssp370_max)

clim2090 <- clim2090 %>% 
  select(PLT_CN=ID1, Year, 7:31) %>% 
  mutate(across(where(is.numeric), .fns = ~ifelse(.x==-9999,NA_real_,.x))) %>%
  group_by(PLT_CN) %>%
  summarise(across(MAT:DD1040, ~mean(.x,na.rm=T), .names="{.col}_2090ssp370_mean"),
            across(MAT:DD1040, ~max(.x,na.rm=T), .names="{.col}_2090ssp370_max"),
            across(MAT:DD1040, ~min(.x,na.rm=T), .names="{.col}_2090ssp370_min"),
            across(MAT:DD1040, ~sd(.x,na.rm=T), .names="{.col}_2090ssp370_sd")) %>% 
  select(PLT_CN, MAT_2090ssp370_max, CMD_2090ssp370_max)

ind.mort.dat <- ind.mort.dat %>% 
  left_join(clim2060, by="PLT_CN") %>% 
  left_join(clim2090, by="PLT_CN") %>% 
  mutate(MAT_maxanom_2060 = (MAT_2060ssp370_max - MAT_ref_mean),
         CMD_maxanom_2060 = (CMD_2060ssp370_max - CMD_ref_mean),
         MAT_maxanom_2060.z = MAT_maxanom_2060/MAT_ref_sd,
         CMD_maxanom_2060.z = CMD_maxanom_2060/CMD_ref_sd,
         MAT_maxanom_2090 = (MAT_2090ssp370_max - MAT_ref_mean),
         CMD_maxanom_2090 = (CMD_2090ssp370_max - CMD_ref_mean),
         MAT_maxanom_2090.z = MAT_maxanom_2090/MAT_ref_sd,
         CMD_maxanom_2090.z = CMD_maxanom_2090/CMD_ref_sd)

seed.dat <- seed.dat %>% 
  left_join(clim2060, by="PLT_CN") %>% 
  left_join(clim2090, by="PLT_CN") %>% 
  mutate(MAT_maxanom_2060 = (MAT_2060ssp370_max - MAT_ref_mean),
         CMD_maxanom_2060 = (CMD_2060ssp370_max - CMD_ref_mean),
         MAT_maxanom_2060.z = MAT_maxanom_2060/MAT_ref_sd,
         CMD_maxanom_2060.z = CMD_maxanom_2060/CMD_ref_sd,
         MAT_maxanom_2090 = (MAT_2090ssp370_max - MAT_ref_mean),
         CMD_maxanom_2090 = (CMD_2090ssp370_max - CMD_ref_mean),
         MAT_maxanom_2090.z = MAT_maxanom_2090/MAT_ref_sd,
         CMD_maxanom_2090.z = CMD_maxanom_2090/CMD_ref_sd)

sap.dat <- sap.dat %>% 
  left_join(clim2060, by="PLT_CN") %>% 
  left_join(clim2090, by="PLT_CN") %>% 
  mutate(MAT_maxanom_2060 = (MAT_2060ssp370_max - MAT_ref_mean),
         CMD_maxanom_2060 = (CMD_2060ssp370_max - CMD_ref_mean),
         MAT_maxanom_2060.z = MAT_maxanom_2060/MAT_ref_sd,
         CMD_maxanom_2060.z = CMD_maxanom_2060/CMD_ref_sd,
         MAT_maxanom_2090 = (MAT_2090ssp370_max - MAT_ref_mean),
         CMD_maxanom_2090 = (CMD_2090ssp370_max - CMD_ref_mean),
         MAT_maxanom_2090.z = MAT_maxanom_2090/MAT_ref_sd,
         CMD_maxanom_2090.z = CMD_maxanom_2090/CMD_ref_sd)

## making prediction dataframes ----

### getting values for fire scenarios

seed.dat %>% 
  filter(fire.sev>0) %>% 
  group_by(ECOSUBCD) %>% 
  summarise(mean.area = mean(area.fire.prop),
            mean.sev = mean(fire.sev)) %>% 
  summary()

lo.area <- 0.05
hi.area <- 0.20

lo.sev <- 0.50
hi.sev <- 0.90

### current climate

m.currlo <- ind.mort.dat %>% 
  mutate(area.fire.prop = lo.area)
m.currhi <- ind.mort.dat %>% 
  mutate(area.fire.prop = hi.area)

s.currlo <- seed.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, lo.sev))
s.currhi <- seed.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, hi.sev))

r.currlo <- sap.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, lo.sev))
r.currhi <- sap.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, hi.sev))

### 2060 climate

m.2060lo <- ind.mort.dat %>% 
  mutate(area.fire.prop = lo.area,
         MAT_maxanom.z = MAT_maxanom_2060.z,
         CMD_maxanom.z = CMD_maxanom_2060.z)
m.2060hi <- ind.mort.dat %>% 
  mutate(area.fire.prop = hi.area,
         MAT_maxanom.z = MAT_maxanom_2060.z,
         CMD_maxanom.z = CMD_maxanom_2060.z)

s.2060lo <- seed.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, lo.sev),
         MAT_maxanom.z = MAT_maxanom_2060.z,
         CMD_maxanom.z = CMD_maxanom_2060.z)
s.2060hi <- seed.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, hi.sev),
         MAT_maxanom.z = MAT_maxanom_2060.z,
         CMD_maxanom.z = CMD_maxanom_2060.z)

r.2060lo <- sap.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, lo.sev),
         MAT_maxanom.z = MAT_maxanom_2060.z,
         CMD_maxanom.z = CMD_maxanom_2060.z)
r.2060hi <- sap.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, hi.sev),
         MAT_maxanom.z = MAT_maxanom_2060.z,
         CMD_maxanom.z = CMD_maxanom_2060.z)

### 2090 climate

m.2090lo <- ind.mort.dat %>% 
  mutate(area.fire.prop = lo.area,
         MAT_maxanom.z = MAT_maxanom_2090.z,
         CMD_maxanom.z = CMD_maxanom_2090.z)
m.2090hi <- ind.mort.dat %>% 
  mutate(area.fire.prop = hi.area,
         MAT_maxanom.z = MAT_maxanom_2090.z,
         CMD_maxanom.z = CMD_maxanom_2090.z)

s.2090lo <- seed.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, lo.sev),
         MAT_maxanom.z = MAT_maxanom_2090.z,
         CMD_maxanom.z = CMD_maxanom_2090.z)
s.2090hi <- seed.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, hi.sev),
         MAT_maxanom.z = MAT_maxanom_2090.z,
         CMD_maxanom.z = CMD_maxanom_2090.z)

r.2090lo <- sap.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, lo.sev),
         MAT_maxanom.z = MAT_maxanom_2090.z,
         CMD_maxanom.z = CMD_maxanom_2090.z)
r.2090hi <- sap.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, hi.sev),
         MAT_maxanom.z = MAT_maxanom_2090.z,
         CMD_maxanom.z = CMD_maxanom_2090.z)

#### posterior predictions ----

#MORT
m19.currlo <- brms::posterior_predict(m19,
                                      ndraws=1000,
                                      newdata = m.currlo %>% 
                                        filter(SPCD==19)) %>% colMeans()
m93.currlo <- brms::posterior_predict(m93,
                                      ndraws=1000,
                                      newdata = m.currlo %>% 
                                        filter(SPCD==93)) %>% colMeans()
m19.currhi <- brms::posterior_predict(m19,
                                      ndraws=1000,
                                      newdata = m.currhi %>% 
                                        filter(SPCD==19)) %>% colMeans()
m93.currhi <- brms::posterior_predict(m93,
                                      ndraws=1000,
                                      newdata = m.currhi %>% 
                                        filter(SPCD==93)) %>% colMeans()

m19.2060lo <- brms::posterior_predict(m19,
                                      ndraws=1000,
                                      newdata = m.2060lo %>% 
                                        filter(SPCD==19)) %>% colMeans()
m93.2060lo <- brms::posterior_predict(m93,
                                      ndraws=1000,
                                      newdata = m.2060lo %>% 
                                        filter(SPCD==93)) %>% colMeans()
m19.2060hi <- brms::posterior_predict(m19,
                                      ndraws=1000,
                                      newdata = m.2060hi %>% 
                                        filter(SPCD==19)) %>% colMeans()
m93.2060hi <- brms::posterior_predict(m93,
                                      ndraws=1000,
                                      newdata = m.2060hi %>% 
                                        filter(SPCD==93)) %>% colMeans()

m19.2090lo <- brms::posterior_predict(m19,
                                      ndraws=1000,
                                      newdata = m.2090lo %>% 
                                        filter(SPCD==19)) %>% colMeans()
m93.2090lo <- brms::posterior_predict(m93,
                                      ndraws=1000,
                                      newdata = m.2090lo %>% 
                                        filter(SPCD==93)) %>% colMeans()
m19.2090hi <- brms::posterior_predict(m19,
                                      ndraws=1000,
                                      newdata = m.2090hi %>% 
                                        filter(SPCD==19)) %>% colMeans()
m93.2090hi <- brms::posterior_predict(m93,
                                      ndraws=1000,
                                      newdata = m.2090hi %>% 
                                        filter(SPCD==93)) %>% colMeans()

#SEED
s19.currlo <- brms::posterior_predict(s19,
                                      ndraws=1000,
                                      newdata = s.currlo %>% 
                                        filter(SPCD==19)) %>% colMeans()
s93.currlo <- brms::posterior_predict(s93,
                                      ndraws=1000,
                                      newdata = s.currlo %>% 
                                        filter(SPCD==93)) %>% colMeans()
s19.currhi <- brms::posterior_predict(s19,
                                      ndraws=1000,
                                      newdata = s.currhi %>% 
                                        filter(SPCD==19)) %>% colMeans()
s93.currhi <- brms::posterior_predict(s93,
                                      ndraws=1000,
                                      newdata = s.currhi %>% 
                                        filter(SPCD==93)) %>% colMeans()

s19.2060lo <- brms::posterior_predict(s19,
                                      ndraws=1000,
                                      newdata = s.2060lo %>% 
                                        filter(SPCD==19)) %>% colMeans()
s93.2060lo <- brms::posterior_predict(s93,
                                      ndraws=1000,
                                      newdata = s.2060lo %>% 
                                        filter(SPCD==93)) %>% colMeans()
s19.2060hi <- brms::posterior_predict(s19,
                                      ndraws=1000,
                                      newdata = s.2060hi %>% 
                                        filter(SPCD==19)) %>% colMeans()
s93.2060hi <- brms::posterior_predict(s93,
                                      ndraws=1000,
                                      newdata = s.2060hi %>% 
                                        filter(SPCD==93)) %>% colMeans()

s19.2090lo <- brms::posterior_predict(s19,
                                      ndraws=1000,
                                      newdata = s.2090lo %>% 
                                        filter(SPCD==19)) %>% colMeans()
s93.2090lo <- brms::posterior_predict(s93,
                                      ndraws=1000,
                                      newdata = s.2090lo %>% 
                                        filter(SPCD==93)) %>% colMeans()
s19.2090hi <- brms::posterior_predict(s19,
                                      ndraws=1000,
                                      newdata = s.2090hi %>% 
                                        filter(SPCD==19)) %>% colMeans()
s93.2090hi <- brms::posterior_predict(s93,
                                      ndraws=1000,
                                      newdata = s.2090hi %>% 
                                        filter(SPCD==93)) %>% colMeans()

#SAPS

r19.currlo <- brms::posterior_predict(r19,
                                      ndraws=1000,
                                      newdata = r.currlo %>% 
                                        filter(SPCD==19)) %>% colMeans()
r93.currlo <- brms::posterior_predict(r93,
                                      ndraws=1000,
                                      newdata = r.currlo %>% 
                                        filter(SPCD==93)) %>% colMeans()
r19.currhi <- brms::posterior_predict(r19,
                                      ndraws=1000,
                                      newdata = r.currhi %>% 
                                        filter(SPCD==19)) %>% colMeans()
r93.currhi <- brms::posterior_predict(r93,
                                      ndraws=1000,
                                      newdata = r.currhi %>% 
                                        filter(SPCD==93)) %>% colMeans()

r19.2060lo <- brms::posterior_predict(r19,
                                      ndraws=1000,
                                      newdata = r.2060lo %>% 
                                        filter(SPCD==19)) %>% colMeans()
r93.2060lo <- brms::posterior_predict(r93,
                                      ndraws=1000,
                                      newdata = r.2060lo %>% 
                                        filter(SPCD==93)) %>% colMeans()
r19.2060hi <- brms::posterior_predict(r19,
                                      ndraws=1000,
                                      newdata = r.2060hi %>% 
                                        filter(SPCD==19)) %>% colMeans()
r93.2060hi <- brms::posterior_predict(r93,
                                      ndraws=1000,
                                      newdata = r.2060hi %>% 
                                        filter(SPCD==93)) %>% colMeans()

r19.2090lo <- brms::posterior_predict(r19,
                                      ndraws=1000,
                                      newdata = r.2090lo %>% 
                                        filter(SPCD==19)) %>% colMeans()
r93.2090lo <- brms::posterior_predict(r93,
                                      ndraws=1000,
                                      newdata = r.2090lo %>% 
                                        filter(SPCD==93)) %>% colMeans()
r19.2090hi <- brms::posterior_predict(r19,
                                      ndraws=1000,
                                      newdata = r.2090hi %>% 
                                        filter(SPCD==19)) %>% colMeans()
r93.2090hi <- brms::posterior_predict(r93,
                                      ndraws=1000,
                                      newdata = r.2090hi %>% 
                                        filter(SPCD==93)) %>% colMeans()

#### binding predictions together

p19.preds <- ind.mort.dat %>%
  filter(SPCD==19) %>%
  mutate(m19.currlo = 1-m19.currlo,
         m19.currhi = 1-m19.currhi,
         m19.2060lo = 1-m19.2060lo,
         m19.2060hi = 1-m19.2060hi,
         m19.2090lo = 1-m19.2090lo,
         m19.2090hi = 1-m19.2090hi) %>%
  group_by(mult.comp.coexist,ECOSUBCD) %>%
  summarise(across(m19.currlo:m19.2090hi,mean)) %>%
  left_join(seed.dat %>%
              filter(SPCD==19) %>%
              mutate(s19.currlo = s19.currlo,
                     s19.currhi = s19.currhi,
                     s19.2060lo = s19.2060lo,
                     s19.2060hi = s19.2060hi,
                     s19.2090lo = s19.2090lo,
                     s19.2090hi = s19.2090hi,
                     r19.currlo = r19.currlo,
                     r19.currhi = r19.currhi,
                     r19.2060lo = r19.2060lo,
                     r19.2060hi = r19.2060hi,
                     r19.2090lo = r19.2090lo,
                     r19.2090hi = r19.2090hi) %>%
              group_by(mult.comp.coexist,ECOSUBCD) %>%
              summarise(across(s19.currlo:r19.2090hi,mean)),
            by=c("mult.comp.coexist","ECOSUBCD"))

p93.preds <- ind.mort.dat %>%
  filter(SPCD==93) %>%
  mutate(m93.currlo = 1-m93.currlo,
         m93.currhi = 1-m93.currhi,
         m93.2060lo = 1-m93.2060lo,
         m93.2060hi = 1-m93.2060hi,
         m93.2090lo = 1-m93.2090lo,
         m93.2090hi = 1-m93.2090hi) %>%
  group_by(mult.comp.coexist,ECOSUBCD) %>%
  summarise(across(m93.currlo:m93.2090hi,mean)) %>%
  left_join(seed.dat %>%
              filter(SPCD==93) %>%
              mutate(s93.currlo = s93.currlo,
                     s93.currhi = s93.currhi,
                     s93.2060lo = s93.2060lo,
                     s93.2060hi = s93.2060hi,
                     s93.2090lo = s93.2090lo,
                     s93.2090hi = s93.2090hi,
                     r93.currlo = r93.currlo,
                     r93.currhi = r93.currhi,
                     r93.2060lo = r93.2060lo,
                     r93.2060hi = r93.2060hi,
                     r93.2090lo = r93.2090lo,
                     r93.2090hi = r93.2090hi) %>%
              group_by(mult.comp.coexist,ECOSUBCD) %>%
              summarise(across(s93.currlo:r93.2090hi,mean)),
            by=c("mult.comp.coexist","ECOSUBCD"))


p.m.preds <- p19.preds %>% 
  left_join(p93.preds,
            by = c("ECOSUBCD","mult.comp.coexist")) %>% 
  ungroup()

#### multinomial prediction

p.m.class <- p.m.preds %>% 
  mutate(class.currlo = predict(m.m, newdata = p.m.preds %>% 
                            mutate(m.pred.19 = m19.currlo,
                                   m.pred.93 = m93.currlo,
                                   s.pred.19 = s19.currlo,
                                   s.pred.93 = s93.currlo,
                                   r.pred.19 = r19.currlo,
                                   r.pred.93 = r93.currlo), type="class"),
         class.currhi = predict(m.m, newdata = p.m.preds %>% 
                            mutate(m.pred.19 = m19.currhi,
                                   m.pred.93 = m93.currhi,
                                   s.pred.19 = s19.currhi,
                                   s.pred.93 = s93.currhi,
                                   r.pred.19 = r19.currhi,
                                   r.pred.93 = r93.currhi), type="class"),
         class.2060lo = predict(m.m, newdata = p.m.preds %>% 
                            mutate(m.pred.19 = m19.2060lo,
                                   m.pred.93 = m93.2060lo,
                                   s.pred.19 = s19.2060lo,
                                   s.pred.93 = s93.2060lo,
                                   r.pred.19 = r19.2060lo,
                                   r.pred.93 = r93.2060lo), type="class"),
         class.2060hi = predict(m.m, newdata = p.m.preds %>% 
                            mutate(m.pred.19 = m19.2060hi,
                                   m.pred.93 = m93.2060hi,
                                   s.pred.19 = s19.2060hi,
                                   s.pred.93 = s93.2060hi,
                                   r.pred.19 = r19.2060hi,
                                   r.pred.93 = r93.2060hi), type="class"),
         class.2090lo = predict(m.m, newdata = p.m.preds %>% 
                            mutate(m.pred.19 = m19.2090lo,
                                   m.pred.93 = m93.2090lo,
                                   s.pred.19 = s19.2090lo,
                                   s.pred.93 = s93.2090lo,
                                   r.pred.19 = r19.2090lo,
                                   r.pred.93 = r93.2090lo), type="class"),
         class.2090hi = predict(m.m, newdata = p.m.preds %>% 
                            mutate(m.pred.19 = m19.2090hi,
                                   m.pred.93 = m93.2090hi,
                                   s.pred.19 = s19.2090hi,
                                   s.pred.93 = s93.2090hi,
                                   r.pred.19 = r19.2090hi,
                                   r.pred.93 = r93.2090hi), type="class"))

### attempting a figure

p.m.class %>% 
  select(ECOSUBCD, class.currlo:class.2090hi) %>% 
  pivot_longer(., cols=class.currlo:class.2090hi, 
               values_to="class",
               names_to="scenario") %>%
  na.omit() %>% 
  filter(scenario%in%c("class.currlo","class.2060lo","class.2090lo")) %>% 
  ggplot(.,
         aes(x = factor(scenario,levels=c("class.currlo",
                                          "class.2060lo",
                                          "class.2090lo")),
             fill = factor(class, levels = c("replacement",
                                             "compositional change",
                                             "structural change",
                                             "resilience")))) +
  geom_bar(stat = "count") +
  scale_fill_manual(name = "joint trajectory",
                    values = c("resilience" = "dodgerblue2",
                               "structural change" = "gold2",
                               "compositional change" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("fill")) +
  labs(x = "time")
  

p.m.class %>% 
  select(ECOSUBCD, class.currlo:class.2090hi) %>% 
  pivot_longer(., cols=class.currlo:class.2090hi, 
               values_to="class",
               names_to="scenario") %>%
  na.omit() %>% 
  filter(scenario%in%c("class.currhi","class.2060hi","class.2090hi")) %>% 
  ggplot(.,
         aes(x = factor(scenario,levels=c("class.currhi",
                                          "class.2060hi",
                                          "class.2090hi")),
             fill = factor(class, levels = c("replacement",
                                             "compositional change",
                                             "structural change",
                                             "resilience")))) +
  geom_bar(stat = "count") +
  scale_fill_manual(name = "joint trajectory",
                    values = c("resilience" = "dodgerblue2",
                               "structural change" = "gold2",
                               "compositional change" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("fill")) +
  labs(x = "time")
















