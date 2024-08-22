clim2020 <- read.csv("D:/abla_pien_coords_13GCMs_ensemble_ssp370_2011-2020Y.csv")
clim2050 <- read.csv("D:/abla_pien_coords_13GCMs_ensemble_ssp370_2041-2050Y.csv")
clim2080 <- read.csv("D:/abla_pien_coords_13GCMs_ensemble_ssp370_2071-2080Y.csv")

load("data/bayes_multinom.Rdata")

clim2020 <- clim2020 %>% 
  select(PLT_CN=ID1, Year, 7:31) %>% 
  mutate(across(where(is.numeric), .fns = ~ifelse(.x==-9999,NA_real_,.x))) %>%
  group_by(PLT_CN) %>%
  summarise(across(MAT:DD1040, ~mean(.x,na.rm=T), .names="{.col}_2020ssp370_mean"),
            across(MAT:DD1040, ~max(.x,na.rm=T), .names="{.col}_2020ssp370_max"),
            across(MAT:DD1040, ~min(.x,na.rm=T), .names="{.col}_2020ssp370_min"),
            across(MAT:DD1040, ~sd(.x,na.rm=T), .names="{.col}_2020ssp370_sd")) %>% 
  select(PLT_CN, MAT_2020ssp370_max, CMD_2020ssp370_max)

clim2050 <- clim2050 %>% 
  select(PLT_CN=ID1, Year, 7:31) %>% 
  mutate(across(where(is.numeric), .fns = ~ifelse(.x==-9999,NA_real_,.x))) %>%
  group_by(PLT_CN) %>%
  summarise(across(MAT:DD1040, ~mean(.x,na.rm=T), .names="{.col}_2050ssp370_mean"),
            across(MAT:DD1040, ~max(.x,na.rm=T), .names="{.col}_2050ssp370_max"),
            across(MAT:DD1040, ~min(.x,na.rm=T), .names="{.col}_2050ssp370_min"),
            across(MAT:DD1040, ~sd(.x,na.rm=T), .names="{.col}_2050ssp370_sd")) %>% 
  select(PLT_CN, MAT_2050ssp370_max, CMD_2050ssp370_max)

clim2080 <- clim2080 %>% 
  select(PLT_CN=ID1, Year, 7:31) %>% 
  mutate(across(where(is.numeric), .fns = ~ifelse(.x==-9999,NA_real_,.x))) %>%
  group_by(PLT_CN) %>%
  summarise(across(MAT:DD1040, ~mean(.x,na.rm=T), .names="{.col}_2080ssp370_mean"),
            across(MAT:DD1040, ~max(.x,na.rm=T), .names="{.col}_2080ssp370_max"),
            across(MAT:DD1040, ~min(.x,na.rm=T), .names="{.col}_2080ssp370_min"),
            across(MAT:DD1040, ~sd(.x,na.rm=T), .names="{.col}_2080ssp370_sd")) %>% 
  select(PLT_CN, MAT_2080ssp370_max, CMD_2080ssp370_max)

ind.mort.dat <- ind.mort.dat %>% 
  left_join(clim2020,by="PLT_CN") %>% 
  left_join(clim2050, by="PLT_CN") %>% 
  left_join(clim2080, by="PLT_CN") %>% 
  mutate(MAT_maxanom_2020 = (MAT_2020ssp370_max - MAT_ref_mean),
         CMD_maxanom_2020 = (CMD_2020ssp370_max - CMD_ref_mean),
         MAT_maxanom_2020.z = MAT_maxanom_2020/MAT_ref_sd,
         CMD_maxanom_2020.z = CMD_maxanom_2020/CMD_ref_sd,
         MAT_maxanom_2050 = (MAT_2050ssp370_max - MAT_ref_mean),
         CMD_maxanom_2050 = (CMD_2050ssp370_max - CMD_ref_mean),
         MAT_maxanom_2050.z = MAT_maxanom_2050/MAT_ref_sd,
         CMD_maxanom_2050.z = CMD_maxanom_2050/CMD_ref_sd,
         MAT_maxanom_2080 = (MAT_2080ssp370_max - MAT_ref_mean),
         CMD_maxanom_2080 = (CMD_2080ssp370_max - CMD_ref_mean),
         MAT_maxanom_2080.z = MAT_maxanom_2080/MAT_ref_sd,
         CMD_maxanom_2080.z = CMD_maxanom_2080/CMD_ref_sd)

seed.dat <- seed.dat %>% 
  left_join(clim2020,by="PLT_CN") %>% 
  left_join(clim2050, by="PLT_CN") %>% 
  left_join(clim2080, by="PLT_CN") %>% 
  mutate(MAT_maxanom_2020 = (MAT_2020ssp370_max - MAT_ref_mean),
         CMD_maxanom_2020 = (CMD_2020ssp370_max - CMD_ref_mean),
         MAT_maxanom_2020.z = MAT_maxanom_2020/MAT_ref_sd,
         CMD_maxanom_2020.z = CMD_maxanom_2020/CMD_ref_sd,
         MAT_maxanom_2050 = (MAT_2050ssp370_max - MAT_ref_mean),
         CMD_maxanom_2050 = (CMD_2050ssp370_max - CMD_ref_mean),
         MAT_maxanom_2050.z = MAT_maxanom_2050/MAT_ref_sd,
         CMD_maxanom_2050.z = CMD_maxanom_2050/CMD_ref_sd,
         MAT_maxanom_2080 = (MAT_2080ssp370_max - MAT_ref_mean),
         CMD_maxanom_2080 = (CMD_2080ssp370_max - CMD_ref_mean),
         MAT_maxanom_2080.z = MAT_maxanom_2080/MAT_ref_sd,
         CMD_maxanom_2080.z = CMD_maxanom_2080/CMD_ref_sd)

sap.dat <- sap.dat %>% 
  left_join(clim2020,by="PLT_CN") %>% 
  left_join(clim2050, by="PLT_CN") %>% 
  left_join(clim2080, by="PLT_CN") %>% 
  mutate(MAT_maxanom_2020 = (MAT_2020ssp370_max - MAT_ref_mean),
         CMD_maxanom_2020 = (CMD_2020ssp370_max - CMD_ref_mean),
         MAT_maxanom_2020.z = MAT_maxanom_2020/MAT_ref_sd,
         CMD_maxanom_2020.z = CMD_maxanom_2020/CMD_ref_sd,
         MAT_maxanom_2050 = (MAT_2050ssp370_max - MAT_ref_mean),
         CMD_maxanom_2050 = (CMD_2050ssp370_max - CMD_ref_mean),
         MAT_maxanom_2050.z = MAT_maxanom_2050/MAT_ref_sd,
         CMD_maxanom_2050.z = CMD_maxanom_2050/CMD_ref_sd,
         MAT_maxanom_2080 = (MAT_2080ssp370_max - MAT_ref_mean),
         CMD_maxanom_2080 = (CMD_2080ssp370_max - CMD_ref_mean),
         MAT_maxanom_2080.z = MAT_maxanom_2080/MAT_ref_sd,
         CMD_maxanom_2080.z = CMD_maxanom_2080/CMD_ref_sd)

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
  mutate(area.fire.prop = lo.area,
         MAT_maxanom.z = MAT_maxanom.z,
         CMD_maxanom.z = CMD_maxanom.z)
m.currobs <- ind.mort.dat %>% 
  mutate(area.fire.prop = area.fire.prop,
         MAT_maxanom.z = MAT_maxanom.z,
         CMD_maxanom.z = CMD_maxanom.z)
m.currhi <- ind.mort.dat %>% 
  mutate(area.fire.prop = hi.area,
         MAT_maxanom.z = MAT_maxanom.z,
         CMD_maxanom.z = CMD_maxanom.z)

s.currlo <- seed.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, lo.sev),
         MAT_maxanom.z = MAT_maxanom.z,
         CMD_maxanom.z = CMD_maxanom.z)
s.currobs <- seed.dat %>% 
  mutate(fire.sev = fire.sev,
         MAT_maxanom.z = MAT_maxanom.z,
         CMD_maxanom.z = CMD_maxanom.z)
s.currhi <- seed.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, hi.sev),
         MAT_maxanom.z = MAT_maxanom.z,
         CMD_maxanom.z = CMD_maxanom.z)

r.currlo <- sap.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, lo.sev),
         MAT_maxanom.z = MAT_maxanom.z,
         CMD_maxanom.z = CMD_maxanom.z)
r.currobs <- sap.dat %>% 
  mutate(fire.sev = fire.sev,
         MAT_maxanom.z = MAT_maxanom.z,
         CMD_maxanom.z = CMD_maxanom.z)
r.currhi <- sap.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, hi.sev),
         MAT_maxanom.z = MAT_maxanom.z,
         CMD_maxanom.z = CMD_maxanom.z)

### 2050 climate

m.2050lo <- ind.mort.dat %>% 
  mutate(area.fire.prop = lo.area,
         MAT_maxanom.z = MAT_maxanom_2050.z,
         CMD_maxanom.z = CMD_maxanom_2050.z)
m.2050obs <- ind.mort.dat %>% 
  mutate(area.fire.prop = area.fire.prop,
         MAT_maxanom.z = MAT_maxanom_2050.z,
         CMD_maxanom.z = CMD_maxanom_2050.z)
m.2050hi <- ind.mort.dat %>% 
  mutate(area.fire.prop = hi.area,
         MAT_maxanom.z = MAT_maxanom_2050.z,
         CMD_maxanom.z = CMD_maxanom_2050.z)

s.2050lo <- seed.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, lo.sev),
         MAT_maxanom.z = MAT_maxanom_2050.z,
         CMD_maxanom.z = CMD_maxanom_2050.z)
s.2050obs <- seed.dat %>% 
  mutate(fire.sev = fire.sev,
         MAT_maxanom.z = MAT_maxanom_2050.z,
         CMD_maxanom.z = CMD_maxanom_2050.z)
s.2050hi <- seed.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, hi.sev),
         MAT_maxanom.z = MAT_maxanom_2050.z,
         CMD_maxanom.z = CMD_maxanom_2050.z)

r.2050lo <- sap.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, lo.sev),
         MAT_maxanom.z = MAT_maxanom_2050.z,
         CMD_maxanom.z = CMD_maxanom_2050.z)
r.2050obs <- sap.dat %>% 
  mutate(fire.sev = fire.sev,
         MAT_maxanom.z = MAT_maxanom_2050.z,
         CMD_maxanom.z = CMD_maxanom_2050.z)
r.2050hi <- sap.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, hi.sev),
         MAT_maxanom.z = MAT_maxanom_2050.z,
         CMD_maxanom.z = CMD_maxanom_2050.z)

### 2080 climate

m.2080lo <- ind.mort.dat %>% 
  mutate(area.fire.prop = lo.area,
         MAT_maxanom.z = MAT_maxanom_2080.z,
         CMD_maxanom.z = CMD_maxanom_2080.z)
m.2080obs <- ind.mort.dat %>% 
  mutate(area.fire.prop = area.fire.prop,
         MAT_maxanom.z = MAT_maxanom_2080.z,
         CMD_maxanom.z = CMD_maxanom_2080.z)
m.2080hi <- ind.mort.dat %>% 
  mutate(area.fire.prop = hi.area,
         MAT_maxanom.z = MAT_maxanom_2080.z,
         CMD_maxanom.z = CMD_maxanom_2080.z)

s.2080lo <- seed.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, lo.sev),
         MAT_maxanom.z = MAT_maxanom_2080.z,
         CMD_maxanom.z = CMD_maxanom_2080.z)
s.2080obs <- seed.dat %>% 
  mutate(fire.sev = fire.sev,
         MAT_maxanom.z = MAT_maxanom_2080.z,
         CMD_maxanom.z = CMD_maxanom_2080.z)
s.2080hi <- seed.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, hi.sev),
         MAT_maxanom.z = MAT_maxanom_2080.z,
         CMD_maxanom.z = CMD_maxanom_2080.z)

r.2080lo <- sap.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, lo.sev),
         MAT_maxanom.z = MAT_maxanom_2080.z,
         CMD_maxanom.z = CMD_maxanom_2080.z)
r.2080obs <- sap.dat %>% 
  mutate(fire.sev = fire.sev,
         MAT_maxanom.z = MAT_maxanom_2080.z,
         CMD_maxanom.z = CMD_maxanom_2080.z)
r.2080hi <- sap.dat %>% 
  mutate(fire.sev = ifelse(fire.sev==0, 0, hi.sev),
         MAT_maxanom.z = MAT_maxanom_2080.z,
         CMD_maxanom.z = CMD_maxanom_2080.z)

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
m19.currobs <- brms::posterior_predict(m19,
                                      ndraws=1000,
                                      newdata = m.currobs %>% 
                                        filter(SPCD==19)) %>% colMeans()
m93.currobs <- brms::posterior_predict(m93,
                                      ndraws=1000,
                                      newdata = m.currobs %>% 
                                        filter(SPCD==93)) %>% colMeans()
m19.currhi <- brms::posterior_predict(m19,
                                      ndraws=1000,
                                      newdata = m.currhi %>% 
                                        filter(SPCD==19)) %>% colMeans()
m93.currhi <- brms::posterior_predict(m93,
                                      ndraws=1000,
                                      newdata = m.currhi %>% 
                                        filter(SPCD==93)) %>% colMeans()

m19.2050lo <- brms::posterior_predict(m19,
                                      ndraws=1000,
                                      newdata = m.2050lo %>% 
                                        filter(SPCD==19)) %>% colMeans()
m93.2050lo <- brms::posterior_predict(m93,
                                      ndraws=1000,
                                      newdata = m.2050lo %>% 
                                        filter(SPCD==93)) %>% colMeans()
m19.2050obs <- brms::posterior_predict(m19,
                                      ndraws=1000,
                                      newdata = m.2050obs %>% 
                                        filter(SPCD==19)) %>% colMeans()
m93.2050obs <- brms::posterior_predict(m93,
                                      ndraws=1000,
                                      newdata = m.2050obs %>% 
                                        filter(SPCD==93)) %>% colMeans()
m19.2050hi <- brms::posterior_predict(m19,
                                      ndraws=1000,
                                      newdata = m.2050hi %>% 
                                        filter(SPCD==19)) %>% colMeans()
m93.2050hi <- brms::posterior_predict(m93,
                                      ndraws=1000,
                                      newdata = m.2050hi %>% 
                                        filter(SPCD==93)) %>% colMeans()

m19.2080lo <- brms::posterior_predict(m19,
                                      ndraws=1000,
                                      newdata = m.2080lo %>% 
                                        filter(SPCD==19)) %>% colMeans()
m93.2080lo <- brms::posterior_predict(m93,
                                      ndraws=1000,
                                      newdata = m.2080lo %>% 
                                        filter(SPCD==93)) %>% colMeans()
m19.2080obs <- brms::posterior_predict(m19,
                                      ndraws=1000,
                                      newdata = m.2080obs %>% 
                                        filter(SPCD==19)) %>% colMeans()
m93.2080obs <- brms::posterior_predict(m93,
                                      ndraws=1000,
                                      newdata = m.2080obs %>% 
                                        filter(SPCD==93)) %>% colMeans()
m19.2080hi <- brms::posterior_predict(m19,
                                      ndraws=1000,
                                      newdata = m.2080hi %>% 
                                        filter(SPCD==19)) %>% colMeans()
m93.2080hi <- brms::posterior_predict(m93,
                                      ndraws=1000,
                                      newdata = m.2080hi %>% 
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
s19.currobs <- brms::posterior_predict(s19,
                                      ndraws=1000,
                                      newdata = s.currobs %>% 
                                        filter(SPCD==19)) %>% colMeans()
s93.currobs <- brms::posterior_predict(s93,
                                      ndraws=1000,
                                      newdata = s.currobs %>% 
                                        filter(SPCD==93)) %>% colMeans()
s19.currhi <- brms::posterior_predict(s19,
                                      ndraws=1000,
                                      newdata = s.currhi %>% 
                                        filter(SPCD==19)) %>% colMeans()
s93.currhi <- brms::posterior_predict(s93,
                                      ndraws=1000,
                                      newdata = s.currhi %>% 
                                        filter(SPCD==93)) %>% colMeans()

s19.2050lo <- brms::posterior_predict(s19,
                                      ndraws=1000,
                                      newdata = s.2050lo %>% 
                                        filter(SPCD==19)) %>% colMeans()
s93.2050lo <- brms::posterior_predict(s93,
                                      ndraws=1000,
                                      newdata = s.2050lo %>% 
                                        filter(SPCD==93)) %>% colMeans()
s19.2050obs <- brms::posterior_predict(s19,
                                      ndraws=1000,
                                      newdata = s.2050obs %>% 
                                        filter(SPCD==19)) %>% colMeans()
s93.2050obs <- brms::posterior_predict(s93,
                                      ndraws=1000,
                                      newdata = s.2050obs %>% 
                                        filter(SPCD==93)) %>% colMeans()
s19.2050hi <- brms::posterior_predict(s19,
                                      ndraws=1000,
                                      newdata = s.2050hi %>% 
                                        filter(SPCD==19)) %>% colMeans()
s93.2050hi <- brms::posterior_predict(s93,
                                      ndraws=1000,
                                      newdata = s.2050hi %>% 
                                        filter(SPCD==93)) %>% colMeans()

s19.2080lo <- brms::posterior_predict(s19,
                                      ndraws=1000,
                                      newdata = s.2080lo %>% 
                                        filter(SPCD==19)) %>% colMeans()
s93.2080lo <- brms::posterior_predict(s93,
                                      ndraws=1000,
                                      newdata = s.2080lo %>% 
                                        filter(SPCD==93)) %>% colMeans()
s19.2080obs <- brms::posterior_predict(s19,
                                      ndraws=1000,
                                      newdata = s.2080obs %>% 
                                        filter(SPCD==19)) %>% colMeans()
s93.2080obs <- brms::posterior_predict(s93,
                                      ndraws=1000,
                                      newdata = s.2080obs %>% 
                                        filter(SPCD==93)) %>% colMeans()
s19.2080hi <- brms::posterior_predict(s19,
                                      ndraws=1000,
                                      newdata = s.2080hi %>% 
                                        filter(SPCD==19)) %>% colMeans()
s93.2080hi <- brms::posterior_predict(s93,
                                      ndraws=1000,
                                      newdata = s.2080hi %>% 
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
r19.currobs <- brms::posterior_predict(r19,
                                      ndraws=1000,
                                      newdata = r.currobs %>% 
                                        filter(SPCD==19)) %>% colMeans()
r93.currobs <- brms::posterior_predict(r93,
                                      ndraws=1000,
                                      newdata = r.currobs %>% 
                                        filter(SPCD==93)) %>% colMeans()
r19.currhi <- brms::posterior_predict(r19,
                                      ndraws=1000,
                                      newdata = r.currhi %>% 
                                        filter(SPCD==19)) %>% colMeans()
r93.currhi <- brms::posterior_predict(r93,
                                      ndraws=1000,
                                      newdata = r.currhi %>% 
                                        filter(SPCD==93)) %>% colMeans()

r19.2050lo <- brms::posterior_predict(r19,
                                      ndraws=1000,
                                      newdata = r.2050lo %>% 
                                        filter(SPCD==19)) %>% colMeans()
r93.2050lo <- brms::posterior_predict(r93,
                                      ndraws=1000,
                                      newdata = r.2050lo %>% 
                                        filter(SPCD==93)) %>% colMeans()
r19.2050obs <- brms::posterior_predict(r19,
                                      ndraws=1000,
                                      newdata = r.2050obs %>% 
                                        filter(SPCD==19)) %>% colMeans()
r93.2050obs <- brms::posterior_predict(r93,
                                      ndraws=1000,
                                      newdata = r.2050obs %>% 
                                        filter(SPCD==93)) %>% colMeans()
r19.2050hi <- brms::posterior_predict(r19,
                                      ndraws=1000,
                                      newdata = r.2050hi %>% 
                                        filter(SPCD==19)) %>% colMeans()
r93.2050hi <- brms::posterior_predict(r93,
                                      ndraws=1000,
                                      newdata = r.2050hi %>% 
                                        filter(SPCD==93)) %>% colMeans()

r19.2080lo <- brms::posterior_predict(r19,
                                      ndraws=1000,
                                      newdata = r.2080lo %>% 
                                        filter(SPCD==19)) %>% colMeans()
r93.2080lo <- brms::posterior_predict(r93,
                                      ndraws=1000,
                                      newdata = r.2080lo %>% 
                                        filter(SPCD==93)) %>% colMeans()
r19.2080obs <- brms::posterior_predict(r19,
                                      ndraws=1000,
                                      newdata = r.2080obs %>% 
                                        filter(SPCD==19)) %>% colMeans()
r93.2080obs <- brms::posterior_predict(r93,
                                      ndraws=1000,
                                      newdata = r.2080obs %>% 
                                        filter(SPCD==93)) %>% colMeans()
r19.2080hi <- brms::posterior_predict(r19,
                                      ndraws=1000,
                                      newdata = r.2080hi %>% 
                                        filter(SPCD==19)) %>% colMeans()
r93.2080hi <- brms::posterior_predict(r93,
                                      ndraws=1000,
                                      newdata = r.2080hi %>% 
                                        filter(SPCD==93)) %>% colMeans()

#### binding predictions together ----

p.m <- p.m %>% 
  left_join(dall.er.ablapien %>% 
              sf::st_drop_geometry() %>% 
              select(ECOSUBCD = MAP_UNIT_S,
                     AREA = AREA_TOTAL_ha_19))

p19.preds <- ind.mort.dat %>%
  filter(SPCD==19) %>%
  mutate(m19.currlo = 1-m19.currlo,
         m19.currobs = 1-m19.currobs,
         m19.currhi = 1-m19.currhi,
         m19.2050lo = 1-m19.2050lo,
         m19.2050obs = 1-m19.2050obs,
         m19.2050hi = 1-m19.2050hi,
         m19.2080lo = 1-m19.2080lo,
         m19.2080obs = 1-m19.2080obs,
         m19.2080hi = 1-m19.2080hi) %>%
  group_by(mult.comp.coexist,ECOSUBCD) %>%
  summarise(across(m19.currlo:m19.2080hi,mean)) %>%
  left_join(seed.dat %>%
              filter(SPCD==19) %>%
              mutate(s19.currlo = s19.currlo,
                     s19.currobs = s19.currobs,
                     s19.currhi = s19.currhi,
                     s19.2050lo = s19.2050lo,
                     s19.2050obs = s19.2050obs,
                     s19.2050hi = s19.2050hi,
                     s19.2080lo = s19.2080lo,
                     s19.2080obs = s19.2080obs,
                     s19.2080hi = s19.2080hi,
                     r19.currlo = r19.currlo,
                     r19.currobs = r19.currobs,
                     r19.currhi = r19.currhi,
                     r19.2050lo = r19.2050lo,
                     r19.2050obs = r19.2050obs,
                     r19.2050hi = r19.2050hi,
                     r19.2080lo = r19.2080lo,
                     r19.2080obs = r19.2080obs,
                     r19.2080hi = r19.2080hi) %>%
              group_by(mult.comp.coexist,ECOSUBCD) %>%
              summarise(across(s19.currlo:r19.2080hi,mean)),
            by=c("mult.comp.coexist","ECOSUBCD"))

p93.preds <- ind.mort.dat %>%
  filter(SPCD==93) %>%
  mutate(m93.currlo = 1-m93.currlo,
         m93.currobs = 1-m93.currobs,
         m93.currhi = 1-m93.currhi,
         m93.2050lo = 1-m93.2050lo,
         m93.2050obs = 1-m93.2050obs,
         m93.2050hi = 1-m93.2050hi,
         m93.2080lo = 1-m93.2080lo,
         m93.2080obs = 1-m93.2080obs,
         m93.2080hi = 1-m93.2080hi) %>%
  group_by(mult.comp.coexist,ECOSUBCD) %>%
  summarise(across(m93.currlo:m93.2080hi,mean)) %>%
  left_join(seed.dat %>%
              filter(SPCD==93) %>%
              mutate(s93.currlo = s93.currlo,
                     s93.currobs = s93.currobs,
                     s93.currhi = s93.currhi,
                     s93.2050lo = s93.2050lo,
                     s93.2050obs = s93.2050obs,
                     s93.2050hi = s93.2050hi,
                     s93.2080lo = s93.2080lo,
                     s93.2080obs = s93.2080obs,
                     s93.2080hi = s93.2080hi,
                     r93.currlo = r93.currlo,
                     r93.currobs = r93.currobs,
                     r93.currhi = r93.currhi,
                     r93.2050lo = r93.2050lo,
                     r93.2050obs = r93.2050obs,
                     r93.2050hi = r93.2050hi,
                     r93.2080lo = r93.2080lo,
                     r93.2080obs = r93.2080obs,
                     r93.2080hi = r93.2080hi) %>%
              group_by(mult.comp.coexist,ECOSUBCD) %>%
              summarise(across(s93.currlo:r93.2080hi,mean)),
            by=c("mult.comp.coexist","ECOSUBCD"))


p.m.preds <- p19.preds %>% 
  left_join(p93.preds,
            by = c("ECOSUBCD","mult.comp.coexist")) %>% 
  ungroup() %>% 
  na.omit()

#### multinomial prediction ----
p.m.test <- p.m.preds %>% 
  select(ECOSUBCD, mult.comp.coexist) %>% 
  left_join(brms::posterior_predict(try, newdata = p.m.preds %>% 
                                      mutate(m.pred.19 = m19.currlo,
                                             m.pred.93 = m93.currlo,
                                             s.pred.19 = s19.currlo,
                                             s.pred.93 = s93.currlo,
                                             r.pred.19 = r19.currlo,
                                             r.pred.93 = r93.currlo,
                                             size=1),
                                    ndraws=1000) %>% 
              colMeans() %>% as.data.frame() %>% 
              mutate(ECOSUBCD = p.m.preds$ECOSUBCD,
                     pred.type = "currlo",
                     time = "curr",
                     dist = "lo"),
            by="ECOSUBCD") %>% 
  bind_rows(brms::posterior_predict(try, newdata = p.m.preds %>% 
                                      mutate(m.pred.19 = m19.currobs,
                                             m.pred.93 = m93.currobs,
                                             s.pred.19 = s19.currobs,
                                             s.pred.93 = s93.currobs,
                                             r.pred.19 = r19.currobs,
                                             r.pred.93 = r93.currobs,
                                             size=1),
                                    ndraws=1000) %>% 
              colMeans() %>% as.data.frame() %>% 
              mutate(ECOSUBCD = p.m.preds$ECOSUBCD,
                     mult.comp.coexist = p.m.preds$mult.comp.coexist,
                     pred.type = "currobs",
                     time = "curr",
                     dist = "obs")) %>% 
  bind_rows(brms::posterior_predict(try, newdata = p.m.preds %>% 
                                      mutate(m.pred.19 = m19.currhi,
                                             m.pred.93 = m93.currhi,
                                             s.pred.19 = s19.currhi,
                                             s.pred.93 = s93.currhi,
                                             r.pred.19 = r19.currhi,
                                             r.pred.93 = r93.currhi,
                                             size=1),
                                    ndraws=1000) %>% 
              colMeans() %>% as.data.frame() %>% 
              mutate(ECOSUBCD = p.m.preds$ECOSUBCD,
                     mult.comp.coexist = p.m.preds$mult.comp.coexist,
                     pred.type = "currhi",
                     time = "curr",
                     dist = "hi")) %>% 
  bind_rows(brms::posterior_predict(try, newdata = p.m.preds %>% 
                                      mutate(m.pred.19 = m19.2050lo,
                                             m.pred.93 = m93.2050lo,
                                             s.pred.19 = s19.2050lo,
                                             s.pred.93 = s93.2050lo,
                                             r.pred.19 = r19.2050lo,
                                             r.pred.93 = r93.2050lo,
                                             size=1),
                                    ndraws=1000) %>% 
              colMeans() %>% as.data.frame() %>% 
              mutate(ECOSUBCD = p.m.preds$ECOSUBCD,
                     mult.comp.coexist = p.m.preds$mult.comp.coexist,
                     pred.type = "2050lo",
                     time = "2050",
                     dist = "lo")) %>% 
  bind_rows(brms::posterior_predict(try, newdata = p.m.preds %>% 
                                      mutate(m.pred.19 = m19.2050obs,
                                             m.pred.93 = m93.2050obs,
                                             s.pred.19 = s19.2050obs,
                                             s.pred.93 = s93.2050obs,
                                             r.pred.19 = r19.2050obs,
                                             r.pred.93 = r93.2050obs,
                                             size=1),
                                    ndraws=1000) %>% 
              colMeans() %>% as.data.frame() %>% 
              mutate(ECOSUBCD = p.m.preds$ECOSUBCD,
                     mult.comp.coexist = p.m.preds$mult.comp.coexist,
                     pred.type = "2050obs",
                     time = "2050",
                     dist = "obs")) %>% 
  bind_rows(brms::posterior_predict(try, newdata = p.m.preds %>% 
                                      mutate(m.pred.19 = m19.2050hi,
                                             m.pred.93 = m93.2050hi,
                                             s.pred.19 = s19.2050hi,
                                             s.pred.93 = s93.2050hi,
                                             r.pred.19 = r19.2050hi,
                                             r.pred.93 = r93.2050hi,
                                             size=1),
                                    ndraws=1000) %>% 
              colMeans() %>% as.data.frame() %>% 
              mutate(ECOSUBCD = p.m.preds$ECOSUBCD,
                     mult.comp.coexist = p.m.preds$mult.comp.coexist,
                     pred.type = "2050hi",
                     time = "2050",
                     dist = "hi")) %>% 
  bind_rows(brms::posterior_predict(try, newdata = p.m.preds %>% 
                                      mutate(m.pred.19 = m19.2080lo,
                                             m.pred.93 = m93.2080lo,
                                             s.pred.19 = s19.2080lo,
                                             s.pred.93 = s93.2080lo,
                                             r.pred.19 = r19.2080lo,
                                             r.pred.93 = r93.2080lo,
                                             size=1),
                                    ndraws=1000) %>% 
              colMeans() %>% as.data.frame() %>% 
              mutate(ECOSUBCD = p.m.preds$ECOSUBCD,
                     mult.comp.coexist = p.m.preds$mult.comp.coexist,
                     pred.type = "2080lo",
                     time = "2080",
                     dist = "lo")) %>% 
  bind_rows(brms::posterior_predict(try, newdata = p.m.preds %>% 
                                      mutate(m.pred.19 = m19.2080obs,
                                             m.pred.93 = m93.2080obs,
                                             s.pred.19 = s19.2080obs,
                                             s.pred.93 = s93.2080obs,
                                             r.pred.19 = r19.2080obs,
                                             r.pred.93 = r93.2080obs,
                                             size=1),
                                    ndraws=1000) %>% 
              colMeans() %>% as.data.frame() %>% 
              mutate(ECOSUBCD = p.m.preds$ECOSUBCD,
                     mult.comp.coexist = p.m.preds$mult.comp.coexist,
                     pred.type = "2080obs",
                     time = "2080",
                     dist = "obs")) %>% 
  bind_rows(brms::posterior_predict(try, newdata = p.m.preds %>% 
                                      mutate(m.pred.19 = m19.2080hi,
                                             m.pred.93 = m93.2080hi,
                                             s.pred.19 = s19.2080hi,
                                             s.pred.93 = s93.2080hi,
                                             r.pred.19 = r19.2080hi,
                                             r.pred.93 = r93.2080hi,
                                             size=1),
                                    ndraws=1000) %>% 
              colMeans() %>% as.data.frame() %>% 
              mutate(ECOSUBCD = p.m.preds$ECOSUBCD,
                     mult.comp.coexist = p.m.preds$mult.comp.coexist,
                     pred.type = "2080hi",
                     time = "2080",
                     dist = "hi")) %>% 
  left_join(p.m %>% 
              select(ECOSUBCD,AREA),
            by="ECOSUBCD")

