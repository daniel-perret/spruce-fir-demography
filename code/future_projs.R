#### 
# 
# read.csv("D:/coords_format.csv",header=T) %>%
#   filter(ID1 %in% unique(ind.mort.dat$PLT_CN)) %>%
#   mutate(ID1 = as.character(ID1),
#          ID2 = as.character(ID2)) %>% 
#   write.csv(.,file="D:/abla_pien_coords.csv")

#### reading in and formatting Future climate data for 2050-2060 decade ----

clim2060 <- read.csv("D:/abla_pien_coords3_13GCMs_ensemble_ssp585_2050-2060Y.csv")
clim2020_2050 <- read.csv("D:/abla_pien_coords_13GCMs_ensemble_ssp585_2020-2050Y.csv")

clim2060 <- clim2060 %>% 
  select(PLT_CN=ID1, Year, 7:31) %>% 
  mutate(across(where(is.numeric), .fns = ~ifelse(.x==-9999,NA_real_,.x))) %>%
  group_by(PLT_CN) %>%
  summarise(across(MAT:DD1040, ~mean(.x,na.rm=T), .names="{.col}_2060ssp585_mean"),
            across(MAT:DD1040, ~max(.x,na.rm=T), .names="{.col}_2060ssp585_max"),
            across(MAT:DD1040, ~min(.x,na.rm=T), .names="{.col}_2060ssp585_min"),
            across(MAT:DD1040, ~sd(.x,na.rm=T), .names="{.col}_2060ssp585_sd")) %>% 
  select(PLT_CN, MAT_2060ssp585_max, CMD_2060ssp585_max)

clim2020_2050 <- clim2020_2050 %>% 
  select(PLT_CN=ID1, Year, 7:31) %>% 
  mutate(across(where(is.numeric), .fns = ~ifelse(.x==-9999,NA_real_,.x))) %>%
  group_by(PLT_CN) %>%
  summarise(across(MAT:DD1040, ~mean(.x,na.rm=T), .names="{.col}_futref_ssp585_mean"),
            across(MAT:DD1040, ~sd(.x,na.rm=T), .names="{.col}_futref_ssp585_sd")) %>% 
  select(PLT_CN, MAT_futref_ssp585_mean, MAP_futref_ssp585_mean)


ind.mort.dat <- ind.mort.dat %>% 
  left_join(clim2060, by="PLT_CN") %>% 
  mutate(MAT_maxanom_2060 = (MAT_2060ssp585_max - MAT_ref_mean),
         CMD_maxanom_2060 = (CMD_2060ssp585_max - CMD_ref_mean),
         MAT_maxanom_2060.z = MAT_maxanom_2060/MAT_ref_sd,
         CMD_maxanom_2060.z = CMD_maxanom_2060/CMD_ref_sd) %>% 
  left_join(clim2020_2050, by = "PLT_CN")

seed.dat <- seed.dat %>% 
  left_join(clim2060, by="PLT_CN") %>% 
  mutate(MAT_maxanom_2060 = (MAT_2060ssp585_max - MAT_ref_mean),
         CMD_maxanom_2060 = (CMD_2060ssp585_max - CMD_ref_mean),
         MAT_maxanom_2060.z = MAT_maxanom_2060/MAT_ref_sd,
         CMD_maxanom_2060.z = CMD_maxanom_2060/CMD_ref_sd) %>% 
  left_join(clim2020_2050, by = "PLT_CN")


sap.dat <- sap.dat %>% 
  left_join(clim2060, by="PLT_CN") %>% 
  mutate(MAT_maxanom_2060 = (MAT_2060ssp585_max - MAT_ref_mean),
         CMD_maxanom_2060 = (CMD_2060ssp585_max - CMD_ref_mean),
         MAT_maxanom_2060.z = MAT_maxanom_2060/MAT_ref_sd,
         CMD_maxanom_2060.z = CMD_maxanom_2060/CMD_ref_sd) %>% 
  left_join(clim2020_2050, by = "PLT_CN")
# posterior predictions with 2050-2060 CLIMATE (with refs); CURRENT DISTURBANCE ----
# 
# m19.pred <- brms::posterior_predict(m19, 
#                                     ndraws=1000,
#                                     newdata = ind.mort.dat %>% 
#                                       filter(SPCD==19) %>% 
#                                       select(-MAT_maxanom.z, -CMD_maxanom.z,
#                                              -MAT_ref_mean, -MAP_ref_mean) %>% 
#                                       rename(MAT_maxanom.z = MAT_maxanom_2060.z,
#                                              CMD_maxanom.z = CMD_maxanom_2060.z,
#                                              MAT_ref_mean = MAT_futref_ssp585_mean,
#                                              MAP_ref_mean = MAP_futref_ssp585_mean)) %>% 
#   colMeans()
# 
# m93.pred <- brms::posterior_predict(m93, 
#                                     ndraws=1000,
#                                     newdata = ind.mort.dat %>% 
#                                       filter(SPCD==93) %>% 
#                                       select(-MAT_maxanom.z, -CMD_maxanom.z,
#                                              -MAT_ref_mean, -MAP_ref_mean) %>%
#                                       rename(MAT_maxanom.z = MAT_maxanom_2060.z,
#                                              CMD_maxanom.z = CMD_maxanom_2060.z,
#                                              MAT_ref_mean = MAT_futref_ssp585_mean,
#                                              MAP_ref_mean = MAP_futref_ssp585_mean)) %>%
#   colMeans()
# 
# s19.pred <- brms::posterior_predict(s19, 
#                                     ndraws=1000,
#                                     newdata = seed.dat %>% 
#                                       filter(SPCD==19) %>% 
#                                       select(-MAT_maxanom.z, -CMD_maxanom.z,
#                                              -MAT_ref_mean, -MAP_ref_mean) %>%
#                                       rename(MAT_maxanom.z = MAT_maxanom_2060.z,
#                                              CMD_maxanom.z = CMD_maxanom_2060.z,
#                                              MAT_ref_mean = MAT_futref_ssp585_mean,
#                                              MAP_ref_mean = MAP_futref_ssp585_mean)) %>%
#   colMeans()
# 
# s93.pred <- brms::posterior_predict(s93, 
#                                     ndraws=1000,
#                                     newdata = seed.dat %>% 
#                                       filter(SPCD==93) %>% 
#                                       select(-MAT_maxanom.z, -CMD_maxanom.z,
#                                              -MAT_ref_mean, -MAP_ref_mean) %>%
#                                       rename(MAT_maxanom.z = MAT_maxanom_2060.z,
#                                              CMD_maxanom.z = CMD_maxanom_2060.z,
#                                              MAT_ref_mean = MAT_futref_ssp585_mean,
#                                              MAP_ref_mean = MAP_futref_ssp585_mean)) %>%
#   colMeans()
# 
# r19.pred <- brms::posterior_predict(r19, 
#                                     ndraws=1000,
#                                     newdata = sap.dat %>% 
#                                       filter(SPCD==93) %>% 
#                                       select(-MAT_maxanom.z, -CMD_maxanom.z,
#                                              -MAT_ref_mean, -MAP_ref_mean) %>%
#                                       rename(MAT_maxanom.z = MAT_maxanom_2060.z,
#                                              CMD_maxanom.z = CMD_maxanom_2060.z,
#                                              MAT_ref_mean = MAT_futref_ssp585_mean,
#                                              MAP_ref_mean = MAP_futref_ssp585_mean)) %>%
#   colMeans()
# 
# r93.pred <- brms::posterior_predict(r93, 
#                                     ndraws=1000,
#                                     newdata = sap.dat %>% 
#                                       filter(SPCD==93) %>% 
#                                       select(-MAT_maxanom.z, -CMD_maxanom.z,
#                                              -MAT_ref_mean, -MAP_ref_mean) %>%
#                                       rename(MAT_maxanom.z = MAT_maxanom_2060.z,
#                                              CMD_maxanom.z = CMD_maxanom_2060.z,
#                                              MAT_ref_mean = MAT_futref_ssp585_mean,
#                                              MAP_ref_mean = MAP_futref_ssp585_mean)) %>%
#   colMeans()




# posterior predictions with 2050-2060 CLIMATE (NO refs); CURRENT DISTURBANCE ----

m19.pred <- brms::posterior_predict(m19, 
                                    ndraws=1000,
                                    newdata = ind.mort.dat %>% 
                                      filter(SPCD==19) %>% 
                                      select(-MAT_maxanom.z, -CMD_maxanom.z) %>% 
                                      rename(MAT_maxanom.z = MAT_maxanom_2060.z,
                                             CMD_maxanom.z = CMD_maxanom_2060.z)) %>% 
  colMeans()

m93.pred <- brms::posterior_predict(m93, 
                                    ndraws=1000,
                                    newdata = ind.mort.dat %>% 
                                      filter(SPCD==93) %>% 
                                      select(-MAT_maxanom.z, -CMD_maxanom.z) %>%
                                      rename(MAT_maxanom.z = MAT_maxanom_2060.z,
                                             CMD_maxanom.z = CMD_maxanom_2060.z)) %>%
  colMeans()

s19.pred <- brms::posterior_predict(s19, 
                                    ndraws=1000,
                                    newdata = seed.dat %>% 
                                      filter(SPCD==19) %>% 
                                      select(-MAT_maxanom.z, -CMD_maxanom.z) %>%
                                      rename(MAT_maxanom.z = MAT_maxanom_2060.z,
                                             CMD_maxanom.z = CMD_maxanom_2060.z)) %>%
  colMeans()

s93.pred <- brms::posterior_predict(s93, 
                                    ndraws=1000,
                                    newdata = seed.dat %>% 
                                      filter(SPCD==93) %>% 
                                      select(-MAT_maxanom.z, -CMD_maxanom.z) %>%
                                      rename(MAT_maxanom.z = MAT_maxanom_2060.z,
                                             CMD_maxanom.z = CMD_maxanom_2060.z)) %>%
  colMeans()

r19.pred <- brms::posterior_predict(r19, 
                                    ndraws=1000,
                                    newdata = sap.dat %>% 
                                      filter(SPCD==93) %>% 
                                      select(-MAT_maxanom.z, -CMD_maxanom.z) %>%
                                      rename(MAT_maxanom.z = MAT_maxanom_2060.z,
                                             CMD_maxanom.z = CMD_maxanom_2060.z)) %>%
  colMeans()

r93.pred <- brms::posterior_predict(r93, 
                                    ndraws=1000,
                                    newdata = sap.dat %>% 
                                      filter(SPCD==93) %>% 
                                      select(-MAT_maxanom.z, -CMD_maxanom.z) %>%
                                      rename(MAT_maxanom.z = MAT_maxanom_2060.z,
                                             CMD_maxanom.z = CMD_maxanom_2060.z)) %>%
  colMeans()

## 2050-2060 climate, no refs, +10% disturbance -----

# there are some choices to make here about how to implement this. I think that what makes the most sense is perhaps... adding 10% to the disturbance footprint and assigning mean severity to random % of plots in ecoregion... let's play with this.

delt <- 0.75

m19.pred <- brms::posterior_predict(m19, 
                                    ndraws=1000,
                                    newdata = ind.mort.dat %>% 
                                      filter(SPCD==19) %>% 
                                      select(-MAT_maxanom.z, -CMD_maxanom.z) %>% 
                                      rename(MAT_maxanom.z = MAT_maxanom_2060.z,
                                             CMD_maxanom.z = CMD_maxanom_2060.z) %>% 
                                      mutate(area.fire.prop = area.fire.prop*delt,
                                             area.fire.prop = ifelse(area.fire.prop>1,1,
                                                                     area.fire.prop))) %>% 
  colMeans()

m93.pred <- brms::posterior_predict(m93, 
                                    ndraws=1000,
                                    newdata = ind.mort.dat %>% 
                                      filter(SPCD==93) %>% 
                                      select(-MAT_maxanom.z, -CMD_maxanom.z) %>%
                                      rename(MAT_maxanom.z = MAT_maxanom_2060.z,
                                             CMD_maxanom.z = CMD_maxanom_2060.z) %>% 
                                      mutate(area.fire.prop = area.fire.prop*delt,
                                             area.fire.prop = ifelse(area.fire.prop>1,1,
                                                                     area.fire.prop))) %>%
  colMeans()

s19.pred <- brms::posterior_predict(s19, 
                                    ndraws=1000,
                                    newdata = seed.dat %>% 
                                      filter(SPCD==19) %>% 
                                      select(-MAT_maxanom.z, -CMD_maxanom.z) %>%
                                      rename(MAT_maxanom.z = MAT_maxanom_2060.z,
                                             CMD_maxanom.z = CMD_maxanom_2060.z) %>% 
                                      mutate(area.fire.prop = area.fire.prop*delt,
                                             area.fire.prop = ifelse(area.fire.prop>1,1,
                                                                     area.fire.prop),
                                             fire.sev = fire.sev*delt,
                                             fire.sev = ifelse(fire.sev>1,1,
                                                               fire.sev))) %>%
  colMeans()

s93.pred <- brms::posterior_predict(s93, 
                                    ndraws=1000,
                                    newdata = seed.dat %>% 
                                      filter(SPCD==93) %>% 
                                      select(-MAT_maxanom.z, -CMD_maxanom.z) %>%
                                      rename(MAT_maxanom.z = MAT_maxanom_2060.z,
                                             CMD_maxanom.z = CMD_maxanom_2060.z) %>% 
                                      mutate(area.fire.prop = area.fire.prop*delt,
                                             area.fire.prop = ifelse(area.fire.prop>1,1,
                                                                     area.fire.prop),
                                             fire.sev = fire.sev*delt,
                                             fire.sev = ifelse(fire.sev>1,1,
                                                               fire.sev))) %>%
  colMeans()

r19.pred <- brms::posterior_predict(r19, 
                                    ndraws=1000,
                                    newdata = sap.dat %>% 
                                      filter(SPCD==93) %>% 
                                      select(-MAT_maxanom.z, -CMD_maxanom.z) %>%
                                      rename(MAT_maxanom.z = MAT_maxanom_2060.z,
                                             CMD_maxanom.z = CMD_maxanom_2060.z) %>% 
                                      mutate(area.fire.prop = area.fire.prop*delt,
                                             area.fire.prop = ifelse(area.fire.prop>1,1,
                                                                     area.fire.prop),
                                             fire.sev = fire.sev*delt,
                                             fire.sev = ifelse(fire.sev>1,1,
                                                               fire.sev))) %>%
  colMeans()

r93.pred <- brms::posterior_predict(r93, 
                                    ndraws=1000,
                                    newdata = sap.dat %>% 
                                      filter(SPCD==93) %>% 
                                      select(-MAT_maxanom.z, -CMD_maxanom.z) %>%
                                      rename(MAT_maxanom.z = MAT_maxanom_2060.z,
                                             CMD_maxanom.z = CMD_maxanom_2060.z) %>% 
                                      mutate(area.fire.prop = area.fire.prop*delt,
                                             area.fire.prop = ifelse(area.fire.prop>1,1,
                                                                     area.fire.prop),
                                             fire.sev = fire.sev*delt,
                                             fire.sev = ifelse(fire.sev>1,1,
                                                               fire.sev))) %>%
  colMeans()


## adding posterior predictions to data frames ----

p19.fut <- ind.mort.dat %>%
  filter(SPCD==19) %>%
  mutate(m.pred = 1-m19.pred) %>%
  group_by(mult.comp.coexist,ECOSUBCD) %>%
  summarise(m.pred = mean(m.pred),
            mean.dia = mean(PREVDIA)) %>%
  left_join(seed.dat %>%
              filter(SPCD==19) %>%
              mutate(s.pred = s19.pred,
                     r.pred = r19.pred) %>%
              group_by(mult.comp.coexist,ECOSUBCD) %>%
              summarise(s.pred = mean(s.pred),
                        r.pred = mean(r.pred)),
            by=c("mult.comp.coexist","ECOSUBCD")) %>% 
  mutate(SPCD=19) %>% 
  na.omit()

p93.fut <- ind.mort.dat %>%
  filter(SPCD==93) %>%
  mutate(m.pred = 1-m93.pred) %>%
  group_by(mult.comp.coexist,ECOSUBCD) %>%
  summarise(m.pred = mean(m.pred),
            mean.dia = mean(PREVDIA)) %>%
  left_join(seed.dat %>%
              filter(SPCD==93) %>%
              mutate(s.pred = s93.pred,
                     r.pred = r93.pred) %>%
              group_by(mult.comp.coexist,ECOSUBCD) %>%
              summarise(s.pred = mean(s.pred),
                        r.pred = mean(r.pred)),
            by=c("mult.comp.coexist","ECOSUBCD")) %>% 
  mutate(SPCD=93) %>% 
  na.omit()


p.m.fut <- p19.fut %>% 
  left_join(p93.fut,
            by = c("ECOSUBCD","mult.comp.coexist"),
            suffix = c(".19",".93")) %>% 
  group_by()


## tossing them into the multinomial prediction machine ----

p.m.fut <- p.m.fut %>% 
  mutate(pred.class = predict(m.m, newdata = p.m.fut, type="class")) %>% 
  bind_cols(round(predict(m.m, newdata = p.m.fut, type = "probs"),2))


## mapping predictions ---- 

ggplot() +
  geom_sf(data = cont %>% 
            as(.,"sf"),
          col=linecolor,
          fill = mapcolor,
          lwd=0.3) +  
  geom_sf(data = states %>% 
            as(.,"sf"),
          fill=NA,
          col=linecolor) +
  geom_sf(data = dall.er.ablapien,
          col = linecolor,
          fill = NA)+
  geom_sf(data = dall.er.ablapien,
          col=NA,
          aes(fill = factor(mult.comp.coexist))) +
  geom_sf(data = states %>% 
            as(.,"sf"),
          col=linecolor,
          fill=NA,
          lty=2) +
  scale_fill_manual(name = "multispecies trajectory",
                    values = c("resilience" = "dodgerblue2",
                               "structural change" = "gold2",
                               "compositional change" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("col","fill"))+
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.position="none",
        panel.background = element_rect(fill="skyblue1"))

dall.er.ablapien2 <- dall.er.ablapien %>% 
  left_join(.,
            p.m.fut,
            by = c("MAP_UNIT_S"="ECOSUBCD")) %>% 
  group_by(MAP_UNIT_S) %>% 
  mutate(class.prob = max(resilience,`structural change`,`compositional change`,replacement,na.rm=T))

ggplot() +
  geom_sf(data = cont %>% 
            as(.,"sf"),
          col=linecolor,
          fill = mapcolor,
          lwd=0.3) +  
  geom_sf(data = states %>% 
            as(.,"sf"),
          fill=NA,
          col=linecolor) +
  geom_sf(data = dall.er.ablapien2,
          col = linecolor)+
  geom_sf(data = dall.er.ablapien2,
          col = NA,
          fill = 'white')+
  geom_sf(data = dall.er.ablapien2,
          col=NA,
          aes(fill = factor(pred.class),
              alpha = class.prob)) +
  geom_sf(data = states %>% 
            as(.,"sf"),
          col=linecolor,
          fill=NA,
          lty=2) +
  scale_fill_manual(name = "multispecies trajectory",
                    values = c("resilience" = "dodgerblue2",
                               "structural change" = "gold2",
                               "compositional change" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("col","fill"))+
  scale_alpha_binned(limits = c(0,1), range=c(-0.25,1),
                     breaks=c(0,0.25,0.5,0.75,1)) +
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.position="none",
        panel.background = element_rect(fill="skyblue1"))


























