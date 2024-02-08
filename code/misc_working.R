## multispecies maps

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
  theme(legend.position="right",
        panel.background = element_rect(fill="skyblue1"))


dall.er.ablapien %>% 
  ggplot(.) +
  geom_density(aes(x = MORT_PERC.tph_19),
               col=NA, fill = "dodgerblue2",
               alpha = 0.6) +
  geom_density(aes(x = MORT_PERC.tph_93),
               col=NA, fill = "firebrick2",
               alpha = 0.6) +
  facet_wrap(facets = ~mult.comp.coexist)


dall.er.ablapien %>% 
  ggplot(aes(x = MORT_PERC.tph_93,
             fill = mult.comp.coexist,
             col = mult.comp.coexist)) +
  geom_density(alpha=0.5) +
  scale_fill_manual(name = "multispecies trajectory",
                    values = c("resilience" = "dodgerblue2",
                               "structural change" = "gold2",
                               "compositional change" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("col","fill"))


## mort/regen density plots

p93 <- ind.mort.dat %>% 
  filter(SPCD==93) %>% 
  bind_cols(mort.pred = 1-predict(m93,type="response")) %>% 
  group_by(mult.comp.coexist,PLT_CN,SPCD,ECOSUBCD) %>% 
  summarise(mort.pred = mean(mort.pred),
            mean.dia = mean(PREVDIA)) %>% 
  left_join(seed.dat %>% 
              filter(SPCD==93) %>% 
              bind_cols(seed.pred = predict(s93,type="response")) %>% 
              select(PLT_CN,SPCD,seed.pred,ECOSUBCD),
            by=c("PLT_CN","SPCD","ECOSUBCD")) %>% 
  group_by(ECOSUBCD, mult.comp.coexist, SPCD) %>% 
  summarise(seed.pred = mean(seed.pred),
            mort.pred = mean(mort.pred)) %>% 
  na.omit()

ind.mort.dat %>% 
  filter(SPCD==19) %>% 
  bind_cols(mort.pred = 1-predict(m19,type="response")) %>% 
  group_by(mult.comp.coexist,PLT_CN,SPCD,ECOSUBCD) %>% 
  summarise(mort.pred = mean(mort.pred),
            mean.dia = mean(PREVDIA)) %>% 
  left_join(seed.dat %>% 
              filter(SPCD==19) %>% 
              bind_cols(seed.pred = predict(s19,type="response")) %>% 
              select(PLT_CN,SPCD,seed.pred,ECOSUBCD),
            by=c("PLT_CN","SPCD","ECOSUBCD")) %>% 
  group_by(ECOSUBCD, mult.comp.coexist, SPCD) %>% 
  summarise(seed.pred = mean(seed.pred),
            mort.pred = mean(mort.pred)) %>% 
  na.omit() %>%
  ggplot(.,
         aes(x=mort.pred,
             y = seed.pred,
             col = SPCD,
             bg = SPCD)) +
  geom_point(alpha=0.2,pch=21)+
  geom_density_2d(lwd=0.75) +
  geom_point(data=p93,alpha=0.2,pch=21)+
  geom_density_2d(data=p93,lwd=0.75) +
  facet_wrap(facets=~factor(mult.comp.coexist, 
                            levels=c("likely persistence",
                                     "vulnerable",
                                     "mismatched trajectories",
                                     "general decline")),
             ncol=1) +
  labs(x = "Predicted probability of mortality",
       y = "Predicted probability of regeneration") +
  scale_color_manual(name = "Species",
                     values = c("19" = "dodgerblue3",
                                "93" = "firebrick2"),
                     aesthetics = c("col","bg"))







try <- ggpredict(m1, 
                 terms = c("MAT_anom [0:2.5, by = 0.05]",
                           "SPCD",
                           "area.fire.prop [0, 0.07, 0.25]",
                           "area.id.prop[0, 0.45, 0.8]")) %>% 
  as.data.frame() %>% 
  rename(MAT_anom=x,
         SPCD=group) %>% 
  mutate(fire.dist = case_when(facet == 0 ~ "low",
                               facet == 0.07 ~ "mid",
                               facet == 0.25 ~ "zhigh"),
         bda.dist = case_when(panel == 0 ~ "low",
                              panel == 0.45 ~ "mid",
                              panel == 0.8 ~ "zhigh"),
         rate = "surv") %>%
  select(-facet,-panel,-std.error) %>% 
  bind_rows(ggpredict(m.s,
                      terms = c("MAT_anom [0:2.5, by = 0.05]",
                                "SPCD",
                                "fire.sev [0,0.5,1]",
                                "bda.sev [0,0.2,0.6]")) %>% 
              as.data.frame() %>% 
              rename(SPCD = group,
                     MAT_anom = x) %>% 
              mutate(fire.dist = case_when(facet == 0 ~ "low",
                                           facet == 0.5 ~ "mid",
                                           facet == 1 ~ "zhigh"),
                     bda.dist = case_when(panel == 0 ~ "low",
                                          panel == 0.2 ~ "mid",
                                          panel == 0.6 ~ "zhigh"),
                     rate="seed") %>% 
              select(-facet,-panel,-std.error))

try %>% 
  filter(SPCD==19) %>% 
  ggplot(aes(x = MAT_anom,
             y = predicted,
             fill = rate,
             col = rate)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.1,
              col = NA) +
  geom_line(lwd=1.7) +
  facet_grid(facets = bda.dist ~ fire.dist,
             labeller = labeller(
               fire.dist = as_labeller(
                 c("low" = "low fire",
                   "mid"= "mid fire",
                   "zhigh" = "high fire")),
               bda.dist = as_labeller(
                 c("low" = "low bda",
                   "mid"= "mid bda",
                   "zhigh" = "high bda"))))



ggpredict(m19, terms = c("MAT_anom [0:2,by=0.1]",
                        "MAT_19802010[0,3,6]",
                        "PREVDIA [5, 11, 30]"),
         condition = c(area.fire.prop = 0, 
                       area.id.prop = 0)) %>% 
  as.data.frame() %>% 
  ggplot(.,
         aes(x = x,
             y = 1 - predicted,
             col = factor(group),
             fill = factor(group))) + 
  geom_ribbon(aes(ymin = 1-conf.low, ymax =1-conf.high),
              alpha=0.2,
              col=NA) +
  geom_line(lwd=1.7) +
  scale_color_manual(name = "MAT group",
                     values = c("0" = "dodgerblue2",
                                "3" = "gold2",
                                "6" = "firebrick2"),
                     aesthetics=c("col","fill")) +
  labs(x = "MAT anomaly",
       y = "Predicted mortality") +
  facet_grid(facets = ~facet)
  
  
  
# PLAYING WITH SAPLING RECRUITMENT

sapling <- all.fia %>% 
  growMort_dlp.metric(db = .,
                      stateVar = "TPA",
                      byPlot = T,
                      treeDomain = DIA<12.7, # DBH in cm
                      grpBy=SPCD,
                      totals = TRUE,
                      nCores = 4,
                      sizeThresh=1,
                      evals = evals,
                      method="TI") %>%
  filter(SPCD%in%c(19,93)) %>% 
  group_by(PLT_CN, SPCD) %>% 
  filter(YEAR==max(YEAR),
         PLT_CN%in%co.pres.plots)  





# exploratory sapling plots

sap.dat %>% 
  ggplot(.) + 
  geom_boxplot(aes(x = factor(SPCD),
                   #group = factor(sap_pres),
                   fill = factor(sap_pres),
                   y = MORT_PERC))

sap.dat %>% 
  ggplot(.) +
  geom_point(aes(x = CURR_BAH,
                  y = sap_count,
                  col = factor(SPCD)),
             alpha=0.7,
             size=3)

























  
  
