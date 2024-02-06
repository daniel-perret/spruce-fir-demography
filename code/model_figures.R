## generating various mortaliaty X regeneration plots from Bayesian models (bayes_model_eval.R, bayes_model.R)

### plotting mortalityXregen densities for different categories

p93 <- ind.mort.dat %>% 
  filter(SPCD==93) %>% 
  bind_cols(fitted(m93) %>% as.data.frame()) %>%
  mutate(mort.pred=1-Estimate) %>% 
  group_by(mult.comp.coexist,PLT_CN,SPCD,ECOSUBCD) %>% 
  summarise(mort.pred = mean(mort.pred),
            mean.dia = mean(PREVDIA)) %>% 
  left_join(seed.dat %>% 
              filter(SPCD==93) %>% 
              bind_cols(fitted(s93) %>% as.data.frame()) %>%
              mutate(seed.pred = Estimate) %>% 
              select(PLT_CN,SPCD,seed.pred,ECOSUBCD),
            by=c("PLT_CN","SPCD","ECOSUBCD")) %>% 
  group_by(ECOSUBCD, mult.comp.coexist, SPCD) %>% 
  summarise(seed.pred = mean(seed.pred),
            mort.pred = mean(mort.pred)) %>% 
  na.omit()

ind.mort.dat %>% 
  filter(SPCD==19) %>% 
  bind_cols(fitted(m19) %>% as.data.frame()) %>%
  mutate(mort.pred=1-Estimate) %>% 
  group_by(mult.comp.coexist,PLT_CN,SPCD,ECOSUBCD) %>% 
  summarise(mort.pred = mean(mort.pred),
            mean.dia = mean(PREVDIA)) %>% 
  left_join(seed.dat %>% 
              filter(SPCD==19) %>% 
              bind_cols(fitted(s19) %>% as.data.frame()) %>%
              mutate(seed.pred = Estimate) %>%               
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
                            levels=c("resilience",
                                     "structural change",
                                     "compositional change",
                                     "replacement")),
             ncol=1) +
  labs(x = "Predicted probability of mortality",
       y = "Predicted probability of regeneration") +
  scale_color_manual(name = "Species",
                     values = c("19" = "dodgerblue3",
                                "93" = "firebrick2"),
                     aesthetics = c("col","bg"))



### ABLA X PIEN for Mortality and Regen for different change categories

p93 <- ind.mort.dat %>% 
  filter(SPCD==93) %>% 
  bind_cols(fitted(m93) %>% as.data.frame()) %>%
  mutate(mort.pred=1-Estimate) %>% 
  group_by(mult.comp.coexist,PLT_CN,SPCD,ECOSUBCD) %>% 
  summarise(mort.pred = mean(mort.pred),
            mean.dia = mean(PREVDIA)) %>% 
  left_join(seed.dat %>% 
              filter(SPCD==93) %>% 
              bind_cols(fitted(s93) %>% as.data.frame()) %>%
              mutate(seed.pred = Estimate) %>% 
              select(PLT_CN,SPCD,seed.pred,ECOSUBCD),
            by=c("PLT_CN","SPCD","ECOSUBCD")) %>% 
  group_by(ECOSUBCD, mult.comp.coexist, SPCD) %>% 
  summarise(seed.pred = mean(seed.pred),
            mort.pred = mean(mort.pred)) %>% 
  na.omit()

p19 <- ind.mort.dat %>% 
  filter(SPCD==19) %>% 
  bind_cols(fitted(m19) %>% as.data.frame()) %>%
  mutate(mort.pred=1-Estimate) %>% 
  group_by(mult.comp.coexist,PLT_CN,SPCD,ECOSUBCD) %>% 
  summarise(mort.pred = mean(mort.pred),
            mean.dia = mean(PREVDIA)) %>% 
  left_join(seed.dat %>% 
              filter(SPCD==19) %>% 
              bind_cols(fitted(s19) %>% as.data.frame()) %>%
              mutate(seed.pred = Estimate) %>% 
              select(PLT_CN,SPCD,seed.pred,ECOSUBCD),
            by=c("PLT_CN","SPCD","ECOSUBCD")) %>% 
  group_by(ECOSUBCD, mult.comp.coexist, SPCD) %>% 
  summarise(seed.pred = mean(seed.pred),
            mort.pred = mean(mort.pred)) %>% 
  na.omit()

p.co <- p93 %>% 
  select(-SPCD) %>% 
  rename(seed.93 = seed.pred,
         mort.93 = mort.pred) %>% 
  left_join(p19 %>% 
              select(-SPCD) %>% 
              rename(seed.19 = seed.pred,
                     mort.19 = mort.pred),
            by=c("ECOSUBCD","mult.comp.coexist"))

p.co %>% 
  group_by(mult.comp.coexist) %>% 
  summarise(mort.19.low = quantile(mort.19,probs=0.05,na.rm=T),
            mort.19.high = quantile(mort.19,probs=0.95,na.rm=T),
            mort.93.low = quantile(mort.93,probs=0.05,na.rm=T),
            mort.93.high = quantile(mort.93,probs=0.95,na.rm=T),
            mort.19.mid = mean(mort.19,na.rm=T),
            mort.93.mid = mean(mort.93,na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(.,
         aes(col = factor(mult.comp.coexist, levels=c("resilience",
                                                      "structural change",
                                                      "compositional change",
                                                      "replacement")),
             bg = factor(mult.comp.coexist, levels=c("resilience",
                                                     "structural change",
                                                     "compositional change",
                                                     "replacement")))) +
  geom_point(data = p.co,
             aes(x = mort.19,
                 y = mort.93),
             pch = 21,
             alpha = 0.2, 
             size = 4,
             col = "black") +
  geom_segment(aes(x = mort.19.low, 
                   xend = mort.19.high,
                   y = mort.93.mid,
                   yend = mort.93.mid),
               lwd=1.2, alpha = 0.9) +
  geom_segment(aes(y = mort.93.low, 
                   yend = mort.93.high,
                   x = mort.19.mid,
                   xend = mort.19.mid),
               lwd=1.2, alpha = 0.9) +
  
  geom_abline(slope = 1, intercept = 0, lty=2, lwd = 1) +
  labs(x = "Subalpine fir mortality",
       y = "Engelmann spruce mortality") +
  lims(x = c(0,1), y = c(0,1))+
  scale_color_manual(name = "Trajectory",
                     values = c("resilience" = "dodgerblue2",
                                "structural change" = "gold2",
                                "compositional change" = "firebrick1",
                                "replacement" = "firebrick4"),
                     aesthetics = c("col", "bg"))


p.co %>% 
  group_by(mult.comp.coexist) %>% 
  summarise(seed.19.low = quantile(seed.19,probs=0.05,na.rm=T),
            seed.19.high = quantile(seed.19,probs=0.95,na.rm=T),
            seed.93.low = quantile(seed.93,probs=0.05,na.rm=T),
            seed.93.high = quantile(seed.93,probs=0.95,na.rm=T),
            seed.19.mid = mean(seed.19,na.rm=T),
            seed.93.mid = mean(seed.93,na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(.,
         aes(col = factor(mult.comp.coexist, levels=c("resilience",
                                                      "structural change",
                                                      "compositional change",
                                                      "replacement")),
             bg = factor(mult.comp.coexist, levels=c("resilience",
                                                     "structural change",
                                                     "compositional change",
                                                     "replacement")))) +
  geom_point(data = p.co,
             aes(x = seed.19,
                 y = seed.93),
             pch = 21,
             alpha = 0.2, 
             size = 4,
             col = "black") +
  geom_segment(aes(x = seed.19.low, 
                   xend = seed.19.high,
                   y = seed.93.mid,
                   yend = seed.93.mid),
               lwd=1.2, alpha = 0.9) +
  geom_segment(aes(y = seed.93.low, 
                   yend = seed.93.high,
                   x = seed.19.mid,
                   xend = seed.19.mid),
               lwd=1.2, alpha = 0.9) +
  
  geom_abline(slope = 1, intercept = 0, lty=2, lwd = 1) +
  labs(x = "Subalpine fir regen",
       y = "Engelmann spruce regen") +
  lims(x = c(0,1), y = c(0,1))+
  scale_color_manual(name = "Trajectory",
                     values = c("resilience" = "dodgerblue2",
                                "structural change" = "gold2",
                                "compositional change" = "firebrick1",
                                "replacement" = "firebrick4"),
                     aesthetics = c("col", "bg"))



## version of the density figure with size dots for fire----


p93 <- ind.mort.dat %>% 
  filter(SPCD==93) %>% 
  bind_cols(fitted(m93) %>% as.data.frame()) %>%
  mutate(mort.pred=1-Estimate) %>% 
  group_by(mult.comp.coexist,PLT_CN,SPCD,ECOSUBCD) %>% 
  summarise(mort.pred = mean(mort.pred,na.rm=T),
            mean.dia = mean(PREVDIA),
            area.fire = mean(area.fire.prop),
            area.id = mean(area.id.prop)) %>% 
  left_join(seed.dat %>% 
              filter(SPCD==93) %>% 
              bind_cols(fitted(s93) %>% as.data.frame()) %>%
              mutate(seed.pred = Estimate) %>% 
              select(PLT_CN,SPCD,seed.pred,ECOSUBCD),
            by=c("PLT_CN","SPCD","ECOSUBCD")) %>% 
  group_by(ECOSUBCD, mult.comp.coexist, SPCD) %>% 
  summarise(seed.pred = mean(seed.pred,na.rm=T),
            mort.pred = mean(mort.pred,na.rm=T),
            area.fire = mean(area.fire),
            area.id = mean(area.id)) %>% 
  na.omit()

p19 <- ind.mort.dat %>% 
  filter(SPCD==19) %>% 
  bind_cols(fitted(m19) %>% as.data.frame()) %>%
  mutate(mort.pred=1-Estimate) %>% 
  group_by(mult.comp.coexist,PLT_CN,SPCD,ECOSUBCD) %>% 
  summarise(mort.pred = mean(mort.pred,na.rm=T),
            mean.dia = mean(PREVDIA),
            area.fire = mean(area.fire.prop),
            area.id = mean(area.id.prop)) %>% 
  left_join(seed.dat %>% 
              filter(SPCD==19) %>% 
              bind_cols(fitted(s19) %>% as.data.frame()) %>%
              mutate(seed.pred = Estimate) %>%               
              select(PLT_CN,SPCD,seed.pred,ECOSUBCD),
            by=c("PLT_CN","SPCD","ECOSUBCD")) %>% 
  group_by(ECOSUBCD, mult.comp.coexist, SPCD) %>% 
  summarise(seed.pred = mean(seed.pred,na.rm=T),
            mort.pred = mean(mort.pred,na.rm=T),
            area.fire = mean(area.fire),
            area.id = mean(area.id)) %>%
  na.omit()

ggplot(p19,
       aes(x=mort.pred,
           y = seed.pred,
           col = SPCD,
           bg = SPCD,
           size = area.fire)) +
  geom_point(alpha=0.2,pch=21)+
  geom_density_2d(lwd=0.75) +
  geom_point(data=p93,alpha=0.2,pch=21)+
  geom_density_2d(data=p93,lwd=0.75) +
  facet_wrap(facets=~factor(mult.comp.coexist, 
                            levels=c("resilience",
                                     "structural change",
                                     "compositional change",
                                     "replacement")),
             ncol=1) +
  labs(x = "Predicted probability of mortality",
       y = "Predicted probability of regeneration") +
  scale_color_manual(name = "Species",
                     values = c("19" = "dodgerblue3",
                                "93" = "firebrick2"),
                     aesthetics = c("col","bg"))






























