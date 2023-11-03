plot.attributes.baa <- growMort_dlp(db = all.fia,
                                byPlot = T,
                                bySpecies = T,
                                stateVar = "BAA",
                                evals = evals,
                                sizeThresh = 5,
                                polys = NULL,
                                returnSpatial = F)


ind.mort.dat <- all.fia$TREE %>% 
  filter(SPCD==19,
         PREV_STATUS_CD==1,
         STATUSCD%in%c(1,2)) %>% 
  select(TRE_CN,PLT_CN,
         PREV_STATUS_CD,STATUSCD,
         DIA,PREVDIA,agent_key) %>% 
  mutate(SURV = ifelse(STATUSCD==2,0,STATUSCD)) %>% 
  left_join(.,
            all.fia$PLOT %>% 
              select(PLT_CN,
                     contains("MAT"),
                     contains("MAP"),
                     contains("CMD"),
                     ECOSUBCD, REMPER,
                     INVYR) %>% 
              mutate(MAT_anom = MAT_remper-MAT_base,
                     MAP_anom = MAP_remper-MAP_base,
                     CMD_anom = CMD_remper-CMD_base),
            by="PLT_CN") %>%
  left_join(mort.area.er %>% 
              mutate(area.fire.prop = area.fire/area.total,
                     area.id.prop = area.id/area.total),
            by="ECOSUBCD") %>% 
  left_join(plot.attributes.baa)

## ---- 

m1 <- glmer(data = ind.mort.dat,
           formula = SURV ~ 
             scale(PREV_BAA) +
             scale(PREVDIA)*scale(MAT_anom)*area.fire.prop + 
             scale(PREVDIA)*scale(MAT_anom)*area.id.prop +
             area.fire.prop:area.id.prop +
             (1|ECOSUBCD),
           family = binomial(link="logit"),
           nAGQ=0,
           control = glmerControl(optim = "nlminbwrap"))

summary(m1)
performance(m1)

# model fit at ecoregion level

m1@frame %>% 
  mutate(fit = predict(m1,
                       type="response",
                       re.form=NULL)) %>% # CONDITIONAL
  group_by(ECOSUBCD) %>% 
  summarise(obs = 1-(sum(SURV)/n()),
            pred = 1-(sum(fit)/n())) %>% 
  ggplot(.,
         aes(x=obs,
             y=pred))+
  geom_point(pch=19, size=3, alpha=0.6)+
  geom_abline(slope=1, intercept=0, col="red")

m1@frame %>% 
  mutate(fit = predict(m1,
                       type="response",
                       re.form=NA)) %>% # MARGINAL
  group_by(ECOSUBCD) %>% 
  summarise(obs = sum(SURV)/n(),
            pred = sum(fit)/n()) %>% 
  ggplot(.,
         aes(x=1-obs,
             y=1-pred))+
  geom_point(pch=19, size=3, alpha=0.6)+
  geom_abline(slope=1, intercept=0, col="red")


# interactions plot

# climate x
ggpredict(m1,
          terms = c("MAT_anom [-1:5, by=0.1]",
                    "area.fire.prop [0,0.1,0.25,0.5]",
                    "area.id.prop [0,0.3,0.6,1]",
                    "PREVDIA [1,7,20]")) %>% 
  as.data.frame() %>% 
  rename(MAT_anom = x,
         area.fire.prop = group,
         area.id.prop = facet,
         PREVDIA = panel) %>% 
  filter(PREVDIA==1) %>% 
  ggplot(., 
         aes(x = MAT_anom,
             y = 1-predicted,
             groups = area.id.prop)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high),
              fill="darkgray",
              alpha=0.3) +
  geom_line(aes(col=area.id.prop),
            lwd=1.5) +
  labs(x = "MAT anomaly (C)",
       y = "Predicted mortality")+
  scale_color_manual(name = "Proportion \ninsects/disease",
                     values = c("0" = "dodgerblue4",
                                "0.3" = "dodgerblue1",
                                #"0.5" = "black",
                                "0.6" = "orange",
                                "1" = "red")) +
  facet_wrap(facets=~area.fire.prop,
             labeller = as_labeller(c("0" = "0% burned",
                                      "0.1"= "10% burned",
                                      "0.25" = "25% burned",
                                      "0.5" = "50% burned")))


# tree size x
ggpredict(m1,
          terms = c("PREVDIA [0:30,by=0.1]",
                    "area.fire.prop [0,0.3,0.6,1]",
                    "area.id.prop [0,0.3,0.6,1]",
                    "MAT_anom [0,1,2,3]")) %>% 
  as.data.frame() %>% 
  rename(PREVDIA = x,
         area.fire.prop = group,
         area.id.prop = facet,
         MAT_anom = panel) %>% 
  filter(MAT_anom==0) %>% 
  ggplot(., 
         aes(x = PREVDIA,
             y = 1-predicted,
             groups = area.id.prop)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high),
              fill="darkgray",
              alpha=0.3) +
  geom_line(aes(col=area.id.prop),
            lwd=1.5) +
  labs(x = "DBH",
       y = "Predicted mortality")+
  scale_color_manual(name = "Proportion \ninsects/disease",
                     values = c("0" = "dodgerblue4",
                                "0.3" = "dodgerblue1",
                                #"0.5" = "black",
                                "0.6" = "orange",
                                "1" = "red")) +
  facet_wrap(facets=~area.fire.prop,
             labeller = as_labeller(c("0" = "0% burned",
                                      "0.3"= "30% burned",
                                      "0.6" = "60 % burned",
                                      "1" = "100% burned")))





# fire x warming
ggpredict(m1,
          terms = c("area.fire.prop [0:1, by=0.01]",
                    "area.id.prop [0,0.3,0.6,1]",
                    "MAT_anom [0,1,2,3]")) %>% 
  as.data.frame() %>% 
  rename(area.fire.prop = x,
         area.id.prop = group,
         MAT_anom = facet) %>% 
  #filter(MAT_anom==1) %>% 
  ggplot(., 
         aes(x = area.fire.prop,
             y = 1-predicted,
             groups = MAT_anom)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high),
              fill="darkgray",
              alpha=0.3) +
  geom_line(aes(col=MAT_anom),
            lwd=1.5) +
  labs(x = "Burned area",
       y = "Predicted mortality")+
  scale_color_manual(name = "Warming",
                     values = c("0" = "dodgerblue4",
                                "1" = "dodgerblue1",
                                #"0.5" = "black",
                                "2" = "orange",
                                "3" = "red")) +
  facet_wrap(facets=~area.id.prop,
             labeller = as_labeller(c("0" = "No bugs",
                                      "0.3"= "30% bugs",
                                      "0.6" = "60% bugs",
                                      "1" = "100% bugs")))

# bugs x warming
ggpredict(m1,
          terms = c("area.id.prop [0:1, by=0.01]",
                    "area.fire.prop [0,0.3,0.6,1]",
                    "MAT_anom [0,1,2,3]")) %>% 
  as.data.frame() %>% 
  rename(area.id.prop = x,
         area.fire.prop = group,
         MAT_anom = facet) %>% 
  #filter(MAT_anom==1) %>% 
  ggplot(., 
         aes(x = area.id.prop,
             y = 1-predicted,
             groups = MAT_anom)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high),
              fill="darkgray",
              alpha=0.3) +
  geom_line(aes(col=MAT_anom),
            lwd=1.5) +
  labs(x = "Bugs area",
       y = "Predicted mortality")+
  scale_color_manual(name = "Warming",
                     values = c("0" = "dodgerblue4",
                                "1" = "dodgerblue1",
                                #"0.5" = "black",
                                "2" = "orange",
                                "3" = "red")) +
  facet_wrap(facets=~area.fire.prop,
             labeller = as_labeller(c("0" = "No burning",
                                      "0.3"= "30% burned",
                                      "0.6" = "60% burned",
                                      "1" = "100% burned")))



