er.mort.dat <- dall.er.abla %>% 
  left_join(mort.area.er %>% 
              mutate(area.fire.prop = area.fire/area.total,
                     area.id.prop = area.id/area.total),
            by = c("MAP_UNIT_S"="ECOSUBCD")) %>% 
  mutate(MORT_RESPONSE = MORT_TOTAL/PREV_TOTAL,
         MORT_RESPONSE = ifelse(is.na(MORT_RESPONSE),0,MORT_RESPONSE)) %>% 
  left_join(all.fia$PLOT %>% 
              select(ECOSUBCD,MAT_base,MAT_remper,CMD_base,CMD_remper) %>% 
              group_by(ECOSUBCD) %>% 
              summarise(MAT_anom.er = mean(MAT_remper-MAT_base,na.rm=T),
                        CMD_anom.er=mean(CMD_remper-CMD_base,na.rm=T)),
            by=c("MAP_UNIT_S"="ECOSUBCD")) %>% 
  filter(!is.na(MAT_anom.er)) %>% 
  mutate(PREV_TPA = PREV_TOTAL/AREA_TOTAL,
         PREV_TPA_scaled = as.numeric(scale(PREV_TPA)),
         MAT_anom.er_scaled = as.numeric(scale(MAT_anom.er)),
         CMD_anom.er_scaled = as.numeric(scale(CMD_anom.er))) %>% 
  sf::st_drop_geometry()





## v1 ----
er.mort.mod <- glm(data = er.mort.dat,
                   formula = MORT_RESPONSE ~
                     PREV_TPA_scaled +
                     area.fire.prop*MAT_anom.er_scaled+
                     area.id.prop*MAT_anom.er_scaled+
                     area.id.prop:area.fire.prop,
                   family = binomial(link="logit"),
                   weights = nPlots_TREE)

summary(er.mort.mod)
summary(m.er)


ggpredict(m.er,
          terms = c("MAT_anom.er_scaled [-4:5,by=0.1]", 
                    "area.id.prop [0, 0.3, 0.6, 1]", 
                    "area.fire.prop [0,0.3,0.6,1]"),
          #condition = c(INVYR=2019),
          #               PREV_BAA_abla_scaled = 0,
          #               PREV_BAA_all_scaled = 0),
          type="fe") %>% as.data.frame() %>% 
  ggplot(.,
         aes((x*s.sd)+s.mean, predicted*100, groups = group)) +
  geom_ribbon(aes(ymin = conf.low*100,
                  ymax = conf.high*100),
              fill="darkgray",
              alpha=0.3)+
  geom_line(aes(col = group),
            lwd=1.5) +
  geom_vline(xintercept = s.mean,
             lty=2)+
  labs(x = "MAT anomaly (C)",
       y= "Predicted % mortality") +
  scale_color_manual(name = "Proportion \ninsects/disease",
                     values = c("0" = "dodgerblue4",
                                "0.3" = "dodgerblue1",
                                #"0.5" = "black",
                                "0.6" = "orange",
                                "1" = "red")) +
  geom_rug(sides = "b", data = mort.decomp, aes(x = MAT_anom), inherit.aes=F)+
  facet_wrap(facets=~facet,
             labeller = as_labeller(c("0" = "0% burned",
                                      "0.3" = "30% burned",
                                      "0.6" = "60% burned",
                                      "1" = "100% burned")))+
  theme(legend.key.size = unit(2,"cm"))
## V2 ----

m2 <- glmer(data = er.mort.dat,
            formula = MORT_RESPONSE ~
              PREV_TPA_scaled +
              area.fire.prop*MAT_anom.er_scaled+
              area.id.prop*MAT_anom.er_scaled+
              area.id.prop:area.fire.prop +
              (1|ECO_GRP_NEW_NAM),
            family = binomial(link="logit"),
            weights = nPlots_TREE)

summary(m2)
performance(m2)
plot(m2@frame$MORT_RESPONSE,predict(m2,type="response"))
abline(0,1,col="red")

ggpredict(m2,
          terms = c("MAT_anom.er_scaled [-4:5,by=0.1]", 
                    "area.id.prop [0, 0.3, 0.6, 1]", 
                    "area.fire.prop [0,0.3,0.6,1]"),
          condition = c("ECO_GRP_NEW_NAM"="Middle Rocky Mountain Steppe"),
          type="fe") %>% as.data.frame() %>% 
  ggplot(.,
         aes((x*s.sd)+s.mean, predicted*100, groups = group)) +
  geom_ribbon(aes(ymin = conf.low*100,
                  ymax = conf.high*100),
              fill="darkgray",
              alpha=0.3)+
  geom_line(aes(col = group),
            lwd=1.5) +
  geom_vline(xintercept = s.mean,
             lty=2)+
  labs(x = "MAT anomaly (C)",
       y= "Predicted % mortality") +
  scale_color_manual(name = "Proportion \ninsects/disease",
                     values = c("0" = "dodgerblue4",
                                "0.3" = "dodgerblue1",
                                #"0.5" = "black",
                                "0.6" = "orange",
                                "1" = "red")) +
  geom_rug(sides = "b", data = mort.decomp, aes(x = MAT_anom), inherit.aes=F)+
  facet_wrap(facets=~facet,
             labeller = as_labeller(c("0" = "0% burned",
                                      "0.3" = "30% burned",
                                      "0.6" = "60% burned",
                                      "1" = "100% burned")))+
  theme(legend.key.size = unit(2,"cm"))



## V3


m2 <- glmer(data = er.mort.dat,
            formula = MORT_RESPONSE ~
              PREV_TPA_scaled +
              area.fire.prop*MAT_anom.er_scaled+
              area.id.prop*MAT_anom.er_scaled+
              area.id.prop:area.fire.prop +
              (MAT_anom.er_scaled|ECO_GRP_NEW_NAM),
            family = binomial(link="logit"),
            weights = nPlots_TREE)

summary(m2)
performance(m2)
plot(m2@frame$MORT_RESPONSE,predict(m2,type="response"))
abline(0,1,col="red")

ggpredict(m2,
          terms = c("MAT_anom.er_scaled [-4:5,by=0.1]", 
                    "area.id.prop [0, 0.3, 0.6, 1]", 
                    "area.fire.prop [0,0.3,0.6,1]"),
          condition = c("ECO_GRP_NEW_NAM"="Middle Rocky Mountain Steppe"),
          type="fe") %>% as.data.frame() %>% 
  ggplot(.,
         aes((x*s.sd)+s.mean, predicted*100, groups = group)) +
  geom_ribbon(aes(ymin = conf.low*100,
                  ymax = conf.high*100),
              fill="darkgray",
              alpha=0.3)+
  geom_line(aes(col = group),
            lwd=1.5) +
  geom_vline(xintercept = s.mean,
             lty=2)+
  labs(x = "MAT anomaly (C)",
       y= "Predicted % mortality") +
  scale_color_manual(name = "Proportion \ninsects/disease",
                     values = c("0" = "dodgerblue4",
                                "0.3" = "dodgerblue1",
                                #"0.5" = "black",
                                "0.6" = "orange",
                                "1" = "red")) +
  geom_rug(sides = "b", data = mort.decomp, aes(x = MAT_anom), inherit.aes=F)+
  facet_wrap(facets=~facet,
             labeller = as_labeller(c("0" = "0% burned",
                                      "0.3" = "30% burned",
                                      "0.6" = "60% burned",
                                      "1" = "100% burned")))+
  theme(legend.key.size = unit(2,"cm"))