library(lme4)
library(performance)
library(ggeffects)

abla.plots <- all.fia$PLOT %>% 
  filter(abla.pres == 1) %>% 
  pull(PLT_CN)

## making ecological province-ish groupings

PLOT.sp <- all.fia$PLOT %>% 
  filter(!is.na(LON)) %>% 
  SpatialPointsDataFrame(coords= .[,c("LON","LAT")],
                         data = .,
                         proj4string = CRS(old.proj)) %>% 
  spTransform(.,CRSobj=CRS(base.proj)) %>% 
  as(.,"sf") %>% 
  sf::st_join(., er2 %>% 
                as(.,"sf") %>% 
                select(ECOPROVCD=MAP_UNIT_S,
                       ECOPROVNAM=MAP_UNIT_N),
              left=T) %>% 
  mutate(ECO_GRP_NEW = case_when(ECOPROVCD %in% c("242","M242") ~ "M242",
                                 ECOPROVCD %in% c("341", "M341","342") ~ "M341",
                                 ECOPROVCD %in% c("331", "M331") ~ "M331",
                                 ECOPROVCD %in% c("313", "M313", "321") ~ "M313",
                                 is.na(ECOPROVCD) ~ "M333",
                                 TRUE~ECOPROVCD),
         ECO_GRP_NEW_NAM = case_when(ECO_GRP_NEW == "M242" ~ "Cascade Mixed Forest",
                                     ECO_GRP_NEW == "M313" ~ "AZ-NM Mountains",
                                     ECO_GRP_NEW == "M331" ~ "Southern Rocky Mountain Steppe",
                                     ECO_GRP_NEW == "M332" ~ "Middle Rocky Mountain Steppe",
                                     ECO_GRP_NEW == "M333" ~ "Northern Rocky Mountain Forest-Steppe",
                                     ECO_GRP_NEW == "M341" ~ "zIntermountain Semi-Desert",
                                     TRUE ~ "Cascade Mixed Forest"))

all.fia$PLOT <- all.fia$PLOT %>% 
  left_join(., PLOT.sp %>% 
              sf::st_drop_geometry() %>% 
              select(PLT_CN, ECOPROVCD,ECOPROVNAM, ECO_GRP_NEW, ECO_GRP_NEW_NAM),
            by="PLT_CN")

evals <- c(21903,41903,61903,81903,161903,301903,321903,351903,411903,491903,531903,561903)

plot.attributes.prev <- growMort_dlp(db = all.fia,
                                    byPlot = T,
                                    bySpecies = F,
                                    stateVar = "BAA",
                                    evals = evals,
                                    #areaDomain = abla.pres==1,
                                    sizeThresh = 5,
                                    polys = NULL,
                                    returnSpatial = F) %>%
  filter(PLT_CN %in% abla.plots)



plot.attributes.baa <- tpa(db = all.fia %>% 
                             clipFIA(mostRecent=T),
                           byPlot=T,
                           bySpecies=F, 
                           areaDomain = abla.pres==1,
                           treeDomain = DIA>=5,
                           returnSpatial = F)


ind.mort.dat <- all.fia$TREE %>% 
  filter(SPCD==19,
         PREV_STATUS_CD==1,
         STATUSCD%in%c(1,2),
         INVYR>2009) %>% 
  left_join(all.fia$TREE %>% 
              select(TRE_CN,
                     PREV_CR = CR,
                     PREV_UNCRCD = UNCRCD),
            by=c("PREV_TRE_CN"="TRE_CN")) %>% 
  select(TRE_CN,PLT_CN,
         PREV_STATUS_CD,STATUSCD,
         DIA,PREVDIA,
         PREV_CR,PREV_UNCRCD,
         agent_key) %>% 
  mutate(SURV = ifelse(STATUSCD==2,0,STATUSCD)) %>% 
  left_join(.,
            all.fia$PLOT %>% 
              select(PLT_CN,
                     contains("MAT"),
                     contains("MAP"),
                     contains("CMD"),
                     ECOSUBCD, REMPER, ECO_GRP_NEW_NAM,
                     INVYR, most.recent) %>% 
              mutate(MAT_anom = MAT_remper-MAT_base,
                     MAP_anom = MAP_remper-MAP_base,
                     CMD_anom = CMD_remper-CMD_base,
                     MAP_relanom = MAP_anom/MAP_base),
            by="PLT_CN") %>%
  left_join(mort.area.er %>% 
              mutate(area.fire.prop = area.fire/area.total,
                     area.id.prop = area.id/area.total),
            by="ECOSUBCD") %>% 
  left_join(plot.attributes.prev) %>% 
  filter(!is.na(MAT_remper),
         !is.na(PREV_BAA))



## ---- 

m1 <- glmer(data = ind.mort.dat,
            formula = SURV ~ 
              scale(PREV_BAA) + scale(PREV_CR) +
              scale(MAT_remper)*scale(MAT_anom) + scale(MAP_remper)*scale(MAP_relanom) +
              scale(PREVDIA)*scale(MAP_relanom)*scale(MAT_anom)*area.fire.prop +
              scale(PREVDIA)*scale(MAP_relanom)*scale(MAT_anom)*area.id.prop +
              # scale(PREVDIA)*scale(MAT_anom)*area.fire.prop +
              # scale(PREVDIA)*scale(MAT_anom)*area.id.prop +
              area.fire.prop:area.id.prop +
              (1|ECOSUBCD),
            family = binomial(link="logit"),
            nAGQ=0,
            control = glmerControl(optim = "nlminbwrap"))


summary(m1)
performance(m1)
check_model(m1)


#### MAT_x interaction grid ---- 
ggpredict(m1,
          terms = c("MAT_anom [-1:3, by=0.1]",
                    "area.fire.prop [0,0.075,0.15,0.30]",
                    "area.id.prop [0,0.29,0.6,1]",
                    "PREVDIA [1,7,20]")) %>% 
  as.data.frame() %>% 
  rename(MAT_anom = x,
         area.fire.prop = group,
         area.id.prop = facet,
         PREVDIA = panel) %>% 
  #filter(area.id.prop == 0.) %>% 
  ggplot(., 
         aes(x = MAT_anom,
             y = 1-predicted,
             groups = factor(PREVDIA))) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high),
              fill="darkgray",
              alpha=0.3) +
  geom_line(aes(col=factor(PREVDIA)),
            lwd=1.7) +
  geom_vline(xintercept = 1.2)+
  labs(x = "MAT anomaly (C)",
       y = "Predicted mortality",
       title = "")+
  scale_color_manual(name = "Tree size",
                     values = c("1" = "orange2",
                                "7" = "dodgerblue3",
                                "20" = "green3")) +
  facet_grid(area.id.prop~area.fire.prop,
             labeller = labeller(
               area.fire.prop=as_labeller(
                 c("0" = "0% burned",
                   "0.075"= "Current conditions \n 7.5% burned",
                   "0.15" = "15% burned",
                   "0.3" = "30% burned")),
               area.id.prop=as_labeller(
                 c("0" = "0% bugs",
                   "0.29"= "Current conditions \n 30% bugs",
                   "0.6" = "60% bugs",
                   "1" = "100% bugs"))))

#### MAP_x interaction grid ----

ggpredict(m1,
          terms = c("MAP_relanom [-0.5:0.9,by=0.01]",        
                    "area.fire.prop [0,0.075,0.15,0.30]",
                    "area.id.prop [0,0.29,0.6,1]",
                    "PREVDIA [1,7,20]")) %>% 
  as.data.frame() %>% 
  rename(MAP_relanom = x,
         area.fire.prop = group,
         area.id.prop = facet,
         PREVDIA = panel) %>% 
  #filter(area.id.prop == 0.) %>% 
  ggplot(., 
         aes(x = MAP_relanom,
             y = 1-predicted,
             groups = factor(PREVDIA))) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high),
              fill="darkgray",
              alpha=0.3) +
  geom_line(aes(col=factor(PREVDIA)),
            lwd=1.7) +
  geom_vline(xintercept = 0.04)+
  labs(x = "MAP anomaly (%)",
       y = "Predicted mortality",
       title = "")+
  scale_color_manual(name = "Tree size",
                     values = c("1" = "orange2",
                                "7" = "dodgerblue3",
                                "20" = "green3")) +
  facet_grid(area.id.prop~area.fire.prop,
             labeller = labeller(
               area.fire.prop=as_labeller(c("0" = "0% burned",
                                            "0.075"= "Current conditions \n 7.5% burned",
                                            "0.15" = "15% burned",
                                            "0.3" = "30% burned")),
               area.id.prop=as_labeller(c("0" = "0% bugs",
                                          "0.29"= "Current conditions \n 30% bugs",
                                          "0.6" = "60% bugs",
                                          "1" = "100% bugs"))))

#### DIA_x interaction grid ----

ggpredict(m1,
          terms = c("PREVDIA [1:30, by=0.1]",        
                    "area.fire.prop [0,0.075,0.15,0.30]",
                    "area.id.prop [0,0.29,0.6,1]",
                    "MAT_anom [0,1,3]")) %>% 
  as.data.frame() %>% 
  rename(PREVDIA = x,
         area.fire.prop = group,
         area.id.prop = facet,
         MAT_anom = panel) %>% 
  #filter(area.id.prop == 0.) %>% 
  ggplot(., 
         aes(x = PREVDIA,
             y = 1-predicted,
             groups = factor(MAT_anom))) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high),
              fill="darkgray",
              alpha=0.3) +
  geom_line(aes(col=factor(MAT_anom)),
            lwd=1.7) +
  geom_vline(xintercept = 0.04)+
  labs(x = "Tree Size (inch DBH)",
       y = "Predicted mortality",
       title = "")+
  scale_color_manual(name = "MAT warming",
                     values = c("0" = "dodgerblue3",
                                "1" = "gold2",
                                "3" = "firebrick3")) +
  facet_grid(area.id.prop~area.fire.prop,
             labeller = labeller(
               area.fire.prop=as_labeller(c("0" = "0% burned",
                                            "0.075"= "Current conditions \n 7.5% burned",
                                            "0.15" = "15% burned",
                                            "0.3" = "30% burned")),
               area.id.prop=as_labeller(c("0" = "0% bugs",
                                          "0.29"= "Current conditions \n 30% bugs",
                                          "0.6" = "60% bugs",
                                          "1" = "100% bugs"))))


#### clim norms by anoms


ggpredict(m1,
          terms = c("MAT_anom [0:3, by=0.1]",
                    "MAT_remper [0,3.4,6]")) %>% 
  as.data.frame() %>% 
  rename(MAT_anom = x,
         MAT_remper = group) %>% 
  ggplot(., 
         aes(x = MAT_anom,
             y = 1-predicted)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  group=MAT_remper),
              fill="darkgray",
              alpha=0.3) +
  geom_line(lwd=1.7,
            aes(col=MAT_remper)) +
  labs(x = "Temperature anomaly",
       y = "Predicted mortality",
       title = "") +
  scale_color_manual(name = "Mean annual temperature",
                     values = c("0" = "dodgerblue3",
                                "3.4" = "gold2",
                                "6" = "firebrick3"))


ggpredict(m1,
          terms = c("MAP_relanom [-0.5:0.5, by=0.05]",
                    "MAP_remper [500,1100,2500]")) %>% 
  as.data.frame() %>% 
  rename(MAP_relanom = x,
         MAP_remper = group) %>% 
  ggplot(., 
         aes(x = MAP_relanom,
             y = 1-predicted)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  group=MAP_remper),
              fill="darkgray",
              alpha=0.3) +
  geom_line(lwd=1.7,
            aes(col=MAP_remper)) +
  labs(x = "Relative MAP change",
       y = "Predicted mortality",
       title = "") +
  scale_color_manual(name = "Mean annual precip",
                     values = c("500" = "gold2",
                                "1100" = "dodgerblue2",
                                "2500" = "forestgreen"))
