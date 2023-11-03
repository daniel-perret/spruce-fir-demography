library(lme4)
library(performance)
library(ggeffects)

### building the dataframe ----

abla.plots <- all.fia$PLOT %>% 
  filter(abla.pres == 1) %>% 
  pull(PLT_CN)

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

# here's the model ----

m3 <- glmer(data = ind.mort.dat,
            formula = SURV ~ 
              scale(PREV_BAA) + scale(PREV_CR) +
              #scale(MAT_remper)*scale(MAT_anom) + scale(MAP_remper)*scale(MAP_relanom) +
              scale(PREVDIA)*scale(MAP_relanom)*scale(MAT_anom)*area.fire.prop +
              scale(PREVDIA)*scale(MAP_relanom)*scale(MAT_anom)*area.id.prop +
              area.fire.prop:area.id.prop +
              (1|ECOSUBCD),
            family = binomial(link="logit"),
            nAGQ=0,
            control = glmerControl(optim = "nlminbwrap"))


## first, for general model validation kind of thing, take fitted values from model and apply them to the growMort estimation... this is going to be more difficult
# complex than I maybe entirely appreciated

weights <- ind.mort.dat %>% 
  select(TRE_CN) %>% 
  mutate(weights = predict(m3,type="response",re.form=NULL))

abla.plots <- all.fia$PLOT %>% 
  filter(abla.pres == 1) %>% 
  pull(PLT_CN)

unweighted <- growMort_dlp_weighted(db = all.fia,
                                    stateVar = "TPA",
                                    treeDomain = SPCD==19,
                                    grpBy = ECOSUBCD,
                                    totals = T,
                                    #byPlot = T,
                                    returnSpatial = F, 
                                    nCores = 4,
                                    variance = T,
                                    sizeThresh=5,
                                    evals = evals,
                                    method="TI",
                                    treeWeights=NULL) %>% 
  filter(YEAR == 2019)

weighted <- growMort_dlp_weighted(db = all.fia,
                                  stateVar = "TPA",
                                  treeDomain = SPCD==19,
                                  grpBy = ECOSUBCD,
                                  totals = T,
                                  #byPlot = T,
                                  returnSpatial = F, 
                                  nCores = 4,
                                  variance = T,
                                  sizeThresh=5,
                                  evals = evals,
                                  method="TI",
                                  treeWeights=weights) %>% 
  filter(YEAR == 2019)

plot(unweighted$CURR_TOTAL, 
     weighted$CURR_TOTAL)
abline(0,1,col="red")
summary(lm(weighted$CURR_TOTAL~unweighted$CURR_TOTAL))

plot(unweighted$MORT_TOTAL, 
     weighted$MORT_TOTAL)
abline(0,1,col="red")
summary(lm(weighted$MORT_TOTAL~unweighted$MORT_TOTAL))

plot(unweighted$CHNG_PERC, 
     weighted$CHNG_PERC,xlim=c(-100,100),ylim=c(-100,100))
abline(0,1,col="red")
summary(lm(weighted$CHNG_PERC~unweighted$CHNG_PERC))


### NOW TO TRY MAKING PREDICTIONS AND ESTIMATION FOR "NEXT" TIMESTEP

next.mort.dat <- all.fia$TREE %>% 
  filter(SPCD==19,
         STATUSCD==1,
         INVYR>2009) %>%
  select(TRE_CN, 
         PLT_CN,
         PREV_CR = CR,
         PREVDIA = DIA) %>% 
  left_join(.,
            all.fia$PLOT %>% 
              select(PLT_CN,
                     contains("MAT"),
                     contains("MAP"),
                     contains("CMD"),
                     ECOSUBCD, REMPER, ECO_GRP_NEW_NAM) %>% 
              mutate(MAT_anom = MAT_remper-MAT_base,
                     MAP_anom = MAP_remper-MAP_base,
                     CMD_anom = CMD_remper-CMD_base,
                     MAP_relanom = MAP_anom/MAP_base)) %>% 
  left_join(plot.attributes.prev %>% 
              select(PLT_CN,
                     PREV_BAA = CURR_BAA)) %>% 
  left_join(plot.attributes.baa %>% 
              select(PLT_CN,
                     PREV_BAA2 = BAA)) %>% 
  mutate(PREV_BAA = ifelse(is.na(PREV_BAA), PREV_BAA2, PREV_BAA)) %>% 
  left_join(mort.area.er %>% 
              mutate(area.fire.prop = area.fire/area.total,
                     area.id.prop = area.id/area.total)) %>% 
  filter(ECOSUBCD %in% ind.mort.dat$ECOSUBCD,
         !is.na(PREV_BAA)) %>% 
  mutate(MAT_anom = MAT_anom + 0,
         area.fire.prop = area.fire.prop + 0)


next.pred <- predict(m3,
                     newdata = next.mort.dat,
                     type="response")

# I think that if I apply next.pred as weights to a normal growMort call, I can take the resulting CURR_TOTAL and subtract it from dall.er.abla$CURR_TOTAL to get an estimate of the predicted mortality. Then put that relative to dall.er.abla$RECR_est to see if predicted mortality outweighs recruitment


next.weighted00 <- growMort_dlp_weighted(db = all.fia,
                                         stateVar = "TPA",
                                         treeDomain = SPCD==19,
                                         grpBy = ECOSUBCD,
                                         totals = T,
                                         #byPlot = T,
                                         returnSpatial = F, 
                                         nCores = 4,
                                         variance = T,
                                         sizeThresh=5,
                                         evals = evals,
                                         method="TI",
                                         treeWeights=ind.mort.dat %>% 
                                           select(TRE_CN) %>% 
                                           mutate(weights = predict(m3,type="response"))) %>% 
  filter(YEAR == 2019)


next.weighted0 <- growMort_dlp_weighted(db = all.fia,
                                        stateVar = "TPA",
                                        treeDomain = SPCD==19,
                                        grpBy = ECOSUBCD,
                                        totals = T,
                                        #byPlot = T,
                                        returnSpatial = F,
                                        nCores = 4,
                                        variance = T,
                                        sizeThresh=5,
                                        evals = evals,
                                        method="TI",
                                        treeWeights=ind.mort.dat %>%
                                          select(TRE_CN) %>%
                                          mutate(weights = predict(m3,
                                                                   newdata = ind.mort.dat %>%
                                                                     mutate(area.fire.prop=0,
                                                                            MAT_anom=0,
                                                                            area.id.prop=0,
                                                                            MAP_relanom=0,
                                                                            PREV_CR=90),
                                                                   type="response",
                                                                   re.form=NULL))) %>%
  filter(YEAR == 2019)

next.weighted1 <- growMort_dlp_weighted(db = all.fia,
                                        stateVar = "TPA",
                                        treeDomain = c(SPCD==19,
                                                       STATUSCD==1),
                                        grpBy = ECOSUBCD,
                                        totals = T,
                                        #byPlot = T,
                                        returnSpatial = F, 
                                        nCores = 4,
                                        variance = T,
                                        sizeThresh=5,
                                        evals = evals,
                                        method="TI",
                                        treeWeights=next.mort.dat %>% 
                                          select(TRE_CN) %>% 
                                          mutate(weights = predict(m3,
                                                                   newdata = next.mort.dat,
                                                                   type="response",
                                                                   re.form=NULL))) %>% 
  filter(YEAR == 2019)

next.weighted2 <- growMort_dlp_weighted(db = all.fia,
                                        stateVar = "TPA",
                                        treeDomain = SPCD==19,
                                        grpBy = ECOSUBCD,
                                        totals = T,
                                        #byPlot = T,
                                        returnSpatial = F, 
                                        nCores = 4,
                                        variance = T,
                                        sizeThresh=5,
                                        evals = evals,
                                        method="TI",
                                        treeWeights=next.mort.dat %>% 
                                          select(TRE_CN) %>% 
                                          mutate(weights = predict(m3,
                                                                   newdata = next.mort.dat %>% 
                                                                     mutate(area.fire.prop = 
                                                                              area.fire.prop+0.25,
                                                                            area.id.prop = 
                                                                              area.id.prop+0.25),
                                                                   type="response",
                                                                   re.form=NULL))) %>% 
  filter(YEAR == 2019)


next.weighted3 <- growMort_dlp_weighted(db = all.fia,
                                        stateVar = "TPA",
                                        treeDomain = SPCD==19,
                                        grpBy = ECOSUBCD,
                                        totals = T,
                                        #byPlot = T,
                                        returnSpatial = F, 
                                        nCores = 4,
                                        variance = T,
                                        sizeThresh=5,
                                        evals = evals,
                                        method="TI",
                                        treeWeights=next.mort.dat %>% 
                                          select(TRE_CN) %>% 
                                          mutate(weights = predict(m3,
                                                                   newdata = next.mort.dat %>% 
                                                                     mutate(MAT_anom = MAT_anom + 1.5,
                                                                            area.fire.prop = 
                                                                              area.fire.prop + 0.25,
                                                                            area.id.prop = 
                                                                              area.id.prop + 0.25),
                                                                   type="response",
                                                                   re.form=NULL))) %>% 
  filter(YEAR == 2019)


next.weighted4 <- growMort_dlp_weighted(db = all.fia,
                                        stateVar = "TPA",
                                        treeDomain = SPCD==19,
                                        grpBy = ECOSUBCD,
                                        totals = T,
                                        #byPlot = T,
                                        returnSpatial = F, 
                                        nCores = 4,
                                        variance = T,
                                        sizeThresh=5,
                                        evals = evals,
                                        method="TI",
                                        treeWeights=ind.mort.dat %>% 
                                          select(TRE_CN) %>% 
                                          mutate(weights = predict(m3,
                                                                   newdata = ind.mort.dat %>% 
                                                                     mutate(MAT_anom = MAT_anom + 1.5),
                                                                   type="response",
                                                                   re.form=NULL))) %>% 
  filter(YEAR == 2019)



## adding those predictions on to the dall.er.abla frame

dall.er.abla <- dall.er.abla %>% select(-contains("weight"),-contains("modfit"))

dall.er.abla <- dall.er.abla %>% 
  left_join(next.weighted00 %>% 
              select(ECOSUBCD,
                     MORT_modfit = MORT_TOTAL,
                     CURR_modfit = CURR_TOTAL),
            by=c("MAP_UNIT_S" = "ECOSUBCD")) %>% 
  left_join(next.weighted0 %>% 
              select(ECOSUBCD,
                     MORT_weight0 = MORT_TOTAL,
                     CURR_weight0 = CURR_TOTAL),
            by=c("MAP_UNIT_S" = "ECOSUBCD")) %>% 
  left_join(next.weighted1 %>% 
              select(ECOSUBCD,
                     MORT_weight1 = MORT_TOTAL,
                     CURR_weight1 = CURR_TOTAL),
            by=c("MAP_UNIT_S" = "ECOSUBCD")) %>% 
  left_join(next.weighted2 %>% 
              select(ECOSUBCD,
                     MORT_weight2 = MORT_TOTAL,
                     CURR_weight2 = CURR_TOTAL),
            by=c("MAP_UNIT_S" = "ECOSUBCD")) %>% 
  left_join(next.weighted3 %>% 
              select(ECOSUBCD,
                     MORT_weight3 = MORT_TOTAL,
                     CURR_weight3 = CURR_TOTAL),
            by=c("MAP_UNIT_S" = "ECOSUBCD")) %>% 
  left_join(next.weighted4 %>% 
              select(ECOSUBCD,
                     MORT_weight4 = MORT_TOTAL,
                     CURR_weight4 = CURR_TOTAL),
            by=c("MAP_UNIT_S" = "ECOSUBCD")) %>% 
  mutate(RECR_estmod = (CURR_modfit-PREV_TOTAL)+MORT_modfit,
         pace.obs = ifelse(MORT_TOTAL> RECR_est,0,1),
         pace00 = ifelse(MORT_modfit > RECR_estmod,0,1),
         pace0 = ifelse(MORT_weight0 > RECR_estmod,0,1),
         pace1 = ifelse(MORT_weight1 > RECR_estmod,0,1),
         pace2 = ifelse(MORT_weight2 > RECR_estmod,0,1),
         pace3 = ifelse(MORT_weight3 > RECR_estmod,0,1),
         pace4 = ifelse(MORT_weight4 > RECR_estmod,0,1))

## making "keeping pace" maps

ggplot() +
  geom_sf(data = cont %>% 
            as(.,"sf"),
          col="darkgray",
          fill = "wheat") + 
  geom_sf(data = states %>% 
            as(.,"sf"),
          fill=NA,
          col="darkgray") +
  geom_sf(data = dall.er.abla %>% 
            filter(!is.na(CHNG_PERC.tpa.abla),
                   nPlots_AREA>9),
          col="darkgray") +
  geom_sf(data = dall.er.abla %>% 
            filter(!is.na(CHNG_PERC.tpa.abla),
                   nPlots_AREA>9),
          col=NA,
          aes(
            # fill = factor(pace.obs)
            # fill = factor(pace00)
             fill = factor(pace0)
            # fill = factor(pace1)
            # fill = factor(pace2)
            # fill = factor(pace3)
            # fill = factor(pace4)
            )) +
  geom_sf(data = states %>% 
            as(.,"sf"),
          fill=NA,
          col="darkgray",
          alpha=0.1,
          lwd=0.3,
          lty=2) +
  theme(panel.background = element_rect(fill="skyblue1")) +
  scale_fill_manual(values = c("0" = "firebrick2",
                               "1" = "dodgerblue3"))+
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.key.size = unit(2,"cm"))#,
        #legend.position="none")

## making density plots

dall.er.abla %>% 
  ggplot(.) +
  geom_density(aes(x=MORT_modfit/PREV_TOTAL),
               fill = "gray",
               col="gray20",
               lwd=1.2) +
  lims(x=c(0,1))

dall.er.abla %>% 
  ggplot(.) +
  geom_density(aes(x=MORT_TOTAL/PREV_TOTAL),
               fill = "gray",
               col="gray20",
               lwd=1.2) +
  geom_density(aes(x=MORT_weight1/CURR_TOTAL),
               fill = "firebrick2",
               alpha=0.5,
               col="firebrick3",
               lwd=1.2) +
  xlab("Ecoregional mortality")+
  lims(x=c(0,1))

dall.er.abla %>% 
  ggplot(.) +
  geom_density(aes(x=MORT_TOTAL/PREV_TOTAL),
               fill = "gray",
               col="gray20",
               lwd=1.2) +
  geom_density(aes(x=MORT_weight2/CURR_TOTAL),
               fill = "firebrick2",
               alpha=0.5,
               col="firebrick3",
               lwd=1.2) +
  xlab("Ecoregional mortality")+
  lims(x=c(0,1))

dall.er.abla %>% 
  ggplot(.) +
  geom_density(aes(x=MORT_TOTAL/PREV_TOTAL),
               fill = "gray",
               col="gray20",
               lwd=1.2) +
  geom_density(aes(x=MORT_weight3/CURR_TOTAL),
               fill = "firebrick2",
               alpha=0.5,
               col="firebrick3",
               lwd=1.2) +
  xlab("Ecoregional mortality")+
  lims(x=c(0,1))



dall.er.abla %>% 
  ggplot(.) +
  geom_density(aes(x=MORT_TOTAL/PREV_TOTAL),
               fill = "gray",
               col="gray20",
               lwd=1.2) +
  geom_density(aes(x=MORT_weight4/CURR_TOTAL),
               fill = "firebrick2",
               alpha=0.5,
               col="firebrick3",
               lwd=1.2) +
  xlab("Ecoregional mortality")+
  lims(x=c(0,1))






