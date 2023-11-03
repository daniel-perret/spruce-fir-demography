t <- mort.decomp.abla %>% 
  ungroup() %>% 
  mutate(TOT_MORTPERC=TOT_MORT/PREV_TOTAL,
         TOT_MORTPERC=ifelse(is.na(TOT_MORTPERC),0,TOT_MORTPERC)) %>% 
  # left_join(mort.decomp %>% 
  #             select(ECOSUBCD,MAT_anom,CMD_anom) %>% 
  #             group_by(ECOSUBCD) %>% 
  #             summarize(MAT_anom.er=mean(na.omit(MAT_anom)),
  #                       CMD_anom.er=mean(na.omit(CMD_anom))),
  #           by=c("MAP_UNIT_S"="ECOSUBCD")) %>% 
  left_join(all.fia$PLOT %>% 
              select(ECOSUBCD,MAT_base,MAT_remper,CMD_base,CMD_remper) %>% 
              group_by(ECOSUBCD) %>% 
              summarise(MAT_anom.er = mean(MAT_remper-MAT_base,na.rm=T),
                        CMD_anom.er=mean(CMD_remper-CMD_base,na.rm=T)),
            by=c("MAP_UNIT_S"="ECOSUBCD")) %>% 
  left_join(mort.area.er %>% 
              mutate(area.fire.prop = area.fire/area.total,
                     area.insect.prop = area.insect/area.total,
                     area.disease.prop = area.disease/area.total,
                     area.id.prop = area.id/area.total,
                     area.fid.prop = area.fid/area.total),
            by=c("MAP_UNIT_S"="ECOSUBCD")) %>% 
  filter(!is.na(MAT_anom.er)) %>% 
  mutate(PREV_TPA = PREV_TOTAL/AREA_TOTAL,
         PREV_TPA_scaled = as.numeric(scale(PREV_TPA)),
         MAT_anom.er_scaled = as.numeric(scale(MAT_anom.er)),
         CMD_anom.er_scaled = as.numeric(scale(CMD_anom.er)))

t2 <- t %>% sf::st_drop_geometry()

m.er <- glm(data = t2,
            formula = TOT_MORTPERC ~ 
              PREV_TPA_scaled +
              area.fire.prop*MAT_anom.er_scaled+
              area.id.prop*MAT_anom.er_scaled+
              area.id.prop:area.fire.prop,
            family = binomial(link="logit"),
            weights = nPlots_TREE)



summary(m.er)

performance(m.er)

m.er$data %>% 
  mutate(PRED=predict(m.er,type="response")) %>% 
  ggplot(.,
         aes(x=TOT_MORTPERC,
             y=PRED))+
  geom_point(pch=19, size=3, alpha=0.6)+
  geom_abline(intercept=0,slope=1,col="red")

m.er$data %>% 
  mutate(PRED=predict(m.er,type="response")) %>% 
  summarise(r = cor(TOT_MORTPERC,PRED))



s.mean <- mean(mort.decomp$MAT_anom)
s.sd <- sd(mort.decomp$MAT_anom)

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


ggpredict(m.er,
          terms = c("MAT_anom.er_scaled [-4:5,by=0.1]", 
                    "area.id.prop [0, 0.3, 0.6, 1]", 
                    "area.fire.prop [0,0.1,0.25,0.5, 0.75, 1]"),
          #condition = c(INVYR=2019),
          #               PREV_BAA_abla_scaled = 0,
          #               PREV_BAA_all_scaled = 0),
          type="fe") %>% as.data.frame() %>% 
  filter(facet==0.1, 
         #!group%in%c(0.3,0.6,1)
         ) %>% 
  ggplot(.,
         aes((x*s.sd)+s.mean, predicted*100, groups = group)) +
  geom_ribbon(aes(ymin = conf.low*100,
                  ymax = conf.high*100),
              fill="darkgray",
              alpha=0.3)+
  geom_line(aes(col = group),
            lwd=4.5) +
  # geom_vline(xintercept = s.mean,
  #            lty=2)+
  labs(x = "",
       y= "") +
  scale_color_manual(name = "Proportion \ninsects/disease",
                     values = c("0" = "dodgerblue4",
                                "0.3" = "dodgerblue1",
                                #"0.5" = "black",
                                "0.6" = "orange",
                                "1" = "red")) +
  # geom_rug(sides = "b", data = mort.decomp, aes(x = MAT_anom), inherit.aes=F)+
  # facet_wrap(facets=~facet,
  #            labeller = as_labeller(c("0" = "0% burned",
  #                                     "0.1" = "10% burned",
  #                                     "0.25" = "25% burned",
  #                                     "0.5" = "50% burned",
  #                                     "0.75" = "75% burned",
  #                                     "1" = "100% burned")),nrow = 3)+
  theme(legend.position = "none",
        axis.text.x = element_text(size=40), axis.ticks = element_line(size=2), axis.ticks.length = unit(0.3,"cm"),
        axis.text.y = element_text(size=40),
        panel.border = element_rect(size=6)) +
  ylim(0,100)
  


#### PREDICTIONS

# baseline, plus 1 degree warming, plus 10% additional insect & fire

t.preds <- t %>% 
  mutate(fitted = predict(m.er,type="response"),
         deg1 = predict(m.er,
                        newdata = t %>% 
                          mutate(MAT_anom.er2 = MAT_anom.er+1,
                                 MAT_anom.er_scaled = (MAT_anom.er2/sd(MAT_anom.er))-mean(MAT_anom.er)),
                        type="response"),
         deg1.id10 = predict(m.er,
                             newdata = t %>% 
                               mutate(MAT_anom.er2 = MAT_anom.er+1,
                                      MAT_anom.er_scaled = (MAT_anom.er2/sd(MAT_anom.er))-mean(MAT_anom.er),
                                      area.id.prop = area.id.prop+0.1),
                             type="response"),
         deg1.id10.fire5 = predict(m.er,
                                    newdata = t %>% 
                                      mutate(MAT_anom.er2 = MAT_anom.er+1,
                                             MAT_anom.er_scaled = (MAT_anom.er2/sd(MAT_anom.er))-mean(MAT_anom.er),
                                             area.id.prop = area.id.prop+0.1,
                                             area.fire.prop = area.fire.prop+0.05),
                                    type="response"),
         deg1.id10.fire10 = predict(m.er,
                                    newdata = t %>% 
                                      mutate(MAT_anom.er2 = MAT_anom.er+1,
                                             MAT_anom.er_scaled = (MAT_anom.er2/sd(MAT_anom.er))-mean(MAT_anom.er),
                                             area.id.prop = area.id.prop+0.1,
                                             area.fire.prop = area.fire.prop+0.1),
                                    type="response"),
         deg1.id10.fire05 = predict(m.er,
                                    newdata = t %>% 
                                      mutate(MAT_anom.er2 = MAT_anom.er+1,
                                             MAT_anom.er_scaled = (MAT_anom.er2/sd(MAT_anom.er))-mean(MAT_anom.er),
                                             area.id.prop = area.id.prop+0.1,
                                             area.fire.prop = area.fire.prop+0.05),
                                    type="response"),
         deg0.id0.fire0 = predict(m.er,
                                  newdata = t %>% 
                                    mutate(area.id.prop = 0,
                                           area.fire.prop = 0),
                                  type="response"),
         deg1.id0.fire0 = predict(m.er,
                                  newdata = t %>% 
                                    mutate(MAT_anom.er2 = MAT_anom.er+1,
                                           MAT_anom.er_scaled = (MAT_anom.er2/sd(MAT_anom.er))-mean(MAT_anom.er),
                                           area.id.prop = 0,
                                           area.fire.prop = 0),
                                  type="response"))





ggplot() +
  geom_sf(data = cont %>% 
            as(.,"sf"),
          col="darkgray",
          fill = "wheat") + 
  geom_sf(data = states %>% 
            as(.,"sf"),
          fill=NA,
          col="darkgray") +
  geom_sf(data = mort.decomp.abla,
          fill="darkgray",
          col=NA) +
  geom_sf(data = t.preds %>% 
            mutate(fitted.MORT = PREV_TOTAL*fitted,
                   #fitted.MORT = PREV_TOTAL*deg0.id0.fire0,
                   #fitted.MORT = PREV_TOTAL*deg1.id0.fire0,
                   #fitted.MORT = PREV_TOTAL*deg1,
                   #fitted.MORT = PREV_TOTAL*deg1.id10,
                   #fitted.MORT = PREV_TOTAL*deg1.id10.fire05,
                   new.pace = ifelse(fitted.MORT>RECR_est,"no","yes")),
          col=NA,
          aes(fill = new.pace)) +
  geom_sf(data = states %>% 
            as(.,"sf"),
          fill=NA,
          col="darkgray",
          alpha=0.1,
          lwd=0.3,
          lty=2) +
  theme(panel.background = element_rect(fill="skyblue1"))+
  scale_fill_discrete(name = "",
                      type=c("firebrick2","dodgerblue3"))+
  #labs(title="Has recruitment kept pace with \ntotal mortality?")+
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.position="none")








ggplot() +
  geom_sf(data = cont %>% 
            as(.,"sf"),
          col="darkgray",
          fill = "wheat") + 
  geom_sf(data = states %>% 
            as(.,"sf"),
          fill=NA,
          col="darkgray") +
  geom_sf(data = er4.abla %>% sf::st_as_sf(),
          fill="darkgray",
          col="darkgray") +
  geom_sf(data = t,
          col=NA,
          aes(fill = TOT_MORTPERC*100)) +
  geom_sf(data = states %>% 
            as(.,"sf"),
          fill=NA,
          col="darkgray",
          alpha=0.1,
          lwd=0.3,
          lty=2) +
  theme(panel.background = element_rect(fill="skyblue1"))+
  scale_fill_steps(name = "Total % mortality",
                   na.value="darkgray",
                   low="white",
                   high="firebrick3",
                   limits=c(0,100),
                   n.breaks=5)+
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.key.size = unit(2,"cm"))




ggplot() +
  geom_sf(data = cont %>% 
            as(.,"sf"),
          col="darkgray",
          fill = "wheat") + 
  geom_sf(data = states %>% 
            as(.,"sf"),
          fill=NA,
          col="darkgray") +
  geom_sf(data = er4.abla %>% sf::st_as_sf(),
          fill="darkgray",
          col="darkgray") +
  geom_sf(data = t,
          col=NA,
          aes(fill = area.fire.prop*100)) +
  geom_sf(data = states %>% 
            as(.,"sf"),
          fill=NA,
          col="darkgray",
          alpha=0.1,
          lwd=0.3,
          lty=2) +
  theme(panel.background = element_rect(fill="skyblue1"))+
  scale_fill_steps(name = "Fire area",
                   na.value="darkgray",
                   low="white",
                   high="firebrick3",
                   limits=c(0,100),
                   n.breaks=9)+
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.key.size = unit(2,"cm"))

ggplot() +
  geom_sf(data = cont %>% 
            as(.,"sf"),
          col="darkgray",
          fill = "wheat") + 
  geom_sf(data = states %>% 
            as(.,"sf"),
          fill=NA,
          col="darkgray") +
  geom_sf(data = er4.abla %>% sf::st_as_sf(),
          fill="darkgray",
          col="darkgray") +
  geom_sf(data = t,
          col=NA,
          aes(fill = area.id.prop*100)) +
  geom_sf(data = states %>% 
            as(.,"sf"),
          fill=NA,
          col="darkgray",
          alpha=0.1,
          lwd=0.3,
          lty=2) +
  theme(panel.background = element_rect(fill="skyblue1"))+
  scale_fill_steps(name = "ID area",
                   na.value="darkgray",
                   low="white",
                   high="firebrick3",
                   limits=c(0,100),
                   n.breaks=9)+
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.key.size = unit(2,"cm"))












