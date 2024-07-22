### Figures for spruce-fir paper

### Figure 1 is a conceptual/box figure showing joint species' trajectories and how they map from Seidl/Turner

### Figure 2 -----

# Map with joint trajectories and mortality sources for each; area percentages as well

## MAP FIGURE
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
                               "restructuring" = "gold2",
                               "reassembly" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("col","fill"))+
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.position="none",
        panel.background = element_rect(fill="skyblue1"))

## MORTALITY SOURCES ESTIMATION AND BAR CHARTS
all.fia$TREE <- all.fia$TREE %>% 
  mutate(focal = case_when(SPCD==19 ~ "abla",
                           SPCD==93 ~ "pien"))

mort.src.est <- all.fia %>% 
  growMort_dlp.metric(db = .,
                      stateVar="TPA",
                      treeDomain = co.pres==1,
                      areaDomain = co.pres==1,
                      totals = TRUE,
                      grpBy = c(mult.comp.coexist, focal, agent_key),
                      returnSpatial = F, 
                      nCores = 4,
                      sizeThresh = 12.7,
                      evals = evals,
                      method="TI") %>% 
  filter(agent_key!="unknown2",
         !is.na(focal)) %>% 
  group_by(mult.comp.coexist) %>% 
  filter(YEAR==max(YEAR))

mort.src.est %>% 
  group_by(mult.comp.coexist, focal) %>% 
  mutate(PREV_TOTAL2=PREV_TOTAL/sum(PREV_TOTAL),
         PREV_TOTAL_SD = sqrt(PREV_TOTAL_VAR)) %>% 
  ggplot(.,
         aes(x = factor(agent_key,levels = c("fire","disease","insect","weather","unknown1","competition","land use", "animal")),
             y = PREV_TPH,
             group=focal))+
  geom_col(aes(fill = as.factor(mult.comp.coexist),
               alpha=focal),
           position=position_dodge(0.9),
           col="black") +
  geom_errorbar(aes(ymin = (PREV_TPH - (CHNG_TPH_SE)),
                    ymax = (PREV_TPH + (CHNG_TPH_SE))),
                position=position_dodge(0.9),
                width=0.3,
                lwd=0.8)+
  facet_wrap(facets=~factor(mult.comp.coexist, levels=c("resilience",
                                                        "restructuring",
                                                        "reassembly",
                                                        "replacement"),),
             scales="fixed",ncol = 1) +
  scale_fill_manual(name = "multispecies trajectory",
                    values = c("resilience" = "dodgerblue2",
                               "restructuring" = "gold2",
                               "reassembly" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("col","fill")) +  
  scale_alpha_manual(name = "species",
                     values = c("abla" = 1,
                                "pien" = 0.3)) +
  xlab("")+
  ylab("Mortality (individuals per hectare)")+
  theme(axis.text.x = element_text(angle=45,hjust=1),
        strip.text.x = element_blank(),
        legend.position="none")

## scenario predictions

p.m.test <- p.m.preds %>%
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
         class.2050lo = predict(m.m, newdata = p.m.preds %>% 
                                  mutate(m.pred.19 = m19.2050lo,
                                         m.pred.93 = m93.2050lo,
                                         s.pred.19 = s19.2050lo,
                                         s.pred.93 = s93.2050lo,
                                         r.pred.19 = r19.2050lo,
                                         r.pred.93 = r93.2050lo), type="class"),
         class.2050hi = predict(m.m, newdata = p.m.preds %>% 
                                  mutate(m.pred.19 = m19.2050hi,
                                         m.pred.93 = m93.2050hi,
                                         s.pred.19 = s19.2050hi,
                                         s.pred.93 = s93.2050hi,
                                         r.pred.19 = r19.2050hi,
                                         r.pred.93 = r93.2050hi), type="class"),
         class.2080lo = predict(m.m, newdata = p.m.preds %>% 
                                  mutate(m.pred.19 = m19.2080lo,
                                         m.pred.93 = m93.2080lo,
                                         s.pred.19 = s19.2080lo,
                                         s.pred.93 = s93.2080lo,
                                         r.pred.19 = r19.2080lo,
                                         r.pred.93 = r93.2080lo), type="class"),
         class.2080hi = predict(m.m, newdata = p.m.preds %>% 
                                  mutate(m.pred.19 = m19.2080hi,
                                         m.pred.93 = m93.2080hi,
                                         s.pred.19 = s19.2080hi,
                                         s.pred.93 = s93.2080hi,
                                         r.pred.19 = r19.2080hi,
                                         r.pred.93 = r93.2080hi), type="class")) %>% 
  left_join(dall.er.ablapien %>% 
              sf::st_drop_geometry() %>% 
              select(ECOSUBCD = MAP_UNIT_S,
                     observed = mult.comp.coexist,
                     AREA = AREA_TOTAL_ha_19))


p.m.test %>% 
  select(ECOSUBCD,class.currlo:class.2080hi,AREA) %>% 
  pivot_longer(cols = class.currlo:class.2080hi,
               names_to=c("class","time","dist"), names_sep=c(6,10),
               values_to="pred") %>%
  select(-class) %>%
  ggplot(.,
         aes(x = factor(time, levels = c("curr","2050","2080")),
             fill = factor(pred, levels = c("replacement","reassembly",
                                            "restructuring","resilience")))) +
  geom_bar(aes(weight=AREA)
  ) +
  scale_fill_manual(name = "trajectory",
                    values = c("resilience" = "dodgerblue2",
                               "restructuring" = "gold2",
                               "reassembly" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("fill")) +
  facet_wrap(facets = ~factor(dist,
                              levels = c("lo","hi")),
             nrow=3) +
  labs(x = "time") +
  theme(axis.text.x = element_text(angle=45, hjust=1))





























