boop <- dall.er.ablapien %>%
  left_join(.,
            p.m.class,
            by = c("MAP_UNIT_S" = "ECOSUBCD")) %>% 
  mutate(opportunity = ifelse(class.2050lo == "resilience" & 
                                class.2050hi != "resilience",
                              1,0),
         opportunity=ifelse(is.na(opportunity),0,opportunity),
         allgood = ifelse(class.2080hi =="resilience",
                          1,0),
         NR = ifelse(grepl("M332|M333|M331A", MAP_UNIT_S),1,0))

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
  geom_sf(data = boop,
          col = linecolor,
          fill = NA)+
  geom_sf(data = boop,
          col=NA,
          aes(fill = factor(allgood))) +
  # geom_sf(data = boop %>% 
  #           filter(NR==1) %>% 
  #           sf::st_union(),
  #         col="red",
  #         fill=NA,
  #         linewidth=1.5) +
  geom_sf(data = states %>% 
            as(.,"sf"),
          col=linecolor,
          fill=NA,
          lty=2) +
  scale_fill_manual(name = "",
                    values = c("1" = "dodgerblue2",
                               "0" = "gray95"),
                    aesthetics = c("col","fill"))+
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.position="right",
        panel.background = element_rect(fill="skyblue1"))


boop %>% 
  filter(opportunity==1) %>% 
  pull(AREA_ha) %>% 
  sum()

boop %>% 
  filter(opportunity==1, NR==1) %>% 
  pull(AREA_ha) %>% 
  sum()




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
  geom_sf(data = boop %>% 
            filter(NR==1),
          col = linecolor,
          fill = NA)+
  geom_sf(data = boop %>% 
            filter(NR==1),
          col=NA,
          aes(fill = factor(opportunity))) +
  # geom_sf(data = boop %>% 
  #           filter(NR==1) %>% 
  #           sf::st_union(),
  #         col="red",
  #         fill=NA,
  #         linewidth=1.5) +
  geom_sf(data = states %>% 
            as(.,"sf"),
          col=linecolor,
          fill=NA,
          lty=2) +
  scale_fill_manual(name = "",
                    values = c("1" = "dodgerblue2",
                               "0" = "gray95"),
                    aesthetics = c("col","fill"))+
  lims(x = c(-2.0e6, -0.75e6),
       y = c(2.350e6,3.15e6)) +
  theme(legend.position="right",
        panel.background = element_rect(fill="skyblue1"))



zoop <- all.fia$COND %>%   
  left_join(popinfo %>% 
               select(PLT_CN,EXPNS)) %>%
  left_join(all.fia$PLOT %>% 
              select(PLT_CN,ECOSUBCD),
            by="PLT_CN") %>% 
  filter(PLT_CN %in% seed.dat$PLT_CN) %>% 
  left_join(boop %>% 
              select(ECOSUBCD=MAP_UNIT_S,
                     opportunity),
            by="ECOSUBCD") %>% 
  filter(opportunity==1)

sum(zoop$EXPNS)

zoop %>% 
  filter(CONDID==1,
         OWNCD==11) %>% 
  mutate(totalarea = sum(EXPNS)) %>% 
  group_by(OWNCD, RESERVCD) %>% 
  summarize(AREA = sum(EXPNS),
            AREA.prop = sum(EXPNS)/mean(totalarea))
  

zoop %>% 
  count(OWNCD,RESERVCD)





















