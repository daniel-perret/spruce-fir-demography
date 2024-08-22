## supplementary map and table

### adding region classifications -- the logic here is combining provinces, then adding subsections that are most proximate to those provicnes (e.g., combining 342H with M332/M333)

rad <- rad %>% 
  mutate(region = case_when(grepl("M332|M333|342H", ECOSUBCD) ~ "NR",
                            grepl("M242", ECOSUBCD) ~ "PNW",
                            grepl("M331|341|313|321|342G", ECOSUBCD) ~ "SR"))

### map of provinces/regions
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
  geom_sf(data = rad,
          col = linecolor,
          fill = NA)+
  geom_sf(data = rad,
          col=NA,
          aes(fill = region)) +
  geom_sf(data = states %>% 
            as(.,"sf"),
          col=linecolor,
          fill=NA,
          lty=2) +
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.position="right",
        panel.background = element_rect(fill="skyblue1"))  

### calculating summary statistics for each province to fill in supplementary table

rad %>% 
  sf::st_drop_geometry() %>% 
  group_by(region) %>% 
  summarise(AREA = sum(AREA))

rad %>% 
  sf::st_drop_geometry() %>% 
  group_by(region) %>% 
  summarise(resist = sum(resilience_2050hi * AREA),
            accept = sum(replacement_2050lo * AREA),
            direct = sum((resilience_2050lo - resilience_2050hi)*AREA))

### doing estimation once again to get info re: land tenure

all.fia$COND <- all.fia$COND %>% 
  mutate(own_res = paste(OWNGRPCD,RESERVCD,sep="_"))

land <- growMort_dlp.metric(db = all.fia,
                                  stateVar = "TPA",
                                  polys = er4.co %>%
                                    sf::st_as_sf() %>%
                                    select(MAP_UNIT_S),
                                  treeDomain = co.pres==1,
                                  areaDomain = co.pres==1,
                                  grpBy = own_res,
                                  totals = TRUE,
                                  returnSpatial = F, 
                                  nCores = 4,
                                  #variance = T,
                                  sizeThresh=thresh,
                                  evals = evals,
                                  method="TI",
                                  treeList=F) %>% 
  group_by(MAP_UNIT_S) %>% 
  filter(YEAR==max(YEAR)) # 2019 inventory for all states but WY

land %>% 
  select(ECOSUBCD=MAP_UNIT_S, own_res, AREA_TOTAL_ha) %>% 
  filter(ECOSUBCD %in% rad$ECOSUBCD) %>%
  mutate(region = case_when(grepl("M332|M333|342H", ECOSUBCD) ~ "NR",
                            grepl("M242", ECOSUBCD) ~ "PNW",
                            grepl("M331|341|313|321|342G", ECOSUBCD) ~ "SR"),
         class = case_when(own_res %in% c("10_0", "20_0") ~ "fed.unres",
                           own_res %in% c("10_1","20_1") ~ "fed.res",
                           own_res %in% c("30_0", "40_0", "30_1") ~ "other")) %>% 
  group_by(region, class) %>% 
  summarise(across(where(is.numeric), sum))

### Figures for northern rockies ------

p.m.test %>% 
  filter(grepl("M332|M333", ECOSUBCD)) %>%
  group_by(pred.type, dist, time) %>% 
  summarise(resilience = sum(resilience*AREA)/areatotal,
            restructuring = sum(restructuring*AREA)/areatotal,
            reassembly = sum(reassembly*AREA)/areatotal,
            replacement = sum(replacement*AREA)/areatotal) %>%
  pivot_longer(cols = resilience:replacement,
               names_to = "trajectory", 
               values_to = "prop") %>% 
  ggplot(.,
         aes(x = factor(time, levels = c("curr","2050","2080")),
             fill = factor(trajectory, levels = c("replacement","reassembly",
                                                  "restructuring","resilience")))) +
  geom_bar(aes(weight=prop)) +
  scale_fill_manual(name = "trajectory",
                    values = c("resilience" = "dodgerblue2",
                               "restructuring" = "gold2",
                               "reassembly" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("fill")) +
  facet_wrap(facets = ~factor(dist,
                              levels = c("lo","obs","hi")),
             nrow=3) +
  labs(x = "time") +
  theme(axis.text.x = element_text(angle=45, hjust=1))


### Figures for PNW - -----

p.m.test %>% 
  filter(grepl("M242", ECOSUBCD)) %>%
  group_by(pred.type, dist, time) %>% 
  summarise(resilience = sum(resilience*AREA)/areatotal,
            restructuring = sum(restructuring*AREA)/areatotal,
            reassembly = sum(reassembly*AREA)/areatotal,
            replacement = sum(replacement*AREA)/areatotal) %>%
  pivot_longer(cols = resilience:replacement,
               names_to = "trajectory", 
               values_to = "prop") %>% 
  ggplot(.,
         aes(x = factor(time, levels = c("curr","2050","2080")),
             fill = factor(trajectory, levels = c("replacement","reassembly",
                                                  "restructuring","resilience")))) +
  geom_bar(aes(weight=prop)) +
  scale_fill_manual(name = "trajectory",
                    values = c("resilience" = "dodgerblue2",
                               "restructuring" = "gold2",
                               "reassembly" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("fill")) +
  facet_wrap(facets = ~factor(dist,
                              levels = c("lo","obs","hi")),
             nrow=3) +
  labs(x = "time") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

### Figures for Southern Rockies------

p.m.test %>% 
  filter(grepl("M331|313", ECOSUBCD)) %>%
  group_by(pred.type, dist, time) %>% 
  summarise(resilience = sum(resilience*AREA)/areatotal,
            restructuring = sum(restructuring*AREA)/areatotal,
            reassembly = sum(reassembly*AREA)/areatotal,
            replacement = sum(replacement*AREA)/areatotal) %>%
  pivot_longer(cols = resilience:replacement,
               names_to = "trajectory", 
               values_to = "prop") %>% 
  ggplot(.,
         aes(x = factor(time, levels = c("curr","2050","2080")),
             fill = factor(trajectory, levels = c("replacement","reassembly",
                                                  "restructuring","resilience")))) +
  geom_bar(aes(weight=prop)) +
  scale_fill_manual(name = "trajectory",
                    values = c("resilience" = "dodgerblue2",
                               "restructuring" = "gold2",
                               "reassembly" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("fill")) +
  facet_wrap(facets = ~factor(dist,
                              levels = c("lo","obs","hi")),
             nrow=3) +
  labs(x = "time") +
  theme(axis.text.x = element_text(angle=45, hjust=1))