
# rewritten estimation code needs us to specify the EvalIDs that we're interested in
evals <- c(21903,41903,61903,81903,161903,301903,321903,351903,411903,491903,531903,561903)

# minimum size threshold for estimate
thresh <- 12.7

all.co <- all.fia$PLOT %>% filter(co.pres ==1) %>% pull(PLT_CN)

all.fia$TREE <- all.fia$TREE %>% 
  mutate(co.pres = ifelse(PLT_CN %in% all.co |
                            PREV_PLT_CN %in% all.co, 1, 0))

## Density change estimates for all species
dtpa.er.sp <- growMort_dlp.metric(db = all.fia,
                                  stateVar = "TPA",
                                  polys = er4.co %>%
                                    sf::st_as_sf() %>%
                                    select(MAP_UNIT_S),
                                  treeDomain = co.pres==1,
                                  grpBy = SPCD,
                                  totals = TRUE,
                                  returnSpatial = T, 
                                  nCores = 4,
                                  #variance = T,
                                  sizeThresh=thresh,
                                  evals = evals,
                                  method="TI",
                                  treeList=F) %>% 
  group_by(MAP_UNIT_S) %>% 
  filter(YEAR==max(YEAR)) # 2019 inventory for all states but WY

## Basal area change estimates for all species
dbaa.er.sp <- all.fia %>% 
  growMort_dlp.metric(db = .,
                      stateVar = "BAA",
                      polys = er4.co %>%
                        sf::st_as_sf() %>% 
                        select(MAP_UNIT_S),
                      treeDomain = co.pres==1,
                      grpBy = SPCD,
                      totals = TRUE,
                      returnSpatial = F, 
                      nCores = 4,
                      #variance = T,
                      sizeThresh=thresh,
                      evals = evals,
                      method="TI") %>% 
  group_by(MAP_UNIT_S) %>% 
  filter(YEAR==max(YEAR))

#combining TPA and BAA change estimates
dall.er.sp <- left_join(dtpa.er.sp, dbaa.er.sp,
                        suffix = c(".tph",".bah"),
                        by=c("MAP_UNIT_S","polyID","SPCD","YEAR","N",
                             "nPlots_TREE","nPlots_AREA","AREA_TOTAL_ha"))



# total agent-specific mortality for all species
dtpa.er.sp.agent <- all.fia %>% 
  growMort_dlp.metric(db = .,
                      stateVar = "TPA",
                      polys = er4.co %>%
                        sf::st_as_sf() %>% 
                        select(MAP_UNIT_S),
                      totals = TRUE,
                      treeDomain = co.pres==1,
                      grpBy = c(SPCD, agent_key),
                      returnSpatial = T, 
                      nCores = 4,
                      #variance = T,
                      sizeThresh = thresh,
                      evals = evals,
                      method="TI") %>% 
  group_by(MAP_UNIT_S) %>% 
  filter(YEAR==max(YEAR))

# pulling out ABLA/PIEN and formatting

dall.er.ablapien <- dall.er.sp %>%
  ungroup() %>% 
  filter(SPCD %in% c(19,93)) %>%
  pivot_wider(names_from = c("SPCD"),
              values_from = c(5:86,88:ncol(.))) %>% 
  filter(CURR_TPH_93 > 0 & CURR_TPH_19 > 0 |
           PREV_TPH_93>0 & CURR_TPH_19>0) %>% 
  mutate(CHNG_PERC.bah_19.c = ifelse(abs(CHNG_PERC_CV.bah_19)>=100, 0, CHNG_PERC.bah_19),
         CHNG_PERC.tph_19.c = ifelse(abs(CHNG_PERC_CV.tph_19)>=100, 0, CHNG_PERC.tph_19),
         CHNG_PERC.bah_93.c = ifelse(abs(CHNG_PERC_CV.bah_93)>=100, 0, CHNG_PERC.bah_93),
         CHNG_PERC.tph_93.c = ifelse(abs(CHNG_PERC_CV.tph_93)>=100, 0, CHNG_PERC.tph_93),
         CHNG_PERC.bah_19.c = ifelse(is.na(CHNG_PERC.bah_19.c), 0, CHNG_PERC.bah_19.c),
         CHNG_PERC.tph_19.c = ifelse(is.na(CHNG_PERC.tph_19.c), 0, CHNG_PERC.tph_19.c),
         CHNG_PERC.bah_93.c = ifelse(is.na(CHNG_PERC.bah_93.c), 0, CHNG_PERC.bah_93.c),
         CHNG_PERC.tph_93.c = ifelse(is.na(CHNG_PERC.tph_93.c), 0, CHNG_PERC.tph_93.c) ) %>% 
  mutate(quad.19 = case_when(CHNG_PERC.tph_19.c < 0 & CHNG_PERC.bah_19.c < 0 ~ "decline",
                             CHNG_PERC.tph_19.c < 0 & CHNG_PERC.bah_19.c >= 0 ~ "development",
                             CHNG_PERC.tph_19.c >= 0 & CHNG_PERC.bah_19.c >= 0 ~ "densification",
                             CHNG_PERC.tph_19.c >= 0 & CHNG_PERC.bah_19.c < 0 ~ "turnover"),
         quad.93 = case_when(CHNG_PERC.tph_93.c < 0 & CHNG_PERC.bah_93.c < 0 ~ "decline",
                             CHNG_PERC.tph_93.c < 0 & CHNG_PERC.bah_93.c >= 0 ~ "development",
                             CHNG_PERC.tph_93.c >= 0 & CHNG_PERC.bah_93.c >= 0 ~ "densification",
                             CHNG_PERC.tph_93.c >= 0 & CHNG_PERC.bah_93.c < 0 ~ "turnover"),
         mult.comp.coexist = case_when(quad.19 == "decline" & quad.93 == "decline" ~ "replacement",
                                       
                                       quad.19 == "turnover" | quad.93 == "turnover" ~ "structural change",
                                       
                                       quad.19 == "decline" & quad.93 %in% c("development","densification") ~ "compositional change",
                                       quad.93 == "decline" & quad.19 %in% c("development","densification") ~ "compositional change",
                                       
                                       quad.19 %in% c("development","densification") & 
                                         quad.93 %in% c("development","densification") ~ "resilience"))


all.fia$PLOT <- all.fia$PLOT %>% 
  left_join(dall.er.ablapien %>% 
              sf::st_drop_geometry() %>% 
              select(MAP_UNIT_S, quad.19, quad.93, mult.comp.coexist),
            by=c("ECOSUBCD"="MAP_UNIT_S"))

