dall.er.ablapien <- dall.er.sp %>%
  ungroup() %>% 
  #sf::st_drop_geometry() %>% 
  filter(SPCD %in% c(19,93)) %>%
  pivot_wider(names_from = c("SPCD"),
              values_from = 5:164) %>% 
  filter(CURR_TPH_93 > 0 & CURR_TPH_19 > 0 |
           PREV_TPH_93>0 & CURR_TPH_19>0) %>% 
  mutate(quad.baa = case_when(CHNG_PERC.baa_19 < 0 & CHNG_PERC.baa_93 < 0 ~ 1,
                              CHNG_PERC.baa_19 < 0 & CHNG_PERC.baa_93 >= 0 ~ 2,
                              CHNG_PERC.baa_19 >= 0 & CHNG_PERC.baa_93 >= 0 ~ 3,
                              CHNG_PERC.baa_19 >= 0 & CHNG_PERC.baa_93 < 0 ~ 4))
 