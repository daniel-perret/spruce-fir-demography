library(brms)

# making p.m dataframe ------

m19.fit <- brms::posterior_predict(m19, ndraws=1000) %>% colMeans()
m93.fit <- brms::posterior_predict(m93, ndraws=1000) %>% colMeans()
s19.fit <- brms::posterior_predict(s19, ndraws=1000) %>% colMeans()
s93.fit <- brms::posterior_predict(s93, ndraws=1000) %>% colMeans()
r19.fit <- brms::posterior_predict(r19, ndraws=1000) %>% colMeans()
r93.fit <- brms::posterior_predict(r93, ndraws=1000) %>% colMeans()



p19 <- ind.mort.dat %>%
  filter(SPCD==19) %>%
  mutate(m.pred = 1-m19.fit) %>%
  group_by(mult.comp.coexist,ECOSUBCD) %>%
  summarise(m.pred = mean(m.pred),
            mean.dia = mean(PREVDIA)) %>%
  left_join(seed.dat %>%
              filter(SPCD==19) %>%
              mutate(s.pred = s19.fit,
                     r.pred = r19.fit) %>%
              group_by(mult.comp.coexist,ECOSUBCD) %>%
              summarise(s.pred = mean(s.pred),
                        r.pred = mean(r.pred)),
            by=c("mult.comp.coexist","ECOSUBCD")) %>% 
  na.omit()

p93 <- ind.mort.dat %>%
  filter(SPCD==93) %>%
  mutate(m.pred = 1-m93.fit) %>%
  group_by(mult.comp.coexist,ECOSUBCD) %>%
  summarise(m.pred = mean(m.pred),
            mean.dia = mean(PREVDIA)) %>%
  left_join(seed.dat %>%
              filter(SPCD==93) %>%
              mutate(s.pred = s93.fit,
                     r.pred = r93.fit) %>%
              group_by(mult.comp.coexist,ECOSUBCD) %>%
              summarise(s.pred = mean(s.pred),
                        r.pred = mean(r.pred)),
            by=c("mult.comp.coexist","ECOSUBCD")) %>% 
  na.omit()

p.m <- p19 %>% 
  left_join(p93,
            by = c("ECOSUBCD","mult.comp.coexist"),
            suffix = c(".19",".93"))

# multinomial model ----

dat <- p.m %>% 
  ungroup() %>% 
  mutate(resilience = ifelse(mult.comp.coexist=="resilience",1,0),
         restructuring = ifelse(mult.comp.coexist=="restructuring",1,0),
         reassembly = ifelse(mult.comp.coexist=="reassembly",1,0),
         replacement = ifelse(mult.comp.coexist=="replacement",1,0))

dat$size <- with(dat, resilience+restructuring+reassembly+replacement)
dat$group <- with(dat, cbind(resilience,restructuring,reassembly,replacement))


try <- brm(bf(group | trials(size) ~ m.pred.19*s.pred.19*r.pred.19 +
                m.pred.93*s.pred.93*r.pred.93),
           data = dat,
           family = multinomial(),
           iter = 10000,
           chains = 4,
           cores = getOption("mc.cores",4))


try.fit <- brms::posterior_predict(try, ndraws=1000) %>% colMeans()

p.m <- p.m %>% 
  bind_cols(try.fit) %>% 
  rowwise() %>% 
  mutate(pred.traj = case_when(max(c(resilience,restructuring,reassembly,replacement))==resilience ~ "resilience",
                               max(c(resilience,restructuring,reassembly,replacement))==restructuring ~ "restructuring",
                               max(c(resilience,restructuring,reassembly,replacement))==reassembly ~ "reassembly",
                               max(c(resilience,restructuring,reassembly,replacement))==replacement ~ "replacement"),
         pred.bin = ifelse(pred.traj == "resilience","resilience","non-resilience"),
         obs.bin = ifelse(mult.comp.coexist == "resilience","resilience","non-resilience")) %>% 
  ungroup()

# adding in AREA estimates
p.m <- p.m %>% 
  left_join(dall.er.ablapien %>% 
              sf::st_drop_geometry() %>% 
              select(ECOSUBCD = MAP_UNIT_S,
                     AREA = AREA_TOTAL_ha_19))
