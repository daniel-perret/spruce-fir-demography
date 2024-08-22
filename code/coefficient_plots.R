library(broom.mixed)

m19.ests <- broom.mixed::tidy(m19)
s19.ests <- broom.mixed::tidy(s19)
r19.ests <- broom.mixed::tidy(r19)
m93.ests <- broom.mixed::tidy(m93)
s93.ests <- broom.mixed::tidy(s93)
r93.ests <- broom.mixed::tidy(r93)

## mortality -------------

m.ests <- m19.ests %>% 
  mutate(species=19) %>% 
  bind_rows(m93.ests %>% 
              mutate(species=93)) %>% 
  mutate(term = substr(term,5,nchar(term)))

m.ests$term <- factor(m.ests$term, 
                      levels = 
                        c("(Intercept)",
                          "phi_(Intercept)",
                          "scalePREVDIA",
                          "scalePREV_CR",
                          "prev.damage",
                          "scalePREV_BAH",
                          "scaleMAT_maxanom.z",
                          "scaleCMD_maxanom.z",
                          "scaleMAT_ref_mean",
                          "scaleCMD_ref_mean",
                          "area.fire.prop",
                          "area.id.prop",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z",
                          "scaleMAT_maxanom.z:scaleMAT_ref_mean",
                          "scaleCMD_maxanom.z:scaleMAT_ref_mean",
                          "scaleMAT_maxanom.z:scaleCMD_ref_mean",
                          "scaleCMD_maxanom.z:scaleCMD_ref_mean",
                          "scaleMAT_ref_mean:scaleCMD_ref_mean",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:scaleMAT_ref_mean",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:scaleCMD_ref_mean",
                          "scaleMAT_maxanom.z:scaleMAT_ref_mean:scaleCMD_ref_mean",
                          "scaleCMD_maxanom.z:scaleMAT_ref_mean:scaleCMD_ref_mean",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:scaleMAT_ref_mean:scaleCMD_ref_mean",
                          "area.fire.prop:area.id.prop",
                          "scaleMAT_maxanom.z:area.fire.prop",
                          "scaleCMD_maxanom.z:area.fire.prop",
                          "scaleMAT_maxanom.z:area.id.prop",
                          "scaleCMD_maxanom.z:area.id.prop",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:area.id.prop",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:area.fire.prop",
                          "scalePREVDIA:scaleMAT_maxanom.z",
                          "scalePREVDIA:scaleCMD_maxanom.z",
                          "scalePREVDIA:scaleMAT_maxanom.z:scaleCMD_maxanom.z",
                          "scalePREVDIA:area.id.prop",
                          "scalePREVDIA:area.fire.prop",
                          "scalePREVDIA:scaleMAT_maxanom.z:area.fire.prop",
                          "scalePREVDIA:scaleCMD_maxanom.z:area.fire.prop",
                          "scalePREVDIA:scaleMAT_maxanom.z:area.id.prop",
                          "scalePREVDIA:scaleCMD_maxanom.z:area.id.prop",
                          "scalePREVDIA:scaleMAT_maxanom.z:scaleCMD_maxanom.z:area.id.prop",
                          "scalePREVDIA:scaleMAT_maxanom.z:scaleCMD_maxanom.z:area.fire.prop"))

m.ests %>% 
  ggplot(.,
         aes(x = estimate,
             y = (term),
             group = species)) +
  geom_segment(aes(x = -Inf, xend = Inf,
                   y = term, yend = term,
                   group = term),
               size = 0.3,
               color = "gray85") +
  geom_vline(xintercept = 0,lty=2) +
  geom_segment(aes(x = conf.low, xend = conf.high,
                   y = term, yend = term,
                   group = species,
                   color = factor(species)),
               lwd = 1,
               alpha=0.7)+
  geom_point(aes(color=factor(species)),
             pch = 19,
             size = 3,
             alpha = 0.7) +
  theme(axis.text.y = element_text(size = 6)) +
  scale_y_discrete(limits=rev) +
  scale_color_manual(name = "Species",
                     aesthetics = "color",
                     values = c('19' = "dodgerblue2",
                                '93' = "goldenrod"))
## regeneration -------------

s.ests <- s19.ests %>% 
  mutate(species=19) %>% 
  bind_rows(s93.ests %>% 
              mutate(species=93))

s.ests$term <- factor(s.ests$term, 
                      levels = 
                        c("(Intercept)",
                          "sd__(Intercept)",
                          "scalePREV_BAH",
                          "scaleMAT_maxanom.z",
                          "scaleCMD_maxanom.z",
                          "scaleMAT_ref_mean",
                          "scaleCMD_ref_mean",            
                          "scalefire.sev",
                          
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z",
                          "scaleMAT_maxanom.z:scaleMAT_ref_mean",
                          "scaleCMD_maxanom.z:scaleMAT_ref_mean",
                          "scaleMAT_maxanom.z:scaleCMD_ref_mean",
                          "scaleCMD_maxanom.z:scaleCMD_ref_mean",
                          "scaleMAT_ref_mean:scaleCMD_ref_mean",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:scaleMAT_ref_mean",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:scaleCMD_ref_mean",
                          "scaleMAT_maxanom.z:scaleMAT_ref_mean:scaleCMD_ref_mean",
                          "scaleCMD_maxanom.z:scaleMAT_ref_mean:scaleCMD_ref_mean",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:scaleMAT_ref_mean:scaleCMD_ref_mean",

                          "scaleMAT_maxanom.z:scalefire.sev",
                          "scaleCMD_maxanom.z:scalefire.sev",
                          "scaleMAT_ref_mean:scalefire.sev",
                          "scaleCMD_ref_mean:scalefire.sev",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:scalefire.sev",
                          "scaleMAT_maxanom.z:scaleMAT_ref_mean:scalefire.sev",
                          "scaleCMD_maxanom.z:scaleMAT_ref_mean:scalefire.sev",
                          "scaleMAT_maxanom.z:scaleCMD_ref_mean:scalefire.sev",
                          "scaleCMD_maxanom.z:scaleCMD_ref_mean:scalefire.sev",
                          "scaleMAT_ref_mean:scaleCMD_ref_mean:scalefire.sev",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:scaleMAT_ref_mean:scalefire.sev",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:scaleCMD_ref_mean:scalefire.sev",
                          "scaleMAT_maxanom.z:scaleMAT_ref_mean:scaleCMD_ref_mean:scalefire.sev",
                          "scaleCMD_maxanom.z:scaleMAT_ref_mean:scaleCMD_ref_mean:scalefire.sev",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:scaleMAT_ref_mean:scaleCMD_ref_mean:scalefire.sev"))

s.ests %>% 
  ggplot(.,
         aes(x = estimate,
             y = (term),
             group = species)) +
  geom_segment(aes(x = -Inf, xend = Inf,
                   y = term, yend = term,
                   group = term),
               size = 0.3,
               color = "gray85") +
  geom_vline(xintercept = 0,lty=2) +
  geom_segment(aes(x = conf.low, xend = conf.high,
                   y = term, yend = term,
                   group = species,
                   color = factor(species)),
               lwd = 1,
               alpha=0.7)+
  geom_point(aes(color=factor(species)),
             pch = 19,
             size = 3,
             alpha = 0.7) +
  theme(axis.text.y = element_text(size = 6)) +
  scale_y_discrete(limits=rev) +
  scale_color_manual(name = "Species",
                     aesthetics = "color",
                     values = c('19' = "dodgerblue2",
                                '93' = "goldenrod"))
## recruitment -------------

r.ests <- r19.ests %>% 
  mutate(species=19) %>% 
  bind_rows(r93.ests %>% 
              mutate(species=93))

r.ests$term <- factor(r.ests$term, 
                      levels = 
                        c("(Intercept)",
                          "sd__(Intercept)",
                          "scalePREV_BAH",
                          "scaleMAT_maxanom.z",
                          "scaleCMD_maxanom.z",
                          "scaleMAT_ref_mean",
                          "scaleCMD_ref_mean",            
                          "scalefire.sev",
                          
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z",
                          "scaleMAT_maxanom.z:scaleMAT_ref_mean",
                          "scaleCMD_maxanom.z:scaleMAT_ref_mean",
                          "scaleMAT_maxanom.z:scaleCMD_ref_mean",
                          "scaleCMD_maxanom.z:scaleCMD_ref_mean",
                          "scaleMAT_ref_mean:scaleCMD_ref_mean",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:scaleMAT_ref_mean",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:scaleCMD_ref_mean",
                          "scaleMAT_maxanom.z:scaleMAT_ref_mean:scaleCMD_ref_mean",
                          "scaleCMD_maxanom.z:scaleMAT_ref_mean:scaleCMD_ref_mean",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:scaleMAT_ref_mean:scaleCMD_ref_mean",

                          "scaleMAT_maxanom.z:scalefire.sev",
                          "scaleCMD_maxanom.z:scalefire.sev",
                          "scaleMAT_ref_mean:scalefire.sev",
                          "scaleCMD_ref_mean:scalefire.sev",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:scalefire.sev",
                          "scaleMAT_maxanom.z:scaleMAT_ref_mean:scalefire.sev",
                          "scaleCMD_maxanom.z:scaleMAT_ref_mean:scalefire.sev",
                          "scaleMAT_maxanom.z:scaleCMD_ref_mean:scalefire.sev",
                          "scaleCMD_maxanom.z:scaleCMD_ref_mean:scalefire.sev",
                          "scaleMAT_ref_mean:scaleCMD_ref_mean:scalefire.sev",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:scaleMAT_ref_mean:scalefire.sev",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:scaleCMD_ref_mean:scalefire.sev",
                          "scaleMAT_maxanom.z:scaleMAT_ref_mean:scaleCMD_ref_mean:scalefire.sev",
                          "scaleCMD_maxanom.z:scaleMAT_ref_mean:scaleCMD_ref_mean:scalefire.sev",
                          "scaleMAT_maxanom.z:scaleCMD_maxanom.z:scaleMAT_ref_mean:scaleCMD_ref_mean:scalefire.sev"))

r.ests %>% 
  ggplot(.,
         aes(x = estimate,
             y = (term),
             group = species)) +
  geom_segment(aes(x = -Inf, xend = Inf,
                   y = term, yend = term,
                   group = term),
               size = 0.3,
               color = "gray85") +
  geom_vline(xintercept = 0,lty=2) +
  geom_segment(aes(x = conf.low, xend = conf.high,
                   y = term, yend = term,
                   group = species,
                   color = factor(species)),
               lwd = 1,
               alpha=0.7)+
  geom_point(aes(color=factor(species)),
             pch = 19,
             size = 3,
             alpha = 0.7) +
  theme(axis.text.y = element_text(size = 6)) +
  scale_y_discrete(limits=rev) +
  scale_color_manual(name = "Species",
                     aesthetics = "color",
                     values = c('19' = "dodgerblue2",
                                '93' = "goldenrod"))


## multinomial classification ----

## making a coefficient figure for the multinomial classification model

class.ests <- broom.mixed::tidy(try) %>% 
  mutate(term = substr(term,3,nchar(term)))

class.ests$term <- factor(class.ests$term, 
                          levels = 
                            c("restructuring_(Intercept)",
                              "restructuring_m.pred.19",
                              "restructuring_s.pred.19",
                              "restructuring_r.pred.19",
                              "restructuring_m.pred.19:s.pred.19",
                              "restructuring_m.pred.19:r.pred.19",
                              "restructuring_s.pred.19:r.pred.19",
                              "restructuring_m.pred.19:s.pred.19:r.pred.19",
                              "restructuring_m.pred.93",
                              "restructuring_s.pred.93",
                              "restructuring_r.pred.93",
                              "restructuring_m.pred.93:s.pred.93",
                              "restructuring_m.pred.93:r.pred.93",
                              "restructuring_s.pred.93:r.pred.93",
                              "restructuring_m.pred.93:s.pred.93:r.pred.93",
                              
                              "reassembly_(Intercept)",
                              "reassembly_m.pred.19",
                              "reassembly_s.pred.19",
                              "reassembly_r.pred.19",
                              "reassembly_m.pred.19:s.pred.19",
                              "reassembly_m.pred.19:r.pred.19",
                              "reassembly_s.pred.19:r.pred.19",
                              "reassembly_m.pred.19:s.pred.19:r.pred.19",
                              "reassembly_m.pred.93",
                              "reassembly_s.pred.93",
                              "reassembly_r.pred.93",
                              "reassembly_m.pred.93:s.pred.93",
                              "reassembly_m.pred.93:r.pred.93",
                              "reassembly_s.pred.93:r.pred.93",
                              "reassembly_m.pred.93:s.pred.93:r.pred.93",
                              
                              "replacement_(Intercept)",
                              "replacement_m.pred.19",
                              "replacement_s.pred.19",
                              "replacement_r.pred.19",
                              "replacement_m.pred.19:s.pred.19",
                              "replacement_m.pred.19:r.pred.19",
                              "replacement_s.pred.19:r.pred.19",
                              "replacement_m.pred.19:s.pred.19:r.pred.19",
                              "replacement_m.pred.93",
                              "replacement_s.pred.93",
                              "replacement_r.pred.93",
                              "replacement_m.pred.93:s.pred.93",
                              "replacement_m.pred.93:r.pred.93",
                              "replacement_s.pred.93:r.pred.93",
                              "replacement_m.pred.93:s.pred.93:r.pred.93"))

class.ests <- class.ests %>% 
  mutate(species = case_when(grepl("19",term) ~ "19",
                             grepl("93",term) ~ "93",
                             TRUE ~ "intercept"),
         rate = case_when(grepl("m.pred.*s.pred.*r.pred",term) ~ "m:s:r",
                          grepl("m.pred.*s.pred",term) ~ "m:s",
                          grepl("m.pred.*r.pred",term) ~ "m:r",
                          grepl("s.pred.*r.pred",term) ~ "s:r",
                          grepl("m.pred",term) ~ "m",
                          grepl("s.pred",term) ~ "s",
                          grepl("r.pred",term) ~ "r",
                          TRUE ~ "Intercept"),
         traj = case_when(grepl("restructuring",term) ~ "restructuring",
                          grepl("reassembly", term) ~ "reassembly",
                          grepl("replacement", term) ~ "replacement"),
         traj_rate = paste(traj,rate,sep="_"),
         traj_rate = factor(traj_rate,
                            levels = c("restructuring_Intercept",
                                       "restructuring_m",
                                       "restructuring_s",
                                       "restructuring_r",
                                       "restructuring_m:s",
                                       "restructuring_m:r",
                                       "restructuring_s:r",
                                       "restructuring_m:s:r",
                                       "reassembly_Intercept",
                                       "reassembly_m",
                                       "reassembly_s",
                                       "reassembly_r",
                                       "reassembly_m:s",
                                       "reassembly_m:r",
                                       "reassembly_s:r",
                                       "reassembly_m:s:r",
                                       "replacement_Intercept",
                                       "replacement_m",
                                       "replacement_s",
                                       "replacement_r",
                                       "replacement_m:s",
                                       "replacement_m:r",
                                       "replacement_s:r",
                                       "replacement_m:s:r"))) 


class.ests %>% 
  ggplot(.,
         aes(x = estimate,
             y = traj_rate,
             group = species)) +
  geom_segment(aes(x = -Inf, xend = Inf,
                   y = traj_rate, yend = traj_rate,
                   group = traj_rate),
               size = 0.3,
               color = "gray85") +
  geom_vline(xintercept = 0,lty=2) +
  geom_segment(aes(x = conf.low, xend = conf.high,
                   y = traj_rate, yend = traj_rate,
                   group = species,
                   color = factor(species)),
               lwd = 1,
               alpha=0.7)+
  geom_point(aes(color=factor(species)),
             pch = 19,
             size = 3,
             alpha = 0.7) +
  theme(axis.text.y = element_text(size = 6)) +
  scale_y_discrete(limits=rev) +
  scale_color_manual(name = "Species",
                     aesthetics = "color",
                     values = c('19' = "dodgerblue2",
                                '93' = "goldenrod",
                                "intercept" = "black"))






