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









