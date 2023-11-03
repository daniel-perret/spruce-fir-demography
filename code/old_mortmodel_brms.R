install.packages("lme4")
install.packages("brms")
install.packages("glmmTMB")
install.packages("performance")

library(lme4)

d <- abla.trees %>% 
  left_join(plot.summary %>% 
              select(PLT_CN, state_key, REMPER),
            by = "PLT_CN") %>%
  filter(PREV_STATUS_CD == 1,
         STATUSCD %in% 1:2,
         !is.na(REMPER)) %>% 
  mutate(DIAcm = ifelse(is.na(DIA),
                        PREVDIA*2.54,
                        DIA*2.54)) %>% 
  select(PLT_CN, state_key, PLOT, REMPER, STATUSCD, DIAcm) %>% 
  mutate(status = ifelse(STATUSCD == 1,
                         0,
                         1))

d.o <- d %>% 
  group_by(PLT_CN) %>% 
  summarise(obs = sum(status==1)/n(),
            REMPER = mean(REMPER)) %>% 
  mutate(obs.rate = 1-(1-obs)^(1/REMPER))

m <- glmer(data = d,
           formula = status ~ (1|PLT_CN),
           family = binomial(link="logit"))

d2 <- d %>% 
  mutate(pred = predict(m,type="response"))

d3 <- d2 %>% 
  group_by(PLT_CN) %>% 
  summarise(pred = mean(pred),
            obs = sum(status==1)/n(),
            REMPER = mean(REMPER)) %>% 
  mutate(pred.rate = 1-(1-pred)^(1/REMPER),
         obs.rate = 1-(1-obs)^(1/REMPER))

d3 %>% 
  ggplot(aes(x = obs,
             y = pred)) +
  geom_point(pch=19,
             size=2,
             alpha=0.6) +
  geom_abline(slope = 1, intercept = 0,
              col="red",
              lwd=1)

d3 %>% 
  ggplot(aes(x = obs.rate,
             y = pred.rate)) +
  geom_point(pch=19,
             cex=2,
             alpha=0.6) +
  geom_abline(slope = 1, intercept = 0,
              col="red",
              lwd=1)

d3 %>% 
  ggplot(.,
         aes()) +
  geom_histogram(aes(x = obs.rate)) +
  geom_histogram(aes(x = pred.rate), fill="red", alpha=0.4)



b <- brms::brm(data = d,
          formula = status ~ (1|PLT_CN),
          family = bernoulli(link="logit"))

# posterior predictive checks
yrep <- posterior_predict(b, ndraws=500)

ppc_dens_overlay(y = b$data$status,
                 yrep = yrep,
                 size=2,
                 alpha = 0.5)

ppc_violin_grouped(y = b$data$status,
                   yrep = yrep,
                   group = d$state_key)


d.p <- yrep %>%
  t(.) %>% 
  as.data.frame(.) %>%
  bind_cols(d) %>% 
  group_by(PLT_CN) %>% 
  summarise(n.trees = n(),
            across(contains("V"),
                   .fns = function(X){return(sum(X==1)/n())}),
            REMPER = mean(REMPER)) %>% 
  rowwise() %>% 
  mutate(mean.pp = mean(c_across(contains("V"))),
         upr95 = quantile(c_across(contains("V")),0.95),
         lwr95 = quantile(c_across(contains("V")),0.05),
         pp.cv = cv(c_across(contains("V")))) %>% 
  mutate(mean.pp.rate = 1-(1-mean.pp)^(1/REMPER),
         upr95.rate = 1-(1-upr95)^(1/REMPER),
         lwr95.rate = 1-(1-lwr95)^(1/REMPER))

d.o %>% 
  left_join(d.p %>% 
              select(PLT_CN,
                     contains(c("pp","95"))),
            by="PLT_CN") %>% 
  ggplot(.,
         aes(x = obs,
             y = mean.pp)) +
  geom_point(pch = 19,
             alpha = 0.3,
             cex = 3) +
  geom_segment(inherit.aes = F,
               aes(x = obs,
                   xend = obs,
                   y = upr95,
                   yend = lwr95,
                   group = PLT_CN),
               alpha=0.3)

d.p %>% 
  ggplot(.,
         aes(x = n.trees,
             y = upr95-lwr95)) +
  geom_point(pch=19,
             alpha=0.3,
             cex=3)

  geom_histogram(inherit.aes = F,
               aes(x = n.trees),
               stat="density")









