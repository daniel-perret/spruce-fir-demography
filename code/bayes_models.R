library(brms)
library(tidyverse)
library(rFIA)
library(lme4)
library(performance)
select <- dplyr::select

m19 <- brm(data = ind.mort.dat %>% 
             filter(SPCD==19),
           formula = SURV ~ 
             #tree level predictors
             scale(PREV_CR) + scale(PREVDIA) + 
             prev.damage +
             #plot level predictors
             scale(MAT_anom)*scale(MAP_relanom)*scale(CMD_relanom)*
             scale(MAT_19802010)*scale(MAP_19802010) +
             #landscape level predictors
             area.fire.prop*area.id.prop +
             #cross-scale interactions
             scale(PREVDIA)*scale(MAT_anom)*scale(MAP_relanom)*area.id.prop*scale(CMD_relanom) +
             scale(PREVDIA)*scale(MAT_anom)*scale(MAP_relanom)*area.fire.prop*scale(CMD_relanom) +
             (1|ECOSUBCD),
           family = binomial(link="logit"),
           iter = 2000,
           chains = 3,
           cores = getOption("mc.cores",3))
# summary(m19)
# performance(m19)
# plot(m19)
# yrep <- posterior_predict(m19, ndraws=25)
# ppc_dens_overlay(y = m19$data$SURV,
#                  yrep = yrep,
#                  size=2,
#                  alpha = 0.5) +
#   xlim(0,1)



m93 <- brm(data = ind.mort.dat %>% 
             filter(SPCD==93),
           formula = SURV ~ 
             #tree level predictors
             scale(PREV_CR) + scale(PREVDIA) + 
             prev.damage +
             #plot level predictors
             scale(MAT_anom)*scale(MAP_relanom)*scale(CMD_relanom)*
             scale(MAT_19802010)*scale(MAP_19802010) +
             #landscape level predictors
             area.fire.prop*area.id.prop +
             #cross-scale interactions
             scale(PREVDIA)*scale(MAT_anom)*scale(MAP_relanom)*area.id.prop*scale(CMD_relanom) +
             scale(PREVDIA)*scale(MAT_anom)*scale(MAP_relanom)*area.fire.prop*scale(CMD_relanom) +
             (1|ECOSUBCD),
           family = binomial(link="logit"),
           iter = 2000,
           chains = 3,
           cores = getOption("mc.cores",3))
# summary(m93)
# performance(m93)

s19 <- brm(data = seed.dat %>% 
               filter(SPCD==19),
             formula = SEED.PRES ~
               scale(MAT_anom)*scale(MAP_relanom)*scale(MAT_19802010)*scale(MAP_19802010) +
               scale(MAT_anom)*fire.sev*bda.sev +
               scale(MAT_anom)*area.id.prop*area.fire.prop+
               scale(SLOPE)*scale(ASPECT)*scale(ELEV) +
               
               #disturbance
               (1|ECOSUBCD),
             family = binomial(link="logit"),
             iter = 2000,
             chains = 3,
             cores = getOption("mc.cores",3))
# summary(s19)
# performance(s19)

s93 <- brm(data = seed.dat %>% 
               filter(SPCD==93),
             formula = SEED.PRES ~
               scale(MAT_anom)*scale(MAP_relanom)*scale(MAT_19802010)*scale(MAP_19802010) +
               scale(MAT_anom)*fire.sev*bda.sev +
               scale(MAT_anom)*area.id.prop*area.fire.prop+
               scale(SLOPE)*scale(ASPECT)*scale(ELEV) +
               
               #disturbance
               (1|ECOSUBCD),
             family = binomial(link="logit"),
             iter = 2000,
             chains = 3,
             cores = getOption("mc.cores",3))
# summary(s93)
# performance(s93)


# ## fire
# m.pred <- ggpredict(m19,
#                     terms = c("MAT_anom[0:3, by=0.1]",
#                               "area.fire.prop [0, 0.1, 0.25]"
#                     )) %>% 
#   as.data.frame() %>% 
#   rename(MAT_anom = x,
#          area.fire = group
#   ) %>%
#   mutate(SPCD="19") %>% 
#   bind_rows(ggpredict(m93,
#                       terms = c("MAT_anom[0:3, by=0.1]",
#                                 "area.fire.prop [0, 0.1, 0.25]"
#                       )) %>% 
#               as.data.frame() %>% 
#               rename(MAT_anom = x,
#                      area.fire = group
#               ) %>% 
#               mutate(SPCD="93"))
# 
# 
# s.pred <- ggpredict(s19,
#                     terms = c("MAT_anom[0:3, by=0.1]",
#                               "area.fire.prop [0, 0.1, 0.25]"
#                     )) %>% 
#   as.data.frame() %>% 
#   rename(MAT_anom = x,
#          area.fire = group
#   ) %>%
#   mutate(SPCD="19") %>% 
#   bind_rows(ggpredict(s93,
#                       terms = c("MAT_anom[0:3, by=0.1]",
#                                 "area.fire.prop [0, 0.1, 0.25]"
#                       )) %>% 
#               as.data.frame() %>% 
#               rename(MAT_anom = x,
#                      area.fire = group
#               ) %>% 
#               mutate(SPCD="93"))
# 
# 
# m.pred %>% 
#   mutate(rate = "surv") %>% 
#   bind_rows(s.pred %>% 
#               mutate(rate="seed")) %>%
#   ggplot(aes(x = MAT_anom,
#              y = predicted,
#              ymin = conf.low,
#              ymax = conf.high,
#              fill = rate,
#              col = rate)) +
#   geom_ribbon(alpha = 0.1,
#               col = NA) +
#   geom_line(lwd=1.7) +
#   facet_grid(facets = area.fire~SPCD)
# 
# ## BDA
# m.pred <- ggpredict(m19,
#                     terms = c("MAT_anom[0:3, by=0.1]",
#                               "area.id.prop[0,0.4,0.8]"
#                     )) %>% 
#   as.data.frame() %>% 
#   rename(MAT_anom = x,
#          area.bda = group
#   ) %>%
#   mutate(SPCD="19") %>% 
#   bind_rows(ggpredict(m93,
#                       terms = c("MAT_anom[0:3, by=0.1]",
#                                 "area.id.prop[0,0.4,0.8]"
#                       )) %>% 
#               as.data.frame() %>% 
#               rename(MAT_anom = x,
#                      area.bda = group
#               ) %>% 
#               mutate(SPCD="93"))
# 
# 
# s.pred <- ggpredict(s19,
#                     terms = c("MAT_anom[0:3, by=0.1]",
#                               "area.id.prop[0,0.4,0.8]"
#                     )) %>% 
#   as.data.frame() %>% 
#   rename(MAT_anom = x,
#          area.bda = group
#   ) %>%
#   mutate(SPCD="19") %>% 
#   bind_rows(ggpredict(s93,
#                       terms = c("MAT_anom[0:3, by=0.1]",
#                                 "area.id.prop[0,0.4,0.8]"
#                       )) %>% 
#               as.data.frame() %>% 
#               rename(MAT_anom = x,
#                      area.bda = group
#               ) %>% 
#               mutate(SPCD="93"))
# 
# 
# m.pred %>% 
#   mutate(rate = "surv") %>% 
#   bind_rows(s.pred %>% 
#               mutate(rate="seed")) %>%
#   ggplot(aes(x = MAT_anom,
#              y = predicted,
#              ymin = conf.low,
#              ymax = conf.high,
#              fill = rate,
#              col = rate)) +
#   geom_ribbon(alpha = 0.1,
#               col = NA) +
#   geom_line(lwd=1.7) +
#   facet_grid(facets = area.bda~SPCD)
# 
# 
# 
# 
# 
# ggpredict(m19,
#           terms = c("MAT_anom[0:3, by=0.1]",
#                     "PREVDIA [2.54, 20, 50]",
#                     "area.id.prop [0, 0.4, 0.8]"
#                     #"area.fire.prop [0, 0.1, 0.25]"
#           )) %>% 
#   as.data.frame() %>% 
#   rename(MAT_anom = x,
#          PREVDIA = group,
#          area.id = facet,
#          #area.fire = facet
#   ) %>%
#   mutate(SPCD="19") %>% 
#   bind_rows(ggpredict(m93,
#                       terms = c("MAT_anom[0:3, by=0.1]",
#                                 "PREVDIA [2.54, 20, 50]",
#                                 "area.id.prop [0, 0.4, 0.8]"
#                                 #"area.fire.prop [0, 0.1, 0.25]"
#                       )) %>% 
#               as.data.frame() %>% 
#               rename(MAT_anom = x,
#                      PREVDIA = group,
#                      area.id = facet,
#                      #area.fire = facet
#               ) %>% 
#               mutate(SPCD="93")) %>% 
#   ggplot(.,
#          aes(x = MAT_anom,
#              y = predicted,
#              ymin = conf.low,
#              ymax = conf.high,
#              col = SPCD,
#              fill = SPCD)) +
#   geom_ribbon(alpha = 0.1, col=NA) +
#   geom_line(lwd=1.7) +
#   facet_grid(facets=area.id~PREVDIA)
# 
