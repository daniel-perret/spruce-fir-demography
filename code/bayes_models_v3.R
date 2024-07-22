library(brms)
library(tidyverse)
library(rFIA)
library(lme4)
library(performance)
select <- dplyr::select

### MORTALITY MODELS
m19 <- brm(data = ind.mort.dat %>%
             filter(SPCD==19),
           formula = bf(SURV ~ (1+exp(-phi))^(-REMPER),
                        phi ~
                          #tree level predictors
                          scale(PREV_CR) + scale(PREVDIA) +
                          prev.damage +
                          #plot level predictors
                          scale(MAT_maxanom.z)*scale(CMD_maxanom.z)*
                          scale(MAT_ref_mean)*scale(CMD_ref_mean) +
                          scale(PREV_BAH) +
                          #landscape level predictors
                          area.fire.prop*area.id.prop +
                          #cross-scale interactions
                          scale(PREVDIA)*scale(MAT_maxanom.z)*scale(CMD_maxanom.z)*area.id.prop +
                          scale(PREVDIA)*scale(MAT_maxanom.z)*scale(CMD_maxanom.z)*area.fire.prop +
                          (1|ECOSUBCD),
                        nl=T),
           family = bernoulli(link="identity"),
           iter = 10000,
           chains = 4,
           cores = getOption("mc.cores",4))

m93 <- brm(data = ind.mort.dat %>%
             filter(SPCD==93),
           formula = bf(SURV ~ (1+exp(-phi))^(-REMPER),
                        phi ~
                          #tree level predictors
                          scale(PREV_CR) + scale(PREVDIA) +
                          prev.damage +
                          #plot level predictors
                          scale(MAT_maxanom.z)*scale(CMD_maxanom.z)*
                          scale(MAT_ref_mean)*scale(CMD_ref_mean) +
                          scale(PREV_BAH) +
                          #landscape level predictors
                          area.fire.prop*area.id.prop +
                          #cross-scale interactions
                          scale(PREVDIA)*scale(MAT_maxanom.z)*scale(CMD_maxanom.z)*area.id.prop +
                          scale(PREVDIA)*scale(MAT_maxanom.z)*scale(CMD_maxanom.z)*area.fire.prop +
                          (1|ECOSUBCD),
                        nl=T),
           family = bernoulli(link="identity"),
           iter = 10000,
           chains = 4,
           cores = getOption("mc.cores",4))
# 
# 
# # SEEDLING PRESENCE MODELS
# s19 <- brm(data = seed.dat %>%
#              filter(SPCD==19),
#            formula = SEED.PRES ~
#              scale(MAT_maxanom.z)*scale(CMD_maxanom.z)*
#              scale(MAT_ref_mean)*scale(CMD_ref_mean)*
#              scale(fire.sev) +
#              scale(PREV_BAH) +
#              (1|ECOSUBCD),
#            family = bernoulli(link="logit"),
#            iter = 20000,
#            chains = 4,
#            cores = getOption("mc.cores",4),
#            control = list(adapt_delta = 0.95))
# 
# s93 <- brm(data = seed.dat %>%
#              filter(SPCD==93),
#            formula = SEED.PRES ~
#              scale(MAT_maxanom.z)*scale(CMD_maxanom.z)*
#              scale(MAT_ref_mean)*scale(CMD_ref_mean)*
#              scale(fire.sev) +
#              scale(PREV_BAH) +
#              (1|ECOSUBCD),
#            family = bernoulli(link="logit"),
#            iter = 20000,
#            chains = 4,
#            cores = getOption("mc.cores",4),
#            control = list(adapt_delta = 0.95))
# 
# 
# # RECRUITMENT PRESENCE MODELS
# r19 <- brm(data = sap.dat %>%
#              filter(SPCD==19),
#            formula = SAP.PRES ~
#              scale(MAT_maxanom.z)*scale(CMD_maxanom.z)*
#              scale(MAT_ref_mean)*scale(CMD_ref_mean)*
#              scale(fire.sev) +
#              scale(PREV_BAH) +
#              (1|ECOSUBCD),
#            family = bernoulli(link="logit"),
#            iter = 20000,
#            chains = 4,
#            cores = getOption("mc.cores",4),
#            control = list(adapt_delta = 0.95))
# 
# r93 <- brm(data = sap.dat %>%
#              filter(SPCD==93),
#            formula = SAP.PRES ~
#              scale(MAT_maxanom.z)*scale(CMD_maxanom.z)*
#              scale(MAT_ref_mean)*scale(CMD_ref_mean)*
#              scale(fire.sev) +
#              scale(PREV_BAH) +
#              (1|ECOSUBCD),
#            family = bernoulli(link="logit"),
#            iter = 20000,
#            chains = 4,
#            cores = getOption("mc.cores",4),
#            control = list(adapt_delta = 0.95))
