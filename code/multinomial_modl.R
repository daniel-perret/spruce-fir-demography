install.packages("nnet")
library(nnet)

p.m$mult.comp.coexist <- factor(p.m$mult.comp.coexist)
p.m$mult.comp.coexist <- relevel(p.m$mult.comp.coexist,ref = "resilience")


m.m <- nnet::multinom(formula = 
                        mult.comp.coexist ~ m.pred.19*m.pred.93 +
                        s.pred.19*s.pred.93 +
                        r.pred.19*r.pred.93,
                      # mult.comp.coexist ~ m.pred.19*s.pred.19*r.pred.19*
                      # m.pred.93*s.pred.93*r.pred.93,
                      data = p.m, maxit=1000)

summary(m.m)

p.m$pred <- predict(m.m,type="probs")

tab <- table(p.m$mult.comp.coexist, p.m$pred)

round((sum(diag(tab))/sum(tab))*100,2)


### VISUALIZATION

m.dat <- new_data(m.m, terms=c("m.pred.19 [0:1,by=0.01]",
                               "m.pred.93 [0:1,by=0.01]")) %>% 
  mutate(pred.cat = predict(m.m,.,type="class")) %>% 
  bind_cols(round(predict(m.m,.,type="probs"),2))

m.dat %>% 
  ggplot(.,
         aes(x = m.pred.19,
             y = m.pred.93,
             fill = pred.cat)) +
  geom_tile(aes(alpha = replacement),
            data = m.dat %>%
              filter(pred.cat=="replacement")) +
  geom_tile(aes(alpha = resilience),
            data = m.dat %>% 
              filter(pred.cat=="resilience")) +
  geom_tile(aes(alpha = `structural change`),
            data = m.dat %>% 
              filter(pred.cat=="structural change")) +
  geom_tile(aes(alpha = `compositional change`),
            data = m.dat %>%
              filter(pred.cat=="compositional change")) +
  scale_fill_manual(name = "multispecies trajectory",
                    values = c("resilience" = "dodgerblue2",
                               "structural change" = "gold2",
                               "compositional change" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("col","bg","fill")) +
  scale_alpha_binned(limits = c(0,1), range=c(-0.25,1),
                     breaks=c(0,0.25,0.5,0.75,1)) +
  lims(x=c(0,1),y=c(0,1)) +
  theme(legend.position = "none")
# 
# m.dat %>% 
#   ggplot(.,
#          aes(x = m.pred.19,
#              y = m.pred.93,
#              fill = pred.cat)) +
#   geom_tile(aes(alpha = `compositional change`,
#                 fill = "compositional change")) +
#   geom_tile(aes(alpha = replacement,
#                 fill = "replacement")) +
#   geom_tile(aes(alpha = resilience,
#                 fill = "resilience")) +
#   geom_tile(aes(alpha = `structural change`,
#                 fill = "structural change")) +
#   scale_fill_manual(name = "multispecies trajectory",
#                     values = c("resilience" = "dodgerblue2",
#                                "structural change" = "gold2",
#                                "compositional change" = "firebrick2",
#                                "replacement" = "firebrick4"),
#                     aesthetics = c("col","bg","fill")) +
#   scale_alpha_binned(limits = c(0,1), range=c(-0.25,1),
#                      breaks=c(0.25,0.5,0.75)) +
#   lims(x=c(0,1),y=c(0,1)) +
#   theme(legend.position = "none")

s.dat <- new_data(m.m, terms=c("s.pred.19 [0:1,by=0.01]",
                               "s.pred.93 [0:1,by=0.01]")) %>% 
  mutate(pred.cat = predict(m.m,.,type="class")) %>% 
  bind_cols(round(predict(m.m,.,type="probs"),2))

s.dat %>% 
  ggplot(.,
         aes(x = s.pred.19,
             y = s.pred.93,
             fill = pred.cat)) +
  geom_tile(aes(alpha = `compositional change`),
            data = s.dat %>%
              filter(pred.cat=="compositional change")) +
  geom_tile(aes(alpha = replacement),
            data = s.dat %>%
              filter(pred.cat=="replacement")) +
  geom_tile(aes(alpha = resilience),
            data = s.dat %>% 
              filter(pred.cat=="resilience")) +
  geom_tile(aes(alpha = `structural change`),
            data = s.dat %>%
              filter(pred.cat=="structural change")) +
  scale_fill_manual(name = "multispecies trajectory",
                    values = c("resilience" = "dodgerblue2",
                               "structural change" = "gold2",
                               "compositional change" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("fill")) +
  scale_alpha_binned(limits = c(0,1), range=c(-0.25,1),
                     breaks=c(0,0.25,0.5,0.75,1)) +  lims(x=c(0,1),y=c(0,1)) +
  theme(legend.position = "none")


r.dat <- new_data(m.m, terms=c("r.pred.19 [0:1,by=0.01]",
                               "r.pred.93 [0:1,by=0.01]")) %>% 
  mutate(pred.cat = predict(m.m,.,type="class")) %>% 
  bind_cols(round(predict(m.m,.,type="probs"),2))

r.dat %>% 
  ggplot(.,
         aes(x = r.pred.19,
             y = r.pred.93,
             fill = pred.cat)) +
  geom_tile(aes(alpha = `compositional change`),
            data = r.dat %>%
              filter(pred.cat=="compositional change")) +
  geom_tile(aes(alpha = replacement),
            data = r.dat %>%
              filter(pred.cat=="replacement")) +
  geom_tile(aes(alpha = resilience),
            data = r.dat %>% 
              filter(pred.cat=="resilience")) +
  geom_tile(aes(alpha = `structural change`),
            data = r.dat %>% 
              filter(pred.cat=="structural change")) +
  scale_fill_manual(name = "multispecies trajectory",
                    values = c("resilience" = "dodgerblue2",
                               "structural change" = "gold2",
                               "compositional change" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("col","bg","fill")) +
  scale_alpha_binned(limits = c(0,1), range=c(-0.25,1),
                     breaks=c(0,0.25,0.5,0.75,1)) +  lims(x=c(0,1),y=c(0,1)) +
  theme(legend.position = "none")



