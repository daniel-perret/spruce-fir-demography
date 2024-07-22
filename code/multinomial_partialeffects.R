
# MORTALITY PARTIALS
try.dat <- new_data(try, terms=c("m.pred.19 [0:1,by=0.01]",
                               "m.pred.93 [0:1,by=0.01]")) %>% 
  brms::posterior_predict(try, ndraws=1000, newdata=.) %>% 
  colMeans()

try.dat <- try.dat %>% bind_cols(new_data(try, terms=c("m.pred.19 [0:1,by=0.01]",
                                            "m.pred.93 [0:1,by=0.01]")))


try.dat %>% 
  rowwise() %>% 
  mutate(resilience = ifelse(resilience == max(c(resilience, 
                                                replacement, 
                                                restructuring, 
                                                reassembly)), 
                             round(resilience,2), 0),
         restructuring = ifelse(restructuring == max(c(resilience, 
                                                replacement, 
                                                restructuring, 
                                                reassembly)), 
                             round(restructuring,2), 0),
         reassembly = ifelse(reassembly == max(c(resilience, 
                                                replacement, 
                                                restructuring, 
                                                reassembly)), 
                             round(reassembly,2), 0),
         replacement = ifelse(replacement == max(c(resilience, 
                                                replacement, 
                                                restructuring, 
                                                reassembly)), 
                             round(replacement,2), 0)) %>%
    ungroup() %>% 
  ggplot(.,
         aes(x = m.pred.19,
             y = m.pred.93)) +
  geom_tile(aes(alpha = replacement,
                fill = "replacement")) +
  geom_tile(aes(alpha = resilience,
                fill = "resilience")) +
  geom_tile(aes(alpha = restructuring,
                fill = "restructuring")) +
  geom_tile(aes(alpha = reassembly,
                fill = "reassembly")) +
  scale_fill_manual(name = "joint trajectory",
                    values = c("resilience" = "dodgerblue2",
                               "restructuring" = "gold2",
                               "reassembly" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("col","bg","fill")) +
  scale_alpha_binned(limits = c(0,1), range=c(-0.25,1),
                     breaks=c(0,0.25,0.5,0.75,1)) +
  lims(x=c(0,1),y=c(0,1)) +
  theme(legend.position = "none")

# SEEDLING PARTIALS
try.dat <- new_data(try, terms=c("s.pred.19 [0:1,by=0.01]",
                               "s.pred.93 [0:1,by=0.01]")) %>% 
  brms::posterior_predict(try, ndraws=1000, newdata=.) %>% 
  colMeans()

try.dat <- try.dat %>% bind_cols(new_data(try, terms=c("s.pred.19 [0:1,by=0.01]",
                                            "s.pred.93 [0:1,by=0.01]")))


try.dat %>% 
  rowwise() %>% 
  mutate(resilience = ifelse(resilience == max(c(resilience, 
                                                replacement, 
                                                restructuring, 
                                                reassembly)), 
                             round(resilience,2), 0),
         restructuring = ifelse(restructuring == max(c(resilience, 
                                                replacement, 
                                                restructuring, 
                                                reassembly)), 
                             round(restructuring,2), 0),
         reassembly = ifelse(reassembly == max(c(resilience, 
                                                replacement, 
                                                restructuring, 
                                                reassembly)), 
                             round(reassembly,2), 0),
         replacement = ifelse(replacement == max(c(resilience, 
                                                replacement, 
                                                restructuring, 
                                                reassembly)), 
                             round(replacement,2), 0)) %>%
    ungroup() %>%
  ggplot(.,
         aes(x = s.pred.19,
             y = s.pred.93)) +
  geom_tile(aes(alpha = replacement,
                fill = "replacement")) +
  geom_tile(aes(alpha = resilience,
                fill = "resilience")) +
  geom_tile(aes(alpha = restructuring,
                fill = "restructuring")) +
  geom_tile(aes(alpha = reassembly,
                fill = "reassembly")) +
  scale_fill_manual(name = "joint trajectory",
                    values = c("resilience" = "dodgerblue2",
                               "restructuring" = "gold2",
                               "reassembly" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("col","bg","fill")) +
  scale_alpha_binned(limits = c(0,1), range=c(-0.25,1),
                     breaks=c(0,0.25,0.5,0.75,1)) +
  lims(x=c(0,1),y=c(0,1)) +
  theme(legend.position = "none")

# RECRUITMENT PARTIALS
try.dat <- new_data(try, terms=c("r.pred.19 [0:1,by=0.01]",
                               "r.pred.93 [0:1,by=0.01]")) %>% 
  brms::posterior_predict(try, ndraws=1000, newdata=.) %>% 
  colMeans()

try.dat <- try.dat %>% bind_cols(new_data(try, terms=c("r.pred.19 [0:1,by=0.01]",
                                            "r.pred.93 [0:1,by=0.01]")))


try.dat %>% 
  rowwise() %>% 
  mutate(resilience = ifelse(resilience == max(c(resilience, 
                                                replacement, 
                                                restructuring, 
                                                reassembly)), 
                             round(resilience,2), 0),
         restructuring = ifelse(restructuring == max(c(resilience, 
                                                replacement, 
                                                restructuring, 
                                                reassembly)), 
                             round(restructuring,2), 0),
         reassembly = ifelse(reassembly == max(c(resilience, 
                                                replacement, 
                                                restructuring, 
                                                reassembly)), 
                             round(reassembly,2), 0),
         replacement = ifelse(replacement == max(c(resilience, 
                                                replacement, 
                                                restructuring, 
                                                reassembly)), 
                             round(replacement,2), 0)) %>%
    ungroup() %>% 
  ggplot(.,
         aes(x = r.pred.19,
             y = r.pred.93)) +
  geom_tile(aes(alpha = replacement,
                fill = "replacement")) +
  geom_tile(aes(alpha = resilience,
                fill = "resilience")) +
  geom_tile(aes(alpha = restructuring,
                fill = "restructuring")) +
  geom_tile(aes(alpha = reassembly,
                fill = "reassembly")) +
  scale_fill_manual(name = "joint trajectory",
                    values = c("resilience" = "dodgerblue2",
                               "restructuring" = "gold2",
                               "reassembly" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("col","bg","fill")) +
  scale_alpha_binned(limits = c(0,1), range=c(-0.25,1),
                     breaks=c(0,0.25,0.5,0.75,1)) +
  lims(x=c(0,1),y=c(0,1)) +
  theme(legend.position = "none")