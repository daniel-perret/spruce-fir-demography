# Currlo
p.m.preds %>% 
  ggplot() +
  geom_density(aes(x=m19.currlo, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s19.currlo, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r19.currlo, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2")) -> p1
p.m.preds %>% 
  ggplot() +
  geom_density(aes(x=m93.currlo, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s93.currlo, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r93.currlo, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2")) -> p2

grid.arrange(p1,p2, nrow=2)  

# Currhi
p.m.preds %>% 
  ggplot() +
  geom_density(aes(x=m19.currhi, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s19.currhi, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r19.currhi, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2")) -> p1
p.m.preds %>% 
  ggplot() +
  geom_density(aes(x=m93.currhi, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s93.currhi, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r93.currhi, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2")) -> p2

grid.arrange(p1,p2, nrow=2)  


#2080lo
p.m.preds %>% 
  ggplot() +
  geom_density(aes(x=m19.2080lo, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s19.2080lo, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r19.2080lo, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2")) -> p1
p.m.preds %>% 
  ggplot() +
  geom_density(aes(x=m93.2080lo, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s93.2080lo, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r93.2080lo, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2")) -> p2

grid.arrange(p1,p2, nrow=2)  

# 2080hi
p.m.preds %>% 
  ggplot() +
  geom_density(aes(x=m19.2080hi, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s19.2080hi, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r19.2080hi, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2")) -> p1
p.m.preds %>% 
  ggplot() +
  geom_density(aes(x=m93.2080hi, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s93.2080hi, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r93.2080hi, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2")) -> p2

grid.arrange(p1,p2, nrow=2)  


## FOR N ROCKIES-----

# Currlo
p.m.preds %>% 
  filter(grepl("M332|M333", ECOSUBCD)) %>%
  ggplot() +
  geom_density(aes(x=m19.currlo, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s19.currlo, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r19.currlo, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2"))+
  lims(x=c(0,1))-> p1
p.m.preds %>% 
  filter(grepl("M332|M333", ECOSUBCD)) %>%
  ggplot() +
  geom_density(aes(x=m93.currlo, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s93.currlo, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r93.currlo, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2"))+
  lims(x=c(0,1))-> p2

grid.arrange(p1,p2, nrow=2)  

# Currhi
p.m.preds %>% 
  filter(grepl("M332|M333", ECOSUBCD)) %>%
  ggplot() +
  geom_density(aes(x=m19.currhi, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s19.currhi, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r19.currhi, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2")) -> p1
p.m.preds %>% 
  filter(grepl("M332|M333", ECOSUBCD)) %>%
  ggplot() +
  geom_density(aes(x=m93.currhi, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s93.currhi, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r93.currhi, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2")) -> p2

grid.arrange(p1,p2, nrow=2)  


#2050lo
p.m.preds %>% 
  filter(grepl("M332|M333", ECOSUBCD)) %>%
  ggplot() +
  geom_density(aes(x=m19.2050lo, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s19.2050lo, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r19.2050lo, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2")) -> p1
p.m.preds %>% 
  filter(grepl("M332|M333", ECOSUBCD)) %>%
  ggplot() +
  geom_density(aes(x=m93.2050lo, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s93.2050lo, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r93.2050lo, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2")) -> p2

grid.arrange(p1,p2, nrow=2)  

# 2050hi
p.m.preds %>% 
  filter(grepl("M332|M333", ECOSUBCD)) %>%
  ggplot() +
  geom_density(aes(x=m19.2050hi, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s19.2050hi, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r19.2050hi, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2")) -> p1
p.m.preds %>% 
  filter(grepl("M332|M333", ECOSUBCD)) %>%
  ggplot() +
  geom_density(aes(x=m93.2050hi, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s93.2050hi, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r93.2050hi, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2")) -> p2

grid.arrange(p1,p2, nrow=2)  

#2080lo
p.m.preds %>% 
  filter(grepl("M332|M333", ECOSUBCD)) %>%
  ggplot() +
  geom_density(aes(x=m19.2080lo, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s19.2080lo, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r19.2080lo, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2"))+
  lims(x=c(0,1))-> p1
p.m.preds %>% 
  filter(grepl("M332|M333", ECOSUBCD)) %>%
  ggplot() +
  geom_density(aes(x=m93.2080lo, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s93.2080lo, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r93.2080lo, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2"))+
  lims(x=c(0,1))-> p2

grid.arrange(p1,p2, nrow=2)  

# 2080hi
p.m.preds %>% 
  filter(grepl("M332|M333", ECOSUBCD)) %>%
  ggplot() +
  geom_density(aes(x=m19.2080hi, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s19.2080hi, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r19.2080hi, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2")) -> p1
p.m.preds %>% 
  filter(grepl("M332|M333", ECOSUBCD)) %>%
  ggplot() +
  geom_density(aes(x=m93.2080hi, fill = "mort"),alpha=0.5) +
  geom_density(aes(x=s93.2080hi, fill = "seed"),alpha=0.5) +
  geom_density(aes(x=r93.2080hi, fill = "saps"),alpha=0.5) +
  scale_fill_manual(name="rate",
                    values = c("mort" = "firebrick3",
                               "seed" = "dodgerblue2",
                               "saps" = "gold2")) -> p2

grid.arrange(p1,p2, nrow=2)  










































