## This script takes bayesian models from "bayes_models_v2.R", draws posterior predictions, and makes some partial effects plots that I use to explore model predictions


load("data/m19v5.Rdata")
load("data/s19v6.Rdata")
load("data/r19v6.Rdata")
load("data/m93v5.Rdata")
load("data/s93v6.Rdata")
load("data/r93v6.Rdata")

## 2sp MAT/CMD partial effects plots ----
# MAT anoms -------
#seedling prob X MAT_maxanom.z

s19.pred <- ggpredict(s19,
                      terms = c("MAT_maxanom.z [1.5:3.5, by = 0.05]")) %>% 
  as.data.frame()

s93.pred <- ggpredict(s93,
                      terms = c("MAT_maxanom.z [1.5:3.5, by = 0.05]")) %>% 
  as.data.frame()

s19.pred %>% 
  ggplot(aes(x = x,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = "ABLA"),
              alpha=0.4)+
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = "PIEN"),
              data = s93.pred,
              alpha=0.4) +
  geom_line(lwd=2,
            aes(col = "ABLA")) +
  geom_line(lwd = 2,
            data = s93.pred,
            aes(col = "PIEN")) + 
  scale_color_manual(values = c("ABLA" = "dodgerblue2",
                                "PIEN" = "goldenrod"),
                     aesthetics = c("fill", "col")) +
  labs(x = "MAT max anomaly (z-score)",
       y = "P(seedling)") +
  theme(legend.position = "none")

#recruit prob X MAT_maxanom.z

r19.pred <- ggpredict(r19,
                      terms = c("MAT_maxanom.z [1.5:3.5, by = 0.05]")) %>% 
  as.data.frame()

r93.pred <- ggpredict(r93,
                      terms = c("MAT_maxanom.z [1.5:3.5, by = 0.05]")) %>% 
  as.data.frame()

r19.pred %>% 
  ggplot(aes(x = x,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = "ABLA"),
              alpha=0.4)+
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = "PIEN"),
              data = r93.pred,
              alpha=0.4) +
  geom_line(lwd=2,
            aes(col = "ABLA")) +
  geom_line(lwd = 2,
            data = r93.pred,
            aes(col = "PIEN")) + 
  scale_color_manual(values = c("ABLA" = "dodgerblue2",
                                "PIEN" = "goldenrod"),
                     aesthetics = c("fill", "col")) +
  labs(x = "MAT max anomaly (z-score)",
       y = "P(sapling recruits)") +
  theme(legend.position = "none")

#mort prob X MAT_maxanom.z

m19.pred <- ggpredict(m19,
                      terms = c("MAT_maxanom.z [1.5:3.5, by = 0.05]")) %>% 
  as.data.frame()

m93.pred <- ggpredict(m93,
                      terms = c("MAT_maxanom.z [1.5:3.5, by = 0.05]")) %>% 
  as.data.frame()

m19.pred %>% 
  ggplot(aes(x = x,
             y = 1-predicted)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  fill = "ABLA"),
              alpha=0.4)+
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  fill = "PIEN"),
              data = m93.pred,
              alpha=0.4) +
  geom_line(lwd=2,
            aes(col = "ABLA")) +
  geom_line(lwd = 2,
            data = m93.pred,
            aes(col = "PIEN")) + 
  scale_color_manual(values = c("ABLA" = "dodgerblue2",
                                "PIEN" = "goldenrod"),
                     aesthetics = c("fill", "col")) +
  labs(x = "MAT max anomaly (z-score)",
       y = "P(mortality)") +
  theme(legend.position = "none")

# CMD anoms -----


#seedling prob X CMD_maxanom.z

s19.pred <- ggpredict(s19,
                      terms = c("CMD_maxanom.z [0.5:3.0, by = 0.05]")) %>% 
  as.data.frame()

s93.pred <- ggpredict(s93,
                      terms = c("MAT_maxanom.z [0.5:3.0, by = 0.05]")) %>% 
  as.data.frame()

s19.pred %>% 
  ggplot(aes(x = x,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = "ABLA"),
              alpha=0.4)+
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = "PIEN"),
              data = s93.pred,
              alpha=0.4) +
  geom_line(lwd=2,
            aes(col = "ABLA")) +
  geom_line(lwd = 2,
            data = s93.pred,
            aes(col = "PIEN")) + 
  scale_color_manual(values = c("ABLA" = "dodgerblue2",
                                "PIEN" = "goldenrod"),
                     aesthetics = c("fill", "col")) +
  labs(x = "CMD max anomaly (z-score)",
       y = "P(seedling)") +
  theme(legend.position = "none")

#recruit prob X CMD_maxanom.z

r19.pred <- ggpredict(r19,
                      terms = c("CMD_maxanom.z [0.5:3.0, by = 0.05]")) %>% 
  as.data.frame()

r93.pred <- ggpredict(r93,
                      terms = c("CMD_maxanom.z [0.5:3.0, by = 0.05]")) %>% 
  as.data.frame()

r19.pred %>% 
  ggplot(aes(x = x,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = "ABLA"),
              alpha=0.4)+
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = "PIEN"),
              data = r93.pred,
              alpha=0.4) +
  geom_line(lwd=2,
            aes(col = "ABLA")) +
  geom_line(lwd = 2,
            data = r93.pred,
            aes(col = "PIEN")) + 
  scale_color_manual(values = c("ABLA" = "dodgerblue2",
                                "PIEN" = "goldenrod"),
                     aesthetics = c("fill", "col")) +
  labs(x = "CMD max anomaly (z-score)",
       y = "P(sapling recruits)") +
  theme(legend.position = "none")

#mort prob X CMD_maxanom.z

m19.pred <- ggpredict(m19,
                      terms = c("CMD_maxanom.z [0.5:3.0, by = 0.05]")) %>% 
  as.data.frame()

m93.pred <- ggpredict(m93,
                      terms = c("CMD_maxanom.z [0.5:3.0, by = 0.05]")) %>% 
  as.data.frame()

m19.pred %>% 
  ggplot(aes(x = x,
             y = 1-predicted)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  fill = "ABLA"),
              alpha=0.4)+
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  fill = "PIEN"),
              data = m93.pred,
              alpha=0.4) +
  geom_line(lwd=2,
            aes(col = "ABLA")) +
  geom_line(lwd = 2,
            data = m93.pred,
            aes(col = "PIEN")) + 
  scale_color_manual(values = c("ABLA" = "dodgerblue2",
                                "PIEN" = "goldenrod"),
                     aesthetics = c("fill", "col")) +
  labs(x = "CMD max anomaly (z-score)",
       y = "P(mortality)") +
  theme(legend.position = "none")


## MAT climate reference x anoms-----

# MORTALITY
# ABLA MAT ref x anom

m19.pred <- ggpredict(m19, 
                      terms = c("MAT_maxanom.z [1.5:3.5, by = 0.05]",
                                "MAT_ref_mean [0, 3, 6]")) %>% 
  as.data.frame() %>% 
  rename(MAT_maxanom.z = x,
         MAT_ref_mean = group)

m19.pred %>% 
  ggplot(.,
         aes(x = MAT_maxanom.z,
             y = 1-predicted)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  fill = MAT_ref_mean),
              alpha = 0.4) +
  geom_line(aes(col = MAT_ref_mean),
            lwd=2) +
  scale_color_manual(name = "Reference\nMAT",
                     values = c("0" = "dodgerblue2",
                                "3" = "goldenrod",
                                "6" = "firebrick2"),
                     aesthetics = c("col","fill")) +
  labs(x = "MAT max anomaly (z-score)",
       y = "P(mortality)")

# PIEN MAT ref x anom

m93.pred <- ggpredict(m93, 
                      terms = c("MAT_maxanom.z [1.5:3.5, by = 0.05]",
                                "MAT_ref_mean [0, 3, 6]")) %>% 
  as.data.frame() %>% 
  rename(MAT_maxanom.z = x,
         MAT_ref_mean = group)

m93.pred %>% 
  ggplot(.,
         aes(x = MAT_maxanom.z,
             y = 1-predicted)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  fill = MAT_ref_mean),
              alpha = 0.4) +
  geom_line(aes(col = MAT_ref_mean),
            lwd=2) +
  scale_color_manual(name = "Reference\nMAT",
                     values = c("0" = "dodgerblue2",
                                "3" = "goldenrod",
                                "6" = "firebrick2"),
                     aesthetics = c("col","fill")) +
  labs(x = "MAT max anomaly (z-score)",
       y = "P(mortality)")

# SEEDLINGS
# ABLA MAT ref x anom

s19.pred <- ggpredict(s19, 
                      terms = c("MAT_maxanom.z [1.5:3.5, by = 0.05]",
                                "MAT_ref_mean [0, 3, 6]")) %>% 
  as.data.frame() %>% 
  rename(MAT_maxanom.z = x,
         MAT_ref_mean = group)

s19.pred %>% 
  ggplot(.,
         aes(x = MAT_maxanom.z,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = MAT_ref_mean),
              alpha = 0.4) +
  geom_line(aes(col = MAT_ref_mean),
            lwd=2) +
  scale_color_manual(name = "Reference\nMAT",
                     values = c("0" = "dodgerblue2",
                                "3" = "goldenrod",
                                "6" = "firebrick2"),
                     aesthetics = c("col","fill")) +
  labs(x = "MAT max anomaly (z-score)",
       y = "P(seedlings)")

# PIEN MAT ref x anom


s93.pred <- ggpredict(s93, 
                      terms = c("MAT_maxanom.z [1.5:3.5, by = 0.05]",
                                "MAT_ref_mean [0, 3, 6]")) %>% 
  as.data.frame() %>% 
  rename(MAT_maxanom.z = x,
         MAT_ref_mean = group)

s93.pred %>% 
  ggplot(.,
         aes(x = MAT_maxanom.z,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = MAT_ref_mean),
              alpha = 0.4) +
  geom_line(aes(col = MAT_ref_mean),
            lwd=2) +
  scale_color_manual(name = "Reference\nMAT",
                     values = c("0" = "dodgerblue2",
                                "3" = "goldenrod",
                                "6" = "firebrick2"),
                     aesthetics = c("col","fill")) +
  labs(x = "MAT max anomaly (z-score)",
       y = "P(seedlings)")

# SAPLINGS
# ABLA MAT ref x anom

r19.pred <- ggpredict(r19, 
                      terms = c("MAT_maxanom.z [1.5:3.5, by = 0.05]",
                                "MAT_ref_mean [0, 3, 6]")) %>% 
  as.data.frame() %>% 
  rename(MAT_maxanom.z = x,
         MAT_ref_mean = group)

r19.pred %>% 
  ggplot(.,
         aes(x = MAT_maxanom.z,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = MAT_ref_mean),
              alpha = 0.4) +
  geom_line(aes(col = MAT_ref_mean),
            lwd=2) +
  scale_color_manual(name = "Reference\nMAT",
                     values = c("0" = "dodgerblue2",
                                "3" = "goldenrod",
                                "6" = "firebrick2"),
                     aesthetics = c("col","fill")) +
  labs(x = "MAT max anomaly (z-score)",
       y = "P(sapling recruits)")

# PIEN MAT ref x anom


r93.pred <- ggpredict(r93, 
                      terms = c("MAT_maxanom.z [1.5:3.5, by = 0.05]",
                                "MAT_ref_mean [0, 3, 6]")) %>% 
  as.data.frame() %>% 
  rename(MAT_maxanom.z = x,
         MAT_ref_mean = group)

r93.pred %>% 
  ggplot(.,
         aes(x = MAT_maxanom.z,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = MAT_ref_mean),
              alpha = 0.4) +
  geom_line(aes(col = MAT_ref_mean),
            lwd=2) +
  scale_color_manual(name = "Reference\nMAT",
                     values = c("0" = "dodgerblue2",
                                "3" = "goldenrod",
                                "6" = "firebrick2"),
                     aesthetics = c("col","fill")) +
  labs(x = "MAT max anomaly (z-score)",
       y = "P(sapling recruits)")



























## CMD cliamte reference x anoms-----

# MORTALITY
# ABLA MAP ref x anom

m19.pred <- ggpredict(m19, 
                      terms = c("CMD_maxanom.z [0.5:3.0, by = 0.05]",
                                "MAP_ref_mean [500, 1000, 1500]")) %>% 
  as.data.frame() %>% 
  rename(CMD_maxanom.z = x,
         MAP_ref_mean = group)

m19.pred %>% 
  ggplot(.,
         aes(x = CMD_maxanom.z,
             y = 1-predicted)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  fill = MAP_ref_mean),
              alpha = 0.4) +
  geom_line(aes(col = MAP_ref_mean),
            lwd=2) +
  scale_color_manual(name = "Reference\nMAP",
                     values = c("firebrick2",
                                "goldenrod",
                                "dodgerblue2"),
                     aesthetics = c("col","fill")) +
  labs(x = "CMD max anomaly (z-score)",
       y = "P(mortality)")

# PIEN MAP ref x anom

m93.pred <- ggpredict(m93, 
                      terms = c("CMD_maxanom.z [0.5:3.0, by = 0.05]",
                                "MAP_ref_mean [500, 1000, 1500]")) %>% 
  as.data.frame() %>% 
  rename(CMD_maxanom.z = x,
         MAP_ref_mean = group)

m93.pred %>% 
  ggplot(.,
         aes(x = CMD_maxanom.z,
             y = 1-predicted)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  fill = MAP_ref_mean),
              alpha = 0.4) +
  geom_line(aes(col = MAP_ref_mean),
            lwd=2) +
  scale_color_manual(name = "Reference\nMAP",
                     values = c("firebrick2",
                                "goldenrod",
                                "dodgerblue2"),
                     aesthetics = c("col","fill")) +
  labs(x = "CMD max anomaly (z-score)",
       y = "P(mortality)")

# SEEDLINGS
# ABLA MAP ref x anom

s19.pred <- ggpredict(s19, 
                      terms = c("CMD_maxanom.z [0.5:3.0, by = 0.05]",
                                "MAP_ref_mean [500, 1000, 1500]")) %>% 
  as.data.frame() %>% 
  rename(CMD_maxanom.z = x,
         MAP_ref_mean = group)

s19.pred %>% 
  ggplot(.,
         aes(x = CMD_maxanom.z,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = MAP_ref_mean),
              alpha = 0.4) +
  geom_line(aes(col = MAP_ref_mean),
            lwd=2) +
  scale_color_manual(name = "Reference\nMAP",
                     values = c("firebrick2",
                                "goldenrod",
                                "dodgerblue2"),
                     aesthetics = c("col","fill")) +
  labs(x = "CMD max anomaly (z-score)",
       y = "P(seedlings)")

# PIEN MAP ref x anom


s93.pred <- ggpredict(s93, 
                      terms = c("CMD_maxanom.z [0.5:3.0, by = 0.05]",
                                "MAP_ref_mean [500, 1000, 1500]")) %>% 
  as.data.frame() %>% 
  rename(CMD_maxanom.z = x,
         MAP_ref_mean = group)

s93.pred %>% 
  ggplot(.,
         aes(x = CMD_maxanom.z,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = MAP_ref_mean),
              alpha = 0.4) +
  geom_line(aes(col = MAP_ref_mean),
            lwd=2) +
  scale_color_manual(name = "Reference\nMAP",
                     values = c("firebrick2",
                                "goldenrod",
                                "dodgerblue2"),
                     aesthetics = c("col","fill")) +
  labs(x = "CMD max anomaly (z-score)",
       y = "P(seedlings)")

# SAPLINGS
# ABLA MAP ref x anom

r19.pred <- ggpredict(r19, 
                      terms = c("CMD_maxanom.z [0.5:3.0, by = 0.05]",
                                "MAP_ref_mean [500, 1000, 1500]")) %>% 
  as.data.frame() %>% 
  rename(CMD_maxanom.z = x,
         MAP_ref_mean = group)

r19.pred %>% 
  ggplot(.,
         aes(x = CMD_maxanom.z,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = MAP_ref_mean),
              alpha = 0.4) +
  geom_line(aes(col = MAP_ref_mean),
            lwd=2) +
  scale_color_manual(name = "Reference\nMAP",
                     values = c("firebrick2",
                                "goldenrod",
                                "dodgerblue2"),
                     aesthetics = c("col","fill")) +
  labs(x = "CMD max anomaly (z-score)",
       y = "P(sapling recruits)")

# PIEN MAP ref x anom


r93.pred <- ggpredict(r93, 
                      terms = c("CMD_maxanom.z [0.5:3.0, by = 0.05]",
                                "MAP_ref_mean [500, 1000, 1500]")) %>% 
  as.data.frame() %>% 
  rename(CMD_maxanom.z = x,
         MAP_ref_mean = group)

r93.pred %>% 
  ggplot(.,
         aes(x = CMD_maxanom.z,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = MAP_ref_mean),
              alpha = 0.4) +
  geom_line(aes(col = MAP_ref_mean),
            lwd=2) +
  scale_color_manual(name = "Reference\nMAP",
                     values = c("firebrick2",
                                "goldenrod",
                                "dodgerblue2"),
                     aesthetics = c("col","fill")) +
  labs(x = "CMD max anomaly (z-score)",
       y = "P(sapling recruits)")



## MORTALITY x tree size x anoms ------

# subalpine fir
m19.pred <- ggpredict(m19,
                      terms = c("PREVDIA [12.7:55, by = 0.5]",
                                "MAT_maxanom.z [1.5,2.5,3.5]"),
                      ) %>% 
  as.data.frame() %>% 
  rename(PREVDIA = x,
         MAT_maxanom.z = group)

m19.pred %>% 
  ggplot(.,
         aes(x = PREVDIA,
             y = 1-predicted)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  fill = MAT_maxanom.z),
              alpha=0.4) +
  geom_line(aes(col = MAT_maxanom.z),
            lwd=2) +
  scale_color_manual(name = "MAT max anomaly\n (z-score)",
                     values = c("dodgerblue2","gold2","firebrick2"),
                     aesthetics = c("fill","col")) +
  labs(x = "Tree size (cm DBH)",
       y = "P(mortality)")

m19.pred <- ggpredict(m19,
                      terms = c("PREVDIA [12.7:55, by = 0.5]",
                                "CMD_maxanom.z [0.5,1.5,2.5]"),
) %>% 
  as.data.frame() %>% 
  rename(PREVDIA = x,
         CMD_maxanom.z = group)

m19.pred %>% 
  ggplot(.,
         aes(x = PREVDIA,
             y = 1-predicted)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  fill = CMD_maxanom.z),
              alpha=0.4) +
  geom_line(aes(col = CMD_maxanom.z),
            lwd=2) +
  scale_color_manual(name = "CMD max anomaly\n (z-score)",
                     values = c("dodgerblue2","gold2","firebrick2"),
                     aesthetics = c("fill","col")) +
  labs(x = "Tree size (cm DBH)",
       y = "P(mortality)")

# Engelmann spruce

m93.pred <- ggpredict(m93,
                      terms = c("PREVDIA [12.7:55, by = 0.5]",
                                "MAT_maxanom.z [1.5,2.5,3.5]"),
) %>% 
  as.data.frame() %>% 
  rename(PREVDIA = x,
         MAT_maxanom.z = group)

m93.pred %>% 
  ggplot(.,
         aes(x = PREVDIA,
             y = 1-predicted)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  fill = MAT_maxanom.z),
              alpha=0.4) +
  geom_line(aes(col = MAT_maxanom.z),
            lwd=2) +
  scale_color_manual(name = "MAT max anomaly\n (z-score)",
                     values = c("dodgerblue2","gold2","firebrick2"),
                     aesthetics = c("fill","col")) +
  labs(x = "Tree size (cm DBH)",
       y = "P(mortality)")

m93.pred <- ggpredict(m93,
                      terms = c("PREVDIA [12.7:55, by = 0.5]",
                                "CMD_maxanom.z [0.5,1.5,2.5]"),
) %>% 
  as.data.frame() %>% 
  rename(PREVDIA = x,
         CMD_maxanom.z = group)

m93.pred %>% 
  ggplot(.,
         aes(x = PREVDIA,
             y = 1-predicted)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  fill = CMD_maxanom.z),
              alpha=0.4) +
  geom_line(aes(col = CMD_maxanom.z),
            lwd=2) +
  scale_color_manual(name = "CMD max anomaly\n (z-score)",
                     values = c("dodgerblue2","gold2","firebrick2"),
                     aesthetics = c("fill","col")) +
  labs(x = "Tree size (cm DBH)",
       y = "P(mortality)")


## disturbance plots ----

# species together  -----
##mort
pred.19 <- ggpredict(m19, 
                     terms = c("area.fire.prop [0:0.5, by=0.01]")) %>% 
  as.data.frame() %>% 
  rename(area.fire = x)

pred.93 <- ggpredict(m93, 
                     terms = c("area.fire.prop [0:0.5, by=0.01]")) %>% 
  as.data.frame() %>% 
  rename(area.fire = x)

pred.19 %>% 
  ggplot(aes(x = area.fire,
             y = 1-predicted)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  fill = "ABLA"),
              alpha=0.4)+
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  fill = "PIEN"),
              data = pred.93,
              alpha=0.4) +
  geom_line(lwd=2,
            aes(col = "ABLA")) +
  geom_line(lwd = 2,
            data = pred.93,
            aes(col = "PIEN")) + 
  scale_color_manual(values = c("ABLA" = "dodgerblue2",
                                "PIEN" = "goldenrod"),
                     aesthetics = c("fill", "col")) +
  labs(x = "Proportion disturbed by fire",
       y = "P(mortality)") +
  theme(legend.position = "none")
## seed
pred.19 <- ggpredict(s19, 
                     terms = c("fire.sev [0:1, by=0.01]")) %>% 
  as.data.frame() %>% 
  rename(fire.sev = x)

pred.93 <- ggpredict(s93, 
                     terms = c("fire.sev [0:1, by=0.01]")) %>% 
  as.data.frame() %>% 
  rename(fire.sev = x)

pred.19 %>% 
  ggplot(aes(x = fire.sev,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = "ABLA"),
              alpha=0.4)+
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = "PIEN"),
              data = pred.93,
              alpha=0.4) +
  geom_line(lwd=2,
            aes(col = "ABLA")) +
  geom_line(lwd = 2,
            data = pred.93,
            aes(col = "PIEN")) + 
  scale_color_manual(values = c("ABLA" = "dodgerblue2",
                                "PIEN" = "goldenrod"),
                     aesthetics = c("fill", "col")) +
  labs(x = "Fire severity (prop. mortality)",
       y = "P(seedling)") +
  theme(legend.position = "none")
# sap
pred.19 <- ggpredict(r19, 
                     terms = c("fire.sev [0:1, by=0.01]")) %>% 
  as.data.frame() %>% 
  rename(fire.sev = x)

pred.93 <- ggpredict(r93, 
                     terms = c("fire.sev [0:1, by=0.01]")) %>% 
  as.data.frame() %>% 
  rename(fire.sev = x)

pred.19 %>% 
  ggplot(aes(x = fire.sev,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = "ABLA"),
              alpha=0.4)+
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = "PIEN"),
              data = pred.93,
              alpha=0.4) +
  geom_line(lwd=2,
            aes(col = "ABLA")) +
  geom_line(lwd = 2,
            data = pred.93,
            aes(col = "PIEN")) + 
  scale_color_manual(values = c("ABLA" = "dodgerblue2",
                                "PIEN" = "goldenrod"),
                     aesthetics = c("fill", "col")) +
  labs(x = "Fire severity (prop. mortality)",
       y = "P(sapling recruits)") +
  theme(legend.position = "none")

## disturbance x anomalies

pred.19 <- ggpredict(m19, 
                      terms = c("MAT_maxanom.z [1.5:3.5, by = 0.05]",
                                "area.fire.prop [0, .15, 0.3]")) %>% 
  as.data.frame() %>% 
  rename(MAT_maxanom.z = x,
         area.fire = group)

pred.19 %>% 
  ggplot(.,
         aes(x = MAT_maxanom.z,
             y = 1-predicted)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  fill = area.fire),
              alpha = 0.4) +
  geom_line(aes(col = area.fire),
            lwd=2) +
  scale_color_manual(name = "Area disturbed\nby fire",
                     values = c("0" = "dodgerblue2",
                                "0.15" = "goldenrod",
                                "0.3" = "firebrick2"),
                     aesthetics = c("col","fill")) +
  labs(x = "MAT max anomaly (z-score)",
       y = "P(mortality)")








#-------

m19.pred <- ggpredict(m19,
                     # terms = c("MAT_maxanom.z [1.5:3.5, by=0.05]",
                    #            "PREVDIA [10,18.8,35]"),
                      terms = c("MAT_ref_mean [-2:9, by = 0.5]",
                                "MAP_ref_mean [500,1000,1500]"),
                      ) %>% 
  as.data.frame() %>% 
  rename(MAT_ref_mean = x,
         MAP_ref_mean = group)

m19.pred %>% 
  ggplot(.,
         aes(x = MAT_ref_mean,
             y = 1-predicted)) +
  geom_ribbon(aes(ymin = 1-conf.low,
                  ymax = 1-conf.high,
                  fill = MAP_ref_mean),
              alpha=0.4) +
  geom_line(aes(col = MAP_ref_mean),
            lwd=2)






















### plotting mortalityXregen densities for different categories----

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
  mutate(SPCD=19) %>% 
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
  mutate(SPCD=93) %>% 
  na.omit()


ggplot(p19,
       aes(x=m.pred,
           y = s.pred,
           col = factor(SPCD),
           bg = factor(SPCD))) +
  geom_point(alpha=0.2,pch=21)+
  geom_density_2d(lwd=0.75) +
  geom_point(data=p93,alpha=0.2,pch=21)+
  geom_density_2d(data=p93,lwd=0.75) +
  facet_wrap(facets=~factor(mult.comp.coexist, 
                            levels=c("resilience",
                                     "structural change",
                                     "compositional change",
                                     "replacement")),
             ncol=1) +
  labs(x = "Predicted probability of mortality",
       y = "Predicted probability of regeneration") +
  scale_color_manual(name = "Species",
                     values = c("19" = "dodgerblue3",
                                "93" = "firebrick2"),
                     aesthetics = c("col","bg"))
