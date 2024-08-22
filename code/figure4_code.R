## code for FIGURE 4

p.m.preds <- read.csv("data/future_scenario_demography.csv",header=T)
p.m.test <- read.csv("data/future_scenario.csv",header=T)

load("data/joint_popestimates.Rdata") # `dall.er.ablapien` object

## run after completing all of the `scenarios.R` script

## arranging future projection dataframes for plots

### summing across predicted joint trajectories weighted by spruce-fir forest area in each ecoregion subsection

areatotal <- sum(p.m$AREA,na.rm=T)

p.m.sums <- p.m.test %>% 
  group_by(pred.type, dist, time) %>% 
  summarise(resilience = sum(resilience*AREA)/areatotal,
            restructuring = sum(restructuring*AREA)/areatotal,
            reassembly = sum(reassembly*AREA)/areatotal,
            replacement = sum(replacement*AREA)/areatotal) %>%
  pivot_longer(cols = resilience:replacement,
               names_to = "trajectory", 
               values_to = "prop")

### making prediction dataframe (unsummed) wider for plotting

p.m.wide <- p.m.test %>% 
  ungroup() %>% 
  select(-time,-dist) %>% 
  pivot_wider(names_from = pred.type, 
              values_from = resilience:replacement)

### joining ecoregion subsection spatial data to prediction dataframe for plotting

rad <- dall.er.ablapien %>% 
  select(ECOSUBCD = MAP_UNIT_S) %>% 
  left_join(p.m.wide)

# Figure 4a-c -- column plots showing the proportion of spruce-fir forest predicted in each joint trajectory class under low, observed, and high disturbance scenarios under current climatic conditions, 2040-2050 climatic conditios, and 2070-2080 climatic conditions

p.m.sums %>% 
  ggplot(.,
         aes(x = factor(time, levels = c("curr","2050","2080")),
             fill = factor(trajectory, levels = c("replacement","reassembly",
                                                  "restructuring","resilience")))) +
  geom_bar(aes(weight=prop)) +
  scale_fill_manual(name = "trajectory",
                    values = c("resilience" = "dodgerblue2",
                               "restructuring" = "gold2",
                               "reassembly" = "firebrick2",
                               "replacement" = "firebrick4"),
                    aesthetics = c("fill")) +
  facet_wrap(facets = ~factor(dist,
                              levels = c("lo","obs","hi")),
             nrow=3) +
  labs(x = "time") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

## Figure 4d - Map showing the probability of resilience in 2040-2050 under the HIGH disturbance scenario -- these are "resist" areas, interpreted as resilience refugias

ggplot() +
  geom_sf(data = cont %>% 
            as(.,"sf"),
          col=linecolor,
          fill = mapcolor,
          lwd=0.3) +  
  geom_sf(data = states %>% 
            as(.,"sf"),
          fill=NA,
          col=linecolor) +
  geom_sf(data = rad,
          col = linecolor,
          fill = NA)+
  geom_sf(data = rad,
          col=NA,
          aes(fill = resilience_2050hi)) +
  geom_sf(data = states %>% 
            as(.,"sf"),
          col=linecolor,
          fill=NA,
          lty=2) +
  scale_color_steps(low = "white", high = "dodgerblue2",
                    aesthetics = "fill",
                    breaks=c(0,0.25,0.5,0.75)) +
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.position="none",
        panel.background = element_rect(fill="skyblue1"))  

## Figure 4e - Map showing the probability of replacement in 2040-2050 under the LOW disturbance scenario -- these are "accept" areas where subalpine forests may be committed to change

ggplot() +
  geom_sf(data = cont %>% 
            as(.,"sf"),
          col=linecolor,
          fill = mapcolor,
          lwd=0.3) +  
  geom_sf(data = states %>% 
            as(.,"sf"),
          fill=NA,
          col=linecolor) +
  geom_sf(data = rad,
          col = linecolor,
          fill = NA)+
  geom_sf(data = rad,
          col=NA,
          aes(fill = replacement_2050lo)) +
  geom_sf(data = states %>% 
            as(.,"sf"),
          col=linecolor,
          fill=NA,
          lty=2) +
  scale_color_steps(low = "white", high = "firebrick4",
                    aesthetics = "fill",
                    breaks=c(0.25,0.5,0.75)) +
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.position="none",
        panel.background = element_rect(fill="skyblue1"))  

## Figure 4f -- Maps showing the change in probability of resilience when moving from a HIGH to LOW disturbance scenario in 2040-2050 -- these are "direct" areas where moving systems to a lower-intensity disturbance regime could greatly bolster resilience

ggplot() +
  geom_sf(data = cont %>% 
            as(.,"sf"),
          col=linecolor,
          fill = mapcolor,
          lwd=0.3) +  
  geom_sf(data = states %>% 
            as(.,"sf"),
          fill=NA,
          col=linecolor) +
  geom_sf(data = rad,
          col = linecolor,
          fill = NA)+
  geom_sf(data = rad,
          col=NA,
          aes(fill = resilience_2050lo-resilience_2050hi)) +
  geom_sf(data = states %>% 
            as(.,"sf"),
          col=linecolor,
          fill=NA,
          lty=2) +
  scale_color_steps2(low = "firebrick4",mid="white", high = "dodgerblue2",
                     aesthetics = "fill", midpoint = 0,
                     breaks=c(-0.75,-0.5,-0.25,0,0.25,0.5,0.75),
                     name="") +
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.position="none",
        panel.background = element_rect(fill="skyblue1"))  





















