## playing with different final figure ideas...

# making dataframe wider and summarizing change
p.m.wide <- p.m.test %>% 
  ungroup() %>% 
  select(-time,-dist) %>% 
  pivot_wider(names_from = pred.type, 
              values_from = resilience:replacement)

thresh <- 0.25

rad <- dall.er.ablapien %>% 
  select(ECOSUBCD = MAP_UNIT_S) %>% 
  left_join(p.m.wide %>% 
              mutate(d.resilience = (resilience_2050lo - resilience_2050hi),
                     d.replacement = replacement_2050hi - replacement_currobs,
                     a.resilience = ifelse(#resilience_currobs>thresh & 
                                             resilience_2050hi>replacement_2050hi &
                                             resilience_2050hi>reassembly_2050hi &
                                             resilience_2050hi>restructuring_2050hi,
                                             1,0),
                     a.replacement = ifelse(replacement_2050lo>resilience_2050lo &
                                              replacement_2050lo>reassembly_2050lo &
                                              replacement_2050lo>restructuring_2050lo,
                                            1,0),
                     rad.cat = case_when(a.resilience == 1 ~ 1,
                                     a.replacement == 1 ~ 2,
                                     TRUE ~ 0)) %>% 
              select(ECOSUBCD, contains("d."),contains("a.")))

# map figures

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
          aes(fill = d.resilience)) +
  geom_sf(data = states %>% 
            as(.,"sf"),
          col=linecolor,
          fill=NA,
          lty=2) +
  scale_color_steps2(low = "white",mid="white", high = "dodgerblue2",
                     aesthetics = "fill", midpoint = 0,
                     breaks=c(0.25,0.5,0.75)) +
  # scale_color_steps2(low = "firebrick3", mid = "white", high = "dodgerblue3",
  #                    aesthetics = "fill",midpoint = 0,
  #                    breaks=c(-0.5,-0.25,0,0.25,0.5)) +
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.position="right",
        panel.background = element_rect(fill="skyblue1"))  

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
          aes(fill = factor(rad.cat))) +
  geom_sf(data = states %>% 
            as(.,"sf"),
          col=linecolor,
          fill=NA,
          lty=2) +
  scale_color_manual(values=c("0" = "white",
                              "1" = "dodgerblue2",
                              "2" = "firebrick3"),
                     aesthetics = "fill") +
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.position="right",
        panel.background = element_rect(fill="skyblue1"))  


## here's another way of doing it

rad2 <- dall.er.ablapien %>% 
  select(ECOSUBCD = MAP_UNIT_S) %>% 
  left_join(p.m.wide)

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
  geom_sf(data = rad2,
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
  # scale_color_steps2(low = "firebrick3", mid = "white", high = "dodgerblue3",
  #                    aesthetics = "fill",midpoint = 0,
  #                    breaks=c(-0.5,-0.25,0,0.25,0.5)) +
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.position="none",
        panel.background = element_rect(fill="skyblue1"))  


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
  geom_sf(data = rad2,
          col=NA,
          aes(fill = replacement_2050lo)) +
  geom_sf(data = states %>% 
            as(.,"sf"),
          col=linecolor,
          fill=NA,
          lty=2) +
  scale_color_steps(low = "white", high = "firebrick4",
                     aesthetics = "fill",#midpoint=0.25,
                     breaks=c(0.25,0.5,0.75)) +
  # scale_color_steps2(low = "firebrick3", mid = "white", high = "dodgerblue3",
  #                    aesthetics = "fill",midpoint = 0,
  #                    breaks=c(-0.5,-0.25,0,0.25,0.5)) +
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.position="none",
        panel.background = element_rect(fill="skyblue1"))  



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
  geom_sf(data = rad2,
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
  # scale_color_steps2(low = "firebrick3", mid = "white", high = "dodgerblue3",
  #                    aesthetics = "fill",midpoint = 0,
  #                    breaks=c(-0.5,-0.25,0,0.25,0.5)) +
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.00e6,3.25e6)) +
  theme(legend.position="none",
        panel.background = element_rect(fill="skyblue1"))  





















