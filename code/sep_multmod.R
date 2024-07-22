p.m <- p.m %>% 
  left_join(dall.er.ablapien %>% 
              sf::st_drop_geometry() %>% 
              select(ECOSUBCD = MAP_UNIT_S, quad.19, quad.93),
            by="ECOSUBCD")

m.m19 <- nnet::multinom(formula = 
                          quad.19 ~ m.pred.19*s.pred.19*r.pred.19,
                        data = p.m, maxit=10000)

p.m$pred19 <- predict(m.m19,type="class")

tab <- table(p.m$quad.19, p.m$pred19)

round((sum(diag(tab))/sum(tab))*100,2)


m.m93 <- nnet::multinom(formula = 
                          quad.93 ~ m.pred.93*s.pred.93*r.pred.93,
                        data = p.m, maxit=10000)

p.m$pred93 <- predict(m.m93,type="class")

tab <- table(p.m$quad.93, p.m$pred93)

round((sum(diag(tab))/sum(tab))*100,2)

predict(m.m93,type="probs") %>% round(3) %>% 
  as.data.frame() %>% 
  mutate(pred = predict(m.m93,type="class"),
         obs = p.m$quad.93) %>%
  View()


p.m <- p.m %>% 
  mutate(pred.traj = case_when(pred19 == "decline" & pred93 == "decline" ~ "replacement",
                               
                               pred19 == "turnover" | pred93 == "turnover" ~ "structural change",
                               
                               pred19 == "decline" & pred93 %in% c("development","densification") ~ "compositional change",
                               pred93 == "decline" & pred19 %in% c("development","densification") ~ "compositional change",
                               
                               pred19 %in% c("development","densification") & 
                                 pred93 %in% c("development","densification") ~ "resilience")) %>% 
  left_join(dall.er.ablapien %>% 
              sf::st_drop_geometry() %>% 
              select(ECOSUBCD=MAP_UNIT_S,
                     mult.comp.coexist),
            by="ECOSUBCD")
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         