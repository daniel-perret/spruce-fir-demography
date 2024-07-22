# dummy dataset for Dave

ind.mort.dat %>% 
  filter(SPCD==19) %>% 
  slice_sample(n=1000) %>% 
  select(TRE_CN, PLT_CN, ECOSUBCD,MORTYR,
         SURV, PREVDIA, PREV_CR,
         prev.damage, PREV_BAH, area.fire.prop,T1,T2, MORTYR, REMPER) %>%
  write.table(., sep=",",
            file = "data/dummydata_fordave2.csv",
            row.names=FALSE)
