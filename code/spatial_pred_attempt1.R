## MAKING SPATIAL PREDICTOR LAYERS

# past.r.template <- past.climna$MAT %>% 
#   mask(.,
#        mask = co.shp.buff) %>% 
#   crop(., y = extent(co.shp.buff)) %>% 
#   projectRaster(., crs = CRS(old.proj)) 
# 
# past.r.template[!is.na(past.r.template[])] <- 1
# 
# 
# pnt.template <- rasterToPoints(past.r.template)
# 
# elevation <- raster("/Users/DanielPerret/Box/01. daniel.perret Workspace/base_spatialdata/elevation.tif")
# 
# names(elevation) <- "elev"
# 
# pnt.template <- bind_cols(pnt.template,
#                           pnt.template %>% 
#                             as.data.frame() %>% 
#                             select(x, y) %>% 
#                             SpatialPoints(., 
#                                           proj4string = CRS(old.proj)) %>% 
#                             raster::extract(x = elevation, y = .) %>% 
#                             as.data.frame())
# 
# pnt.template <-  pnt.template %>% 
#   as.data.frame() %>% 
#   mutate(ID1 = "a",
#          ID2 = "a") %>% 
#   rename(lat = y,
# #          long = x, 
# #          el = ".") %>% 
# #   select(ID1, ID2, lat, long, el)
# 
# write.csv(pnt.template, "pastClimNAtemplate.csv", row.names=F)
# 
# pnt.template <- read.csv("pastClimNAtemplate.csv")
# 
# ## recent data
# climna.pnts <- read.csv("pastClimNAtemplate_Decade_2011_2020Y.csv",
#                         header=T)
# 
# 
# MAT <- past.r.template
# MAT[which(MAT[]==1)] <- climna.pnts$MAT
# MAP <- past.r.template
# MAP[which(MAP[]==1)] <- climna.pnts$MAP
# CMD <- past.r.template
# CMD[which(CMD[]==1)] <- climna.pnts$CMD
# 
# recent.climna <- stack(MAT,MAP,CMD)
# names(recent.climna) <- c("MAT_recent","MAP_recent","CMD_recent")
# 
# ## future data
# climna.pnts <- read.csv("FutClimNA_2041_2060_ssp370.csv",
#                         header=T)
# 
# MAT <- past.r.template
# MAT[which(MAT[]==1)] <- climna.pnts$MAT
# MAP <- past.r.template
# MAP[which(MAP[]==1)] <- climna.pnts$MAP
# CMD <- past.r.template
# CMD[which(CMD[]==1)] <- climna.pnts$CMD
# 
# ssp370.climna <- stack(MAT,MAP,CMD)
# names(ssp370.climna) <- c("MAT_ssp370","MAP_ssp370","CMD_ssp370")
# 
# 
# climna.pnts <- read.csv("FutClimNA_2041_2060_ssp585_2.csv",
#                         header=T)
# 
# MAT <- past.r.template
# MAT[which(MAT[]==1)] <- climna.pnts$MAT
# MAP <- past.r.template
# MAP[which(MAP[]==1)] <- climna.pnts$MAP
# CMD <- past.r.template
# CMD[which(CMD[]==1)] <- climna.pnts$CMD
# 
# ssp585.climna <- stack(MAT,MAP,CMD)
# names(ssp585.climna) <- c("MAT_ssp585","MAP_ssp585","CMD_ssp585")
# 
# 
# ####
# 
# allClimNA <- stack(past.climna.p,recent.climna,ssp370.climna,ssp585.climna)
# names(allClimNA)

writeRaster(allClimNA %>% 
              terra::rast(),filename = "climNA_all.tif", overwrite=T)

allClimNA <- raster::stack("data/climNA_all.tif")

########### filling rasters

past.climna.p <- past.climna %>% 
  mask(.,
       mask = co.shp.buff) %>% 
  crop(., y = extent(co.shp.buff)) %>% 
  projectRaster(., crs = CRS(old.proj)) 
names(past.climna.p) <- c("MAT_past","MAP_past","CMD_past")
past.climna.p$MAT_past[] <- past.climna.p$MAT_past[]/10

climna.all.pnts <- allClimNA %>% 
  rasterToPoints() %>% 
  as.data.frame() %>% 
  mutate(MAT_anom = MAT_recent - MAT_past,
         MAP_relanom = (MAP_recent-MAP_past)/MAP_past,
         CMD_relanom = (CMD_recent-CMD_past)/CMD_past,
         MAT_anom370 = MAT_ssp370 - MAT_past,
         MAP_relanom370 = (MAP_ssp370-MAP_past)/MAP_past,
         CMD_relanom370 = (CMD_ssp370-CMD_past)/CMD_past,
         MAT_anom585 = MAT_ssp585 - MAT_past,
         MAP_relanom585 = (MAP_ssp585-MAP_past)/MAP_past,
         CMD_relanom585 = (CMD_ssp585-CMD_past)/CMD_past)

############# making prediction dataframe

spat.predictors19 <- climna.all.pnts %>% 
  SpatialPointsDataFrame(data = .,
                         coords = .[,c("x","y")],
                         proj4string = CRS(proj4string(allClimNA))) %>% 
  spTransform(.,
              CRSobj = CRS(base.proj)) %>% 
  sf::st_as_sf() %>% 
  sf::st_join(.,
              dall.er.ablapien %>% 
                select(ECOSUBCD = MAP_UNIT_S,
                       area.fire.prop,
                       area.id.prop)) %>% 
  left_join(ind.mort.dat %>%
              filter(SPCD==19) %>%
              group_by(ECOSUBCD) %>%
              summarise(PREVDIA = mean(PREVDIA),
                        PREV_CR = mean(PREV_CR),
                        prev.damage = sum(prev.damage)/n()
                        #prev.damage="0"
              ),
            by="ECOSUBCD") %>%
  # mutate(PREVDIA = 20,
  #        PREV_CR = 60,
  #        prev.damage = "0") %>%
  sf::st_drop_geometry() %>% 
  rename(MAT_19802010 = MAT_past,
         MAP_19802010 = MAP_past) %>% 
  na.omit() %>% 
  SpatialPointsDataFrame(data = .,
                         coords = .[,c("x","y")],
                         proj4string = CRS(old.proj)) %>% 
  spTransform(., CRSobj = CRS(base.proj)) %>%  
  as.data.frame() %>% 
  rename(x.utm = x.1,
         y.utm = y.1)


spat.predictors93 <- climna.all.pnts %>% 
  SpatialPointsDataFrame(data = .,
                         coords = .[,c("x","y")],
                         proj4string = CRS(proj4string(allClimNA))) %>% 
  spTransform(.,
              CRSobj = CRS(base.proj)) %>% 
  sf::st_as_sf() %>% 
  sf::st_join(.,
              dall.er.ablapien %>% 
                select(ECOSUBCD = MAP_UNIT_S,
                       area.fire.prop,
                       area.id.prop)) %>% 
  left_join(ind.mort.dat %>%
              filter(SPCD==93) %>%
              group_by(ECOSUBCD) %>%
              summarise(PREVDIA = mean(PREVDIA),
                        PREV_CR = mean(PREV_CR),
                        prev.damage = sum(prev.damage)/n()
                        #prev.damage="0"
              ),
            by="ECOSUBCD") %>%
  # mutate(PREVDIA = 20,
  #        PREV_CR = 60,
  #        prev.damage = "0") %>%
  sf::st_drop_geometry() %>% 
  rename(MAT_19802010 = MAT_past,
         MAP_19802010 = MAP_past) %>% 
  na.omit() %>% 
  SpatialPointsDataFrame(data = .,
                         coords = .[,c("x","y")],
                         proj4string = CRS(old.proj)) %>% 
  spTransform(., CRSobj = CRS(base.proj)) %>%  
  as.data.frame() %>% 
  rename(x.utm = x.1,
         y.utm = y.1)

################## abla mort ----------------

spat.pred.m19 <- spat.predictors19 %>% 
  mutate(predicted.current = predict(m19,
                                     newdata = spat.predictors19,
                                     type = "response", 
                                     re.form = NULL,
                                     allow.new.levels=T),
         predicted.ssp370 = predict(m19,
                                    newdata = spat.predictors19 %>% 
                                      mutate(MAT_anom = MAT_anom370,
                                             MAP_relanom = MAP_relanom370,
                                             CMD_relanom = CMD_relanom370),
                                    type = "response", 
                                    re.form = NULL,
                                    allow.new.levels=T),
         predicted.ssp585 = predict(m19,
                                    newdata = spat.predictors19 %>% 
                                      mutate(MAT_anom = MAT_anom585,
                                             MAP_relanom = MAP_relanom585,
                                             CMD_relanom = CMD_relanom585),
                                    type = "response", 
                                    re.form = NULL,
                                    allow.new.levels=T)) %>% 
  mutate(ssp370.diff = predicted.ssp370 - predicted.current,
         ssp585.diff = predicted.ssp585 - predicted.current,
         ssp370.diffperc = ssp370.diff/predicted.current,
         ssp585.diffperc = ssp585.diff/predicted.current)

## current
library(rasterVis)
terra::rast(x = spat.pred.m19 %>% 
              select(x,y,predicted),
            crs = old.proj) %>% 
  terra::project(., base.proj) %>% 
  gplot(.,
        maxpixels = 1e6) +
  geom_raster(aes(x = x,
                  y = y,
                  fill = 1-value),
              interpolate = F) +
  geom_sf(data = cont %>%
            as(.,"sf"),
          #col=linecolor,
          inherit.aes = F,
          fill = NA,
          lwd=1) +
  geom_sf(data = states %>% 
            as(., "sf"),
          inherit.aes=F,
          fill=NA,
          lwd=0.5) +
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.25e6,3.25e6)) +
  scale_fill_steps(high = "firebrick2",
                   low = "ivory",
                   #midpoint = 0.5,
                   n.breaks=7,
                   na.value = NA,
                   name = "Dec. Mort.") +
  labs(x=NULL,
       y=NULL,
       title = "Subalpine fir",
       subtitle = "predicted decadal mortality \ncurrent conditions")

## ssp370 difference
terra::rast(x = spat.pred.m19 %>% 
              select(x,y,ssp370.diff),
            crs = old.proj) %>% 
  terra::project(., base.proj) %>% 
  gplot(.,
        maxpixels = 1e6) +
  geom_raster(aes(x = x,
                  y = y,
                  fill = -1*value),
              interpolate = F) +
  geom_sf(data = cont %>%
            as(.,"sf"),
          #col=linecolor,
          inherit.aes = F,
          fill = NA,
          lwd=1) +
  geom_sf(data = states %>% 
            as(., "sf"),
          inherit.aes=F,
          fill=NA,
          lwd=0.5) +
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.25e6,3.25e6)) +
  scale_fill_steps2(high = "firebrick2",
                    low = "dodgerblue2",
                    mid = "ivory",
                    midpoint = 0,
                    #n.breaks=7,
                    breaks=c(-0.75,-0.5,-0.25,0,.25,.5,.75),
                    na.value = NA,
                    #limits = c(-1.5,1.5),
                    name = "Mort Change"
  ) +
  labs(x=NULL,
       y=NULL,
       title = "Subalpine fir",
       subtitle = "predicted mortality change \n2041-2060, SSP3-7.0")

## ssp585 difference
terra::rast(x = spat.pred.m19 %>% 
              select(x,y,ssp585.diff),
            crs = old.proj) %>% 
  terra::project(., base.proj) %>% 
  gplot(.,
        maxpixels = 1e6) +
  geom_raster(aes(x = x,
                  y = y,
                  fill = -1*value),
              interpolate = F) +
  geom_sf(data = cont %>%
            as(.,"sf"),
          #col=linecolor,
          inherit.aes = F,
          fill = NA,
          lwd=1) +
  geom_sf(data = states %>% 
            as(., "sf"),
          inherit.aes=F,
          fill=NA,
          lwd=0.5) +
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.25e6,3.25e6)) +
  scale_fill_steps2(high = "firebrick2",
                    low = "dodgerblue2",
                    mid = "ivory",
                    midpoint = 0,
                    breaks=c(-0.75,-0.5,-0.25,0,.25,.5,.75),
                    #n.breaks=7,
                    na.value = NA,
                    #limits = c(-1.5,1.5),
                    name = "Mort Change"
  ) +
  labs(x=NULL,
       y=NULL,
       title = "Subalpine fir",
       subtitle = "predicted mortality change \n2041-2060, SSP5-8.5")

########################## pien mort -----------------

spat.pred.m93 <- spat.predictors93 %>% 
  mutate(predicted.current = predict(m93,
                                     newdata = spat.predictors93,
                                     type = "response", 
                                     re.form = NULL,
                                     allow.new.levels=T),
         predicted.ssp370 = predict(m93,
                                    newdata = spat.predictors93 %>% 
                                      mutate(MAT_anom = MAT_anom370,
                                             MAP_relanom = MAP_relanom370,
                                             CMD_relanom = CMD_relanom370),
                                    type = "response", 
                                    re.form = NULL,
                                    allow.new.levels=T),
         predicted.ssp370dist = predict(m93,
                                    newdata = spat.predictors93 %>% 
                                      mutate(MAT_anom = MAT_anom370,
                                             MAP_relanom = MAP_relanom370,
                                             CMD_relanom = CMD_relanom370,
                                             area.id.prop = area.id.prop+0.25),
                                    type = "response", 
                                    re.form = NULL,
                                    allow.new.levels=T),
         predicted.ssp585 = predict(m93,
                                    newdata = spat.predictors93 %>% 
                                      mutate(MAT_anom = MAT_anom585,
                                             MAP_relanom = MAP_relanom585,
                                             CMD_relanom = CMD_relanom585),
                                    type = "response", 
                                    re.form = NULL,
                                    allow.new.levels=T)) %>% 
  mutate(ssp370.diff = predicted.ssp370 - predicted.current,
         ssp370.diffdist = predicted.ssp370dist - predicted.current,
         ssp585.diff = predicted.ssp585 - predicted.current,
         ssp370.diffperc = ssp370.diff/predicted.current,
         ssp585.diffperc = ssp585.diff/predicted.current)

## current
terra::rast(x = spat.pred.m93 %>% 
              select(x,y,predicted.current),
            crs = old.proj) %>% 
  terra::project(., base.proj) %>% 
  gplot(.,
        maxpixels = 1e6) +
  geom_raster(aes(x = x,
                  y = y,
                  fill = 1-value),
              interpolate = F) +
  geom_sf(data = cont %>%
            as(.,"sf"),
          #col=linecolor,
          inherit.aes = F,
          fill = NA,
          lwd=1) +
  geom_sf(data = states %>% 
            as(., "sf"),
          inherit.aes=F,
          fill=NA,
          lwd=0.5) +
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.25e6,3.25e6)) +
  scale_fill_steps(high = "firebrick2",
                   low = "ivory",
                   #midpoint = 0.5,
                   n.breaks=7,
                   na.value = NA,
                   name = "Dec. Mort.") +
  labs(x=NULL,
       y=NULL,
       title = "Engelmann spruce",
       subtitle = "predicted decadal mortality \ncurrent conditions")

## ssp370 difference
terra::rast(x = spat.pred.m93 %>% 
              select(x,y,ssp370.diffdist),
            crs = old.proj) %>% 
  terra::project(., base.proj) %>% 
  gplot(.,
        maxpixels = 1e6) +
  geom_raster(aes(x = x,
                  y = y,
                  fill = -1*value),
              interpolate = F) +
  geom_sf(data = cont %>%
            as(.,"sf"),
          #col=linecolor,
          inherit.aes = F,
          fill = NA,
          lwd=1) +
  geom_sf(data = states %>% 
            as(., "sf"),
          inherit.aes=F,
          fill=NA,
          lwd=0.5) +
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.25e6,3.25e6)) +
  scale_fill_steps2(high = "firebrick2",
                    low = "dodgerblue2",
                    mid = "ivory",
                    midpoint = 0,
                    #n.breaks=7,
                    breaks=c(-0.75,-0.5,-0.25,0,.25,.5,.75),
                    na.value = NA,
                    #limits = c(-1.5,1.5),
                    name = "Mort Change"
  ) +
  labs(x=NULL,
       y=NULL,
       title = "Engelmann spruce",
       subtitle = "predicted mortality change \n2041-2060, SSP3-7.0")

## ssp585 difference
terra::rast(x = spat.pred.m93 %>% 
              select(x,y,ssp585.diff),
            crs = old.proj) %>% 
  terra::project(., base.proj) %>% 
  gplot(.,
        maxpixels = 1e6) +
  geom_raster(aes(x = x,
                  y = y,
                  fill = -1*value),
              interpolate = F) +
  geom_sf(data = cont %>%
            as(.,"sf"),
          #col=linecolor,
          inherit.aes = F,
          fill = NA,
          lwd=1) +
  geom_sf(data = states %>% 
            as(., "sf"),
          inherit.aes=F,
          fill=NA,
          lwd=0.5) +
  lims(x = c(-2.5e6, -0.5e6),
       y = c(1.25e6,3.25e6)) +
  scale_fill_steps2(high = "firebrick2",
                    low = "dodgerblue2",
                    mid = "ivory",
                    midpoint = 0,
                    breaks=c(-0.75,-0.5,-0.25,0,.25,.5,.75),
                    #n.breaks=7,
                    na.value = NA,
                    #limits = c(-1.5,1.5),
                    name = "Mort Change"
  ) +
  labs(x=NULL,
       y=NULL,
       title = "Engelmann spruce",
       subtitle = "predicted mortality change \n2041-2060, SSP5-8.5")






























