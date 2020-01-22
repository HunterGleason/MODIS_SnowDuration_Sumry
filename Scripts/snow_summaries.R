

##############################################################################################
#Code for summarising M*D10A1 snow duration data of Bevignton et al. (2019)
#Hunter Gleason
#FLNRORD
#Dec 18, 2019
##############################################################################################

##############################################################################################
#Import dependencies
##############################################################################################

library(RColorBrewer)
library(tidyverse)
library(bcmaps)
library(sf)
library(boot)
library(gridExtra)
library(raster)
library(RStoolbox)
library(ggrepel)
library(ggridges)
library(scales)
library(egg)

##############################################################################################
# LOAD DATA: Get BC Natural Resource Districts (NRD) and intersect with M*D10A1 snow duration samples from Bevignton et al. (2018)
##############################################################################################

#Set seed
  # set.seed(101)

# Load Shapefiles
    
  # Natural Resource Districts
    nrd <- nr_districts() %>%
      dplyr::mutate(dname_shrt = str_sub(DISTRICT_NAME,1,-27), rnum = row_number()) %>% 
      dplyr::arrange(rnum) %>% 
      dplyr::select(DISTRICT_NAME, rnum, REGION_ORG_UNIT, dname_shrt) %>% 
      mutate(n = factor(paste(rnum, "=", DISTRICT_NAME)))
    nrd$n <- factor(nrd$n, levels = nrd$n)

  # Natural Resource Regions
    nrr <- nr_regions()
  
  # Outline of British Columbia
    bc <- bc_bound_hres()
    
  # MODIS Samples (Bevington et al. 2018) *LARGE*
    # sd_samp_pts<-st_read('Data/samples_raw_65k_MODIS_water.gpkg')
    # sd_samp_pts<-sd_samp_pts %>% filter(waterMask == 0)
    # sd_by_nrd <- sd_samp_pts %>% st_transform(3005) %>% st_join(nrd, join = st_intersects)
    # write_sf(sd_by_nrd, "Data/samples_raw_65k_MODIS_water_intersection.gpkg")
    sd_by_nrd <- read_sf("Data/samples_raw_65k_MODIS_water_intersection.gpkg")
    
# Load Raster 
    
  # TRIM Hillshade (Located on network drive)
    # dem <- raster("../../../!_Geospatial/TRIM/bc_elevation_250m_bcalb_hs_msk.tif")
    # dem[dem>250] <- NA
    # dem_msk <- raster::mask(dem, mask = bc_bound_hres(class = "sp"))
    # writeRaster(dem_msk, "Data/hillshade_clip_250m.tif")
    # 250 m 
    dem_msk <- raster("Data/hillshade_clip_250m.tif")
    # 25 m 
    # dem_msk <- raster("N:/Data_DEM/DEM_BC_TRIM/bc_elevation_250m_bcalb_hs.tif")    
    
##############################################################################################
# PREPARE SNOW SAMPLE DATA
##############################################################################################
    
    
    sd_by_nrd <- sd_by_nrd %>% rename(sdon = SDon, 
                         sdoff = SDoff,
                         sddur = SDdur,
                         sdobs = SDobs)
    
    sd_by_nrd <- sd_by_nrd %>% 
      # Classify observations as snow, no snow, or no data
      mutate(na_flag = case_when(
        sdon == -9 & sdoff == -9 & sddur == 0  ~ "no_snow",
        sdon == -9 & sdoff == -9 & sddur == -9 ~ "no_data",
        sdon >= 0  & sdoff >= 0  & sddur >= 0  ~ "snow")) %>% 
      # Remove NO DATA
      filter(na_flag != "no_data") %>% 
      # Remove observations that do not intersect NRD
      filter(!is.na(dname_shrt)) %>% 
      # Replace -9 (error) with NA for ON and OFF
      mutate(sdon = as.numeric(sub(-9, NA, sdon)),
             sdoff = as.numeric(sub(-9, NA, sdoff)),
             # Add Calendar Date instead of days since sep. 1
             sdon_date = as.Date.character('2000-09-01') + lubridate::days(sdon),
             # Format Calendar Date as JD
             sdon_jd = format(sdon_date, "%j")) %>% 
      group_by(dname_shrt) %>% 
      # Calculate group means
      mutate(sddur_mean = mean(sddur, na.rm = T))

  # Calculate % snow coverage per region
    sd_snow_perc <- sd_by_nrd %>% 
      st_drop_geometry() %>% 
      group_by(rnum, na_flag) %>% 
      summarise(n = n()) %>% 
      pivot_wider(names_from = na_flag, values_from = n) %>% 
      mutate(percent = 100*(snow/(no_snow+snow))) %>% 
      dplyr::select(rnum, percent)
    
  # Add percent results to main df
    sd_by_nrd <- merge(sd_by_nrd, sd_snow_perc)
      
##############################################################################################
# Figure: Map of Study Area
##############################################################################################
    
#Make plot of NRDs, save to working dir
  nrd_plt <- ggplot() +
    # TRIM Hillshade (optional)
    ggR(dem_msk, maxpixels = ncell(dem), ggLayer = T) +
    # BC outline
    geom_sf(data = bc, color = "grey30", size = 0.5, fill = NA) +
    # NR District names to legend (without outline)
    geom_sf(data = nrd, color = NA, fill = NA, aes(alpha = n)) +
    # NR District outline
    geom_sf(data = nrd, color = "red", size = 1.2, fill = NA, alpha = 0.6) +
    # NR Region outlines
    geom_sf(data = nrr, color = "black", size = 0.8,  fill = NA) +
    # Merge NRD and NRR labels to ensure no overlap (repel)
    geom_label_repel(data = rbind(mutate(nrd,lab=rnum,class="NRD") %>% dplyr::select(lab,class),
                                  mutate(nrr,lab=sub("Natural Resource Region", "", REGION_NAME), class = "NRR") %>% dplyr::select(lab, class)),
                     aes(label = lab, geometry = geometry, size = class, fill = class),
                     fontface = "bold", 
                     stat = "sf_coordinates",
                     point.padding = NA,
                     arrow = NULL, #arrow(length = unit(0.02, "npc")),
                     segment.size = 0.5, segment.colour = NA, 
                     nudge_x = 0,
                     nudge_y = 0,
                     hjust = 0.5,
                     vjust = 0.5, 
                     show.legend = F) +
    scale_size_manual(values = c(3,4)) + 
    scale_fill_manual(values = c("white","white")) +
    labs(alpha = "Natural Resource District") +
    labs(x = "", y = "") +
    guides(alpha = guide_legend(ncol = 1, )) + 
    theme_minimal() +
    theme(legend.position = c(0.85,0.7), panel.border = element_rect(fill=NA), panel.grid = element_blank()); nrd_plt
  
  ggsave(filename = 'nrd_map_hs.png', device = 'png',path = 'Figures/Maps/',width=15,height = 12,units = c('in'))

##############################################################################################
# Figure: Ridge-Plots 
##############################################################################################

  rig_sdon <- ggplot() + 
      geom_density_ridges2(data = sd_by_nrd, 
                           aes(x = sdoff, 
                               y = reorder(paste0(dname_shrt, " (", round(sddur_mean,0), " days, ", round(percent,0), "%)"), sddur, FUN = median, na.rm = T), 
                               fill = "SDon"), 
                           scale = 1, rel_min_height = 0.01, color = NA, bandwidth = 2) + 
      geom_density_ridges2(data = sd_by_nrd, 
                           aes(x = sdon, 
                               y = reorder(paste0(dname_shrt, " (", round(sddur_mean,0), " days, ", round(percent,0), "%)"), sddur, FUN = median, na.rm = T), 
                               fill= "SDoff"), 
                           scale = 1, rel_min_height = 0.01, color = NA, bandwidth = 2) + 
      scale_fill_manual(values = c("blue","red"), labels = c(expression("SD"[ON]),expression("SD"[OFF]))) + 
      scale_x_continuous(breaks = c(0,91,181,273,365), 
                     sec.axis = dup_axis(breaks = c(0,100,200,300,365), labels = c(0,100,200,300,365), name = "Days since September 1 (DSS)"), 
                     labels = 
                       c(format(as.Date.character('2000-09-01') + lubridate::days(0),"%b"),
                         format(as.Date.character('2000-09-01') + lubridate::days(91),"%b"),
                         format(as.Date.character('2000-09-01') + lubridate::days(181),"%b"),
                         format(as.Date.character('2000-09-01') + lubridate::days(273),"%b"),
                         format(as.Date.character('2000-09-01') + lubridate::days(365),"%b")), expand = c(0,0)) + 
      labs(x="Date", y = "Natural Resource District", fill = "Snow Metric: ") + 
      theme_classic() +
      theme(legend.position = "bottom", panel.border = element_rect(color = "black", fill = NA)); rig_sdon

  ggsave(filename = 'rig_sdon.png',plot = rig_sdon,device = 'png',path = 'Figures/BoxPlots/',width=8.5,height = 8.5,units = c('in'))
  
##############################################################################################
# Get ATMOS
##############################################################################################

  library(ratmos)
  ao <- get_ao()
  oni <- get_oni()
  pna <- get_pna()
  pdo <- get_pdo()
  all <- merge(merge(merge(oni,pdo),pna), ao) %>% 
    pivot_longer(c("ONI","PDO","PNA","AO"), names_to = "index", values_to = "value")
  
  
  ggplot(all, aes(x = Date, y = value)) + 
    geom_col(aes(fill = value), color = "black", size = 0.5)+
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 10, name = "RdYlBu"), 
                         limits = c(-4,4), 
                         guide = guide_colorbar(barwidth = 1, barheight = 50, frame.colour = "black", ticks.colour = "black", frame.linewidth = 1, ticks.linewidth = 1)) +
    scale_x_date(limits = c(as.Date("2000-01-01"),as.Date("2020-01-01")), expand = c(0,0), date_breaks = "2 years", date_labels = "%Y") + 
    facet_wrap(~index, scales = "free_y", ncol = 1) + 
    labs(fill = "", x = "", y = "Index Value") +
    egg::theme_article(base_size = 30)
  
  ggsave(filename = 'tel.png', device = 'png',path = 'Figures/',width=15,height = 15,units = c('in'))
  
##############################################################################################
# Figure: ATMOS LM
##############################################################################################
  
# Fall Indices
  fall <- all %>% 
    filter(Month %in% c(9,10,11)) %>% 
    group_by(Year, index) %>% 
    summarise(value = mean(value, na.rm = T))

# Function to Normalize Indices
  normalizzzer <- function(x){
    dn <- (x - min(x)) / (max(x) - min(x))
    return(dn)}

# Add normalized index value as column
  fall <- fall %>% 
    ungroup() %>%
    group_by(index) %>%
    mutate(norm = normalizzzer(value))

# Calculate
  sd_mean <- sd_by_nrd %>% 
    st_drop_geometry() %>% 
    mutate(Year = year) %>% 
    dplyr::select(-year) %>% 
    group_by(dname_shrt, Year) %>% 
    summarise(sd_dur = mean(sddur, na.rm = T))

# Merge Indices with Mean for LM
  sd_mean_fall <- merge(fall, sd_mean)

# Raw lm
  sd_mean_fall_lm <- sd_mean_fall %>%
    dplyr::group_by(dname_shrt,index) %>%
    do(broom::tidy(lm(sd_dur~value, data = ., na.action = na.omit))) %>%
    dplyr::filter(term != "(Intercept)") %>% 
    mutate(p.value.c = cut(p.value, c(-Inf,0.01,0.05,Inf),labels = c("**","*","")))
  
# Normalized lm
  sd_mean_fall_nlm <- sd_mean_fall %>%
    dplyr::group_by(dname_shrt,index) %>%
    do(broom::tidy(lm(sd_dur~norm, data = ., na.action = na.omit))) %>%
    dplyr::filter(term != "(Intercept)") %>% 
    mutate(p.value.c = cut(p.value, c(-Inf,0.01,0.05,Inf),labels = c("<0.01","<0.05",">=0.05")))

# Normalized lm add GEO
  sd_mean_fall_nlm_geo <- merge(nrd, sd_mean_fall_nlm)
  
# Min Max Estimate for Plot Legent
  max = max(sd_mean_fall_nlm_geo$estimate)
  min = min(sd_mean_fall_nlm_geo$estimate)

# Map of Normalized Estimate
  ggplot(sd_mean_fall_nlm_geo) + 
    geom_sf(aes(fill = estimate)) + 
    geom_point(data = st_centroid(sd_mean_fall_nlm_geo), aes(geometry = geometry, x = stat(x), y = stat(y), size = p.value.c, shape = p.value.c), stat = "sf_coordinates") +
    scale_shape_manual(values = c(8,16,21)) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 10, name = "RdYlBu"),
                         guide = guide_colorbar(barwidth = 3, barheight = 40, frame.colour = "black", ticks.colour = "black", frame.linewidth = 1, ticks.linewidth = 1), 
                         values=rescale(c(min,0,max)), limits=c(min,max), breaks = seq(signif(min,1),signif(max,1),20)) +
    facet_wrap(~index) +
    theme_void(base_size = 30) + 
    labs(fill = "Normalized Slope", size = "P-value", shape = "P-value") 

  ggsave(filename = 'lm_maps_norm2.png', device = 'png',path = 'Figures/',width=15,height = 15,units = c('in'))

# Table of Raw lm
  sd_mean_fall_lm_table <- sd_mean_fall_lm %>% 
    dplyr::select(-statistic, -std.error, -p.value, -term) %>% 
    pivot_wider(names_from = index, values_from = c(estimate, p.value.c))

  write.csv(sd_mean_fall_lm_table, "Tables/sd_mean_fall_lm.csv")  

    
##############################################################################################
# TABLE 3-1 Summarise M*D10A1 data by NRD
##############################################################################################

#Get mean and std. dev. of SD metrics by NRD
sd_sumry <- sd_by_nrd %>% 
    dplyr::select(rnum, 
                  dname_shrt, 
                  sdon, 
                  sdoff,
                  sddur) %>% 
    sf::st_drop_geometry() %>% 
    pivot_longer(cols = c(-rnum, -dname_shrt), 
                 names_to = "Metric", 
                 values_to = "Value") %>%
    mutate(Value = round(Value, 1)) %>% 
    group_by(rnum, 
             dname_shrt, 
             Metric) %>% 
    summarise(mean = mean(Value, na.rm=T),
              sd = sd(Value, na.rm=T)) %>% 
    pivot_wider(names_from = Metric, 
                values_from = c(mean,sd)) %>% 
    dplyr::arrange(rnum)
      

    
#Write summary table to working directory
write_csv(sd_sumry, path = 'Tables/sumry_tab_2.csv')


##############################################################################################
# Map summary statitics by NRD
##############################################################################################

#Join summary results to NRD sf layer
sd_joined <- nrd %>% 
  merge(sd_sumry)

# SDON Plot summary results by NRD
sd_on_mean_plt <- ggplot() +
    ggR(dem_msk, maxpixels = ncell(dem_msk)/2, ggLayer = T) +
    
    geom_sf(data = sd_joined, aes(fill = mean_sdon), alpha = 0.4)+
    geom_sf(data = bc, color = "grey30", size = 0.5, fill = NA) +
    geom_point(data = st_centroid(sd_joined), aes(geometry = geometry, x = stat(x), y = stat(y), size = sd_sdon), stat = "sf_coordinates") +
  
    labs(fill = expression(SD["ON"]), 
         size = "SD", 
         x = "", y = "") +
    
    # scale_size_continuous(breaks = seq(0,100,15)) +
    scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(n = 9, "BuPu")), limits = c(min(sd_joined$mean_sdon),max(sd_joined$mean_sdon)), breaks = seq(50,100,10)) +
    
    guides(fill = guide_colorbar(barwidth = 2, barheight = 15, 
                                 frame.colour = "black", ticks.colour = "black", 
                                 draw.ulim = T, draw.llim = T, )) +
    
    theme_minimal(base_size = 20) + 
    theme(panel.grid = element_blank(), axis.text = element_blank(),
          plot.margin=grid::unit(c(0,0,0,0), "mm"), 
          legend.position = c(0.85,0.7),
          legend.box = "horizontal"); sd_on_mean_plt

ggsave(filename = 'sd_on_mean_plt.png',plot = sd_on_mean_plt ,device = 'png',path = 'Figures/Maps/',width=12,height = 8.5,units = c('in'))

#SDOFF Plot summary results by NRD
sd_off_mean_plt <- ggplot() +
  ggR(dem_msk, maxpixels = ncell(dem_msk)/2, ggLayer = T) +
  
  geom_sf(data = sd_joined, aes(fill = mean_sdoff), alpha = 0.4)+
  geom_sf(data = bc, color = "grey30", size = 0.5, fill = NA) +
  geom_point(data = st_centroid(sd_joined), aes(geometry = geometry, x = stat(x), y = stat(y), size = sd_sdoff), stat = "sf_coordinates") +
  
  labs(fill = expression(SD["OFF"]), 
       size = "SD", 
       x = "", y = "") +
  
  # scale_size_continuous(breaks = seq(0,100,15)) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 9, "BuPu"), limits = c(min(sd_joined$mean_sdoff),max(sd_joined$mean_sdoff)), breaks = seq(150,300,25)) +
  
  guides(fill = guide_colorbar(barwidth = 2, barheight = 15, 
                               frame.colour = "black", ticks.colour = "black", 
                               draw.ulim = T, draw.llim = T, )) +
  
  theme_minimal(base_size = 20) + 
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"), 
        legend.position = c(0.85,0.7),
        legend.box = "horizontal"); sd_off_mean_plt

ggsave(filename = 'sd_off_mean_plt.png',plot = sd_off_mean_plt ,device = 'png',path = 'Figures/Maps/',width=12,height = 8.5,units = c('in'))

#SDDUR Plot summary results by NRD
sd_dur_mean_plt <- ggplot() +
  ggR(dem_msk, maxpixels = ncell(dem_msk)/2, ggLayer = T) +
  
  geom_sf(data = sd_joined, aes(fill = mean_sddur), alpha = 0.4)+
  geom_sf(data = bc, color = "grey30", size = 0.5, fill = NA) +
  geom_point(data = st_centroid(sd_joined), aes(geometry = geometry, x = stat(x), y = stat(y), size = sd_sddur), stat = "sf_coordinates") +
  
  labs(fill = expression(SD["DUR"]), 
       size = "SD", 
       x = "", y = "") +
  
  # scale_size_continuous(breaks = seq(0,100,15)) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 9, "BuPu"), limits = c(min(sd_joined$mean_sddur),max(sd_joined$mean_sddur)), breaks = seq(25,225,25)) +
  
  guides(fill = guide_colorbar(barwidth = 2, barheight = 15, 
                               frame.colour = "black", ticks.colour = "black", 
                               draw.ulim = T, draw.llim = T, )) +
  
  theme_minimal(base_size = 20) + 
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"), 
        legend.position = c(0.85,0.7),
        legend.box = "horizontal"); sd_dur_mean_plt

ggsave(filename = 'sd_dur_mean_plt.png',plot = sd_dur_mean_plt ,device = 'png',path = 'Figures/Maps/',width=12,height = 8.5,units = c('in'))

##############################################################################################
#Annual Analysis of snow metrics 
##############################################################################################


#Generate boxplots of all snow metrics by hydrologic year, save to project dir
# box_anual_sdon <- 


sd_summ_yr <- 
  sd_by_nrd %>% 
  ungroup() %>% 
  dplyr::select(year, 
                sdon, 
                sdoff,
                sddur) %>% 
  sf::st_drop_geometry() %>% 
  pivot_longer(cols = c(-year), 
               names_to = "Metric", 
               values_to = "Value") %>%
  mutate(Value = round(Value, 1)) %>% 
  group_by(year, 
           Metric) %>% 
  summarise(mean = mean(Value, na.rm=T),
            sd = sd(Value, na.rm=T)) %>% 
  # pivot_wider(names_from = Metric, 
  #             values_from = c(mean,sd)) %>% 
  dplyr::arrange(year) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  group_by(Metric) %>% 
  mutate(grmean = mean(mean)) %>% 
  mutate(dif = mean-grmean)
  
sd_summ_yr$Metric <- factor(sd_summ_yr$Metric)
levels(sd_summ_yr$Metric) <- c("SD[DUR]","SD[OFF]","SD[ON]")
sd_summ_yr$Metric <- factor(sd_summ_yr$Metric, levels = c("SD[ON]","SD[OFF]","SD[DUR]"))

sd_summ_yr_labs <- sd_summ_yr %>%
  dplyr::group_by(Metric) %>%
  do(broom::tidy(lm(mean~year, data = ., na.action = na.omit))) %>%
  dplyr::filter(term != "(Intercept)") %>% 
  mutate(p.value.c = cut(p.value, c(-Inf,0.01,0.05,Inf),labels = c("**","*","")))


sd_summ_yr %>% 
    ggplot(aes(year,mean)) + 
      geom_smooth(method = "lm", se = F, color = "black") +
      geom_hline(aes(yintercept = grmean, group = Metric), color = "black", linetype = 2) +
      geom_errorbar(aes(x = year, 
                        ymin = mean-sd,
                        ymax = mean+sd,
                        color = Metric), show.legend = F, width = 0.3, alpha = 0.6) +
      geom_line(aes(color = Metric), show.legend = F) + 
      geom_point(aes(fill = Metric), shape = 21, size = 3, show.legend = F) + 
      geom_text(data = sd_summ_yr_labs, aes(x = 2000, y = c(110,320,290), label = Metric), parse = T) +
      geom_text(data = sd_summ_yr_labs, aes(x = 2008, y = c(110,320,290), label = paste("Slope:", signif(estimate, 2), "days per year, p-value:", signif(p.value,2)))) +
      #egg::theme_article() + 
      facet_wrap(~Metric, scales="free", ncol = 1, 
                 strip.position = "left", 
                 labeller = as_labeller(c("SD[ON]" = "SD[ON]", 
                                          "SD[OFF]" = "SD[OFF]", 
                                          "SD[DUR]" = "SD[DUR]") )) + 
  scale_color_manual(values = c("blue","red","black")) + 
  scale_fill_manual(values = c("blue","red","black")) + 
  scale_x_continuous(breaks = seq(2000,2020, 2)) + 
  labs(x = "", y = "") +
  theme(strip.background = element_blank(),strip.placement = "outside")

# ggsave(filename = 'timeseries.png',device = 'png',path = 'Figures/',width=7,height = 7,units = c('in'))

sd_summ_yr %>% 
  ungroup() %>% 
  dplyr::select(year, Metric, mean, sd) %>% 
  pivot_wider(names_from = Metric, values_from = c(mean, sd))


#Get mean and std. dev. of SD metrics by hydrologic year 
# sd_anual_sumry <- sd_by_nrd %>%
#   group_by(year) %>%
#   summarise(mean_sdon = mean(sdon, na.rm = TRUE), sd_sdon = sd(sdon, na.rm = TRUE),mean_sdoff = mean(sdoff, na.rm = TRUE), sd_sdoff = sd(sdoff, na.rm = TRUE),mean_sddur = mean(sddur, na.rm = TRUE), sd_sddur = sd(sddur, na.rm = TRUE))

#Write summary table to working directory
write_csv(sd_summ_yr %>% as_tibble(), path = 'Tables/sumry_tab_yrly.csv')



##############################################################################################
#Get boot straped CI's for summary stats by hydrologic year
##############################################################################################


# ##Map bootstrap function to compute mean for each SD metric by hydrologic year
# sd_cis_mu_yrly<-sd_by_nrd %>%
#   group_by(year) %>%
#   summarise(sdon_lw = boot.ci(boot(data = sdon,statistic=mu,R=reps),type=c("basic"))$basic[4],
#             sdon_up = boot.ci(boot(data = sdon,statistic=mu,R=reps),type=c("basic"))$basic[5],
#             sdoff_lw = boot.ci(boot(data = sdoff,statistic=mu,R=reps),type=c("basic"))$basic[4],
#             sdoff_up = boot.ci(boot(data = sdoff,statistic=mu,R=reps),type=c("basic"))$basic[5],
#             sddur_lw = boot.ci(boot(data = sddur,statistic=mu,R=reps),type=c("basic"))$basic[4],
#             sddur_up = boot.ci(boot(data = sddur,statistic=mu,R=reps),type=c("basic"))$basic[5])
# 
# #Write bootstrap CI's to working directory 
# write_csv(sd_cis_mu_yrly %>% as_tibble() %>% select(year,sdon_lw,sdon_up,sdoff_lw,sdoff_up,sddur_lw,sddur_up), path = 'Tables/boot_ci_mu_yrly.csv')
# 
# 
# 
# #Map bootstrap function to compute std. dev. for each SD metric by hydrologic year
# sd_cis_sdev_yrly<-sd_by_nrd %>%
#   group_by(year) %>%
#   summarise(sdon_lw = boot.ci(boot(data = sdon,statistic=sdev,R=reps),type=c("basic"))$basic[4],
#             sdon_up = boot.ci(boot(data = sdon,statistic=sdev,R=reps),type=c("basic"))$basic[5],
#             sdoff_lw = boot.ci(boot(data = sdoff,statistic=sdev,R=reps),type=c("basic"))$basic[4],
#             sdoff_up = boot.ci(boot(data = sdoff,statistic=sdev,R=reps),type=c("basic"))$basic[5],
#             sddur_lw = boot.ci(boot(data = sddur,statistic=sdev,R=reps),type=c("basic"))$basic[4],
#             sddur_up = boot.ci(boot(data = sddur,statistic=sdev,R=reps),type=c("basic"))$basic[5])
# 
# #Write bootstrap CI's to working directory 
# write_csv(sd_cis_sdev_yrly %>% as_tibble() %>% select(year,sdon_lw,sdon_up,sdoff_lw,sdoff_up,sddur_lw,sddur_up), path = 'Tables/boot_ci_sdev_yrly.csv')
# 
# 
# 
# 
# mod <- stack("../Git_Snow_MODIS/Data/MODIS/Derived/Annual_Snow_Metrics/MD10A1_SD_2017_clp.tif")
# 
# sdon <- mod$MD10A1_SD_2017_clp.1
# sdon <- projectRaster(sdon, crs = crs(bc))
# sdon[sdon<0] <- NA
# 
# ggR(sdon, geom_raster = T) + theme_void() 



##############################################################################################
#Get boot straped CI's for summary stats by NRD
##############################################################################################

# #Functions to pass to 'boot'
# mu<-function(data,ind)
# {
#   return(mean(data[ind],na.rm=TRUE))
# }
# sdev<-function(data,ind)
# {
#   return(sd(data[ind],na.rm=TRUE))
# }
# 
# #Bootstrap repititions
# reps=500
# 
# #Map bootstrap function to compute mean for each SD metric by NRD
# sd_cis_mu<-sd_by_nrd %>%
#   group_by(dname_shrt) %>%
#   summarise(sdon_lw = boot.ci(boot(data = sdon,statistic=mu,R=reps),type=c("basic"))$basic[4],
#             sdon_up = boot.ci(boot(data = sdon,statistic=mu,R=reps),type=c("basic"))$basic[5],
#             sdoff_lw = boot.ci(boot(data = sdoff,statistic=mu,R=reps),type=c("basic"))$basic[4],
#             sdoff_up = boot.ci(boot(data = sdoff,statistic=mu,R=reps),type=c("basic"))$basic[5],
#             sddur_lw = boot.ci(boot(data = sddur,statistic=mu,R=reps),type=c("basic"))$basic[4],
#             sddur_up = boot.ci(boot(data = sddur,statistic=mu,R=reps),type=c("basic"))$basic[5])
# 
# #Write bootstrap CI's to working directory 
# write_csv(sd_cis_mu %>% as_tibble() %>% select(dname_shrt,sdon_lw,sdon_up,sdoff_lw,sdoff_up,sddur_lw,sddur_up), path = 'Tables/boot_ci_mu.csv')
# 
# #Map bootstrap function to compute std. dev. for each SD metric by NRD
# sd_cis_sdev<-sd_by_nrd %>%
#   group_by(dname_shrtm Metric) %>%
#   summarise(sdon_lw = boot.ci(boot(data = sdon,statistic=sdev,R=reps),type=c("basic"))$basic[4],
#             sdon_up = boot.ci(boot(data = sdon,statistic=sdev,R=reps),type=c("basic"))$basic[5],
#             sdoff_lw = boot.ci(boot(data = sdoff,statistic=sdev,R=reps),type=c("basic"))$basic[4],
#             sdoff_up = boot.ci(boot(data = sdoff,statistic=sdev,R=reps),type=c("basic"))$basic[5],
#             sddur_lw = boot.ci(boot(data = sddur,statistic=sdev,R=reps),type=c("basic"))$basic[4],
#             sddur_up = boot.ci(boot(data = sddur,statistic=sdev,R=reps),type=c("basic"))$basic[5])
# 
# #Write bootstrap CI's to working directory
# write_csv(sd_cis_sdev %>% as_tibble() %>% select(dname_shrt,sdon_lw,sdon_up,sdoff_lw,sdoff_up,sddur_lw,sddur_up), path = 'Tables/boot_ci_sdev.csv')
