##############################################################################################
#Code for summarising M*D10A1 snow duration data of Bevignton et al. (2019)
#Hunter Gleason
#FLNRORD
#Dec 18, 2019
##############################################################################################

##############################################################################################
#Import dependencies
##############################################################################################

library(tidyverse)
library(bcmaps)
library(sf)
library(boot)
library(gridExtra)

##############################################################################################
# Get BC Natural Resource Districts (NRD) and intersect with M*D10A1 snow duration samples from Bevignton et al. (2018)
##############################################################################################

#Set seed
set.seed(101)

#Get BC NRD as sf object
nrd <- nr_districts() %>%
  mutate(dname_shrt = str_sub(DISTRICT_NAME,1,-27),rnum = row_number())


#Make plot of NRDs, save to working dir
nrd_plt <- ggplot() + geom_sf(data = nrd, aes(fill=REGION_ORG_UNIT))+geom_sf_label(data=nrd,aes(label = rnum)) + labs(y='Lat.', x = 'Lon.')
nrd_plt
ggsave(filename = 'nrd_map',plot = nrd_plt ,device = 'png',path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Figures/Maps/',width=8.5,height = 8.5,units = c('in'))

#Write out plot legend as CSV
write_csv(as.data.frame(nrd %>% select(rnum,dname_shrt))[,c(1:2)] , path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Tables/legend.csv')


# Bevington et al. (2018) M*D10A1 Snow Duration Samples (local copy)
sd_samp_pts<-st_read('~/Dropbox/FLNRO/Projects/snow_summaries_note/SD_Random_Samples.gpkg')

#Join the SD samples to the NR Districts via spatial intersection 
sd_by_nrd <- sd_samp_pts %>%
  st_join(nrd, join = st_intersects)

##############################################################################################
#Create Box-Plots for SDon, SDoff and SDdur by NRD, save copies to working directory.
##############################################################################################

box_sdon <- ggplot(sd_by_nrd, aes(x=dname_shrt, y=sdon)) + geom_boxplot(outlier.size=.1) + theme_classic() +coord_flip() + labs(y="M*D10A1 Snow Onset Date (Days Since 1-Sep)", x = "")
box_sdon
ggsave(filename = 'box_sdon',plot = box_sdon,device = 'png',path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Figures/BoxPlots/',width=8.5,height = 8.5,units = c('in'))

box_sdoff <- ggplot(sd_by_nrd, aes(x=dname_shrt, y=sdoff)) + geom_boxplot(outlier.size=.1)+ theme_classic() + coord_flip() + labs(y="M*D10A1 Snow Meltoff Date (Days Since 1-Sep)", x = "")
box_sdoff
ggsave(filename = 'box_sdoff',plot = box_sdoff,device = 'png',path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Figures/BoxPlots/',width=8.5,height = 8.5,units = c('in'))

box_sddur <- ggplot(sd_by_nrd, aes(x=dname_shrt, y=sddur)) + geom_boxplot(outlier.size=.1) + theme_classic()+ coord_flip() + labs(y="M*D10A1 Snow Duration (Days)", x = "")
box_sddur
ggsave(filename = 'box_sddur',plot = box_sddur,device = 'png',path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Figures/BoxPlots/',width=8.5,height = 8.5,units = c('in'))

##############################################################################################
#Summarise M*D10A1 data by NRD
##############################################################################################

#Get mean and std. dev. of SD metrics by NRD
sd_sumry <- sd_by_nrd %>%
  group_by(dname_shrt) %>%
  summarise(mean_sdon = mean(sdon, na.rm = TRUE), sd_sdon = sd(sdon, na.rm = TRUE),mean_sdoff = mean(sdoff, na.rm = TRUE), sd_sdoff = sd(sdoff, na.rm = TRUE),mean_sddur = mean(sddur, na.rm = TRUE), sd_sddur = sd(sddur, na.rm = TRUE))

#Write summary table to working directory
write_csv(sd_sumry %>% as_tibble() %>% select(dname_shrt,mean_sdon,sd_sdon,mean_sdoff, sd_sdoff, mean_sddur, sd_sddur), path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Tables/sumry_tab.csv')

##############################################################################################
#Get boot straped CI's for summary stats by NRD
##############################################################################################

#Functions to pass to 'boot'
mu<-function(data,ind)
{
  return(mean(data[ind],na.rm=TRUE))
}
sdev<-function(data,ind)
{
  return(sd(data[ind],na.rm=TRUE))
}

#Bootstrap repititions
reps=500

#Map bootstrap function to compute mean for each SD metric by NRD
sd_cis_mu<-sd_by_nrd %>%
  group_by(dname_shrt) %>%
  summarise(sdon_lw = boot.ci(boot(data = sdon,statistic=mu,R=reps),type=c("basic"))$basic[4],
            sdon_up = boot.ci(boot(data = sdon,statistic=mu,R=reps),type=c("basic"))$basic[5],
            sdoff_lw = boot.ci(boot(data = sdoff,statistic=mu,R=reps),type=c("basic"))$basic[4],
            sdoff_up = boot.ci(boot(data = sdoff,statistic=mu,R=reps),type=c("basic"))$basic[5],
            sddur_lw = boot.ci(boot(data = sddur,statistic=mu,R=reps),type=c("basic"))$basic[4],
            sddur_up = boot.ci(boot(data = sddur,statistic=mu,R=reps),type=c("basic"))$basic[5])

#Write bootstrap CI's to working directory 
write_csv(sd_cis_mu %>% as_tibble() %>% select(dname_shrt,sdon_lw,sdon_up,sdoff_lw,sdoff_up,sddur_lw,sddur_up), path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Tables/boot_ci_mu.csv')

#Map bootstrap function to compute std. dev. for each SD metric by NRD
sd_cis_sdev<-sd_by_nrd %>%
  group_by(dname_shrt) %>%
  summarise(sdon_lw = boot.ci(boot(data = sdon,statistic=sdev,R=reps),type=c("basic"))$basic[4],
            sdon_up = boot.ci(boot(data = sdon,statistic=sdev,R=reps),type=c("basic"))$basic[5],
            sdoff_lw = boot.ci(boot(data = sdoff,statistic=sdev,R=reps),type=c("basic"))$basic[4],
            sdoff_up = boot.ci(boot(data = sdoff,statistic=sdev,R=reps),type=c("basic"))$basic[5],
            sddur_lw = boot.ci(boot(data = sddur,statistic=sdev,R=reps),type=c("basic"))$basic[4],
            sddur_up = boot.ci(boot(data = sddur,statistic=sdev,R=reps),type=c("basic"))$basic[5])

#Write bootstrap CI's to working directory
write_csv(sd_cis_sdev %>% as_tibble() %>% select(dname_shrt,sdon_lw,sdon_up,sdoff_lw,sdoff_up,sddur_lw,sddur_up), path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Tables/boot_ci_sdev.csv')

##############################################################################################
#Map summary statitics by NRD
##############################################################################################


#Join summary results to NRD sf layer
sd_joined<-nrd %>%
  st_join(sd_sumry)


#Plot summary results by NRD
sd_on_mean_plt <- ggplot() + geom_sf(data = sd_joined, aes( fill = mean_sdon))+labs(fill=expression(paste(SD["ON"]," Average", sep = " ")))
sd_on_sd_plt <- ggplot() + geom_sf(data = sd_joined, aes( fill = sd_sdon))+labs(fill=expression(paste(SD["ON"]," Std. Dev.", sep = " ")))
sd_off_mean_plt <- ggplot() + geom_sf(data = sd_joined, aes( fill = mean_sdoff))+labs(fill=expression(paste(SD["OFF"]," Average", sep = " ")))
sd_off_sd_plt <- ggplot() + geom_sf(data = sd_joined, aes( fill = sd_sdoff))+labs(fill=expression(paste(SD["OFF"]," Std. Dev.", sep = " ")))
sd_dur_mean_plt <- ggplot() + geom_sf(data = sd_joined, aes( fill = mean_sddur))+labs(fill=expression(paste(SD["DUR"]," Average", sep = " ")))
sd_dur_sd_plt <- ggplot() + geom_sf(data = sd_joined, aes( fill = sd_sddur))+labs(fill=expression(paste(SD["DUR"]," Std. Dev.", sep = " ")))

#Save plots to working directory
sd_on_mean_plt 
ggsave(filename = 'mean_sdon_map',plot = sd_on_mean_plt ,device = 'png',path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Figures/Maps/',width=8.5,height = 8.5,units = c('in'))

sd_on_sd_plt
ggsave(filename = 'sd_sdon_map',plot = sd_on_sd_plt ,device = 'png',path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Figures/Maps/',width=8.5,height = 8.5,units = c('in'))

sd_off_mean_plt
ggsave(filename = 'mean_sdoff_map',plot = sd_off_mean_plt ,device = 'png',path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Figures/Maps/',width=8.5,height = 8.5,units = c('in'))

sd_off_sd_plt
ggsave(filename = 'sd_sdoff_map',plot = sd_off_sd_plt ,device = 'png',path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Figures/Maps/',width=8.5,height = 8.5,units = c('in'))


sd_dur_mean_plt
ggsave(filename = 'mean_sddur_map',plot = sd_dur_mean_plt ,device = 'png',path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Figures/Maps/',width=8.5,height = 8.5,units = c('in'))

sd_dur_sd_plt
ggsave(filename = 'sd_sddur_map',plot = sd_dur_sd_plt ,device = 'png',path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Figures/Maps/',width=8.5,height = 8.5,units = c('in'))



##############################################################################################
#Annual Analysis of snow metrics 
##############################################################################################


#Generate boxplots of all snow metrics by hydrologic year, save to project dir
box_anual_sdon <- ggplot(sd_by_nrd, aes(x=year,group=year, y=sdon)) + geom_boxplot(outlier.size=.1) + theme_classic()+ labs(y="M*D10A1 Snow Onset Date (Days Since 1-Sep)", x = "")
box_anual_sdon
ggsave(filename = 'box_sdon_yrly',plot = box_anual_sdon,device = 'png',path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Figures/BoxPlots/',width=8.5,height = 8.5,units = c('in'))


box_anual_sdoff <- ggplot(sd_by_nrd, aes(x=year,group=year, y=sdoff)) + geom_boxplot(outlier.size=.1) + theme_classic()+ labs(y="M*D10A1 Snow Meltoff Date (Days Since 1-Sep)", x = "")
box_anual_sdoff
ggsave(filename = 'box_sdoff_yrly',plot = box_anual_sdoff,device = 'png',path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Figures/BoxPlots/',width=8.5,height = 8.5,units = c('in'))


box_anual_sddur <- ggplot(sd_by_nrd, aes(x=year,group=year, y=sddur)) + geom_boxplot(outlier.size=.1) + theme_classic()+ labs(y="M*D10A1 Snow Duration (Days)", x = "")
box_anual_sddur
ggsave(filename = 'box_sddur_yrly',plot = box_anual_sddur,device = 'png',path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Figures/BoxPlots/',width=8.5,height = 8.5,units = c('in'))




#Get mean and std. dev. of SD metrics by hydrologic year 
sd_anual_sumry <- sd_by_nrd %>%
  group_by(year) %>%
  summarise(mean_sdon = mean(sdon, na.rm = TRUE), sd_sdon = sd(sdon, na.rm = TRUE),mean_sdoff = mean(sdoff, na.rm = TRUE), sd_sdoff = sd(sdoff, na.rm = TRUE),mean_sddur = mean(sddur, na.rm = TRUE), sd_sddur = sd(sddur, na.rm = TRUE))

#Write summary table to working directory
write_csv(sd_anual_sumry %>% as_tibble() %>% select(year,mean_sdon,sd_sdon,mean_sdoff, sd_sdoff, mean_sddur, sd_sddur), path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Tables/sumry_tab_yrly.csv')



##############################################################################################
#Get boot straped CI's for summary stats by hydrologic year
##############################################################################################


##Map bootstrap function to compute mean for each SD metric by hydrologic year
sd_cis_mu_yrly<-sd_by_nrd %>%
  group_by(year) %>%
  summarise(sdon_lw = boot.ci(boot(data = sdon,statistic=mu,R=reps),type=c("basic"))$basic[4],
            sdon_up = boot.ci(boot(data = sdon,statistic=mu,R=reps),type=c("basic"))$basic[5],
            sdoff_lw = boot.ci(boot(data = sdoff,statistic=mu,R=reps),type=c("basic"))$basic[4],
            sdoff_up = boot.ci(boot(data = sdoff,statistic=mu,R=reps),type=c("basic"))$basic[5],
            sddur_lw = boot.ci(boot(data = sddur,statistic=mu,R=reps),type=c("basic"))$basic[4],
            sddur_up = boot.ci(boot(data = sddur,statistic=mu,R=reps),type=c("basic"))$basic[5])

#Write bootstrap CI's to working directory 
write_csv(sd_cis_mu_yrly %>% as_tibble() %>% select(year,sdon_lw,sdon_up,sdoff_lw,sdoff_up,sddur_lw,sddur_up), path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Tables/boot_ci_mu_yrly.csv')



#Map bootstrap function to compute std. dev. for each SD metric by hydrologic year
sd_cis_sdev_yrly<-sd_by_nrd %>%
  group_by(year) %>%
  summarise(sdon_lw = boot.ci(boot(data = sdon,statistic=sdev,R=reps),type=c("basic"))$basic[4],
            sdon_up = boot.ci(boot(data = sdon,statistic=sdev,R=reps),type=c("basic"))$basic[5],
            sdoff_lw = boot.ci(boot(data = sdoff,statistic=sdev,R=reps),type=c("basic"))$basic[4],
            sdoff_up = boot.ci(boot(data = sdoff,statistic=sdev,R=reps),type=c("basic"))$basic[5],
            sddur_lw = boot.ci(boot(data = sddur,statistic=sdev,R=reps),type=c("basic"))$basic[4],
            sddur_up = boot.ci(boot(data = sddur,statistic=sdev,R=reps),type=c("basic"))$basic[5])

#Write bootstrap CI's to working directory 
write_csv(sd_cis_sdev_yrly %>% as_tibble() %>% select(year,sdon_lw,sdon_up,sdoff_lw,sdoff_up,sddur_lw,sddur_up), path = '~/Dropbox/FLNRO/Projects/snow_summaries_note/Tables/boot_ci_sdev_yrly.csv')


