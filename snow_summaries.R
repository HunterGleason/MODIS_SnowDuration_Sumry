#Import dependencies
library(tidyverse)
library(bcmaps)
library(sf)
library(boot)
library(parallel)
library(snow)

# NR Districts
nrd <- nr_districts()

# Bevington et al., 2018 M*D10A1 Snow Duration Samples 
sd_samp_pts<-st_read('~/Dropbox/FLNRO/Projects/snow-dur-analysis/R/snow_summaries_note/SD_Random_Samples.gpkg')

#Join the SD sample to the NR Districts via spatial intersection 
sd_by_nrd <- sd_samp_pts %>%
  st_join(nrd, join = st_intersects) %>%
  mutate(dname_shrt = str_sub(DISTRICT_NAME,1,-27))





#Create Box-Plots for SDon, SDoff and SDdur by NR District
box_sdon <- ggplot(sd_by_nrd, aes(x=dname_shrt, y=sdon)) + geom_boxplot(outlier.size=.1) + theme_classic() +coord_flip() + labs(y="M*D10A1 Snow On Date (Days Since 1-Sep)", x = "")
box_sdon

box_sdoff <- ggplot(sd_by_nrd, aes(x=dname_shrt, y=sdoff)) + geom_boxplot(outlier.size=.1) + coord_flip() + labs(y="M*D10A1 Snow Off Date (Days Since 1-Sep)", x = "")
box_sdoff

box_sddur <- ggplot(sd_by_nrd, aes(x=dname_shrt, y=sddur)) + geom_boxplot(outlier.size=.1) + coord_flip() + labs(y="M*D10A1 Snow Duration (Days)", x = "")
box_sddur





#Summarise M*D10A1 data by NR District
sd_sumry <- sd_by_nrd %>%
   group_by(DISTRICT_NAME) %>%
   summarise(mean_sdon = mean(sdon, na.rm = TRUE), sd_sdon = sd(sdon, na.rm = TRUE),mean_sdoff = mean(sdoff, na.rm = TRUE), sd_sdoff = sd(sdoff, na.rm = TRUE),mean_sddur = mean(sddur, na.rm = TRUE), sd_sddur = sd(sddur, na.rm = TRUE))



# Get boot straped CI's for dsitrict means
f<-function(data,ind)
{
  return(mean(data[ind],na.rm=TRUE))
}

sd_cis<-sd_by_nrd %>%
  group_by(DISTRICT_NAME) %>%
  summarise(sdon_se_lw = boot.ci(boot(data = sdon,statistic=f,R=100),type=c("basic"))$basic[4],
            sdon_se_up = boot.ci(boot(data = sdon,statistic=f,R=100),type=c("basic"))$basic[5],
            sdoff_se_lw = boot.ci(boot(data = sdoff,statistic=f,R=100),type=c("basic"))$basic[4],
            sdoff_se_up = boot.ci(boot(data = sdoff,statistic=f,R=100),type=c("basic"))$basic[5],
            sddur_se_lw = boot.ci(boot(data = sddur,statistic=f,R=100),type=c("basic"))$basic[4],
            sdodur_se_up = boot.ci(boot(data = sddur,statistic=f,R=100),type=c("basic"))$basic[5])


#Join summary results to NR District sf layer
sd_joined<-nrd %>% 
  st_join(sd_sumry)

hm<-as.data.frame((sd_joined[,c(1:19)]))
hm$geometry<-NULL


write_csv(hm,'/home/huntergleason/Downloads/poo.csv')



#Plot summary results by NR District
sd_on_mean_plt <- ggplot() + geom_sf(data = sd_joined, aes( fill = mean_sdon))+labs(fill=expression(paste(SD["ON"]," Average", sep = " ")))
sd_on_sd_plt <- ggplot() + geom_sf(data = sd_joined, aes( fill = sd_sdon))+labs(fill=expression(paste(SD["ON"]," Std. Dev.", sep = " ")))
sd_off_mean_plt <- ggplot() + geom_sf(data = sd_joined, aes( fill = mean_sdoff))+labs(fill=expression(paste(SD["OFF"]," Average", sep = " ")))
sd_off_sd_plt <- ggplot() + geom_sf(data = sd_joined, aes( fill = sd_sdoff))+labs(fill=expression(paste(SD["OFF"]," Std. Dev.", sep = " ")))
sd_dur_mean_plt <- ggplot() + geom_sf(data = sd_joined, aes( fill = mean_sddur))+labs(fill=expression(paste(SD["DUR"]," Average", sep = " ")))
sd_dur_sd_plt <- ggplot() + geom_sf(data = sd_joined, aes( fill = sd_sddur))+labs(fill=expression(paste(SD["DUR"]," Std. Dev.", sep = " ")))


sd_on_mean_plt 
sd_on_sd_plt 
sd_off_mean_plt 
sd_off_sd_plt 
sd_dur_mean_plt 
sd_dur_sd_plt 



  



