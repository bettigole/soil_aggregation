# setup script

library(tidyverse)
library(cowplot)
options(dplyr.summarise.inform = FALSE)
#see readme for description of input data/models
caney<-read.csv('data/cff_carbon_0_30_mgmt.csv')[,c(5,6)]
l7<-read.csv('data/l7_carbon_0_30_mgmt.csv')[,c(6,10)] %>% filter(mgmt!=0)
sb<-read.csv('data/sb_0_30_carbon_mgmt.csv')[,c(6,8)]
