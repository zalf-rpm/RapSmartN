# clean environment  
rm(list=ls())

# packages
library(tidyverse)
library(ggplot2)
library(ggpubr)

# parameters
heute <- as.character(today())

# load MONICA output 
out <- read.table("./out/out.csv"
                  , sep=","
                  , dec="."
                  , header=TRUE)

str(out)
out$Date <- as.Date(out$Date, Format="%Y-%m-%d")
out$Year <- as.factor(out$Year)


out <- out %>%
  filter(Experiment == "EX1" |Experiment == "EX2" |Experiment == "EX3" |Experiment == "EX4"  )

# Aboveground Biomass
AbBiom_plot <- out %>%
  ggplot(aes(DOY, AbBiom))+
  geom_line(aes(colour=Year), linewidth=0.8)+
  ggtitle("Aboveground Biomass DM")+
  ylab("Aboveground Biomass DM [kg/ha]")+
  xlab("Day of Year")+
  scale_y_continuous(breaks = seq(0, 3500, by = 200), limits=c(0,NA))+
  scale_x_continuous(breaks = seq(200, 350, by = 10))+
  theme_minimal()+
  theme(legend.position="bottom")

ggsave(paste0( "plot/",heute, "_AbBiom.jpg"), AbBiom_plot, width = 20, height = 15, units = "cm", dpi=300, create.dir=TRUE)

  
# Total Nitrogen Uptake
SumNUp_plot <- out %>%
  ggplot(aes(DOY, SumNUp))+
  geom_line(aes(colour=Year), linewidth=0.8)+
  ggtitle("Total Nitrogen Uptake")+
  ylab("Total Nitrogen Uptake [kg/ha]")+
  xlab("Day of Year")+
  scale_y_continuous(breaks = seq(0, 180, by = 10), limits=c(0,NA))+
  scale_x_continuous(breaks = seq(200, 350, by = 10))+
  theme_minimal()+
  theme(legend.position="bottom")


ggsave(paste0( "plot/",heute, "_SumNUp.jpg"), SumNUp_plot, width = 20, height = 15, units = "cm", dpi=300, create.dir=TRUE)

# NOTES:
# Day of year wäre sinnvoll für die Darstellung

# ? ist die oberirdische Biomasse wirklich als Trockenmasse angegeben???