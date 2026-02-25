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


# Seperate Number from Experiments
out$EXNr <- as.integer(substring(out$Experiment, 3, 4))

out$Experiment <- as.factor(out$Experiment)


# Select 2026 Experiments
out_m2 <- out %>%
  filter(EXNr >= 5  & EXNr <= 36)
           

# Reorder Experiments 
out_m2$Experiment <- with(out_m2, reorder(Experiment, EXNr, mean))

# Maximale N-Aufnahme nach Saattermin
tapply(out_m2$SumNUp, out_m2$Experiment, max)

outshort <- out_m2%>%
  filter(Date == "2025-11-30")

outshort%>%
  ggplot(aes(EXNr, SumNUp))+
  geom_point()


# Aboveground Biomass
AbBiom_plot <- out_m2 %>%
  ggplot(aes(DOY, AbBiom))+
  geom_line(aes(colour=Experiment), linewidth=0.8)+
  ggtitle("Aboveground Biomass DM")+
  ylab("Aboveground Biomass DM [kg/ha]")+
  xlab("Day of Year")+
  scale_y_continuous(breaks = seq(0, 3500, by = 200), limits=c(0,NA))+
  scale_x_continuous(breaks = seq(200, 350, by = 10))+
  theme_minimal()

ggsave(paste0( "plot/",heute, "_AbBiom_MONICA02.jpg"), AbBiom_plot, width = 20, height = 15, units = "cm", dpi=300, create.dir=TRUE)


# Total Nitrogen Uptake
SumNUp_plot <- out_m2 %>%
  ggplot(aes(DOY, SumNUp))+
  geom_line(aes(colour=Experiment), linewidth=0.8)+
  ggtitle("Total Nitrogen Uptake")+
  ylab("Total Nitrogen Uptake [kg/ha]")+
  xlab("Day of Year")+
  scale_y_continuous(breaks = seq(0, 180, by = 10), limits=c(0,NA))+
  scale_x_continuous(breaks = seq(200, 350, by = 10))+
  theme_minimal()


ggsave(paste0( "plot/",heute, "_SumNUp_MONICA02.jpg"), SumNUp_plot, width = 20, height = 15, units = "cm", dpi=300, create.dir=TRUE)
