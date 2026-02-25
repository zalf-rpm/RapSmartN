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
out_m3 <- out %>%
  filter(EXNr >= 36)

# Add the soil information
out_m3 <- out_m3 %>%
  mutate(soil = cut(EXNr,
                    breaks = c(-Inf, 37, 42, 47, 52, Inf),
                    labels = c("soil5", "soil1", "soil2", "soil3", "soil4"),
                    right = FALSE,
                    ordered_result = TRUE))

out_m3.1 <- out_m3 %>%
  filter(Experiment=="EX57" | Experiment=="EX52" | Experiment=="EX47"| Experiment== "EX42"|Experiment=="EX37")

out_m3.2 <- out_m3 %>%
  filter(Experiment=="EX58" | Experiment=="EX53" | Experiment=="EX48"| Experiment== "EX43"|Experiment=="EX38")

out_m3.3 <- out_m3 %>%
  filter(Experiment=="EX59" | Experiment=="EX54" | Experiment=="EX49"| Experiment== "EX44"|Experiment=="EX39")

out_m3.4 <- out_m3 %>%
  filter(Experiment=="EX60" | Experiment=="EX55" | Experiment=="EX50"| Experiment== "EX45"|Experiment=="EX40")

out_m3.5 <- out_m3 %>%
  filter(Experiment=="EX61" | Experiment=="EX56" | Experiment=="EX51"| Experiment== "EX46"|Experiment=="EX41")

# Aboveground Biomass
AbBiom_plot <- out_m3 %>%
  ggplot(aes(DOY, AbBiom))+
  geom_line(aes(colour=Experiment), linewidth=0.8)+
  ggtitle("Aboveground Biomass DM")+
  ylab("Aboveground Biomass DM [kg/ha]")+
  xlab("Day of Year")+
  scale_y_continuous(breaks = seq(0, 3500, by = 200), limits=c(0,NA))+
  scale_x_continuous(breaks = seq(200, 350, by = 10))+
  theme_minimal()

# Aboveground Biomass
AbBiom_plot_m3.1 <- out_m3.1 %>%
  ggplot(aes(DOY, AbBiom))+
  geom_line(aes(colour=Experiment), linewidth=0.8)+
  ggtitle("Aboveground Biomass DM Day of Sowing: 16.08.2025")+
  ylab("Aboveground Biomass DM [kg/ha]")+
  xlab("Day of Year")+
  scale_y_continuous(breaks = seq(0, 3500, by = 200), limits=c(0,NA))+
  scale_x_continuous(breaks = seq(200, 350, by = 10))+
  theme_minimal()

# Aboveground Biomass
AbBiom_plot_m3.2 <- out_m3.2 %>%
  ggplot(aes(DOY, AbBiom))+
  geom_line(aes(colour=Experiment), linewidth=0.8)+
  ggtitle("Aboveground Biomass DM Day of Sowing: 21.08.2025")+
  ylab("Aboveground Biomass DM [kg/ha]")+
  xlab("Day of Year")+
  scale_y_continuous(breaks = seq(0, 3500, by = 200), limits=c(0,NA))+
  scale_x_continuous(breaks = seq(200, 350, by = 10))+
  theme_minimal()

# Aboveground Biomass
AbBiom_plot_m3.3 <- out_m3.3 %>%
  ggplot(aes(DOY, AbBiom))+
  geom_line(aes(colour=Experiment), linewidth=0.8)+
  ggtitle("Aboveground Biomass DM Day of Sowing: 26.08.2025")+
  ylab("Aboveground Biomass DM [kg/ha]")+
  xlab("Day of Year")+
  scale_y_continuous(breaks = seq(0, 3500, by = 200), limits=c(0,NA))+
  scale_x_continuous(breaks = seq(200, 350, by = 10))+
  theme_minimal()

# Aboveground Biomass
AbBiom_plot_m3.4 <- out_m3.4 %>%
  ggplot(aes(DOY, AbBiom))+
  geom_line(aes(colour=Experiment), linewidth=0.8)+
  ggtitle("Aboveground Biomass DM Day of Sowing: 31.08.2025")+
  ylab("Aboveground Biomass DM [kg/ha]")+
  xlab("Day of Year")+
  scale_y_continuous(breaks = seq(0, 3500, by = 200), limits=c(0,NA))+
  scale_x_continuous(breaks = seq(200, 350, by = 10))+
  theme_minimal()

# Aboveground Biomass
AbBiom_plot_m3.5 <- out_m3.5 %>%
  ggplot(aes(DOY, AbBiom))+
  geom_line(aes(colour=Experiment), linewidth=0.8)+
  ggtitle("Aboveground Biomass DM Day of Sowing: 05.09.2025")+
  ylab("Aboveground Biomass DM [kg/ha]")+
  xlab("Day of Year")+
  scale_y_continuous(breaks = seq(0, 3500, by = 200), limits=c(0,NA))+
  scale_x_continuous(breaks = seq(200, 350, by = 10))+
  theme_minimal()





ggsave(paste0( "plot/",heute, "_AbBiom_MONICA02.jpg"), AbBiom_plot, width = 20, height = 15, units = "cm", dpi=300, create.dir=TRUE)


# Total Nitrogen Uptake
SumNUp_plot <- out_m3 %>%
  ggplot(aes(DOY, SumNUp))+
  geom_line(aes(colour=Experiment), linewidth=0.8)+
  ggtitle("Total Nitrogen Uptake")+
  ylab("Total Nitrogen Uptake [kg/ha]")+
  xlab("Day of Year")+
  scale_y_continuous(breaks = seq(0, 180, by = 10), limits=c(0,NA))+
  scale_x_continuous(breaks = seq(200, 350, by = 10))+
  theme_minimal()


ggsave(paste0( "plot/",heute, "_SumNUp_MONICA02.jpg"), SumNUp_plot, width = 20, height = 15, units = "cm", dpi=300, create.dir=TRUE)