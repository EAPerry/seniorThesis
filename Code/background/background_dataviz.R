#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Background Data Visualizations -----------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# Contributor(s) : Evan Perry
# Last Revised : 2023-01-17
# Version : 2
# 
# Purpose : 
#   The purpose of this script is to create the data visualizations needed for 
#   background chapters of the paper (chapters 1-3).
# 
# Inputs :
#   
# 
# 
# Outputs : 
# 
# 
# 
# Outline: (Crtl + Shift + O)
#
#
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Setup -----------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Importing packages
library(tidyverse)
library(NatParksPalettes)

# Setting working directory
setwd("C:/Users/eaper/Senior Thesis/seniorThesis")

# Remove scientific notation
options(scipen = 999)

# ggplot styling
theme_set(theme_bw())
options(ggplot2.continuous.colour= natparks.pals(name = "Acadia", type="continuous"))
options(ggplot2.continuous.fill = natparks.pals(name = "Acadia", type="continuous"))
options(ggplot2.discrete.colour= natparks.pals(name = "Charmonix", n=7))
options(ggplot2.discrete.fill = natparks.pals(name = "Charmonix", n=7))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Reading Data ----------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

dt1 <- read_csv("Data/background/ghg_stacked.csv")
dt2 <- read_csv("Data/background/ghg_economic.csv")
dt3 <- read_csv("Data/background/ghg_inventory.csv")
dt4 <- read_csv("Data/background/ghg_global.csv")
dt5 <- read_csv("Data/background/ghg_cap.csv")
dt6 <- read_csv("Data/background/ekc.csv")
# dt7 <- read_csv("Data/background/pop_data.csv")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Chapter 1 Figures -----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

### ghg_stacked ----------------------------------------------------------------

dt1 <- dt1 %>% rename("gas" = "Emissions by Gas, MMT CO2 eq.") %>% 
  filter(gas != "Total") %>% 
  gather(year, emissions, '1990':'2019') %>% 
  mutate(year = as.Date(year, "%Y"))

dt1$gas <- factor(
  dt1$gas , 
  levels=c("Carbon dioxide", "Methane", "Nitrous oxide", "Fluorinated gases"))

pdf("Writing/Draft/figures/chapter1_figures/ghg_stacked.pdf", 
    width = 6, height = 3)
ggplot(dt1, aes(x=year, y=emissions, fill=gas)) + 
  geom_area() +
  xlab("\nYear") +
  ylab("Emissions \n(Millions of metric tons of CO2e)\n") +
  theme(legend.title = element_blank())
dev.off()

### ghg_economic ---------------------------------------------------------------

dt2 <- dt2 %>% 
  rename("sector" = "Emissions by Economic Sector, MMT CO2 eq.") %>% 
  filter(sector != "Total") %>% 
  gather(year, emissions, '1990':'2019') %>% 
  mutate(year = as.Date(year, "%Y"))

dt2$sector <- factor(dt2$sector , levels=c(
  "Transportation","Electricity generation","Industry","Agriculture",
  "Commercial","Residential","U.S. territories"
))

pdf("Writing/Draft/figures/chapter1_figures/ghg_economic.pdf", 
    width = 6, height = 3)
ggplot(dt2, aes(x=year, y=emissions, fill=sector)) + 
  geom_area() +
  xlab("\nYear") +
  ylab("Emissions \n(Millions of metric tons of CO2e)\n") +
  theme(legend.title = element_blank())
dev.off()


### ghg_inventory --------------------------------------------------------------

dt3 <- dt3 %>% 
  rename("inventory" = "Emissions by Inventory Sector, MMT CO2 eq.") %>% 
  filter(inventory != "Gross total", inventory != "Net total") %>% 
  gather(year, emissions, '1990':'2019') %>% 
  mutate(year = as.Date(year, "%Y"))

dt3$inventory <- factor(dt3$inventory , levels=c(
  "Energy","Agriculture","Industrial processes","Waste",
  "Land use and forestry"
))

pdf("Writing/Draft/figures/chapter1_figures/ghg_inventory.pdf", 
    width = 6, height = 3)
ggplot(dt3, aes(x=year, y=emissions, fill=inventory)) + 
  geom_area() +
  xlab("\nYear") +
  ylab("Emissions \n(Millions of metric tons of CO2e)\n") +
  theme(legend.title = element_blank())
dev.off()


### ghg_global -----------------------------------------------------------------

states <- c("China","European Union (27)","India","Indonesia",
            "Russia","United States")

dt4 <- dt4 %>% 
  filter(entity %in% c(states, "World"))

temp1 <- dt4 %>% 
  filter(entity %in% states) %>% 
  group_by(year) %>% 
  summarise(ghg_states = sum(ghg))

temp2 <- dt4 %>% 
  filter(entity == "World") %>%
  mutate(entity = "Other") %>% 
  select("entity", "year", "ghg")

temp1 <- merge(temp1, temp2, by="year")
temp1$ghg <- temp1$ghg - temp1$ghg_states
temp1 <- temp1 %>% select("entity","year","ghg")

dt4 <- dt4 %>% 
  filter(entity %in% states) %>% 
  select("entity", "year", "ghg")
dt4 <- rbind(dt4, temp1)

dt4$entity <- factor(dt4$entity, levels=c(
  "China","United States","India","European Union (27)","Russia","Indonesia","Other"
))


pdf("Writing/Draft/figures/chapter1_figures/ghg_global.pdf", 
    width = 6, height = 3)
ggplot(dt4, aes(x=year, y=ghg/1000000000, fill=entity)) + 
  geom_area() +
  xlab("\nYear") +
  ylab("Emissions \n(Billions of metric tons of CO2e)\n") +
  theme(legend.title = element_blank())
dev.off()

rm(temp1, temp2, states)

### ghg_cap --------------------------------------------------------------------

states <- c("India","China","Indonesia","Germany","Russia","United States")

dt5 <- dt5 %>% 
  filter(year == "2016", entity %in% states)

dt5$entity <- factor(dt5$entity, levels= states)

pdf("Writing/Draft/figures/chapter1_figures/ghg_cap.pdf", 
    width = 6, height = 3)
ggplot(dt5, aes(x=ghg_cap, y=entity)) + 
  geom_bar(stat = "identity", fill=natparks.pals("Charmonix", 1)) +
  xlab("\nAnnual Greenhouse Gas Emissions per Captia (tons CO2e/person)") +
  ylab("Country\n") +
  theme_bw()
dev.off()

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Chapter 2 Figures -----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

### ekc ------------------------------------------------------------------------

dt6 <- dt6 %>% 
  filter(Year == "2017") %>% 
  select(-c("Continent")) %>% 
  na.omit()

pdf("Writing/Draft/figures/chapter2_figures/ekc.pdf", width = 3, height = 3)
ggplot(dt6, aes(x=gdp_cap, y=mrate_pol)) + 
  geom_point(stat = "identity", color=natparks.pals("Charmonix", 1)) +
  scale_x_continuous(trans='log10') + 
  # scale_y_continuous(trans='log10') +
  xlab("\nlog GDP per Capita") +
  ylab("Ambient Air Pollution Mortality Rate\n(Annual Deaths per 100,000)\n") +
  theme_bw()
dev.off()

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


