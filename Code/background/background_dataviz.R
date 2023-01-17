library(tidyverse)

# Read them in

dt1 <- read_csv("./background_data/data_2.csv")
dt2 <- read_csv("./background_data/data_3.csv")
dt3 <- read_csv("./background_data/data_4.csv")
dt4 <- read_csv("./background_data/data_5.csv")
dt5 <- read_csv("./background_data/data_6.csv")
dt6 <- read_csv("./background_data/data_8.csv")

# ghg_stacked

dt1 <- dt1 %>% rename("gas" = "Emissions by Gas, MMT CO2 eq.") %>% 
  filter(gas != "Total") %>% 
  gather(year, emissions, '1990':'2019') %>% 
  mutate(year = as.Date(year, "%Y"))

dt1$gas <- factor(dt1$gas , levels=c("Carbon dioxide", "Methane", "Nitrous oxide", "Fluorinated gases"))

pdf("ghg_stacked.pdf", width = 6, height = 3)
ggplot(dt1, aes(x=year, y=emissions, fill=gas)) + 
  geom_area() +
  scale_fill_brewer(palette="Set2") +
  xlab("\nYear") +
  ylab("Emissions \n(Millions of metric tons of CO2e)\n") +
  theme_bw() +
  theme(legend.title = element_blank())
dev.off()

# ghg_economic

dt2 <- dt2 %>% rename("sector" = "Emissions by Economic Sector, MMT CO2 eq.") %>% 
  filter(sector != "Total") %>% 
  gather(year, emissions, '1990':'2019') %>% 
  mutate(year = as.Date(year, "%Y"))

dt2$sector <- factor(dt2$sector , levels=c(
  "Transportation","Electricity generation","Industry","Agriculture",
  "Commercial","Residential","U.S. territories"
))

pdf("ghg_economic.pdf", width = 6, height = 3)
ggplot(dt2, aes(x=year, y=emissions, fill=sector)) + 
  geom_area() +
  scale_fill_brewer(palette="Set2") +
  xlab("\nYear") +
  ylab("Emissions \n(Millions of metric tons of CO2e)\n") +
  theme_bw() +
  theme(legend.title = element_blank())
dev.off()


# ghg_inventory

dt3 <- dt3 %>% rename("inventory" = "Emissions by Inventory Sector, MMT CO2 eq.") %>% 
  filter(inventory != "Gross total", inventory != "Net total") %>% 
  gather(year, emissions, '1990':'2019') %>% 
  mutate(year = as.Date(year, "%Y"))

dt3$inventory <- factor(dt3$inventory , levels=c(
  "Energy","Agriculture","Industrial processes","Waste",
  "Land use and forestry"
))

pdf("ghg_inventory.pdf", width = 6, height = 3)
ggplot(dt3, aes(x=year, y=emissions, fill=inventory)) + 
  geom_area() +
  scale_fill_brewer(palette="Set2") +
  xlab("\nYear") +
  ylab("Emissions \n(Millions of metric tons of CO2e)\n") +
  theme_bw() +
  theme(legend.title = element_blank())
dev.off()


# ghg_global

states <- c("China","European Union (27)","India","Indonesia",
            "Russia","United States")

temp <- dt5 %>% filter(entity %in% states) %>% 
  group_by(year) %>%
  summarise(main_ghg = sum(ghg))


total <- dt5 %>% filter(entity == "World")

ghg <- total$ghg - temp$main_ghg
year <- 1990:2016
entity <- rep("Other",27)

df_new <- tibble(entity, year, ghg)

df5 <- dt5 %>% filter(entity %in% states) %>% 
  select(-c(code))

final <- rbind(df5, df_new)

final <- final %>% mutate(year = as.character(year)) %>% 
  mutate(year = as.Date(year, "%Y"))

final$entity <- factor(final$entity, levels=c(
  "China","United States","India","European Union (27)","Russia","Indonesia","Other"
))

pdf("ghg_international.pdf", width = 6, height = 3)
ggplot(final, aes(x=year, y=ghg, fill=entity)) + 
  geom_area() +
  scale_fill_brewer(palette="Set2") +
  xlab("\nYear") +
  ylab("Emissions \n(Billions of metric tons of CO2e)\n") +
  theme_bw() +
  theme(legend.title = element_blank())
dev.off()




df6 <- read_csv("./background_data/data_7.csv") %>% 
  filter(year == "2016", entity %in% c(states, "Germany"))

t <- c("India","China","Indonesia","Germany","Russia","United States")

df6$entity <- factor(df6$entity, levels= t)

pdf("ghg_cap.pdf", width = 6, height = 3)
ggplot(df6, aes(x=ghg_cap, y=entity)) + 
  geom_bar(stat = "identity", fill = "#66C2A5") +
  xlab("\nAnnual Greenhouse Gas Emissions per Captia (tons CO2e/person)") +
  ylab("Country\n") +
  theme_bw()
dev.off()


dt9 <- read_csv("./background_data/data_9.csv") %>% 
  filter(Year == "2017") %>% 
  select(-c("Continent"))

dt9 <- na.omit(dt9)


pdf("EKC.pdf", width = 3, height = 3)
ggplot(dt9, aes(x=log(gdp_cap), y=mrate_pol)) + 
  geom_point(stat = "identity", color = "#00647d") +
  xlab("\nlog GDP per Capita") +
  ylab("Ambient Air Pollution Mortality Rate\n(Annual Deaths per 100,000)\n") +
  theme_bw()
dev.off()





















library("stargazer")

f <- function(x,y){
  return ((19*exp(-x) + 8)*(209) - (y/499)*(209))
}

euler <- function(f, x0, y0, h, n) {
  x <- x0
  y <- y0
  hy0 <- h * f(x0, y0)
  hy <- hy0
  
  for(i in 1:n) {
    y0 <- y0 + hy0
    x0 <- x0 + h
    hy0 <- h*f(x0, y0)
    x <- c(x, x0)
    y <- c(y, y0)
    hy <- c(hy, hy0)
  }
  
  return(data.frame(n=0:n, x = x, y = y, hy = hy))
}

t <- euler(f, 0, 5489, 1, 10)

t <- t %>% mutate(
  year = x + 2019, 
  C = y/484)

colnames(t)

t <- t[, c("n","x","year", "y", "hy", "C")]

stargazer(t, summary=F)


for (i in 0:n){
  paste("[")
}


for (i in 1:length(t$x)){
  cat('[', t$x[i], ",", t$y[i], '], ', sep="")
}







