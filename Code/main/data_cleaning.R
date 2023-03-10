#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Data Cleaning: Senior Thesis -------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# Contributor(s) : Evan Perry
# Last Revised : 2023-03-03
# Version : 1
# 
# Purpose : 
# 
# 
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
library(lubridate)
library(readxl)
library(NatParksPalettes)
library(tmap)
library(sf) 
library(tigris)


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
# Generator Data ---------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Generators
egrid_gen <- read_excel("Data/main/eGRID/egrid2019_data.xlsx", 
                    sheet="GEN19",
                    skip = 1)

# Plants
egrid_plant <- read_excel("Data/main/eGRID/egrid2019_data.xlsx", 
                          sheet="PLNT19",
                          skip = 1) 

egrid_plant <- egrid_plant %>% 
  select(
     ORISPL, NERC, SUBRGN, FIPSST, FIPSCNTY, LAT, LON, PLHTRT, 
     PLNOXAN, PLSO2AN, PLHTIANT
  )

# PM2.5
pm25 <- read_excel("Data/main/eGRID/eGRID2020 DRAFT PM Emissions.xlsx", 
                   sheet = "2019 PM Plant-level Data", 
                   skip = 1)

pm25 <- pm25 %>% 
  select(
    ORISPL, PLPM25RA
  )

egrid_plant <- left_join(egrid_plant, pm25, by="ORISPL")


# Emissions coefficients
emis_coefs <- read_csv("Data/main/eGRID/eGRID2019 Table C-1.csv") %>% 
  select(-c("Fuel Type", "Source"))
colnames(emis_coefs) <- c("FUELG1", "co2_mmbtu", "ch4_mmbtu", "n2o_mmbtu")
# Clean these up so that they are in the proper units:
#   > tons CO2/MMBTU --> tonnes CO2/MMBTU   (GWP = 1)
#   > lbs CH4/MMBTU --> tonnes CH4/MMBTU    (GWP = 27.9)
#   > lbs N2O MMBTU --> tonnes N2O/MMBTU    (GWP = 273)
# GWP from the table in Section 1.2
emis_coefs <- emis_coefs %>% 
  mutate(
    co2_mmbtu = 0.907185*co2_mmbtu,
    ch4_mmbtu = 0.0004535925*ch4_mmbtu,
    n2o_mmbtu = 0.0004535925*n2o_mmbtu,
    co2e_mmbtu = co2_mmbtu + 27.9*ch4_mmbtu + 273*n2o_mmbtu
  )
emis_coefs <- emis_coefs %>% select(FUELG1, co2e_mmbtu)


# Only use those generators that fit each of the following criteria:
#   1. Operating in 2019
#   2. Located in the Western Interconnection
#   3. Primary fuel is natural gas, coal, or oil

coal <- c("BIT", "COG", "LIG", "RC", "SGC", "SUB", "WC")
oil <- c("DFO", "JF", "KER", "PC", "RFO", "WO")
gas <- c("BU", "NG", "PG")
fossil_fuels <- c(coal, oil, gas)

gens <- egrid_gen %>% 
  filter(
    GENSTAT == "OP",
    FUELG1 %in% fossil_fuels
  )

gens <- left_join(gens, egrid_plant, by="ORISPL")
gens <- left_join(gens, emis_coefs, by="FUELG1")

gens <- gens %>% 
  filter(NERC == "WECC") %>% 
  mutate(
    FUEL = case_when(
      FUELG1 %in% coal ~ "Coal",
      FUELG1 %in% oil ~ "Oil",
      T ~ "Gas"
    ),
    MKT_REGION = case_when(
      SUBRGN == "CAMX" ~ "California",
      SUBRGN %in% c("AZNM", "SPSO") ~ "Southwest",
      SUBRGN %in% c("NWPP", "RMPA", "MROW") ~ "Northwest",
      T ~ "Bad news..."
    ),
    PLHTRT = PLHTRT * 1000/1000000, # BTU/kWh --> MMBTU/MWh
    age = 2019 - GENYRONL,
    nox_mmbtu = 0.907185*PLNOXAN/PLHTIANT,
    nox_mmbtu = ifelse(is.na(nox_mmbtu), 0, nox_mmbtu),
    so2_mmbtu = 0.907185*PLSO2AN/PLHTIANT,
    so2_mmbtu = ifelse(is.na(so2_mmbtu), 0, so2_mmbtu),
    pm25_mmbtu = PLPM25RA*0.907185/2000,
    pm25_mmbtu = ifelse(is.na(pm25_mmbtu), 0, pm25_mmbtu)
  )


# Remove the one pair of generators that are in Missouri but allegedly still 
# connected to the Western Interconnection???
gens <- gens %>% filter(MKT_REGION != "Bad news...")

# We also need to drop a few generators that have negative heat rates.
# Of course they don't actually have a negative heat rate, but they do have
# negative net generation, meaning they're so incredibly underused that they 
# consume more power than they actually generate in a year.

# gens %>% 
#   filter(PLHTRT < 0) %>% 
#   group_by(FUEL) %>% 
#   summarise(
#     count = n(),
#     CAP = sum(NAMEPCAP)
#   )
# Dropped Nameplate Capacity by fuel group: 
#   GAS: -2 Generators, -49.8 MW in CA
#   OIL: -13 Generators, -28 MW in CO
gens <- gens %>% filter(PLHTRT > 0)

# Drop gas generators with unusually high heat rates
# This shouldn't really affect the analysis---their capacity factors are 0
gens <- gens %>% filter(!(FUEL == "Gas" & PLHTRT*co2e_mmbtu > 2))

# Reorganize and clean up the dataframe
gens <- gens %>% 
  select(
    # Generator IDs
    ORISPL, 
    GENID,
    # Geographic IDs
    MKT_REGION,
    PSTATABB,
    FIPSST, 
    FIPSCNTY, 
    SUBRGN, 
    LAT, 
    LON, 
    # Age
    GENYRONL,
    age,
    # Fuel
    FUEL, 
    FUELG1, 
    # Capacity Factor
    CFACT, 
    # Nameplate Capacity
    NAMEPCAP, 
    # Heat Rate
    PLHTRT,
    # Emissions Coefficient
    co2e_mmbtu,
    # Air Pollutant Coefficients
    nox_mmbtu, 
    so2_mmbtu,
    pm25_mmbtu
  )

colnames(gens) <- c(
  "plantID", "genID", "region", "state", "FIPS2", "FIPS3", "subregion",
  "lat", "lon", "year_built", "age", "fuel_cat", "fuel", "cap_fac", "capacity", 
  "hrate", "co2e_mmbtu", "nox_mmbtu", "so2_mmbtu", "pm25_mmbtu"
)

# Tables & Figures to Make Here:
#   1. Summary stats of the generators by fuel category
#   2. Map of the regions with generator locations?
#   3. Distribution of heat rates and emissions factors by fuel type
#   4. Distribution of generators by region
#   5. Distribution of capacity by region

pdf("Writing/Draft/figures/chapter5_figures/EI_region_violin.pdf",
    width = 6, height = 3.5)
ggplot(gens) + 
  geom_violin(
    aes(y=hrate*co2e_mmbtu, x=region, fill=fuel_cat), 
    trim = T) + 
  geom_text(data = gens %>%  count(region, fuel_cat), 
            mapping = aes(
              y = -0.45, 
              x = region, 
              group = fuel_cat, 
              label=paste("(", n, ")", sep="")
            ), 
            color="black", size = 3, position = position_dodge(1)) +
  ylab('Tonnes CO2e/MWh\n') +
  xlab("\nRegion") +
  ylim(-0.75, 4) + 
  theme(legend.title = element_blank())
dev.off()

temp <- gens %>% 
  select(plantID, genID, fuel_cat, hrate, nox_mmbtu, so2_mmbtu, pm25_mmbtu)
temp <- temp %>% 
  pivot_longer(cols = c(nox_mmbtu, so2_mmbtu, pm25_mmbtu), 
               names_to = "pollutant",
               values_to = "emissions") %>% 
  mutate(
    pollutant = case_when(
      pollutant == "nox_mmbtu" ~ "NOx",
      pollutant == "so2_mmbtu" ~ "SO2",
      pollutant == "pm25_mmbtu" ~ "PM2.5",
      T ~ "AAA"
    )
  )

p1 <- ggplot(temp %>% filter(pollutant=="NOx"), 
             aes(y=emissions*hrate*1000, x=fuel_cat, fill=fuel_cat)) + 
  geom_boxplot() + 
  scale_y_sqrt(breaks=c(1,10,20,30,40)) + 
  ylab('kg NOx/MWh') +
  xlab("") +
  theme(legend.title = element_blank(), legend.position = "none")

p2 <- ggplot(temp %>% filter(pollutant=="SO2"), 
       aes(y=emissions*hrate*1000, x=fuel_cat, fill=fuel_cat)) + 
  geom_boxplot() + 
  scale_y_sqrt(breaks = c(1,3,6,9,12)) + 
  ylab('kg SO2/MWh') +
  xlab("") +
  theme(legend.title = element_blank(), legend.position = "none")

p3 <- ggplot(temp %>% filter(pollutant=="PM2.5"), 
       aes(y=emissions*hrate*1000, x=fuel_cat, fill=fuel_cat)) + 
  geom_boxplot() + 
  scale_y_sqrt() + 
  ylab('kg PM2.5/MWh') +
  xlab("") +
  theme(legend.title = element_blank(), legend.position = "none")

pdf("Writing/Draft/figures/chapter5_figures/local_poll_EI.pdf",
    width = 6, height = 3.5)
ggpubr::ggarrange(p1, p2, p3, ncol = 3, nrow = 1, labels = c("A","B","C"))
dev.off()


temp <- left_join(egrid_gen, egrid_plant, by="ORISPL") %>% 
  filter(NERC == "WECC", GENSTAT == "OP") %>% 
  mutate(
    fuel_cat = case_when(
      FUELG1 %in% coal ~ "Coal",
      FUELG1 %in% oil ~ "Oil",
      FUELG1 %in% gas ~ "Gas",
      T ~ "Other"
    ),
    MKT_REGION = case_when(
      SUBRGN == "CAMX" ~ "California",
      SUBRGN %in% c("AZNM", "SPSO") ~ "Southwest",
      SUBRGN %in% c("NWPP", "RMPA", "MROW") ~ "Northwest",
      T ~ "Bad news..."
    )
  ) %>% 
  filter(MKT_REGION != "Bad news...")


p1 <- ggplot(temp %>% 
         group_by(MKT_REGION, fuel_cat) %>% 
         summarise(NAMEPCAP = sum(NAMEPCAP))
       ) + 
  geom_bar(aes(y=NAMEPCAP, x=MKT_REGION, fill=fuel_cat),
           position="stack", stat="identity") +
  xlab("\nRegion") +
  ylab("Operating Capacity (MW)\n") + 
  theme(legend.title = element_blank())

p2 <- ggplot(temp, aes(fill=fuel_cat, x=MKT_REGION)) + 
  geom_bar(position="stack", stat="count") +
  xlab("\nRegion") +
  ylab("# of Generators\n") + 
  theme(legend.title = element_blank())
  
pdf("Writing/Draft/figures/chapter5_figures/regional_gens.pdf",
    width = 7, height = 4)
print({
  ggpubr::ggarrange(p2, p1, ncol = 2, nrow = 1, labels = c("A","B"),
                    common.legend = TRUE, legend = "right")
})
dev.off()


rm(egrid_gen, egrid_plant, emis_coefs, p1, p2, p3, pm25, temp, coal, 
   fossil_fuels, gas, oil)

# WECC Map

wecc_shp <- st_read("Data/main/NERC_Regions/NERC_Regions_Subregions.dbf") %>% 
  filter(SUBNAME %in% c("AZ-NM-SNV", "CA-MX US", "RMPA", "NWPP"))
wecc_shp <- st_transform(wecc_shp, st_crs(5070))
wecc_shp <- wecc_shp %>% 
  mutate(
    Region = case_when(
      SUBNAME %in% c("RMPA", "NWPP") ~ "Northwest",
      SUBNAME == "AZ-NM-SNV" ~ "Southwest",
      SUBNAME == "CA-MX US" ~ "California",
      T ~ "Other"
    ),
    SUBNAME = case_when(
      SUBNAME == "CA-MX US" ~ "CAMX",
      SUBNAME == "AZ-NM-SNV" ~ "AZNM",
      T ~ SUBNAME
    )
  )

wecc_states <- st_transform(states(), st_crs(5070))
# wecc_states <- st_intersection(wecc_states, wecc_shp)
wecc_states <- st_filter(wecc_states, wecc_shp)

# Overlap
# Intersect
# Crosses

pdf("Writing/Draft/figures/chapter3_figures/WECC_map.pdf",
    width = 5, height = 4)
tm_shape(wecc_states, unit="mi") +
  tm_borders(col = "black", lwd = 1) + 
  tm_shape(wecc_shp) +
  tm_polygons("Region", alpha = 0.7,
              palette = natparks.pals(name = "Charmonix", n=3)
              ) +
  tm_text("SUBNAME") +
  tm_shape(wecc_shp) +
  tm_borders(col = "black", lwd = 3) + 
  tm_compass(type = "arrow", position = c(0.9, 0.8)) +
  tm_scale_bar() + 
  tm_layout(frame = F, attr.position = c(0.35, 0.01))
dev.off()






# Summary stats table






#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# Allowance Prices in California

permit_pr <- read_csv("Data/main/reference/nc-allowance_prices.csv")
permit_pr <- permit_pr %>% 
  rowwise %>% 
  mutate(
    month = as.numeric(gsub(
      "[^0-9.-]", "", unlist(str_split(`Auction Quarter`, pattern = " "))[1]
      )) * 3 -2,
    year = as.numeric(
      unlist(str_split(`Auction Quarter`, pattern = " "))[2]
    ),
    Date = make_date(year = year, month = month, day = 1)
  )

permit_pr <- permit_pr %>% 
  pivot_longer(cols = c("Current Auction Settlement Price", "Auction Reserve Price"), 
               values_to = "Price", names_to = "price_type")

pdf("Writing/Draft/figures/chapter3_figures/allowance_prices.pdf",
    width = 6, height = 3)
ggplot(permit_pr, aes(x = Date, y = Price, color = price_type)) + 
  geom_line(lwd = 2) + 
  labs(x = "\nDate", y = "Price\n") + 
  scale_y_continuous(labels = scales::label_dollar()) + 
  scale_color_discrete(labels = c("Price Floor", "Auction Price")) + 
  theme(legend.title = element_blank())
dev.off()

states_w_pr <- states()
states_w_pr <- states_w_pr %>% 
  filter(GEOID < 60, !(STUSPS %in% c("AK", "HI"))) %>% 
  mutate(
    price = case_when(
      NAME %in% c("Connecticut", "Delaware", "Maine", "Maryland", "Massachusetts", 
                  "New Hampshire", "New Jersey", "New York", "Rhode Island", 
                  "Vermont", "Virginia") ~ "Electricity Only",
      NAME %in% c("California", "Washington") ~ "Economy Wide",
      T ~ "No Carbon Pricing"
    )
  )
  
pdf("Writing/Draft/figures/chapter3_figures/cap_programs.pdf",
    width = 6, height = 3.5)
tm_shape(st_transform(states_w_pr, st_crs(5070)), unit="mi") +
  tm_polygons("price", palette = natparks.pals(name = "Charmonix", n=3), 
              border.col = "white", title = "", legend.is.portrait = F
              ) +
  tm_compass(type = "arrow", position = c(0.02, 0.2), size = 1) +
  tm_scale_bar() + 
  tm_layout(frame = F, attr.position = c(0.05, 0.05), legend.outside = T, 
            legend.outside.position = "bottom") 
dev.off()




