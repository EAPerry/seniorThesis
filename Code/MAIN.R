#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Senior Thesis Script ---------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# Contributor(s) : Evan Perry
# Last Revised : 2023-04-22
# Version : 2
# 
# Purpose : 
# 
# This file reads, cleans, and analyzes all data and simulation work for Evan
# Perry's senior thesis, titled ``The Implications of Carbon Pricing For 
# Environmental Inequality."
# 
# NOTE: This script will run the power grid simulations, but does so by calling
# the Python program ``simulation.py". Because this program takes some time to
# run (about 3 hours), it is commented out in this script. Run the program by
# just uncommenting that portion of the script.
# 
# Outline: (Crtl + Shift + O)
# 
#   1. Code to Change When Replicating
#   2. Setup
#   3. Build Generator Sample
#       a. Generators & Emissions
#       b. Fuel Prices
#   4. Demand
#   5. K-Means Clustering
#       a. K-Means on Generators
#       b. K-Means on Demand
#   6. Run the Simulation (uncomment to run the simulation)
#   7. Process Main Simulation
#       a. Find the data
#       b. Hourly Generation
#       c. Generation Outcomes
#       d. Investment Outcomes
#       e. Emissions Outcomes
#   8. Process the High-Cost Simulation
#   9. Disadvantaged Communities
#   10. Environmental Inequality Gap
#   11. Reference Data & Figures
# 
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Code to Change When Replicating ---------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Setting working directory
setwd("C:/Users/eaper/Senior Thesis/seniorThesis")

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
library(stargazer)
library(reticulate)
options(tigris_use_cache = TRUE)

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
## Build Generator Sample ------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

### Generators & Emissions -----------------------------------------------------

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
    ORISPL, NERC, SUBRGN, PSTATABB, FIPSST, FIPSCNTY, LAT, LON, PLPRMFL, PLHTRT, 
    CAPFAC, PLNOXAN, PLSO2AN, PLHTIANT
  )

# PM2.5
pm25 <- read_excel("Data/main/eGRID/eGRID2020 DRAFT PM Emissions.xlsx", 
                   sheet = "2019 PM Plant-level Data", 
                   skip = 1)

pm25 <- pm25 %>%  select(ORISPL, PLPM25RA)

egrid_plant <- left_join(egrid_plant, pm25, by="ORISPL")


# Emissions coefficients
emis_coefs <- read_csv("Data/main/eGRID/eGRID2019 Table C-1.csv") %>% 
  select(-c("Fuel Type", "Source"))
colnames(emis_coefs) <- c("PLPRMFL", "co2_mmbtu", "ch4_mmbtu", "n2o_mmbtu")

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
emis_coefs <- emis_coefs %>% select(PLPRMFL, co2e_mmbtu)
emis_coefs <- distinct(emis_coefs)

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

gens <- gens %>% 
  group_by(ORISPL) %>% 
  summarise(capacity = sum(NAMEPCAP))

plants <- left_join(egrid_plant, gens, by="ORISPL")
plants <- left_join(plants, emis_coefs, by="PLPRMFL")

plants <- plants %>% 
  filter(
    !is.na(capacity), 
    NERC == "WECC"
  ) %>% 
  mutate(
    FUEL = case_when(
      PLPRMFL %in% coal ~ "Coal",
      PLPRMFL %in% oil ~ "Oil",
      T ~ "Gas"
    ),
    MKT_REGION = case_when(
      SUBRGN == "CAMX" ~ "California",
      SUBRGN %in% c("AZNM", "SPSO") ~ "Southwest",
      SUBRGN %in% c("NWPP", "RMPA", "MROW") ~ "Northwest",
      T ~ "Bad news..."
    ),
    PLHTRT = PLHTRT * 1000/1000000, # BTU/kWh --> MMBTU/MWh
    nox_mmbtu = 0.907185*PLNOXAN/PLHTIANT,
    nox_mmbtu = ifelse(is.na(nox_mmbtu), 0, nox_mmbtu),
    so2_mmbtu = 0.907185*PLSO2AN/PLHTIANT,
    so2_mmbtu = ifelse(is.na(so2_mmbtu), 0, so2_mmbtu),
    pm25_mmbtu = PLPM25RA*0.907185/2000,
    pm25_mmbtu = ifelse(is.na(pm25_mmbtu), 0, pm25_mmbtu)
  )

# Remove the one pair of generators that are in Missouri but allegedly still 
# connected to the Western Interconnection???
plants <- plants %>% filter(MKT_REGION != "Bad news...")

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
plants <- plants %>% filter(PLHTRT > 0)

# Drop gas generators with unusually high heat rates
# This shouldn't really affect the analysis---their capacity factors are 0
plants <- plants %>% filter(!(FUEL == "Gas" & PLHTRT*co2e_mmbtu > 2))

# Add an integer for the region (for the simulation)
plants <- plants %>%
  mutate(
    region_int = case_when(
      MKT_REGION == "California" ~ 0,
      MKT_REGION == "Northwest" ~ 1,
      MKT_REGION == "Southwest" ~ 2
    )
  )

# Reorganize and clean up the dataframe
plants <- plants %>% 
  select(
    # Plant IDs
    ORISPL, 
    # Geographic IDs
    MKT_REGION,
    region_int,
    PSTATABB,
    FIPSST, 
    FIPSCNTY, 
    SUBRGN, 
    LAT, 
    LON, 
    # Fuel
    FUEL, 
    PLPRMFL, 
    # Capacity Factor
    CAPFAC, 
    # Nameplate Capacity
    capacity, 
    # Heat Rate
    PLHTRT,
    # Emissions Coefficient
    co2e_mmbtu,
    # Air Pollutant Coefficients
    nox_mmbtu, 
    so2_mmbtu,
    pm25_mmbtu
  )

colnames(plants) <- c(
  "plantID", "region", "region_int",  "state", "FIPS2", "FIPS3", "subregion",
  "lat", "lon", "fuel_cat", "fuel", "cap_fac", "capacity", 
  "hrate", "co2e_mmbtu", "nox_mmbtu", "so2_mmbtu", "pm25_mmbtu"
)

pdf("Writing/Draft/figures/chapter5_figures/EI_region_violin.pdf",
    width = 6, height = 3.5)
ggplot(plants) + 
  geom_violin(
    aes(y=hrate*co2e_mmbtu, x=region, fill=fuel_cat), 
    trim = T) + 
  geom_text(data = plants %>%  count(region, fuel_cat), 
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


temp <- plants %>% 
  select(plantID, fuel_cat, hrate, nox_mmbtu, so2_mmbtu, pm25_mmbtu)
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


### Fuel Prices ----------------------------------------------------------------

# Coal prices

coal_pr <- read_csv(
  "Data/main/fuel prices/Coal_shipments_to_the_electric_power_sector__price_by_plant_state.csv",
  skip = 4,
  na = c("--", NA)
)

nw <- c("Oregon", "Washington", "Idaho", "Nevada", "Utah", "Colorado", 
        "Wyoming", "Montana")
sw <- c("Arizona", "New Mexico")

coal_pr <- coal_pr %>% 
  mutate(
    description = gsub("All coal : ", "", description),
    region = case_when(
      description == "California" ~ "California",
      description %in% nw ~ "Northwest",
      description %in% sw ~ "Southwest",
      description == "United States" ~ "National",
      T ~ "Other"
    )
  )

coal_pr <- coal_pr %>% 
  pivot_longer(
    cols = starts_with("Q"), 
    names_to = "Date",
    values_to = "coal_price_ton"
  ) %>% 
  filter(region != "Other") %>% 
  rowwise() %>% 
  mutate(
    month = as.numeric(gsub(
      "[^0-9.-]", "", unlist(str_split(`Date`, pattern = " "))[1]
    )) * 3 -2,
    year = as.numeric(
      unlist(str_split(`Date`, pattern = " "))[2]
    ),
    Date = make_date(year = year, month = month, day = 1),
    # $/Short ton --> $/mmBTU: see https://www.eia.gov/tools/faqs/faq.php?id=72&t=2
    coal_price = coal_price_ton/19.331
  )

coal_pr <- coal_pr %>% 
  group_by(region, Date) %>% 
  summarise(
    coal_price = mean(coal_price, na.rm = T)
  ) %>% 
  filter(!is.nan(coal_price))

pdf("Writing/Draft/figures/chapter5_figures/coal_prices.pdf",
    width = 5, height = 3)
ggplot(coal_pr, aes(x = Date, y = coal_price, color = region)) + 
  geom_line(lwd = 1.5) + 
  labs(x = "\nDate", y = "Price per MMBtu\n") + 
  scale_y_continuous(labels = scales::label_dollar()) + 
  theme(legend.title = element_blank())
dev.off()

# Get the average of the most recent prices (3 years)
coal_pr <- coal_pr %>% 
  group_by(region) %>% 
  mutate(
    start_period = max(Date) - 3*365 - 1
  ) %>% 
  ungroup() %>% 
  filter(Date >= start_period) %>% 
  group_by(region) %>% 
  summarise(
    coal_price = mean(coal_price, na.rm = T)
  ) %>% 
  mutate(fuel_cat = "Coal") %>% 
  rename(fuel_price = coal_price) %>% 
  select(fuel_cat, region, fuel_price)

# Gas prices

gas_pr <- read_excel(
  "Data/main/fuel prices/NG_PRI_SUM_A_EPG0_PEU_DMCF_M.xls", 
  sheet = 2,
  skip = 2
)

colnames(gas_pr) <- gsub(
  " Natural Gas Price Sold to Electric Power Consumers (Dollars per Thousand Cubic Feet)",
  "", colnames(gas_pr), fixed = T)

gas_pr <- gas_pr %>% 
  select(-c(2)) %>% 
  pivot_longer(
    cols = !Date,
    names_to = "state",
    values_to = "fuel_price_tcf"
  ) %>% 
  mutate(
    region = case_when(
      state == "California" ~ "California",
      state %in% nw ~ "Northwest",
      state %in% sw ~ "Southwest",
      T ~ "Other"
    ),
    # Conversion factor from: https://www.eia.gov/todayinenergy/detail.php?id=18371
    fuel_price = fuel_price_tcf*1000000/1000/1030
  )

gas_pr <- gas_pr %>% 
  filter(region != "Other") %>% 
  group_by(Date, region) %>% 
  summarise(
    fuel_price = mean(fuel_price, na.rm=T)
  ) %>% 
  filter(!is.nan(fuel_price))

pdf("Writing/Draft/figures/chapter5_figures/gas_prices.pdf",
    width = 5, height = 3)
ggplot(gas_pr, aes(x = Date, y = fuel_price, color = region)) + 
  geom_line(lwd = 1.5) + 
  labs(x = "\nDate", y = "Price per MMBtu\n") + 
  scale_y_continuous(labels = scales::label_dollar()) + 
  theme(legend.title = element_blank())
dev.off()

gas_pr <- gas_pr %>% 
  filter(Date >= as.Date("2019-01-01")) %>% 
  group_by(region) %>% 
  summarise(
    fuel_price = mean(fuel_price, na.rm=T)
  ) %>% 
  mutate(
    fuel_cat = "Gas"
  )

gas_pr <- gas_pr[c("fuel_cat", "region", "fuel_price")]

# Oil prices

oil_pr <- read_excel("Data/main/fuel prices/RWTCm.xls", sheet = 2, skip = 2)
colnames(oil_pr) <- c("Date", "fuel_price_bar")

# Conversion factor: https://www.eia.gov/energyexplained/units-and-calculators/
oil_pr <- oil_pr %>% 
  mutate(
    fuel_price = fuel_price_bar/5.691
  ) %>% 
  filter(
    Date >= as.Date("2019-01-01")
  )

oil_pr <- data.frame(
  fuel_cat = rep("Oil", 3),
  region = c("California", "Northwest", "Southwest"),
  fuel_price = rep(mean(oil_pr$fuel_price), 3)
)

# Incorporate fuel prices
fuel_pr <- bind_rows(list(coal_pr, gas_pr, oil_pr))
plants <- left_join(plants, fuel_pr, by=c("region", "fuel_cat"))

# Output table of fuel prices
fuel_pr <- fuel_pr %>% 
  filter(region != "National") %>% 
  mutate(fuel_price = round(fuel_price, 3))
colnames(fuel_pr) <- c("Fuel", "Region", "Price ($/MMBtu)")

stargazer(
  fuel_pr,
  summary = F,
  digits = 2,
  out = "Results/cleaning-tables/fuel-prices.tex",
  style = "aer"
)

summ_df <- plants %>% 
  mutate(
    cap_fac = ifelse(cap_fac > 1, 1, cap_fac),
    cap_fac = ifelse(cap_fac < 0, 0, cap_fac),
    co2e_MWh = co2e_mmbtu*hrate,
    nox_MWh = nox_mmbtu*hrate*1000,
    so2_MWh = so2_mmbtu*hrate*1000,
    pm25_MWh = pm25_mmbtu*hrate*1000,
    usd_MWh = fuel_price*hrate
  ) %>% 
  select(
    fuel_cat, cap_fac, capacity, hrate, usd_MWh, co2e_MWh, nox_MWh, so2_MWh, pm25_MWh
  )

# temp <- summ_df %>%
#   filter(fuel_cat == "Oil") %>%
#   select(-c("fuel_cat"))
# temp <- as.data.frame(temp)
# 
# stargazer(temp, summary = TRUE, summary.stat = c("mean", "sd", "min", "median", "max"),
#           covariate.labels = c("Capacity Factor",
#                                "Capacity (MW)",
#                                "Heat Rate (MMBtu/MWh)",
#                                "Input Price (\\$/MWh)",
#                                "tonnes CO$_2$e/MWh",
#                                "kg NO$_x$/MWh",
#                                "kg SO$_2$/MWh",
#                                "kg PM2.5/MWh"))

# plants %>% count(fuel_cat)

rm(coal_pr, gas_pr, oil_pr, fuel_pr, nw, sw, summ_df)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Demand ----------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

demand_sch <- list()

for (i in 1:3){
  
  r <- c("CAL", "NW", "SW")[i]
  
  df <- read_excel(
    paste("Data/main/demand/Region_", r, ".xlsx", sep=""),
    sheet = "Published Hourly Data",
    col_types = c(rep("guess", 6), rep("numeric", 50))
  )
  
  df <- df %>% 
    filter(`UTC time` >= ymd_hms("2019-01-01 00:00:00"),
           `UTC time` < ymd_hms("2022-01-01 00:00:00"))
  
  df[is.na(df)] = 0
  
  df <- df %>% 
    mutate(
      resid_D = D -`NG: NUC` -`NG: WAT` -`NG: SUN` -`NG: WND` -`NG: OTH` -`NG: UNK`
    ) %>% 
    select(Region, `UTC time`, resid_D)
  
  demand_sch[[i]] <- df
  
}

demand_sch <- demand_sch %>% 
  bind_rows() %>% 
  mutate(
    Region = case_when(
      Region == "CAL" ~ "California",
      Region == "NW" ~ "Northwest",
      Region == "SW" ~ "Southwest"
    ),
    H = hour(`UTC time`),
    D = weekdays(`UTC time`),
    M = month(`UTC time`)
  )

pdf("Writing/Draft/figures/chapter5_figures/residual_demand_region.pdf",
    width = 6, height = 4)
ggplot(demand_sch, aes(x= resid_D/1000, fill = Region)) +
  facet_wrap(facets = ~Region, nrow = 2, ncol=2) + 
  geom_histogram(color = "white", bins = 20) + 
  labs(x = "\nResidual Demand (1000 MW)", y = "Frequency (# of Hours)\n") + 
  theme(legend.position = "none")
dev.off()


pdf("Writing/Draft/figures/chapter5_figures/hourly_demand.pdf",
    width = 6, height = 4)
ggplot(demand_sch, 
       aes(x=ymd_hm(paste("2022-01-01: ", str_pad((H+16)%%24, 0, "right", "0"), ":00")), 
           y=resid_D)) +
  geom_bar(stat = "summary", fun = "mean", 
           fill=natparks.pals(name = "Charmonix", n=1)) +
  scale_x_datetime(
    breaks = as_datetime(
      c("2022-01-01 00:00:00", "2022-01-01 06:00:00", "2022-01-01 12:00:00", "2022-01-01 18:00:00")),
    labels = c("12am", "6am", "12pm", "6pm")
  ) +
  labs(y="Residual Demand (MW)\n", x="\nHour of Day")
dev.off()

pdf("Writing/Draft/figures/chapter5_figures/monthly_demand.pdf",
    width = 6, height = 4)
ggplot(demand_sch, aes(x = M, y = resid_D)) +
  geom_bar(stat = "summary", fun = "mean", fill=natparks.pals(name = "Charmonix", n=1)) +
  labs(x="\nMonth", y="Residual Demand (MW)\n") +
  scale_x_continuous(breaks = 1:12, labels = month.abb)
dev.off()

rm(gens, df, i, r)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## K-Means Clustering ----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### K-Means on Generators ------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Goal---get all the power plants into 30 groups:
# Rules:
#   1. Same group generators must be in the same region
#   2. Same group generators must use the same fuel
# 
# Each Region Gets 5 Groups:
#   1. Coal Group
#   2-9. 3 Gas Groups
#   10. Oil Group

cluster_i <- list()
plants_list_i <- split(plants, plants$region)

set.seed(1989)

for (i in 1:length(plants_list_i)){
  
  plants_i <- plants_list_i[[i]]
  plants_list_j <- split(plants_i, plants_i$fuel_cat)
  
  cluster_j <- list()
  
  for (j in 1:length(plants_list_j)){
    
    plants_j <- plants_list_j[[j]]
    
    # Pick K based on the fuel
    k_picker <- c("Coal" = 1, "Gas" = 8, "Oil" = 1)
    K <- k_picker[unique(plants_j$fuel_cat)]
    
    temp <- plants_j %>% 
      select(hrate, capacity)
    
    km_j <- kmeans(temp, K, nstart = 25)
    
    plants_j$group <- paste(
      unique(plants_j$region), unique(plants_j$fuel_cat), km_j$cluster
    )
    
    cluster_j[[j]] <- plants_j
    
  }
  
  cluster_i[[i]] <- bind_rows(cluster_j)
  
}

plants <- bind_rows(cluster_i)

# Clean up the names of these groups
plants <- plants %>% 
  rowwise() %>% 
  mutate(
    group = unlist(str_split(group, pattern = " "))[3],
    group = ifelse(fuel_cat == "Coal" | fuel_cat == "Oil", "", group),
    group = str_trim(paste(region, fuel_cat, group))
  )

pdf("Writing/Draft/figures/chapter5_figures/kclusters_hrate.pdf",
    width = 6, height = 8)
ggplot(plants, aes(x=hrate, y=group, fill=fuel_cat)) + 
  geom_boxplot() + 
  ylab("") +
  xlab('\nHeat Rate (mmBTU/MWh)') +
  scale_y_discrete(limits=rev) +
  theme(legend.title = element_blank(), legend.position = "none")
dev.off()

pdf("Writing/Draft/figures/chapter5_figures/kclusters_capacity.pdf",
    width = 6, height = 8)
ggplot(plants, aes(x=capacity, y=group, fill=fuel_cat)) + 
  geom_boxplot() + 
  ylab("") +
  xlab('\nCapacity (MW)') +
  scale_y_discrete(limits=rev) +
  theme(legend.title = element_blank(), legend.position = "none")
dev.off()

pdf("Writing/Draft/figures/chapter5_figures/kclusters_cap_fac.pdf",
    width = 6, height = 8)
ggplot(plants, aes(x=cap_fac, y=group, fill=fuel_cat)) + 
  geom_boxplot() + 
  ylab("") +
  xlab('\nCapacity Factor') +
  scale_y_discrete(limits=rev) +
  theme(legend.title = element_blank(), legend.position = "top")
dev.off()

# Now create a dataframe with the aggregated groups of power plants
cluster_plants <- plants[c(3,10,13:20)]

cluster_plants <- cluster_plants %>% 
  group_by(group, fuel_cat) %>% 
  summarise(
    region = as.integer(mean(region_int)),
    plant_count = n(),
    total_capacity = sum(capacity),
    mean_capacity = mean(capacity),
    hrate = mean(hrate),
    co2e_mmbtu = mean(co2e_mmbtu),
    nox_mmbtu = mean(nox_mmbtu),
    so2_mmbtu = mean(so2_mmbtu),
    pm25_mmbtu = mean(pm25_mmbtu),
    fuel_price = mean(fuel_price)
  ) %>% 
  ungroup()

# Now we want to cluster into different investment groups, again based on rates. 
# To do so, we'll split into two regions---California and Not-California. For 
# each region, we'll split into two groups based on the heat rates. Unlike the 
# last clustering, investment types do not need to have the same fuel type. 
cluster_plants$cal <- ifelse(cluster_plants$region == 0, 1, 0)

temp_oil <- cluster_plants %>% 
  filter(fuel_cat == "Oil")
temp_oil$inv_group <- rep("Oil", nrow(temp_oil))
temp_oil$inv_int <- rep(0, nrow(temp_oil))
cluster_plants <- cluster_plants %>% 
  filter(fuel_cat != "Oil")
  
cluster_i <- list()
plants_list_i <- split(cluster_plants, cluster_plants$cal)

set.seed(1989)
for (i in 1:length(plants_list_i)){
  
  plants_i <- plants_list_i[[i]]
  temp <- plants_i %>% 
    select(hrate, mean_capacity)
  
  km_i <- kmeans(temp, 2, nstart = 25)
  
  temp_region = ifelse(unique(plants_i$cal) == 0, "Not California", "California")
  plants_i$inv_group <- paste(temp_region, km_i$cluster)
  
  cluster_i[[i]] <- plants_i
  
}

cluster_plants <- bind_rows(cluster_i)

cluster_plants <- cluster_plants %>% 
  mutate(
    inv_int = case_when(
      inv_group == "California 1" ~ 1,
      inv_group == "California 2" ~ 2,
      inv_group == "Not California 1" ~ 3,
      inv_group == "Not California 2" ~ 4
    )
  )

cluster_plants <- rbind(cluster_plants, temp_oil)
cluster_plants <- cluster_plants[order(cluster_plants$group),]

cluster_plants <- cluster_plants %>% 
  mutate(
    j_bar = 0.015 * hrate,
    inv_cost =  1044900 * (plant_count * j_bar)^(1/0.9875)
  )

write_csv(cluster_plants, "Results/kmeans-plants.csv")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### K-Means on Demand ----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

cluster_demand <- demand_sch %>% 
  pivot_wider(
    names_from = Region,
    values_from = resid_D
  )

set.seed(1989)
cluster_demand <- kmeans(cluster_demand[5:7], 24, nstart = 25, iter.max=30)
# This does flash a warning at us but this is just because the many of the rows
# of the dataframe are very close, which scares the algorithm it uses, this
# does not prevent us from getting the 24 representative hours we want though

demand_sch <- demand_sch %>% 
  pivot_wider(values_from = resid_D, names_from = Region)
demand_sch$cluster <- cluster_demand$cluster
cluster_demand <- as.data.frame(cluster_demand$centers)
cluster_demand$hour <- 1:24
cluster_demand <- cluster_demand[c("hour", "California", "Northwest", "Southwest")]

write_csv(cluster_demand, "Results/kmeans-demand.csv")
rm(cluster_i, cluster_j, km_i, km_j, plants_i, plants_j, plants_list_i, 
   plants_list_j, temp, temp_oil, i, j, K, k_picker, temp_region)

cluster_plants$gen_group <- 1:30

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Run the Simulation ----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# The actual simulation is coded in a separate Python script. This next line 
# just calls this script and runs it. Note that these simulations take some time
# to execute (about 3 hours). 

# py_run_file("Code/main/simulation.py")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Process Main Simulation -----------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

### Find the data --------------------------------------------------------------

# List all the generation files
gen_files <- setdiff(
  list.files(
    path = "Results/simulation-results", pattern = "gen",
    full.names = T, recursive = T),
  list.files(
    path = "Results/simulation-results/high-cost", pattern = "gen",
    full.names = T, recursive = T)
)

# List all the investment files
inv_files <- setdiff(
  list.files(
    path = "Results/simulation-results", pattern = "costs",
    full.names = T, recursive = T),
  list.files(
    path = "Results/simulation-results/high-cost", pattern = "costs",
    full.names = T, recursive = T)
)

### Hourly Generation ----------------------------------------------------------

# Write a function to read and process the simulation results for generation
process_hourly_gen <- function(file_list){
  
  # Read in the generation results
  gen_sim <- lapply(
    file_list, 
    function (x) read_csv(x, col_types = list('n', 'n', 'n', 'n', 'c'))
  )
  gen_sim <- bind_rows(gen_sim)
  
  # Rename the columns
  colnames(gen_sim) <- c(
    "row_num", "California", "Northwest", "Southwest", "scenario"
  )
  
  # Add in generation group and hour identifiers
  gen_sim <- gen_sim %>% 
    mutate(
      gen_group = row_num %% 30 + 1,
      hour = row_num %/% 30 + 1,
    )
  
  # Add in the carbon tax level and the BCA status
  tau_dict <- c(
    'a' = 0, 'b' = 20, 'c' = 40, 'd' = 60, 'e' = 80,
    'f' = 0, 'g' = 20, 'h' = 40, 'i' = 60, 'j' = 80
  )
  bca_dict <- c(
    'a' = 0, 'b' = 0, 'c' = 0, 'd' = 0, 'e' = 0, 
    'f' = 1, 'g' = 1, 'h' = 1, 'i' = 1, 'j' = 1
  )
  
  gen_sim <- gen_sim %>% 
    mutate(
      total_plants = California + Northwest + Southwest,
      tau = tau_dict[scenario],
      bca = bca_dict[scenario]
    )
  
  # Add in capacity data
  gen_sim$mean_cap <- rep(cluster_plants$mean_capacity, 24*10)
  gen_sim$hourly_cap <- rep(cluster_plants$total_capacity, 24*10)
  
  # Calculate the generation and capacity factors
  gen_sim <- gen_sim %>% 
    mutate(
      hourly_gen = total_plants * mean_cap * 0.9,
      hourly_cap_fac = hourly_gen/hourly_cap
    ) %>% 
    select(
      gen_group, scenario, hour, tau, bca, hourly_cap_fac
    )
  
  temp <- merge(
    plants, 
    cluster_plants %>% select(group, gen_group, inv_group, inv_int),
    by = "group"
  )
  
  gen_sim <- merge(gen_sim, temp, by="gen_group")
  
  return(gen_sim)
  
}

process_inv <- function(file_names){
  
  all_inv = lapply(
      file_names, function (x) read_csv(x, col_types = list('n', 'n', 'n', 'c'))
    )
  all_inv = bind_rows(all_inv)
  
  colnames(all_inv) <- c('inv_opt', 'tot_cost', 'inv_cost', 'scenario')
  
  temp <- all_inv %>% 
    mutate(inv_opt = inv_opt + 1) %>% 
    group_by(scenario) %>% 
    mutate(
      min = ifelse(tot_cost == min(tot_cost), 1, 0)
    ) %>% 
    ungroup()
  
  temp <- temp %>% filter(min == 1)
  temp <- temp %>% select(scenario, inv_opt)
  
  inv_scn = read_csv("Results/simulation-results/inv_scenarios.csv")
  colnames(inv_scn) <- as.character(1:16)
  inv_scn$gen_group <- 1:30
  
  df_list <- list()
  
  for (i in 1:nrow(temp)){
    
    my_sim_scenario = temp$scenario[i]
    my_inv_choice = temp$inv_opt[i]
    
    inv_choice = inv_scn[,as.character(my_inv_choice)]
    colnames(inv_choice) = c("inv_choice")
    
    df = data.frame(
      scenario = rep(my_sim_scenario, nrow(inv_scn)),
      gen_group = 1:30,
      inv_choice
    )
    
    df_list[[i]] <- df
    
  }
  
  return(bind_rows(df_list))
  
}

sim_gen_hourly <- process_hourly_gen(gen_files)
sim_inv <- process_inv(inv_files)

hourly_sim <- left_join(sim_gen_hourly, sim_inv, by=c('scenario','gen_group'))

hourly_sim <- hourly_sim %>% 
  mutate(
    hourly_gen = hourly_cap_fac * capacity,
    sim_hrate = ifelse(inv_choice == 1, (1 - 0.015)*hrate, hrate),
    hourly_heat_input = hourly_gen * sim_hrate,
    hourly_co2e = co2e_mmbtu * hourly_heat_input,
    hourly_nox  = nox_mmbtu * hourly_heat_input,
    hourly_so2  = so2_mmbtu * hourly_heat_input,
    hourly_pm25  = pm25_mmbtu * hourly_heat_input
  )

hourly_weights <- demand_sch %>% 
  group_by(cluster) %>% 
  mutate(weights = n()) %>% 
  ungroup() %>% 
  mutate(weights = weights/n()) %>% 
  distinct(cluster, weights) %>% 
  rename(hour = cluster)

hourly_sim <- merge(hourly_sim, hourly_weights, by="hour")

hourly_sim <- hourly_sim[,c(
  "plantID",
  "scenario",
  "hour",
  "region",
  "region_int",
  "state",
  "FIPS2",
  "FIPS3",
  "lat",
  "lon",
  "bca",
  "tau",
  "group",
  "gen_group",
  "weights",
  "fuel_cat",
  "fuel",
  "capacity",
  "hrate",
  "fuel_price",
  "cap_fac",
  "co2e_mmbtu",
  "nox_mmbtu",
  "so2_mmbtu",
  "pm25_mmbtu",
  "hourly_cap_fac",
  "hourly_gen",
  "hourly_heat_input",
  "inv_int",
  "inv_choice",
  "sim_hrate",
  "hourly_co2e",
  "hourly_nox",
  "hourly_so2",
  "hourly_pm25"
)]

hourly_sim <- hourly_sim[
  order(hourly_sim[,'plantID'], hourly_sim[,'scenario'], hourly_sim[,'hour']),
]

annual_sim <- hourly_sim %>% 
  group_by(plantID, scenario) %>% 
  mutate(
    annual_gen = sum(hourly_gen * weights) * 24 * 365,
    annual_heat_input = sum(hourly_heat_input * weights) * 24 * 365,
    annual_cap_fac = annual_gen/(capacity * 24 * 365),
    annual_co2e = co2e_mmbtu * annual_heat_input,
    annual_nox = nox_mmbtu * annual_heat_input,
    annual_so2 = so2_mmbtu * annual_heat_input,
    annual_pm25 = pm25_mmbtu * annual_heat_input
  ) %>% 
  select(
    -c('hour','weights', 'hourly_cap_fac', 'hourly_gen', 'hourly_heat_input', 
            'hourly_co2e', 'hourly_nox', 'hourly_so2', 'hourly_pm25')) %>% 
  distinct()

### Generation Outcomes ---------------------------------------------------------

# With BCA

temp <- annual_sim %>% 
  filter(bca == 1) %>% 
  group_by(fuel_cat, region, tau) %>% 
  summarise(
    annual_gen = sum(annual_gen)
  )

# temp <- temp %>% 
#   group_by(region, tau) %>% 
#   summarise(
#     annual_gen = sum(annual_gen)
#   )
# 
# (65104053 - 70190296)/70190296 * 100
# (113086551 - 111429787)/(70190296 - 65038932)
# (187482733 - 183988121)/(70190296 - 65038932)

temp <- temp %>% 
  group_by(fuel_cat, tau) %>% 
  summarise(
    annual_gen = sum(annual_gen)
  )

temp <- temp %>% 
  filter(fuel_cat != "Oil")

temp <- temp %>% 
  mutate(
    pct_chng = ifelse(
      fuel_cat == "Gas", (annual_gen - temp$annual_gen[6])/temp$annual_gen[6],
      (annual_gen - temp$annual_gen[1])/temp$annual_gen[1]
    )
  )

# (159705384 - 205902820)/205902820 * 100

pdf("Writing/Draft/figures/chapter5_figures/gen_fuel_bca_pct.pdf",
    width = 6, height = 4)
ggplot(temp, aes(x= tau, y=pct_chng, color = fuel_cat)) +
  geom_line(lwd=2) +  
  geom_point(color = "white", size = 5) +
  geom_point(size = 4) +
  labs(x='\nCalifornia Emissions Price ($/tonne)', 
       y='% Change in Generation\nFrom Carbon Price\n') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(legend.title = element_blank(), legend.position = "right")
dev.off()

temp <- annual_sim %>% 
  filter(bca == 1) %>% 
  group_by(fuel_cat, region, tau) %>% 
  summarise(
    annual_gen = sum(annual_gen)
  )

pdf("Writing/Draft/figures/chapter5_figures/gen_fuel_bca.pdf",
    width = 6, height = 4)
ggplot(temp, aes(x= tau, y=annual_gen, fill = fuel_cat)) +
  geom_bar(stat = 'identity', position = 'fill') + 
  labs(x='\nCalifornia Emissions Price ($/tonne)', 
       y='% of Generation\n') +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank())
dev.off()

pdf("Writing/Draft/figures/chapter5_figures/gen_region_bca.pdf",
    width = 6, height = 4)
ggplot(temp, aes(x= tau, y=annual_gen, fill = region)) +
  geom_bar(stat = 'identity', position = 'fill') + 
  labs(x='\nCalifornia Emissions Price ($/tonne)', 
       y='% of Generation\n') +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank())
dev.off()

# Without BCA

temp <- annual_sim %>% 
  filter(bca == 0) %>% 
  group_by(fuel_cat, region, tau) %>% 
  summarise(
    annual_gen = sum(annual_gen)
  )

# temp <- temp %>% 
#   group_by(region, tau) %>% 
#   summarise(
#     annual_gen = sum(annual_gen)
#   )
# (70190296 - 58740134)/70190296*100

pdf("Writing/Draft/figures/chapter5_figures/gen_fuel_nobca.pdf",
    width = 6, height = 4)
ggplot(temp, aes(x= tau, y=annual_gen, fill = fuel_cat)) +
  geom_bar(stat = 'identity', position = 'fill') + 
  labs(x='\nCalifornia Emissions Price ($/tonne)', 
       y='% of Generation\n') +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank())
dev.off()

pdf("Writing/Draft/figures/chapter5_figures/gen_region_nobca.pdf",
    width = 6, height = 4)
ggplot(temp, aes(x= tau, y=annual_gen, fill = region)) +
  geom_bar(stat = 'identity', position = 'fill') + 
  labs(x='\nCalifornia Emissions Price ($/tonne)', 
       y='% of Generation\n') +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank())
dev.off()


### Investment Outcomes --------------------------------------------------------

# Normal investment costs
temp <- annual_sim %>% 
  filter(bca == 0, inv_choice == 1) %>% 
  group_by(tau, region) %>% 
  summarize(
    count = n()
  )

pdf("Writing/Draft/figures/chapter5_figures/inv_region.pdf",
    width = 6, height = 4)
ggplot(temp, aes(x= tau, y = count, fill = region)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x='\nCalifornia Emissions Price ($/tonne)', 
       y='# Investing Plants\n') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank())
dev.off()


### Emissions Outcomes ---------------------------------------------------------

# With BCA

temp_a <- annual_sim %>% 
  filter(bca == 1) %>% 
  group_by(region, tau) %>% 
  summarise(
    annual_co2e = sum(annual_co2e),
    annual_nox = sum(annual_nox),
    annual_so2 = sum(annual_so2),
    annual_pm25 = sum(annual_pm25)
  )

temp_b <- temp_a %>% filter(tau == 0) 
temp_b <- c(
  "California" = temp_b$annual_co2e[1],
  "Northwest" = temp_b$annual_co2e[2],
  "Southwest" = temp_b$annual_co2e[3]
)
temp_b <- temp_a %>% 
  mutate(
    co2e_changes = annual_co2e - temp_b[region]
  ) %>% 
  filter(tau != 0)


# (176278756 - 180111618 + 43943627 - 66367657)/(36552761 - 29139031)

# temp <- temp %>% 
#   group_by(tau) %>% 
#   summarise(
#     annual_co2e = sum(annual_co2e),
#     annual_nox = sum(annual_nox),
#     annual_so2 = sum(annual_so2),
#     annual_pm25 = sum(annual_pm25)
#   )
# 
# 
# (29139031 - 36552761)/36552761
# (176278756 - 180111618)/180111618
# (43943627 - 66367657)/66367657
# (249361415 - 283032035)/283032035
# 249361415 - 283032035
# 33670620/4.6

p1 <- ggplot(temp_a, aes(x= tau, y=annual_co2e/10^6, fill = region)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x='\nCalifornia Emissions \nPrice ($/tonne)', 
       y='Annual CO2e Emissions \n(Million tonnes)') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank())

p2 <- ggplot(temp_b, aes(x= tau, y=co2e_changes/10^6, fill = region)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x='\nCalifornia Emissions \nPrice ($/tonne)', 
       y='Annual CO2e Emissions \nReductions (Million tonnes)') +
  scale_x_continuous(breaks = c(20, 40, 60, 80)) +
  theme(legend.title = element_blank())

pdf("Writing/Draft/figures/chapter5_figures/sim_co2e_bca.pdf",
    width = 6, height = 4)
ggpubr::ggarrange(p1, p2, ncol = 2, nrow = 1, labels = c("A","B"), 
                  common.legend = T)
dev.off()

# NOx

temp_b <- temp_a %>% filter(tau == 0) 
temp_b <- c(
  "California" = temp_b$annual_nox[1],
  "Northwest" = temp_b$annual_nox[2],
  "Southwest" = temp_b$annual_nox[3]
)
temp_b <- temp_a %>% 
  mutate(
    nox_changes = annual_nox - temp_b[region]
  ) %>% 
  filter(tau != 0)

p1 <- ggplot(temp_a, aes(x= tau, y=annual_nox/10^3, fill = region)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x="",
       y='Annual NOx Emissions \n(Thousand tonnes)') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank())

p2 <- ggplot(temp_b, aes(x= tau, y=nox_changes/10^3, fill = region)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x="",
       y='Annual NOx Emissions \nReductions (Thousand tonnes)') +
  scale_x_continuous(breaks = c(20, 40, 60, 80)) +
  theme(legend.title = element_blank())

# pdf("Writing/Draft/figures/chapter5_figures/sim_nox_bca.pdf",
#     width = 6, height = 4)
# ggpubr::ggarrange(p1, p2, ncol = 2, nrow = 1, labels = c("A","B"), 
#                   common.legend = T)
# dev.off()

# SO2

temp_b <- temp_a %>% filter(tau == 0) 
temp_b <- c(
  "California" = temp_b$annual_so2[1],
  "Northwest" = temp_b$annual_so2[2],
  "Southwest" = temp_b$annual_so2[3]
)
temp_b <- temp_a %>% 
  mutate(
    so2_changes = annual_so2 - temp_b[region]
  ) %>% 
  filter(tau != 0)

p3 <- ggplot(temp_a, aes(x= tau, y=annual_so2/10^3, fill = region)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x='', 
       y='Annual SO2 Emissions \n(Thousand tonnes)') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank())

p4 <- ggplot(temp_b, aes(x= tau, y=so2_changes/10^3, fill = region)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x='', 
       y='Annual SO2 Emissions \nReductions (Thousand tonnes)') +
  scale_x_continuous(breaks = c(20, 40, 60, 80)) +
  theme(legend.title = element_blank())

# pdf("Writing/Draft/figures/chapter5_figures/sim_so2_bca.pdf",
#     width = 6, height = 4)
# ggpubr::ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, labels = c("A","B", "C", "D"), 
#                   common.legend = T, 
#                   )
# dev.off()

# PM25

temp_b <- temp_a %>% filter(tau == 0) 
temp_b <- c(
  "California" = temp_b$annual_pm25[1],
  "Northwest" = temp_b$annual_pm25[2],
  "Southwest" = temp_b$annual_pm25[3]
)
temp_b <- temp_a %>% 
  mutate(
    pm25_changes = annual_pm25 - temp_b[region]
  ) %>% 
  filter(tau != 0)

p5 <- ggplot(temp_a, aes(x= tau, y=annual_pm25/10^3, fill = region)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x='\nCalifornia Emissions \nPrice ($/tonne)', 
       y='Annual PM2.5 Emissions \n(Thousand tonnes)') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank())

p6 <- ggplot(temp_b, aes(x= tau, y=pm25_changes/10^3, fill = region)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x='\nCalifornia Emissions \nPrice ($/tonne)', 
       y='Annual PM2.5 Emissions \nReductions (Thousand tonnes)') +
  scale_x_continuous(breaks = c(20, 40, 60, 80)) +
  theme(legend.title = element_blank())

pdf("Writing/Draft/figures/chapter5_figures/sim_pol_bca.pdf",
    width = 6, height = 9)
ggpubr::ggarrange(p1, p2, p3, p4, p5, p6,
                  ncol = 2, nrow = 3, labels = c("A","B", "C", "D", "E", "F"), 
                  common.legend = T)
dev.off()


# pdf("Writing/Draft/figures/chapter5_figures/sim_pm25_bca.pdf",
#     width = 6, height = 4)
# ggplot(temp, aes(x= tau, y=annual_pm25, fill = region)) +
#   geom_bar(stat = 'identity', position = 'stack') + 
#   labs(x='\nCalifornia Emissions Price ($/tonne)', 
#        y='Annual PM2.5 Emissions (tonnes)\n') +
#   scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
#   theme(legend.title = element_blank())
# dev.off()

# With no BCA

temp <- annual_sim %>% 
  filter(bca == 0) %>% 
  group_by(region, tau) %>% 
  summarise(
    annual_co2e = sum(annual_co2e),
    annual_nox = sum(annual_nox),
    annual_so2 = sum(annual_so2),
    annual_pm25 = sum(annual_pm25)
  )

temp_a <- temp

temp_b <- temp_a %>% filter(tau == 0) 
temp_b <- c(
  "California" = temp_b$annual_co2e[1],
  "Northwest" = temp_b$annual_co2e[2],
  "Southwest" = temp_b$annual_co2e[3]
)
temp_b <- temp_a %>% 
  mutate(
    co2e_changes = annual_co2e - temp_b[region]
  ) %>% 
  filter(tau != 0)

# temp <- temp %>%
#   group_by(tau) %>%
#   summarise(
#     annual_co2e = sum(annual_co2e),
#     annual_nox = sum(annual_nox),
#     annual_so2 = sum(annual_so2),
#     annual_pm25 = sum(annual_pm25)
#   )
# (276222223 - 283032035)/283032035
# (26028114 - 36552761)/36552761
# (186771085- 180111618)/180111618
# (67101854 - 66367657)/66367657
# (186771085 - 180111618 + 67101854 - 66367657)/(36552761 - 26028114)



p1 <- ggplot(temp_a, aes(x= tau, y=annual_co2e/10^6, fill = region)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x='\nCalifornia Emissions \nPrice ($/tonne)', 
       y='Annual CO2e Emissions \n(Million tonnes)') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank())

p2 <- ggplot(temp_b, aes(x= tau, y=co2e_changes/10^6, fill = region)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x='\nCalifornia Emissions \nPrice ($/tonne)', 
       y='Annual CO2e Emissions \nReductions (Million tonnes)') +
  scale_x_continuous(breaks = c(20, 40, 60, 80)) +
  theme(legend.title = element_blank())

pdf("Writing/Draft/figures/chapter5_figures/sim_co2e_nobca.pdf",
    width = 6, height = 4)
ggpubr::ggarrange(p1, p2, ncol = 2, nrow = 1, labels = c("A","B"), 
                  common.legend = T)
dev.off()


temp <- annual_sim %>% 
  filter(bca == 0) %>% 
  group_by(region, tau) %>% 
  summarise(
    annual_co2e = sum(annual_co2e),
    annual_nox = sum(annual_nox),
    annual_so2 = sum(annual_so2),
    annual_pm25 = sum(annual_pm25)
  )

pdf("Writing/Draft/figures/chapter5_figures/sim_co2e_nobca.pdf",
    width = 6, height = 4)
ggplot(temp, aes(x= tau, y=annual_co2e/10^9, fill = region)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x='\nCalifornia Emissions Price ($/tonne)', 
       y='Annual CO2e Emissions (Gigatonnes)\n') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank())
dev.off()

pdf("Writing/Draft/figures/chapter5_figures/sim_nox_nobca.pdf",
    width = 6, height = 4)
ggplot(temp, aes(x= tau, y=annual_nox, fill = region)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x='\nCalifornia Emissions Price ($/tonne)', 
       y='Annual NOx Emissions (tonnes)\n') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank())
dev.off()

pdf("Writing/Draft/figures/chapter5_figures/sim_so2_nobca.pdf",
    width = 6, height = 4)
ggplot(temp, aes(x= tau, y=annual_so2, fill = region)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x='\nCalifornia Emissions Price ($/tonne)', 
       y='Annual SO2 Emissions (tonnes)\n') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank())
dev.off()

pdf("Writing/Draft/figures/chapter5_figures/sim_pm25_nobca.pdf",
    width = 6, height = 4)
ggplot(temp, aes(x= tau, y=annual_pm25, fill = region)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x='\nCalifornia Emissions Price ($/tonne)', 
       y='Annual PM2.5 Emissions (tonnes)\n') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank())
dev.off()

### Generation Diagnostics -----------------------------------------------------

# i.e. Does the model actually do a good job at predicting this thing?

egrid_2019 <- read_excel("Data/main/eGRID/egrid2019_data.xlsx", 
                        sheet="PLNT19",
                        skip = 1)
egrid_2019 <- egrid_2019 %>% filter(ORISPL %in% plants$plantID) %>% 
  select(ORISPL, PLNGENAN)

egrid_2020 <- read_excel("Data/main/eGRID/eGRID2020_Data_v2.xlsx", 
                         sheet="PLNT20",
                         skip = 1)
egrid_2020 <- egrid_2020 %>% filter(ORISPL %in% plants$plantID) %>% 
  select(ORISPL, PLNGENAN)

egrid_2021 <- read_excel("Data/main/eGRID/eGRID2021_data.xlsx", 
                         sheet="PLNT21",
                         skip = 1)
egrid_2021 <- egrid_2021 %>% filter(ORISPL %in% plants$plantID) %>% 
  select(ORISPL, PLNGENAN)

egrid_ave <- list(egrid_2019, egrid_2020, egrid_2021)
egrid_ave <- bind_rows(egrid_ave)
egrid_ave <- egrid_ave %>% 
  group_by(ORISPL) %>% 
  summarise(ave_annual_gen = mean(PLNGENAN))

diag_df <- merge(annual_sim, egrid_ave, by.x="plantID", by.y="ORISPL", all.x = T)

diag_list <- split(diag_df, diag_df$scenario)

# for (i in 1:length(diag_list)){
#   
#   df <- diag_list[[i]]
#   mod <- lm('ave_annual_gen ~ annual_gen', data = df)
#   mod <- summary(mod)
#   diag_list[[i]] <- mod 
#   
# }
# 
# # l <- diag_list[[1]]$coefficients
# 
# temp <- data.frame(
#   scenario = unique(annual_sim$scenario),
#   mod_coef = unlist(lapply(diag_list, function (x) x$coefficients[2,1])),
#   mod_fit = unlist(lapply(diag_list, function (x) x$r.squared))
# )
# 
# ggplot(diag_df %>% filter(scenario == "a")) +
#   geom_abline(slope = 1, intercept = 0, 
#               lwd = 2, col = natparks.pals(name = "Charmonix", n=1)) +
#   geom_point(aes(x = annual_gen, y = ave_annual_gen)) + 
#   scale_x_sqrt() + 
#   scale_y_sqrt()

df <- diag_df %>% filter(scenario == "g")

df$sim_mkt_shr <- df$annual_gen / sum(df$annual_gen) * 10^3
df$emp_mkt_shr <- df$ave_annual_gen / sum(df$ave_annual_gen, na.rm = T) * 10^3

summ_table <- data.frame()

t1 = t.test(df$sim_mkt_shr, df$emp_mkt_shr, 
            paired = TRUE, alternative = "two.sided")
t2 = t.test(df$sim_mkt_shr, df$emp_mkt_shr, 
            paired = TRUE, alternative = "less")
t3 = t.test(df$sim_mkt_shr, df$emp_mkt_shr, 
            paired = TRUE, alternative = "greater")

summ_table <- list(t1, t2, t3)
summ_table <- lapply(summ_table, function (x) data.frame(broom::tidy(x)))
summ_table <- bind_rows(summ_table)
stargazer(summ_table, summary = F, rownames = F)


pdf("Writing/Draft/figures/chapter5_figures/scatter_mkt_share.pdf",
    width = 6, height = 4)
ggplot(df) +
  geom_abline(slope = 1, intercept = 0,
              lwd = 1, col = natparks.pals(name = "Charmonix", n=4)[4]) +
  geom_point(aes(x = sim_mkt_shr, y = emp_mkt_shr, color = fuel_cat), shape = 3) +
  labs(x = bquote('\nSimulated Market Share'~(x10^3)), 
       y = bquote('Empirical Market Share'~(x10^3))) +
  theme(legend.title = element_blank())
dev.off()

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Process the High-Cost Simulation --------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


hc_gen_files <- list.files(path = "Results/simulation-results/high-cost", 
                      pattern = "gen", full.names = T, recursive = T)
hc_inv_files <- list.files(path = "Results/simulation-results/high-cost", 
                      pattern = "cost", full.names = T, recursive = T)

hc_sim_gen_hourly <- process_hourly_gen(hc_gen_files)
hc_sim_inv <- process_inv(hc_inv_files)

hc_hourly_sim <- left_join(hc_sim_gen_hourly, 
                           hc_sim_inv, by=c('scenario','gen_group'))

hc_hourly_sim <- hc_hourly_sim %>% 
  mutate(
    hourly_gen = hourly_cap_fac * capacity,
    sim_hrate = ifelse(inv_choice == 1, (1 - 0.015)*hrate, hrate),
    hourly_heat_input = hourly_gen * sim_hrate,
    hourly_co2e = co2e_mmbtu * hourly_heat_input,
    hourly_nox  = nox_mmbtu * hourly_heat_input,
    hourly_so2  = so2_mmbtu * hourly_heat_input,
    hourly_pm25  = pm25_mmbtu * hourly_heat_input
  )

hc_hourly_sim <- merge(hc_hourly_sim, hourly_weights, by="hour")

hc_hourly_sim <- hc_hourly_sim[,c(
  "plantID",
  "scenario",
  "hour",
  "region",
  "region_int",
  "state",
  "FIPS2",
  "FIPS3",
  "lat",
  "lon",
  "bca",
  "tau",
  "group",
  "gen_group",
  "weights",
  "fuel_cat",
  "fuel",
  "capacity",
  "hrate",
  "fuel_price",
  "cap_fac",
  "co2e_mmbtu",
  "nox_mmbtu",
  "so2_mmbtu",
  "pm25_mmbtu",
  "hourly_cap_fac",
  "hourly_gen",
  "hourly_heat_input",
  "inv_int",
  "inv_choice",
  "sim_hrate",
  "hourly_co2e",
  "hourly_nox",
  "hourly_so2",
  "hourly_pm25"
)]

hc_hourly_sim <- hc_hourly_sim[
  order(hc_hourly_sim[,'plantID'], 
        hc_hourly_sim[,'scenario'], 
        hc_hourly_sim[,'hour']),
]

hc_annual_sim <- hc_hourly_sim %>% 
  group_by(plantID, scenario) %>% 
  mutate(
    annual_gen = sum(hourly_gen * weights) * 24 * 365,
    annual_heat_input = sum(hourly_heat_input * weights) * 24 * 365,
    annual_cap_fac = annual_gen/(capacity * 24 * 365),
    annual_co2e = co2e_mmbtu * annual_heat_input,
    annual_nox = nox_mmbtu * annual_heat_input,
    annual_so2 = so2_mmbtu * annual_heat_input,
    annual_pm25 = pm25_mmbtu * annual_heat_input
  ) %>% 
  select(
    -c('hour','weights', 'hourly_cap_fac', 'hourly_gen', 'hourly_heat_input', 
       'hourly_co2e', 'hourly_nox', 'hourly_so2', 'hourly_pm25')) %>% 
  distinct()

temp <- hc_annual_sim %>% 
  filter(bca == 1, inv_choice == 1) %>% 
  group_by(tau, region) %>% 
  summarize(
    count = n()
  )

pdf("Writing/Draft/figures/chapter5_figures/hc_inv_region.pdf",
    width = 6, height = 4)
ggplot(temp, aes(x= tau, y = count, fill = region)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x='\nCalifornia Emissions Price ($/tonne)', 
       y='# Investing Plants\n') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank())
dev.off()

rm(cluster_demand, cluster_plants, hc_annual_sim, hc_hourly_sim, 
   hc_sim_gen_hourly, hc_sim_inv, hourly_weights, sim_inv, temp, gen_files, 
   hc_gen_files, hc_inv_files, inv_files, process_hourly_gen, process_inv)
rm(p1, p2, p3, p4, p5, p6, temp_a, temp_b)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Disadvantaged Communities ---------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Import all the EJ data
all_comms <- read_csv("Data/main/disadvantaged/EJSCREEN_2021_USPR_Tracts_part1.csv")
temp <- read_csv("Data/main/disadvantaged/EJSCREEN_2021_USPR_Tracts_part2.csv")
all_comms <- bind_rows(all_comms, temp)
rm(temp)

# Import the geography of the WECC
wecc_shp <- st_read("Data/main/NERC_Regions/NERC_Regions_Subregions.dbf") %>% 
  filter(SUBNAME %in% c("AZ-NM-SNV", "CA-MX US", "RMPA", "NWPP"))
wecc_shp <- st_transform(wecc_shp, st_crs(5070))

# Grab the tract shapefiles for the US states that intersect with the WECC
my_states <- c(
  "CA", "OR", "WA", "AZ", "NM", "CO", "NV", "UT", "WY", "ID", "MT", "NE", "SD"
)

# Loop through the states grabbing all their census tracts (2010 tracts)
wecc_tracts <- list()

for (i in 1:length(my_states)){
  my_state <- my_states[i]
  df <- tracts(state = my_state, year=2010)
  wecc_tracts[[i]] <- df
}

wecc_tracts <- bind_rows(wecc_tracts)
wecc_tracts <- st_transform(wecc_tracts, st_crs(5070))

# Identify just the census tracts that intersect the WECC
wecc_tracts <- st_filter(wecc_tracts, wecc_shp)

# Add the EJ data to the shapefile
all_comms <- all_comms %>% mutate(ID = str_pad(ID, 11, "left", "0"))
all_comms <- merge(
  all_comms, wecc_tracts, all.x = F, all.y = T, by.x="ID", by.y="GEOID10"
)

# There are 10 tracts in the shapefile that do not appear in the EJ data for 
# unknown reasons
# temp2 <- wecc_tracts[!(wecc_tracts$GEOID10 %in% comms$ID),]

# tribal_ct <- tribal_census_tracts()



# Indicators Available

# Environmental Exposure Indicators (1)
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# P_OZONE	Percentile for Ozone
# P_PM25	Percentile for Particulate Matter 2.5
# P_LDPNT	Percentile for Lead paint
# P_DSLPM	Percentile for  2017 Diesel particulate matter
# P_CANCR	Percentile for  2017 Air toxics cancer risk
# P_RESP	Percentile for  2017 Air toxics respiratory HI
# P_PTRAF	Percentile for Traffic proximity

# Environmental Effect Indicators (2)
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# P_PWDIS	Percentile for Wastewater discharge
# P_PNPL	Percentile for Superfund proximity
# P_PRMP	Percentile for RMP facility proximity
# P_PTSDF	Percentile for Hazardous waste proximity
# P_UST	Percentile for Underground storage tanks


# Sensitive Population Indicators (1)
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# P_UNDR5PCT	Percentile for % under age 5
# P_OVR64PCT	Percentile for % over age 64

# Socioeconomic Factor Indicators (2)
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# P_MINORPCT	Percentile for % people of color
# P_LWINCPCT	Percentile for % low income
# P_LESHSPCT	Percentile for % less than high school education
# P_LNGISPCT	Percentile for % linguistically isolated
# P_UNEMPPCT	Percentile for Unemployment rate
# P_VULEOPCT	Percentile for Demographic Index


all_comms <- all_comms %>% 
  rowwise() %>% 
  mutate(
    # Population
    na_pop1 = sum(is.na(P_UNDR5PCT), 
                  is.na(P_OVR64PCT)),
    na_pop2 = sum(is.na(P_MINORPCT), 
                  is.na(P_LWINCPCT), 
                  is.na(P_LESHSPCT),
                  is.na(P_LNGISPCT), 
                  is.na(P_UNEMPPCT),
                  is.na(P_VULEOPCT)),
    na_pop = na_pop1 + na_pop2,
    redo_Pop1 = sum(
      P_UNDR5PCT, P_OVR64PCT, na.rm = T)
    /(2 - na_pop1),
    redo_Pop2 = sum(P_MINORPCT, P_LWINCPCT, P_LESHSPCT, P_LNGISPCT, 
                    P_UNEMPPCT, P_VULEOPCT, na.rm=T)/(6 - na_pop2),
    redo_Pop = ifelse(na_pop < 4 & na_pop1 < 2, 
                      .5*sum(redo_Pop1, redo_Pop2, na.rm=T), NA),
    # Pollution
    na_pol1 = sum(is.na(P_OZONE), 
                  is.na(P_PM25), 
                  is.na(P_LDPNT), 
                  is.na(P_DSLPM), 
                  is.na(P_CANCR), 
                  is.na(P_RESP), 
                  is.na(P_PTRAF)
    ),
    na_pol2 = sum(is.na(P_PWDIS),
                  is.na(P_PNPL),
                  is.na(P_PRMP),
                  is.na(P_PTSDF),
                  is.na(P_UST)
    ),
    na_pol = na_pol1 + na_pol2,
    redo_Pol1 = sum(
      P_OZONE,P_PM25,P_LDPNT,P_DSLPM,P_CANCR,P_RESP,P_PTRAF, na.rm = T)
    /(7 - na_pol1),
    redo_Pol2 = sum(
      P_PWDIS, P_PNPL, P_PRMP, P_PTSDF, P_UST, na.rm=T)/(5 - na_pol2),
    redo_Pol = ifelse(na_pol < 4,  sum((2/3)*redo_Pol1, (1/3)*redo_Pol2, na.rm=T), NA)
  )

rescale_10 <- function(x){
  upper_x <- max(x, na.rm = T)
  y <- 10*x/upper_x
  return(y)
}

# Rescale the two subindexes
all_comms$redo_Pol_scaled <- rescale_10(all_comms$redo_Pol)
all_comms$redo_Pop_scaled <- rescale_10(all_comms$redo_Pop)

# Create the overall index
all_comms$DAC_score <- all_comms$redo_Pop_scaled * all_comms$redo_Pol_scaled

# Get Percentiles for the Pollution Score
temp <- all_comms[!is.na(all_comms$redo_Pol),]
temp$Pol_score_pctl <- ecdf(temp$redo_Pol)(temp$redo_Pol)
temp <- temp[,c("ID","Pol_score_pctl")]
all_comms <- left_join(all_comms, temp, by=c("ID"))

# Get Percentiles for the DAC Score
temp <- all_comms[!is.na(all_comms$DAC_score),]
temp$DAC_score_pctl <- ecdf(temp$DAC_score)(temp$DAC_score)
temp <- temp[,c("ID","DAC_score_pctl")]
all_comms <- left_join(all_comms, temp, by=c("ID"))

# Find the census tracts that overlap with tribal tracts
temp <- tribal_census_tracts(year = 2021)
temp <- st_transform(temp, crs = st_crs(5070))
temp <- temp[temp$ALAND/2589988 > 10,]
all_comms <- st_as_sf(all_comms, crs= st_crs(5070))
temp <- st_overlaps(all_comms, temp)
temp <- !unlist(lapply(temp, is_empty))
all_comms$tribal <- temp

# Classify areas as meeting DAC requirements
all_comms <- all_comms %>% 
  mutate(
    DAC_status = ifelse(
      DAC_score_pctl > 0.75 | Pol_score_pctl > 0.95 | tribal,
      "Disadvantaged", "Non-Disadvantaged"
    ),
    DAC_status  = ifelse(is.na(DAC_status), "Non-Disadvantaged", DAC_status)
  )



sum(all_comms$DAC_status == "Disadvantaged")
# 4587
sum(all_comms$DAC_status == "Disadvantaged")/nrow(all_comms)
# 0.2950408
sum(all_comms$DAC_score_pctl > 0.75, na.rm = T)/nrow(all_comms)
# 0.2488583
sum(all_comms$Pol_score_pctl > 0.95 & is.na(all_comms$DAC_score_pctl), na.rm = T)/nrow(all_comms)
# 0
sum(all_comms$tribal & (all_comms$DAC_score_pctl > 0.75), na.rm = T)/nrow(all_comms)
# 0.001093459
sum(all_comms$tribal & (all_comms$DAC_score_pctl <= 0.75), na.rm = T)/nrow(all_comms)
# 0.04013636

# Things to do next:
# 1. Create good percentiles for the whole thing
# 2. Set Up the two conditions we can reasonably work with and classify
# 3. Filter just to California and compare with those classifications

cal_comms <- all_comms %>% 
  filter(startsWith(ID, "06"))

cal_dacs <- read_excel(
  "Data/main/disadvantaged/CalEnviroScreen_2022/SB535DACresultsdatadictionary_F_2022.xlsx",
  sheet = "CES4.0FINAL_results",
  na = "NA"
)

cal_dacs$`Census Tract` <- str_pad(cal_dacs$`Census Tract`, 11, "left", "0")
temp <- merge(cal_comms, cal_dacs, by.x="ID", by.y="Census Tract")
temp <- temp %>% 
  mutate(
    DAC_status_og = ifelse(`CES 4.0 Percentile` >= 75, 
                           "Disadvantaged", "Non-disadvantaged"),
    DAC_status_og = ifelse(is.na(DAC_status_og), "Non-disadvantaged", 
                           DAC_status_og)
  )


pdf("Writing/Draft/figures/chapter5_figures/DAC_des_scatter.pdf",
    width = 7, height = 4)
ggplot(temp, aes(x=`CES 4.0 Percentile`, y=DAC_score_pctl*100, 
                 color =  paste(DAC_status, DAC_status_og))) +
  geom_point(alpha = 0.15) + 
  scale_color_discrete(
    labels = c('DAC : DAC', 'DAC : Non-DAC', 'Non-DAC : DAC', 'Non-DAC : Non-DAC'),
    name = "Rep. Status : Official Status"
    ) +
  labs(x = "\nCalEnviroScreen 4.0 Percentile",
       y = "Replicated DAC Percentile\n")
dev.off()

temp$DAC_status_pctl <- ifelse(temp$DAC_score_pctl > 0.75, "Disadvantaged", "Non-disadvantaged")

temp <- temp %>% 
  mutate(
    DAC_status_pctl = ifelse(DAC_score_pctl > 0.75, "Disadvantaged", "Non-disadvantaged"),
    DAC_status_pctl = ifelse(is.na(DAC_status_pctl),"Non-disadvantaged", DAC_status_pctl)
  )

pdf("Writing/Draft/figures/chapter5_figures/DAC_designation.pdf",
    width = 5, height = 4)
ggplot(temp, aes(x = DAC_status_og, fill=DAC_status_pctl)) +
  geom_bar(position = "fill", stat="count") +
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(x="\nDesignation in CalEnviroScreen 4.0") +
  scale_fill_discrete(name = "Replicated\nDesignation") + 
  theme(
    legend.position = "right",
    axis.title.y = element_blank()
  )
dev.off()

# st_set_geometry(temp, NULL) %>% count(DAC_status, DAC_status_og)

# summ_df <- st_set_geometry(all_comms, NULL) %>% 
#   filter(DAC_status == "Non-Disadvantaged") %>% 
#   mutate(
#     MINORPCT = MINORPCT*100,
#     LOWINCPCT = LOWINCPCT*100,
#     LESSHSPCT = LESSHSPCT*100,
#     UNDER5PCT = UNDER5PCT*100,
#     OVER64PCT = OVER64PCT*100,
#     UNEMPPCT = UNEMPPCT*100
#   ) %>% 
#   select(
#     P_PTRAF,
#     P_PWDIS,
#     P_PNPL,
#     P_PTSDF,
#     P_OZONE,
#     P_PM25,
#     ACSTOTPOP,
#     MINORPCT,
#     LOWINCPCT,
#     LESSHSPCT,
#     UNDER5PCT,
#     OVER64PCT,
#     UNEMPPCT
#   ) %>% 
#   as.data.frame()
# 
# 
# stargazer(summ_df, summary = TRUE, 
#           summary.stat = c("median", "mean", "sd"),
#           digits = 1,
#           covariate.labels = c(
#             '%-ile Traffic proximity',
#             '%-ile Wastewater discharge',
#             '%-ile Superfund proximity',
#             '%-ile Hazardous waste proximity',
#             '%-ile Ozone',
#             '%-ile Particulate Matter 2.5',
#             'Total Population',
#             '% People of color',
#             '% Low income',
#             '% Less than HS dipolma',
#             '% Under age 5',
#             '% Over age 64',
#             'Unemployment rate'
#           ))


# # Trying to Figure Out What California is doing
# #::::::::::::::::::::::::::::::::::::::::::::::::
# 
# cal_dacs <- read_excel(
#   "Data/main/disadvantaged/CalEnviroScreen_2022/SB535DACresultsdatadictionary_F_2022.xlsx", 
#   sheet = "CES4.0FINAL_results",
#   na = "NA"
#   )
# 
# df <- cal_dacs %>% 
#   rowwise() %>% 
#   mutate(
#     # Population
#     na_pop1 = sum(is.na(`Asthma Pctl`), 
#                   is.na(`Low Birth Weight Pctl`), 
#                   is.na(`Cardiovascular Disease Pctl`)),
#     na_pop2 = sum(is.na(`Education Pctl`), 
#                   is.na(`Linguistic Isolation Pctl`), 
#                   is.na(`Poverty Pctl`),
#                   is.na(`Unemployment Pctl`), 
#                   is.na(`Housing Burden Pctl`)),
#     na_pop = na_pop1 + na_pop2,
#     redo_Pop1 = sum(
#       `Asthma Pctl`, `Low Birth Weight Pctl`, `Cardiovascular Disease Pctl`, na.rm = T)
#     /(3 - na_pop1),
#     redo_Pop2 = sum(
#       `Education Pctl`, `Linguistic Isolation Pctl`, `Poverty Pctl`,
#       `Unemployment Pctl`, `Housing Burden Pctl`, na.rm=T)/(5 - na_pop2),
#     redo_Pop = ifelse(na_pop < 4, .5*sum(redo_Pop1, redo_Pop2, na.rm=T), NA),
#     error_Pop = `Pop. Char.` - redo_Pop,
#     # Pollution
#     na_pol1 = sum(is.na(`Ozone Pctl`), 
#                   is.na(`PM2.5 Pctl`), 
#                   is.na(`Lead Pctl`), 
#                   is.na(`Tox. Release Pctl`), 
#                   is.na(`Diesel PM Pctl`), 
#                   is.na(`Drinking Water Pctl`), 
#                   is.na(`Pesticides Pctl`), 
#                   is.na(`Traffic Pctl`)
#                   ),
#     na_pol2 = sum(is.na(`Cleanup Sites Pctl`),
#                   is.na(`Groundwater Threats Pctl`),
#                   is.na(`Imp. Water Bodies Pctl`),
#                   is.na(`Haz. Waste Pctl`),
#                   is.na(`Solid Waste Pctl`)
#                   ),
#     na_pol = na_pol1 + na_pol2,
#     redo_Pol1 = sum(
#       `Ozone Pctl`, `PM2.5 Pctl`, `Lead Pctl`, `Tox. Release Pctl`,
#       `Diesel PM Pctl`,`Drinking Water Pctl`,`Pesticides Pctl`,`Traffic Pctl`, na.rm = T)
#     /(8 - na_pol1),
#     redo_Pol2 = sum(
#       `Cleanup Sites Pctl`,`Groundwater Threats Pctl`,`Imp. Water Bodies Pctl`,
#       `Haz. Waste Pctl`,`Solid Waste Pctl`, na.rm=T)/(5 - na_pol2),
#     redo_Pol = ifelse(na_pol < 4,  sum((2/3)*redo_Pol1, (1/3)*redo_Pol2, na.rm=T), NA),
#     error_Pol = `Pollution Burden` - redo_Pol
#   ) 
# 
# rescale_10 <- function(x){
#   upper_x <- max(x, na.rm = T)
#   y <- 10*x/upper_x
#   return(y)
# }
# 
# df$redo_Pol_scaled <- rescale_10(df$redo_Pol)
# df$redo_Pop_scaled <- rescale_10(df$redo_Pop)
# 
# df <- df %>% 
#   mutate(
#     redo_CES = redo_Pop_scaled * redo_Pol_scaled,
#     error_rate = redo_CES - `CES 4.0 Score`
#   ) %>% 
#   select(
#     `Census Tract`, 
#     `Asthma Pctl`, `Low Birth Weight Pctl`, `Cardiovascular Disease Pctl`,
#     `Education Pctl`, `Linguistic Isolation Pctl`, `Poverty Pctl`,
#     `Unemployment Pctl`, `Housing Burden Pctl`, na_pop1, na_pop2, na_pop,
#     redo_Pop1, redo_Pop2, redo_Pop, `Pop. Char.`, error_Pop,
#     na_pol1, na_pol2, na_pol,
#     redo_Pol1, redo_Pol2, redo_Pol, `Pollution Burden`, error_Pol,
#     redo_Pop_scaled, redo_Pol_scaled, redo_CES, `CES 4.0 Score`, error_rate
#   )

temp <- st_set_geometry(all_comms, NULL) %>% 
  select(ID, DAC_status)
wecc_tracts <- merge(wecc_tracts, temp, by.x = "GEOID10", by.y = "ID")

rm(
  all_comms, cal_comms, cal_dacs, demand_sch, df, hourly_sim, sim_gen_hourly, 
  temp, i, my_state, my_states, rescale_10
)
gc()

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Environmental Inequality Gap ------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Metric 1: No Buffer ----------------------------------------------------------

temp <- st_set_geometry(wecc_tracts, NULL) %>% 
  select(GEOID10, DAC_status, STATEFP10, COUNTYFP10, ALAND10) %>% 
  # Just to avoid an issue
  mutate( 
    ALAND10 = ifelse(ALAND10 == 0, 1, ALAND10)
  )

annual_sim_sf <- st_as_sf(
  x = annual_sim, 
  coords = c("lon", "lat"), 
  crs =  "WGS84") %>% 
  st_transform(crs = st_crs(wecc_tracts))

plants_sf <- st_as_sf(
  x = plants, 
  coords = c("lon", "lat"), 
  crs =  "WGS84") %>% 
  st_transform(crs = st_crs(wecc_tracts))

ei_gap1 <- st_join(plants_sf, wecc_tracts)
ei_gap1 <- st_set_geometry(ei_gap1, NULL) %>% 
  select(plantID, GEOID10)
ei_gap1 <- merge(annual_sim, ei_gap1, by="plantID")
ei_gap1 <- ei_gap1 %>% 
  group_by(GEOID10, scenario, bca, tau) %>% 
  summarise(
    annual_nox = sum(annual_nox), 
    annual_so2 = sum(annual_so2), 
    annual_pm25 = sum(annual_pm25)
  )

temp_spt <- split(ei_gap1, ei_gap1$scenario)
ei_gap1 <- lapply(
  temp_spt, 
  function (x) {
    y = merge(temp, x, by = "GEOID10", all.x = T)
    my_scenario = unique(x$scenario)
    my_bca = unique(x$bca)
    my_tau = unique(x$tau)
    y$scenario = rep(my_scenario, nrow(y))
    y$bca = rep(my_bca, nrow(y))
    y$tau = rep(my_tau, nrow(y))
    return(y)
  }
)
ei_gap1 <- bind_rows(ei_gap1)
ei_gap1 <- ei_gap1 %>% 
  mutate_at(vars('annual_nox', 'annual_so2', 'annual_pm25'), 
            ~replace(., is.na(.), 0))

temp <- ei_gap1 %>% 
  group_by(DAC_status, scenario, bca, tau) %>% 
  summarise(
    annual_nox = mean(annual_nox/ALAND10*2589988),
    annual_so2 = mean(annual_so2/ALAND10*2589988),
    annual_pm25 = mean(annual_pm25/ALAND10*2589988)
  )

# NOx

pdf("Writing/Draft/figures/chapter5_figures/ei_gap_bca_nox.pdf",
    width = 5, height = 4)
ggplot(temp %>% filter(bca == 1), aes(x= tau, y=annual_nox, color = DAC_status)) +
  geom_line(lwd=2) +  
  geom_point(color = "white", size = 5) +
  geom_point(size = 4) +
  labs(x='\nCalifornia Emissions Price ($/tonne)', 
       y='Tonnes NOx per sq. mile\n') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank(), legend.position = "top")
dev.off()


pdf("Writing/Draft/figures/chapter5_figures/ei_gap_nobca_nox.pdf",
    width = 7, height = 4)
ggplot(temp %>% filter(bca == 0), aes(x= tau, y=annual_nox, color = DAC_status)) +
  geom_line(lwd=2) +  
  geom_point(color = "white", size = 5) +
  geom_point(size = 4) +
  labs(x='\nCalifornia Emissions Price ($/tonne)', 
       y='Tonnes NOx per sq. mile\n') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank(), legend.position = "top")
dev.off()

# wecc_tracts$DAC_status
# 
# stwecc_tracts %>%
#   group_by(STATEFP10) %>% 
#   summarize(
#     prop_dis = sum(DAC_status == "Disadvantaged")
#   )

# SO2

# pdf("Writing/Draft/figures/chapter5_figures/ei_gap_bca_so2.pdf",
#     width = 5, height = 4)
# ggplot(temp %>% filter(bca == 1), aes(x= tau, y=annual_so2, color = DAC_status)) +
#   geom_line(lwd=2) +  
#   geom_point(color = "white", size = 5) +
#   geom_point(size = 4) +
#   labs(x='\nCalifornia Emissions Price ($/tonne)', 
#        y='Tonnes SO2 per sq. mile\n') +
#   scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
#   theme(legend.title = element_blank(), legend.position = "top")
# dev.off()
# 

# pdf("Writing/Draft/figures/chapter5_figures/ei_gap_nobca_so2.pdf",
#     width = 5, height = 4)
# ggplot(temp %>% filter(bca == 0), aes(x= tau, y=annual_so2, color = DAC_status)) +
#   geom_line(lwd=2) +  
#   geom_point(color = "white", size = 5) +
#   geom_point(size = 4) +
#   labs(x='\nCalifornia Emissions Price ($/tonne)', 
#        y='Tonnes SO2 per sq. mile\n') +
#   scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
#   theme(legend.title = element_blank(), legend.position = "top")
# dev.off()


# SO2

# pdf("Writing/Draft/figures/chapter5_figures/ei_gap_bca_pm25.pdf",
#     width = 5, height = 4)
# ggplot(temp %>% filter(bca == 1), aes(x= tau, y=annual_pm25, color = DAC_status)) +
#   geom_line(lwd=2) +  
#   geom_point(color = "white", size = 5) +
#   geom_point(size = 4) +
#   labs(x='\nCalifornia Emissions Price ($/tonne)', 
#        y='Tonnes PM2.5 per sq. mile\n') +
#   scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
#   theme(legend.title = element_blank(), legend.position = "top")
# dev.off()
# 

# pdf("Writing/Draft/figures/chapter5_figures/ei_gap_nobca_pm25.pdf",
#     width = 5, height = 4)
# ggplot(temp %>% filter(bca == 0), aes(x= tau, y=annual_pm25, color = DAC_status)) +
#   geom_line(lwd=2) +  
#   geom_point(color = "white", size = 5) +
#   geom_point(size = 4) +
#   labs(x='\nCalifornia Emissions Price ($/tonne)', 
#        y='Tonnes PM2.5 per sq. mile\n') +
#   scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
#   theme(legend.title = element_blank(), legend.position = "top")
# dev.off()


p1 <- ggplot(temp %>% filter(bca == 1), aes(x= tau, y=annual_so2, 
                                            color = DAC_status)) +
  geom_line(lwd=2) +  
  geom_point(color = "white", size = 5) +
  geom_point(size = 4) +
  labs(x='\nCalifornia Emissions Price \n($/tonne)', 
       y='Tonnes SO2 per sq. mile\n') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank(), legend.position = "top")

p2 <- ggplot(temp %>% filter(bca == 1), aes(x= tau, y=annual_pm25, 
                                            color = DAC_status)) +
  geom_line(lwd=2) +  
  geom_point(color = "white", size = 5) +
  geom_point(size = 4) +
  labs(x='\nCalifornia Emissions Price \n($/tonne)', 
       y='Tonnes PM2.5 per sq. mile\n') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank(), legend.position = "top")

pdf("Writing/Draft/figures/chapter5_figures/ei_gap_so2_pm25.pdf",
    width = 6, height = 4)
ggpubr::ggarrange(p1, p2, ncol = 2, nrow = 1, labels = c("A","B"), 
                  common.legend = T)
dev.off()

# Just California things hahahha
temp <- ei_gap1 %>% 
  filter(STATEFP10 == "06") %>% 
  group_by(DAC_status, scenario, bca, tau) %>% 
  summarise(
    annual_nox = mean(annual_nox/ALAND10*2589988),
    annual_so2 = mean(annual_so2/ALAND10*2589988),
    annual_pm25 = mean(annual_pm25/ALAND10*2589988)
  )

pdf("Writing/Draft/figures/chapter5_figures/ei_gap_bca_nox_cal.pdf",
    width = 5, height = 4)
ggplot(temp %>% filter(bca == 1), aes(x= tau, y=annual_nox, color = DAC_status)) +
  geom_line(lwd=2) +  
  geom_point(color = "white", size = 5) +
  geom_point(size = 4) +
  labs(x='\nCalifornia Emissions Price ($/tonne)', 
       y='Tonnes NOx per sq. mile\n') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank(), legend.position = "top")
dev.off()

# pdf("Writing/Draft/figures/chapter5_figures/ei_gap_nobca_nox_cal.pdf",
#     width = 7, height = 4)
# ggplot(temp %>% filter(bca == 0), aes(x= tau, y=annual_nox, color = DAC_status)) +
#   geom_line(lwd=2) +  
#   geom_point(color = "white", size = 5) +
#   geom_point(size = 4) +
#   labs(x='\nCalifornia Emissions Price ($/tonne)', 
#        y='Tonnes NOx per sq. mi\n') +
#   scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
#   theme(legend.title = element_blank(), legend.position = "right")
# dev.off()

p1 <- ggplot(temp %>% filter(bca == 1), aes(x= tau, y=annual_so2, 
                                            color = DAC_status)) +
  geom_line(lwd=2) +  
  geom_point(color = "white", size = 5) +
  geom_point(size = 4) +
  labs(x='\nCalifornia Emissions Price \n($/tonne)', 
       y='Tonnes SO2 per sq. mile\n') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank(), legend.position = "top")

p2 <- ggplot(temp %>% filter(bca == 1), aes(x= tau, y=annual_pm25, 
                                            color = DAC_status)) +
  geom_line(lwd=2) +  
  geom_point(color = "white", size = 5) +
  geom_point(size = 4) +
  labs(x='\nCalifornia Emissions Price \n($/tonne)', 
       y='Tonnes PM2.5 per sq. mile\n') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank(), legend.position = "top")

pdf("Writing/Draft/figures/chapter5_figures/ei_gap_so2_pm25_cal.pdf",
    width = 6, height = 4)
ggpubr::ggarrange(p1, p2, ncol = 2, nrow = 1, labels = c("A","B"), 
                  common.legend = T)
dev.off()

rm(p1, p2, plants_sf, temp, temp_spt)
gc()

# Metric 2: X-Mile Buffer Zones

plant_coords <- st_as_sf(
    x = plants,
    coords = c("lon", "lat"),
    crs =  "WGS84") %>%
  st_transform(crs = st_crs(wecc_tracts))


ei_with_buffers <- function(X){
  
  buffer_WECC <- st_buffer(wecc_tracts, dist = 1000*X*0.621371)
  buffer_WECC$buffer_areas <- st_area(buffer_WECC)
  
  temp <- st_set_geometry(buffer_WECC, NULL) %>% 
    select(GEOID10, DAC_status, STATEFP10, COUNTYFP10, buffer_areas)
  
  ei_gap2 <- st_join(plant_coords, buffer_WECC)
  ei_gap2 <- st_set_geometry(ei_gap2, NULL) %>% 
    select(plantID, GEOID10)
  ei_gap2 <- merge(annual_sim, ei_gap2, by="plantID")
  
  ei_gap2 <- ei_gap2 %>% 
    group_by(GEOID10, scenario, bca, tau) %>% 
    summarise(
      annual_nox = sum(annual_nox), 
      annual_so2 = sum(annual_so2), 
      annual_pm25 = sum(annual_pm25)
    )
  
  temp_spt <- split(ei_gap2, ei_gap2$scenario)
  ei_gap2 <- lapply(
    temp_spt, 
    function (x) {
      y = merge(temp, x, by = "GEOID10", all.x = T)
      my_scenario = unique(x$scenario)
      my_bca = unique(x$bca)
      my_tau = unique(x$tau)
      y$scenario = rep(my_scenario, nrow(y))
      y$bca = rep(my_bca, nrow(y))
      y$tau = rep(my_tau, nrow(y))
      return(y)
    }
  )
  
  ei_gap2 <- bind_rows(ei_gap2)
  ei_gap2 <- ei_gap2 %>% 
    mutate_at(vars('annual_nox', 'annual_so2', 'annual_pm25'), 
              ~replace(., is.na(.), 0)) %>% 
    mutate(buffer_areas = units::drop_units(buffer_areas))
  
  temp <- ei_gap2 %>% 
    group_by(DAC_status, scenario, bca, tau) %>% 
    summarise(
      annual_nox = mean(annual_nox/buffer_areas*2589988),
      annual_so2 = mean(annual_so2/buffer_areas*2589988),
      annual_pm25 = mean(annual_pm25/buffer_areas*2589988)
    )
  temp$buffer_dist <- rep(X, nrow(temp))
  
  return(temp)
  
}

sample_dist <- list(0, 5, 10, 15)
ei_gap_buffered <- lapply(sample_dist, ei_with_buffers)
ei_gap_buffered <- bind_rows(ei_gap_buffered)


temp <- ei_gap_buffered %>% 
  filter(bca == 1) %>% 
  pivot_wider(names_from = DAC_status, values_from = 5:7) %>% 
  mutate(
    nox_diff = annual_nox_Disadvantaged - `annual_nox_Non-Disadvantaged`,
    so2_diff = annual_so2_Disadvantaged - `annual_so2_Non-Disadvantaged`,
    pm25_diff = annual_pm25_Disadvantaged - `annual_pm25_Non-Disadvantaged`
  )


p1 <- ggplot(temp, aes(x= tau, y=nox_diff, color = as.factor(buffer_dist))) +
  geom_line(lwd=2) +  
  geom_point(color = "white", size = 5) +
  geom_point(size = 4) +
  labs(x='\nCalifornia Emissions Price ($/tonne)', 
       y='Tonnes NOx per sq. mile\n') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank(), legend.position = "top")

p2 <- ggplot(temp, aes(x= tau, y=so2_diff, color = as.factor(buffer_dist))) +
  geom_line(lwd=2) +  
  geom_point(color = "white", size = 5) +
  geom_point(size = 4) +
  labs(x='\nCalifornia Emissions Price ($/tonne)', 
       y='Tonnes SO2 per sq. mile\n') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank(), legend.position = "top")

p3 <- ggplot(temp, aes(x= tau, y=pm25_diff, color = as.factor(buffer_dist))) +
  geom_line(lwd=2) +  
  geom_point(color = "white", size = 5) +
  geom_point(size = 4) +
  labs(x='\nCalifornia Emissions Price ($/tonne)', 
       y='Tonnes PM2.5 per sq. mile\n') +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme(legend.title = element_blank(), legend.position = "top")

pdf("Writing/Draft/figures/chapter5_figures/ei_gap_buffers.pdf",
    width = 5, height = 9)
ggpubr::ggarrange(p1, p2, p3, ncol = 1, nrow = 3, labels = c("A","B","C"), 
                  common.legend = T)
dev.off()


# # NOx
# 
# pdf("Writing/Draft/figures/chapter5_figures/ei_gap_bca_nox.pdf",
#     width = 5, height = 4)
# ggplot(temp %>% filter(bca == 1), aes(x= tau, y=annual_nox, color = DAC_status)) +
#   geom_line(lwd=2) +  
#   geom_point(color = "white", size = 5) +
#   geom_point(size = 4) +
#   labs(x='\nCalifornia Emissions Price ($/tonne)', 
#        y='Tonnes NOx per sq. mile\n') +
#   scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
#   theme(legend.title = element_blank(), legend.position = "top")
# dev.off()
# 
# 


# ex_buffer <- st_buffer(wecc_tracts[20,], dist = 1000*30*0.621371)
# ex_buffer$buffer_area <- paste(
#   "Area =", round(ex_buffer$buffer_area/2589988, digits=0), "mi2"
#   )

# table_test <- st_join(annual_sim_sf %>% filter(tau == 0, bca == 1),
#                       wecc_tracts %>% select(GEOID10, DAC_status))
# table_test <- st_set_geometry(table_test, NULL)
# 
# entry_1 <- table_test %>% filter(DAC_status == "Non-Disadvantaged") %>% 
#   group_by(region) %>% 
#   summarise(annual_gen = sum(annual_gen))
# 
# 53666799/(53666799 + 154538239 + 55743529)
# 16523497/(16523497 + 29151151 + 55337023)
# 
# entry_1 <- table_test %>% filter(DAC_status == "Disadvantaged")
# mean(entry_1$co2e_mmbtu)
# hist(entry_1$co2e_mmbtu * entry_1$hrate)


### Metric 2: The buffer 

# # Loop through the states grabbing all their census tracts (2010 tracts)
# # all_tracts <- list()
# 
# # for (i in 1:length(my_states)){
# #   my_state <- my_states[i]
# #   df <- tracts(state = my_state, year=2010)
# #   all_tracts[[i]] <- df
# # }
# # 
# # all_tracts <- bind_rows(all_tracts) %>% 
# #   st_transform(crs = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
# # all_tracts <- all_tracts[all_tracts$GEOID10 %in% wecc_tracts$GEOID10,]
# # setcrs("+proj=longlat +datum=WGS84 +no_defs +type=crs")
# 
# plant_coords <- st_as_sf(
#     x = plants, 
#     coords = c("lon", "lat"), 
#     crs =  "WGS84") %>% 
#   st_transform(crs = st_crs(wecc_tracts))
# 
# get_plants_within <- function(x) {
#   
#   dist_list <- st_is_within_distance(
#     plant_coords, 
#     wecc_tracts[x,], 
#     dist = 1000*30*0.621371
#   ) 
#   
#   dist_vec <- unlist(lapply(dist_list, function (x) ifelse(is_empty(x), 0, 1)))
#   
#   target_index <- which(dist_vec == 1)
#   
#   target_plants <- plant_coords$plantID[target_index]
#   
#   my_rows <- length(target_plants)
#   
#   temp_df <- data.frame(
#     ID = rep(wecc_tracts$GEOID10[x], my_rows),
#     plantID = target_plants
#   )
#   
#   return(temp_df)
#   
# }
# 
# tracts_to_plants <- lapply(as.list(1:nrow(wecc_tracts)), get_plants_within)
# tracts_to_plants <- bind_rows(tracts_to_plants)
# gc()
# 
# ei_gap <- tracts_to_plants
# ei_gap <- merge(ei_gap, annual_sim, by="plantID", all.x=T)
# 
# ei_gap <- ei_gap %>% 
#   group_by(ID, scenario, bca, tau) %>% 
#   summarize(
#     annual_co2e = sum(annual_co2e),
#     annual_nox = sum(annual_nox),
#     annual_so2 = sum(annual_so2),
#     annual_pm25 = sum(annual_pm25)
#   )
# 
# 
# # Get the area of the buffered zone for each census tract
# buffer_areas <- st_buffer(wecc_tracts, dist = 1000*30*0.621371)
# buffer_areas <- st_area(buffer_areas)
# wecc_tracts$buffer_area <- buffer_areas
# 
# # Show an example of this pollution concentration measurement
# ex_buffer <- st_buffer(wecc_tracts[20,], dist = 1000*30*0.621371)
# ex_buffer$buffer_area <- paste(
#   "Area =", round(ex_buffer$buffer_area/2589988, digits=0), "mi2"
#   )
#                                
# ex_plants <- st_is_within_distance(
#   plant_coords, 
#   wecc_tracts[20,], 
#   dist = 1000*30*0.621371
# ) 
# ex_plants <- unlist(lapply(ex_plants, function (x) ifelse(is_empty(x), 0, 1)))
# ex_plants <- plant_coords[which(ex_plants == 1),]
# ex_plants <- merge(ex_plants, annual_sim %>% filter(scenario == "a"), by = "plantID")
# 
# ex_counties <- c("103", "021", "007", "063", "089", "035")
# 
# ex_plants$annual_nox <- paste(round(ex_plants$annual_nox, digits = 0), "tonnes")
# ex_plants$annual_so2 <- paste(round(ex_plants$annual_so2, digits = 0), "tonnes")
# ex_plants$annual_pm25 <- paste(round(ex_plants$annual_pm25, digits = 0), "tonnes")
# 
# pdf("Writing/Draft/figures/chapter5_figures/ex_buffer.pdf",
#     width = 5, height = 4)
# tm_shape(ex_buffer, unit = "mi") +
#   tm_borders(alpha=0) +
#   tm_shape(
#     wecc_tracts %>% filter(STATEFP10 == "06", COUNTYFP10 %in% ex_counties)
#     ) +
#   tm_borders(col = "gray", alpha = .5) +
#   tm_shape(ex_buffer, unit = "mi") +
#   tm_borders(col = "#880011", lwd = 3) +
#   tm_shape(wecc_tracts[20,]) +
#   tm_polygons(col =  natparks.pals(name = "Charmonix", n=7)[3]) +
#   tm_text("GEOID10", col = "white") +
#   tm_shape(ex_plants) +
#   tm_dots(col = "black", size = .75) + 
#   tm_text("annual_nox", ymod = 1, col = "black") + 
#   tm_shape(ex_buffer) +
#   tm_text("buffer_area", ymod = -7.5, xmod = 7, col = "#880011") + 
#   tm_compass(type = "arrow", position = c(0.85, 0.85)) +
#   tm_scale_bar() + 
#   tm_layout(attr.position = c(0.65, 0.01))
# dev.off()
# 
# # Bring in the simulated pollution totals for each census tract
# 
# ei_gap <- split(ei_gap, ei_gap$scenario)
# ei_gap <- lapply(
#   ei_gap, 
#   function (x) {
#     y = merge(wecc_tracts, x, by. = "GEOID10", by.y = "ID", all.x = T)
#     my_scenario = unique(x$scenario)
#     my_bca = unique(x$bca)
#     my_tau = unique(x$tau)
#     y$scenario = rep(my_scenario, nrow(y))
#     y$bca = rep(my_bca, nrow(y))
#     y$tau = rep(my_tau, nrow(y))
#     return(y)
#   }
# )
# ei_gap <- bind_rows(ei_gap)
# 
# ei_gap <- ei_gap %>% 
#   mutate_at(vars('annual_co2e', 'annual_nox', 'annual_so2', 
#                  'annual_pm25'), ~replace(., is.na(.), 0))
# 
# ei_gap <- merge(
#   ei_gap, 
#   st_set_geometry(all_comms %>% select(ID, DAC_status), NULL), 
#   by.x = "GEOID10", by.y = "ID")
# 
# gc()
# 
# temp <- st_set_geometry(ei_gap, NULL) %>% 
#   filter(bca == 1) %>% 
#   group_by(scenario, tau, DAC_status) %>% 
#   summarise(
#     annual_co2e = mean(annual_co2e/(buffer_area/2589988)),
#     annual_nox = mean(annual_nox/(buffer_area/2589988)),
#     annual_so2 = mean(annual_so2/(buffer_area/2589988)),
#     annual_pm25 = mean(annual_pm25/(buffer_area/2589988))
#   )
# 
# ggplot(split_ct_poll, aes(x= tau, y=annual_nox, fill = DAC_status)) +
#   geom_bar(stat = 'identity', fun = mean, position = 'fill') + 
#   labs(x='\nCalifornia Emissions Price ($/tonne)', 
#        y='% of Generation\n') +
#   scale_y_continuous(labels = scales::percent_format()) +
#   scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
#   theme(legend.title = element_blank())
# 
# #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# # library(raster)
# # library(rasterize)
# 
### Metric 3: The grid
# 
# test_sim <- st_as_sf(annual_sim, coords = c('lon','lat'), crs = st_crs(plant_coords))
# test_grid <- st_make_grid(wecc_shp, n = c(578,249))
# 
# 578 * 249
# 

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Reference Data & Figures ----------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

### WECC Map

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
wecc_states <- st_filter(wecc_states, wecc_shp)

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


### Allowance Prices in California 

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


### Carbon Pricing Map 

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





