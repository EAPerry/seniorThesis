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
library(stargazer)
library(reticulate)

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

# Quick Check
df <- read_excel("Data/main/eGRID/egrid2019_data.xlsx", 
                 sheet="PLNT19",
                 skip = 1)

df <- df %>% 
  filter(
    NERC == "WECC",
    PLFUELCT %in% c("COAL", "GAS", "OIL")
  )

sum(df$NAMEPCAP)


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
#   3. Distribution of heat rates and emissions factors by fuel type    X
#   4. Distribution of generators by region                             X
#   5. Distribution of capacity by region                               X

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


ggplot(temp, aes(x=hrate, y=fuel_cat, fill=fuel_cat)) + 
  geom_boxplot() + 
  ylab("") +
  xlab('\nmmBTU/MWh') +
  theme(legend.title = element_blank(), legend.position = "none")


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
  labs(x = "\nDate", y = "Price per mmBTU\n") + 
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
  labs(x = "\nDate", y = "Price per mmBTU\n") + 
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
gens <- left_join(gens, fuel_pr, by=c("region", "fuel_cat"))

# Output table of fuel prices
fuel_pr <- fuel_pr %>% 
  filter(region != "National") %>% 
  mutate(fuel_price = round(fuel_price, 3))
colnames(fuel_pr) <- c("Fuel", "Region", "Price ($/mmBTU)")

stargazer(
  fuel_pr,
  summary = F,
  digits = 2,
  out = "Results/cleaning-tables/fuel-prices.tex",
  style = "aer"
)

summ_df <- gens %>% 
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
    fuel_cat, age, cap_fac, capacity, hrate, usd_MWh, co2e_MWh, nox_MWh, so2_MWh, pm25_MWh
  )

# temp <- summ_df %>% 
#   filter(fuel_cat == "Oil") %>% 
#   select(-c("fuel_cat"))
# temp <- as.data.frame(temp)
# 
# stargazer(temp, summary = TRUE, summary.stat = c("mean", "sd", "min", "median", "max"),
#           covariate.labels = c("Generator Age (years)", 
#                                "Capacity Factor",
#                                "Capacity (MW)",
#                                "Heat Rate (mmBTU/MWh)",
#                                "Input Price (\\$/MWh)",
#                                "tonnes CO$2$e/MWh",
#                                "kg NO$_x$/MWh",
#                                "kg SO$_2$/MWh",
#                                "kg PM2.5/MWh"))

rm(coal_pr, gas_pr, oil_pr, fuel_pr, nw, sw, temp, summ_df)

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
    width = 4, height = 6)
ggplot(demand_sch, aes(x= resid_D, fill = Region)) +
  facet_wrap(facets = ~Region, nrow = 3) + 
  geom_histogram(color = "white") + 
  labs(x = "\nResidual Demand (MW)", y = "Frequency\n") + 
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


# # For now, we'll ignore the negative residual demand situation
# 
# bad_resid_D <- demand_sch %>% 
#   filter(resid_D <= 0)
# 
# L <- bad_resid_D %>% 
#   add_count(`UTC time`) %>% 
#   filter(n > 1)
# 
# bad_cal <- bad_resid_D %>% 
#   filter(Region == "CAL")

# For K-Means clustering, I'll actually want to do a little pivot wide here

# pre_clstr_D <- demand_sch %>% 
#   pivot_wider(
#     names_from = Region,
#     values_from = resid_D
#   )
# 
# set.seed(1989)
# km_D <- kmeans(pre_clstr_D[5:7], 24*3, nstart = 25)
# 
# km_D <- as.data.frame(km_D$centers)
# 
# rm(df, pre_clstr_D, i, r)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## K-Means Clustering ----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### K-Means on Generators ------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Goal---get all the generators into 15 groups:
# Rules:
#   1. Same group generators must be in the same region
#   2. Same group generators must use the same fuel
# 
# Each Region Gets 5 Groups:
#   1. Coal Group
#   2-4. 3 Gas Groups
#   5. Oil Group

cluster_i <- list()
gens_list_i <- split(gens, gens$region)

set.seed(1989)

for (i in 1:length(gens_list_i)){
  
  gens_i <- gens_list_i[[i]]
  gens_list_j <- split(gens_i, gens_i$fuel_cat)
  
  cluster_j <- list()
  
  for (j in 1:length(gens_list_j)){
    
    gens_j <- gens_list_j[[j]]
    
    # Pick K based on the fuel
    k_picker <- c("Coal" = 1, "Gas" = 3, "Oil" = 1)
    K <- k_picker[unique(gens_j$fuel_cat)]
    
    temp <- gens_j %>% 
      select(hrate)
    
    km_j <- kmeans(temp, K, nstart = 25)
    
    gens_j$group <- paste(
        unique(gens_j$region), unique(gens_j$fuel_cat), km_j$cluster
      )
    
    cluster_j[[j]] <- gens_j
    
  }
  
  cluster_i[[i]] <- bind_rows(cluster_j)
  
}

cluster_gens <- bind_rows(cluster_i)

# unique(cluster_gens$group)
# temp <- cluster_gens %>% 
#   group_by(group) %>% 
#   summarise(
#     count = n(),
#     hrate_mean = mean(hrate),
#     hrate_median = median(hrate),
#     hrate_sd = sd(hrate),
#     co2e_mmbtu = mean(co2e_mmbtu),
#     nox_mmbtu = mean(nox_mmbtu),
#     so2_mmbtu = mean(so2_mmbtu),
#     pm25_mmbtu = mean(pm25_mmbtu)
#   )

cluster_gens <- cluster_gens %>% 
  rowwise() %>% 
  mutate(
    group = unlist(str_split(group, pattern = " "))[3],
    group = ifelse(fuel_cat == "Coal" | fuel_cat == "Oil", "", group),
    group = str_trim(paste(region, fuel_cat, group))
    # group = paste(
    #   str_pad(region, 12, "right", " "), 
    #   str_pad(str_trim(paste(fuel_cat, group)), 8, "left", " "), sep=""
    # )
  )

pdf("Writing/Draft/figures/chapter5_figures/kclusters_hrate.pdf",
    width = 6, height = 5.5)
ggplot(cluster_gens, aes(x=hrate, y=group, fill=fuel_cat)) + 
  geom_boxplot() + 
  ylab("") +
  xlab('\nHeat Rate (mmBTU/MWh)') +
  scale_y_discrete(limits=rev) +
  theme(legend.title = element_blank(), legend.position = "none")
dev.off()

pdf("Writing/Draft/figures/chapter5_figures/kclusters_cap_fac.pdf",
    width = 6, height = 6)
ggplot(cluster_gens, aes(x=cap_fac, y=group, fill=fuel_cat)) + 
  geom_boxplot() + 
  ylab("") +
  xlab('\nCapacity Factor') +
  scale_y_discrete(limits=rev) +
  theme(legend.title = element_blank(), legend.position = "top")
dev.off()

out_cluster_gens <- cluster_gens[15:22]

out_cluster_gens <- out_cluster_gens %>% 
  group_by(group) %>% 
  summarise(
    capacity = sum(capacity),
    hrate = mean(hrate),
    co2e_mmbtu = mean(co2e_mmbtu),
    nox_mmbtu = mean(nox_mmbtu),
    so2_mmbtu = mean(so2_mmbtu),
    pm25_mmbtu = mean(pm25_mmbtu),
    fuel_price = mean(fuel_price)
  )

write_csv(out_cluster_gens, "Results/cleaning-tables/kmeans_gens.csv")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### K-Means on Demand ----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

cluster_demand <- demand_sch %>% 
  pivot_wider(
    names_from = Region,
    values_from = resid_D
  )

cluster_demand <- cluster_demand %>% 
  mutate(
    total_d = California + Northwest + Southwest
  )

mean(cluster_demand$total_d)

set.seed(1989)
cluster_demand <- kmeans(cluster_demand[5:7], 24, nstart = 25)
cluster_demand <- as.data.frame(cluster_demand$centers)
cluster_demand$hour <- 1:24
cluster_demand <- cluster_demand[c("hour", "California", "Northwest", "Southwest")]

write_csv(cluster_demand, "Results/cleaning-tables/kmeans_demand.csv")

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

# Create the overal index
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

ggplot(temp, aes(x=`CES 4.0 Percentile`, y=DAC_score_pctl, 
                 color =  paste(DAC_status, DAC_status_og))) +
  geom_point()

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
    
























#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Reference Data & Figures ----------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

### WECC Map -------------------------------------------------------------------

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


### Allowance Prices in California ---------------------------------------------

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


### Carbon Pricing Map ---------------------------------------------------------

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





