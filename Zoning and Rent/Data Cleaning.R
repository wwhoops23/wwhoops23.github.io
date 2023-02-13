
library(tidyverse)
library(dplyr)
library(stringr)


#######################################
# Import Data
#######################################

pluto_2020 <- read.csv("pluto_20v8.csv") 
pluto_2020 <- pluto_2020 %>%
  select(bbl, borough, block, lot, residfar, builtfar, zonedist1, splitzone, 
         resarea, lotarea, ct2010, cb2010)

pluto_2019 <- read.csv("pluto_19v2.csv") 
pluto_2019 <- pluto_2019 %>%
  select(bbl, borough, block, lot, residfar, builtfar, zonedist1, splitzone, 
         resarea, lotarea, ct2010, cb2010)

pluto_2018 <- read.csv("pluto_18v2_1.csv") 
pluto_2018 <- pluto_2018 %>%
  select(bbl, borough, block, lot, residfar, builtfar, zonedist1, splitzone,
         resarea, lotarea, ct2010, cb2010)

pluto_2017_mt <- read.csv("MN2017V11.csv") 
pluto_2017_mt <- pluto_2017_mt %>%
  select(Borough, Block, Lot, ResidFAR, BuiltFAR, ZoneDist1, SplitZone, 
         ResArea, LotArea, CT2010, CB2010) %>%
  mutate(bbl = paste(pluto_2017_mt$Borough,"0", 
                     sprintf("%04d",pluto_2017_mt$Block),
                     sprintf("%04d",pluto_2017_mt$Lot), 
                     sep=""))

pluto_2017_qn <- read.csv("QN2017V11.csv") 
pluto_2017_qn <- pluto_2017_qn %>%
  select(Borough, Block, Lot, ResidFAR, BuiltFAR, ZoneDist1, SplitZone, 
         ResArea, LotArea, CT2010, CB2010) %>%
  mutate(bbl = paste(pluto_2017_qn$Borough,"0", 
                     sprintf("%04d",pluto_2017_qn$Block),
                     sprintf("%04d",pluto_2017_qn$Lot), 
                     sep=""))

pluto_2017_bx <- read.csv("BX2017V11.csv") 
pluto_2017_bx <- pluto_2017_bx %>%
  select(Borough, Block, Lot, ResidFAR, BuiltFAR, ZoneDist1, SplitZone, 
         ResArea, LotArea, CT2010, CB2010) %>%
  mutate(bbl = paste(pluto_2017_bx$Borough,"0", 
                     sprintf("%04d",pluto_2017_bx$Block),
                     sprintf("%04d",pluto_2017_bx$Lot), 
                     sep=""))

pluto_2017_bk <- read.csv("BK2017V11.csv") 
pluto_2017_bk <- pluto_2017_bk %>%
  select(Borough, Block, Lot, ResidFAR, BuiltFAR, ZoneDist1, SplitZone,
         ResArea, LotArea, CT2010, CB2010) %>%
  mutate(bbl = paste(pluto_2017_bk$Borough,"0", 
                     sprintf("%04d",pluto_2017_bk$Block),
                     sprintf("%04d",pluto_2017_bk$Lot), 
                     sep=""))

pluto_2017_si <- read.csv("SI2017V11.csv") 
pluto_2017_si <- pluto_2017_si %>%
  select(Borough, Block, Lot, ResidFAR, BuiltFAR, ZoneDist1, SplitZone,
         ResArea, LotArea, CT2010, CB2010) %>%
  mutate(bbl = paste(pluto_2017_si$Borough,"0", 
                     sprintf("%04d",pluto_2017_si$Block),
                     sprintf("%04d",pluto_2017_si$Lot), 
                     sep=""))

#######################################
# Combine Data
#######################################


pluto_2017 <- rbind(pluto_2017_bk, pluto_2017_bx, pluto_2017_mt, pluto_2017_qn, pluto_2017_si )

names(pluto_2020) <- toupper(names(pluto_2020))
names(pluto_2019) <- toupper(names(pluto_2019))
names(pluto_2018) <- toupper(names(pluto_2018))
names(pluto_2017) <- toupper(names(pluto_2017))


pluto_2020$YEAR <- 2020
pluto_2019$YEAR <- 2019
pluto_2018$YEAR <- 2018
pluto_2017$YEAR <- 2017

pluto_full <- rbind(pluto_2020, pluto_2019, pluto_2018, pluto_2017)


#######################################
# Add columns
#######################################

#Clean and create new columns
pluto_full <- pluto_full %>%
  #Clean columns
  mutate(RESIDFAR = ifelse(ZONEDIST1=="R9-1", 6.02,RESIDFAR),
        ZONEDIST1 = ifelse(ZONEDIST1=="",NA,ZONEDIST1),
  #Calc resid area
        TOTAREA = LOTAREA*RESIDFAR
  )

#Create lags
pluto_full <- pluto_full %>%
arrange(BBL, YEAR) %>%
  group_by(BBL) %>%
  #Calc lags
  mutate(RESIDFAR.lag = lag(RESIDFAR, n=1, default = NA),
        Zoning.lag = lag(ZONEDIST1, n=1, default=NA),
        TOTAREA.lag = lag(TOTAREA, n=1, default=NA)
  )

#Calc diffs
pluto_full <- pluto_full %>%
  mutate(YoY_residfar = coalesce(RESIDFAR - RESIDFAR.lag,0),
        YOY_totarea = coalesce(TOTAREA-TOTAREA.lag,0),
        Zoning.diff = ifelse(ZONEDIST1 != Zoning.lag,1,0),
        YoY.diff = ifelse(YoY_residfar != 0, 1,0)
  )

#Clean Space
rm(pluto_2017, pluto_2018, pluto_2019, pluto_2020)
rm(pluto_2017_mt, pluto_2017_bx, pluto_2017_bk, pluto_2017_si, pluto_2017_qn)


#######################################
# Tract aggregation
#######################################

# Tract aggregation
pluto_full.tract <- pluto_full %>%
  group_by(BOROUGH, CT2010, YEAR) %>%
  summarise(min_far = min(RESIDFAR, na.rm = TRUE),
            max_far = max(RESIDFAR, na.rm = TRUE),
            avg_far = mean(RESIDFAR, na.rm = TRUE),
            totarea = sum(TOTAREA, na.rm = TRUE),
            totarea.lag = sum(TOTAREA.lag, na.rm = TRUE),
            tot_zones = n_distinct(ZONEDIST1),
            tot_block = n_distinct(BLOCK),
            tot_lots = n(),
            tot_diffs = sum(YoY.diff, na.rm = TRUE)
  )

###########################################
# Rent / Demographic Data
###########################################

##Rent:

acs.nycrent.2020 <- read_csv("ACSDP5Y2020.DP04.csv") %>% 
  select(NAME, DP04_0134E) %>% 
  filter(NAME != "Geographic Area Name") %>% 
  mutate(DP04_0134E = as.integer(DP04_0134E),
         year = 2020) %>% 
  rename(median_rent = DP04_0134E) %>% 
  separate(NAME, c("census_tract", "county_tract", "state_tract"),
           sep = ", ") 

acs.nycrent.2019 <- read_csv("ACSDP5Y2019.DP04.csv") %>% 
  select(NAME, DP04_0134E) %>% 
  filter(NAME != "Geographic Area Name") %>% 
  mutate(DP04_0134E = as.integer(DP04_0134E),
         year = 2019) %>% 
  rename(median_rent = DP04_0134E) %>% 
  separate(NAME, c("census_tract", "county_tract", "state_tract"),
           sep = ", ") 

acs.nycrent.2018 <- read_csv("ACSDP5Y2018.DP04.csv") %>% 
  select(NAME, DP04_0134E) %>% 
  filter(NAME != "Geographic Area Name") %>% 
  mutate(DP04_0134E = as.integer(DP04_0134E),
         year = 2018) %>% 
  rename(median_rent = DP04_0134E) %>% 
  separate(NAME, c("census_tract", "county_tract", "state_tract"),
           sep = ", ") 

acs.nycrent.2017 <- read_csv("ACSDP5Y2017.DP04.csv") %>% 
  select(NAME, DP04_0134E) %>% 
  filter(NAME != "Geographic Area Name") %>% 
  mutate(DP04_0134E = as.integer(DP04_0134E), 
         year = 2017) %>% 
  rename(median_rent = DP04_0134E) %>% 
  separate(NAME, c("census_tract", "county_tract", "state_tract"),
           sep = ", ") 

acs.nycrent.2017_20 <- bind_rows(acs.nycrent.2017, acs.nycrent.2018, acs.nycrent.2019, 
                                 acs.nycrent.2020)

acs.nycrent.2017_20 <- acs.nycrent.2017_20 %>% 
  mutate(census_tract = (str_replace(census_tract,"Census Tract ", "")))

--------------------------------------------------------------------------------
  ##Demographics:
  
  acs.nyc.2020 <- read_csv("ACSDP5Y2020.DP05.csv") %>% 
  select(NAME, DP05_0001E, DP05_0037PE, DP05_0038PE,
         DP05_0039PE, DP05_0044PE, DP05_0052PE, DP05_0070PE) %>% 
  filter(NAME != "Geographic Area Name") %>% 
  mutate(DP05_0001E = as.numeric(DP05_0001E),
         year = 2020)  %>% 
  rename(pop = DP05_0001E, percent_white = DP05_0037PE,
         percent_black = DP05_0038PE, percent_aian =DP05_0039PE, percent_asian = DP05_0044PE, 
         percent_nhpi = DP05_0052PE,
         percent_hispanic_latino = DP05_0070PE )  %>% 
  separate(NAME, c("census_tract", "county_tract", "state_tract"),
           sep = ", ") 

acs.nyc.2019 <- read_csv("ACSDP5Y2019.DP05.csv") %>% 
  select(NAME, DP05_0001E, DP05_0037PE, DP05_0038PE,
         DP05_0039PE, DP05_0044PE, DP05_0052PE, DP05_0070PE) %>% 
  filter(NAME != "Geographic Area Name") %>% 
  mutate(DP05_0001E = as.numeric(DP05_0001E),
         year = 2019)  %>% 
  rename(pop = DP05_0001E, percent_white = DP05_0037PE,
         percent_black = DP05_0038PE, percent_aian =DP05_0039PE, percent_asian = DP05_0044PE, 
         percent_nhpi = DP05_0052PE,
         percent_hispanic_latino = DP05_0070PE )  %>% 
  separate(NAME, c("census_tract", "county_tract", "state_tract"),
           sep = ", ") 

acs.nyc.2018 <- read_csv("ACSDP5Y2018.DP05.csv") %>% 
  select(NAME, DP05_0001E,DP05_0037PE, DP05_0038PE,
         DP05_0039PE, DP05_0044PE, DP05_0052PE, DP05_0070PE) %>% 
  filter(NAME != "Geographic Area Name") %>% 
  mutate(DP05_0001E = as.numeric(DP05_0001E),
         year = 2018)  %>% 
  rename(pop = DP05_0001E, percent_white = DP05_0037PE,
         percent_black = DP05_0038PE, percent_aian =DP05_0039PE, percent_asian = DP05_0044PE, 
         percent_nhpi = DP05_0052PE,
         percent_hispanic_latino = DP05_0070PE )  %>% 
  separate(NAME, c("census_tract", "county_tract", "state_tract"),
           sep = ", ") 

acs.nyc.2017 <- read_csv("ACSDP5Y2017.DP05.csv") %>% 
  select(NAME, DP05_0001E, DP05_0037PE, DP05_0038PE,
         DP05_0039PE, DP05_0044PE, DP05_0052PE, DP05_0070PE) %>% 
  filter(NAME != "Geographic Area Name") %>% 
  mutate(DP05_0001E = as.numeric(DP05_0001E),
         year = 2017)  %>% 
  rename(pop = DP05_0001E, percent_white = DP05_0037PE,
         percent_black = DP05_0038PE, percent_aian =DP05_0039PE, percent_asian = DP05_0044PE, 
         percent_nhpi = DP05_0052PE,
         percent_hispanic_latino = DP05_0070PE )  %>% 
  separate(NAME, c("census_tract", "county_tract", "state_tract"),
           sep = ", ") 

#AIAN = American Indian and Alaska native
#NHPI = Native Hawaiian and other Pacific islander:

acs.nyc.2017_20 <- bind_rows(acs.nyc.2017, acs.nyc.2018, acs.nyc.2019, 
                             acs.nyc.2020)

acs.nyc.2017_20 <- acs.nyc.2017_20 %>% 
  mutate(census_tract = (str_replace(census_tract,"Census Tract ", "")))

##Rent and Demographics Combined for 2017-2020

rent <- acs.nycrent.2017_20 %>% left_join(acs.nyc.2017_20) 
rent <- rent %>%
  mutate(census_tract = as.numeric(census_tract)) %>%
  mutate(borough = ifelse(county_tract == "Kings County", "BK",
                          ifelse(county_tract == "Bronx County", "BX",
                                 ifelse(county_tract == "Queens County", "QN", 
                                        ifelse(county_tract == "New York County", "MN", "SI"
                                        )
                                 )
                          )
  )
  )

#######################################
# Rent-Zoning Data
#######################################

zoning_rent.tract <- pluto_full.tract %>%
  left_join(rent, by = c("CT2010" = "census_tract",  "YEAR" = "year", "BOROUGH" = "borough")) %>%
  select(BOROUGH, CT2010, YEAR, totarea, median_rent, percent_white) %>%
  drop_na() %>%
  mutate(BCT = paste(BOROUGH, CT2010, sep=""))

#######################################
# Rent-Zoning Data
#######################################

zoning_rent.tract <- zoning_rent.tract %>%
  arrange(BOROUGH,CT2010, YEAR) %>%
  group_by(BOROUGH,CT2010) %>%
    mutate(rent_YOY = coalesce(median_rent - lag(median_rent, n=1, default=NA),0),
             RESAREA_YOY = coalesce(totarea - lag(totarea, n=1, default=NA),0)) %>%
    mutate(change_flag = ifelse(RESAREA_YOY != 0,1,0)) %>%
  ungroup()

zoning_rent.tract <- zoning_rent.tract %>%
  mutate(density = ifelse(totarea>= 2288040,1,0))

saveRDS(zoning_rent.tract, file = "CombinedData.RData")

  




