
# Use install.library(dplyr) to add this package if have not already done so
library(dplyr)

# Add country/commodity data. Must change this each time and check the seperator + use as.numeric function to ensure read in correctly
country_commodity <- read.csv("GCP/Peru/PERU_PALM_2012_2015.csv",header = T, sep = ";")
  country_commodity$TOTAL.FOB.Value.US <- as.numeric(country_commodity$TOTAL.FOB.Value.US)
  country_commodity$TOTAL.Net.Weight.Kg <- as.numeric(country_commodity$TOTAL.Net.Weight.Kg)

# Add name lookup (can check str before & after)
lkup_countries <- read.csv("GCP/country_EN.csv",header = T)
lkup_countries <- distinct(lkup_countries)

# Add region lookup (can check str before & after)
lkup_regions <- read.csv("GCP/country_region.csv", header = T)
lkup_regions <- distinct(lkup_regions)

# Add key areas lookup
lkup_keyareas <- c("China", "Europe (EU)", "Europe (other)", "North America")

# Add English country name & region name
Country_Commodity <- left_join(country_commodity, lkup_countries, by = "Country.of.Destiny")
Country_Commodity <- left_join(Country_Commodity, lkup_regions, by = "CountryDestinyEN")

# Test if all copied or if duplicated. If missing values are found they will need to be added to the lookup tables
which(is.na(Country_Commodity$Importer.region))

# Summarise by region
country_commodity_region <- Country_Commodity %>%
  select(Importer.region, TOTAL.FOB.Value.US) %>%
  mutate(ratio = (TOTAL.FOB.Value.US/sum(TOTAL.FOB.Value.US))) %>%
  group_by( Importer.region)                  %>%
  summarise( totalFOB = sum(TOTAL.FOB.Value.US),
             ratio = sprintf("%1.4f %%", 100*(sum(ratio)))
             )

names(country_commodity_region) <- c("Importer region", "FOB Value (USD)", "Probable trade weighting")


# Summarise by Exporter and add column for ratio
country_commodity_Ex <- Country_Commodity %>%
  select(Exporter, CountryDestinyEN, Importer.region ,TOTAL.FOB.Value.US) %>%
  mutate(ratio = (TOTAL.FOB.Value.US/sum(TOTAL.FOB.Value.US))) %>%
  group_by(Exporter, Importer.region) %>%
  summarise( totalFOB = sum(TOTAL.FOB.Value.US),
             ratio = sprintf("%1.4f %%", 100*(sum(ratio)))
             )

country_commodity_Ex <- filter(country_commodity_Ex, Importer.region %in% lkup_keyareas)
names(country_commodity_Ex) <- c("Exporter","Importer region", "FOB Value (USD)", "Probable trade weighting")


# Summarise by Importer and add column for ratio
country_commodity_Im <- Country_Commodity %>%
  select(Importer, CountryDestinyEN, Importer.region ,TOTAL.FOB.Value.US) %>%
  mutate(ratio = (TOTAL.FOB.Value.US/sum(TOTAL.FOB.Value.US))) %>%
  group_by(Importer, Importer.region) %>%
  summarise( totalFOB = sum(TOTAL.FOB.Value.US),
             ratio = sprintf("%1.4f %%", 100*(sum(ratio)))
             )

country_commodity_Im <- filter(country_commodity_Im, Importer.region %in% lkup_keyareas)
names(country_commodity_Im) <- c("Importer","Importer region", "FOB Value (USD)", "Probable trade weighting")

# Check processed and unprocessed totals match
identical(sum(country_commodity_region$`FOB Value (USD)`),sum(country_commodity$TOTAL.FOB.Value.US))

## save as csv: destinations must be changed to suit
write.csv(country_commodity_region,"Output/PER_Palm_Global_2012_2015.csv")
write.csv(country_commodity_Ex,"Output/PER_Palm_Ex_2012_2015.csv")
write.csv(country_commodity_Im,"Output/PER_Palm_Im_2012_2015.csv")


