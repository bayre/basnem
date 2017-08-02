
# Use install.library(dplyr) to add this package if have not already done so
library(dplyr)

# Add country/commodity data. Must change this each time and check the seperator
country_commodity <- read.csv("GCP/Brazil/BRAZIL_BEEF_2012_2016.csv",header = T, sep = ",")

# Add a step to get names in same format as Colombian/Peruvian data
names(country_commodity) <- c("Name","Exporter","Importer", "CountryDestinyEN", "Indicator", "TOTAL.FOB.Value.US", "Year")

# Use as.numeric function to ensure read in correctly
country_commodity$TOTAL.FOB.Value.US <- as.numeric(country_commodity$TOTAL.FOB.Value.US)

# Add region lookup (can check str before & after)
lkup_regions <- read.csv("GCP/country_region.csv", header = T)
lkup_regions <- distinct(lkup_regions)

# Add key areas lookup
lkup_keyareas <- c("China", "Europe (EU)", "Europe (other)", "North America")

# Add English country name & region name
Country_Commodity <- left_join(country_commodity, lkup_regions, by = "CountryDestinyEN")

# Test if all copied. If missing values are found they will need to be added to the lookup tables
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
write.csv(country_commodity_region,"Output/BR_Beef_Global_2012_2016.csv")
write.csv(country_commodity_Ex,"Output/BR_Beef_Ex_2012_2016.csv")
write.csv(country_commodity_Im,"Output/BR_Beef_Im_2012_2016.csv")


