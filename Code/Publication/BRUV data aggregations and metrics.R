# PNMS BRUV Analysis - data aggregations and visualisations
# -------------------------------------------

# Load required packages
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(vegan)     # For diversity indices

# ---------------------------------------------------------
# Step 1: Load the data
# ---------------------------------------------------------

#Load data
bruv_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /PNMS_BRUV_2022.csv", stringsAsFactors = FALSE)

# Check the structure of the data
str(bruv_data)
head(bruv_data)

#General bits

#Avoid scientific annotations
options(scipen = 999)

# Set a consistent color palette
zone_colors <- c("PNMS North" = "#0088FE", "DFZ West" = "#00C49F", "PNMS South" = "#FFBB28")

# Set factor levels to ensure consistent zone ordering
bruv_data$Zone <- factor(bruv_data$Zone, levels = c("PNMS North", "DFZ West", "PNMS South"))

# ---------------------------------------------------------
# Step 2: Aggregate MaxN from Sample to String level
# ---------------------------------------------------------
# For each species within each string, take the maximum MaxN
# This avoids double-counting the same individuals across samples

string_level_maxn <- bruv_data %>%
  # Group by string and species identifiers
  group_by(String, Site, Zone, Family, Binomial, `Common.name`) %>%
  # Get maximum MaxN across all samples in a string
  summarize(
    MaxN_string = max(MaxN, na.rm = TRUE),
    .groups = 'drop'
  )

# View the result
head(string_level_maxn)

# Check the number of unique strings
n_distinct(string_level_maxn$String)

# ---------------------------------------------------------
# Step 3: Aggregate Biomass from Sample to String level
# ---------------------------------------------------------
# For each species within each string, take the maximum Biomass
# This avoids double-counting the same individuals across samples

string_level_biomass <- bruv_data %>%
  # Group by string and species identifiers
  group_by(String, Site, Zone, Family, Binomial, `Common.name`) %>%
  # Calculate biomass for each sample and take the maximum across samples
  summarize(
    Biomass_kg_string = max(Biomass_kg, na.rm = TRUE),
    .groups = 'drop'
  )

# View the result
head(string_level_biomass)

# ---------------------------------------------------------
# Step 4: Combine MaxN and Biomass at String level
# ---------------------------------------------------------
# Join the MaxN and Biomass data at string level
string_level_combined <- string_level_maxn %>%
  left_join(string_level_biomass, 
            by = c("String", "Site", "Zone", "Family", "Binomial", "Common.name"))

# View the result
head(string_level_combined)

# ---------------------------------------------------------
# Step 5: Calculate summary metrics by Site
# ---------------------------------------------------------
# Calculate site-level metrics from string-level data

# Calculate site-level metrics with variation (SD and SE)
site_metrics_with_variation <- string_level_combined %>%
  # First calculate string-level totals to get variation
  group_by(Site, Zone, String) %>%
  summarize(
    # Sum MaxN and Biomass for each string
    StringTotalMaxN = sum(MaxN_string, na.rm = TRUE),
    StringTotalBiomass_kg = sum(Biomass_kg_string, na.rm = TRUE),
    StringSpeciesRichness = n_distinct(Binomial),
    StringFamilyRichness = n_distinct(Family),
    .groups = 'drop'
  ) %>%
  # Now group by site to calculate means and variations
  group_by(Site, Zone) %>%
  summarize(
    # Sampling effort
    NumStrings = n(),
    
    # MaxN metrics with variation
    TotalMaxN = sum(StringTotalMaxN, na.rm = TRUE),
    MeanMaxNPerString = mean(StringTotalMaxN, na.rm = TRUE),
    SDMaxNPerString = sd(StringTotalMaxN, na.rm = TRUE),
    SEMaxNPerString = SDMaxNPerString / sqrt(NumStrings),
    
    # Biomass metrics with variation
    TotalBiomass_kg = sum(StringTotalBiomass_kg, na.rm = TRUE),
    MeanBiomassPerString = mean(StringTotalBiomass_kg, na.rm = TRUE),
    SDBiomassPerString = sd(StringTotalBiomass_kg, na.rm = TRUE),
    SEBiomassPerString = SDBiomassPerString / sqrt(NumStrings),
    
    # Species richness metrics with variation
    MeanSpeciesRichness = mean(StringSpeciesRichness, na.rm = TRUE),
    SDSpeciesRichness = sd(StringSpeciesRichness, na.rm = TRUE),
    SESpeciesRichness = SDSpeciesRichness / sqrt(NumStrings),
    
    # Family richness metrics with variation
    MeanFamilyRichness = mean(StringFamilyRichness, na.rm = TRUE),
    SDFamilyRichness = sd(StringFamilyRichness, na.rm = TRUE),
    SEFamilyRichness = SDFamilyRichness / sqrt(NumStrings),
    
    # Overall species and family richness across all strings at the site
    TotalSpeciesRichness = n_distinct(string_level_combined$Binomial[string_level_combined$Site == first(Site)]),
    TotalFamilyRichness = n_distinct(string_level_combined$Family[string_level_combined$Site == first(Site)]),
    
    .groups = 'drop'
  ) %>%
  arrange(Zone, Site)

# View the result
print(site_metrics_with_variation)

# ----------------------------------------------------------------
# Alternative approach that handles an important edge case
# ----------------------------------------------------------------
# This handles the case where there's only one string at a site
# (which would result in NA for SD and SE since you can't calculate SD from one value)

site_metrics_safe <- string_level_combined %>%
  # First calculate string-level totals
  group_by(Site, Zone, String) %>%
  summarize(
    StringTotalMaxN = sum(MaxN_string, na.rm = TRUE),
    StringTotalBiomass_kg = sum(Biomass_kg_string, na.rm = TRUE),
    StringSpeciesRichness = n_distinct(Binomial),
    StringFamilyRichness = n_distinct(Family),
    .groups = 'drop'
  ) %>%
  # Now group by site to calculate means and variations
  group_by(Site, Zone) %>%
  summarize(
    # Sampling effort
    NumStrings = n(),
    
    # MaxN metrics with variation
    TotalMaxN = sum(StringTotalMaxN, na.rm = TRUE),
    MeanMaxNPerString = mean(StringTotalMaxN, na.rm = TRUE),
    # Use if-else to handle single-string sites
    SDMaxNPerString = ifelse(NumStrings > 1, sd(StringTotalMaxN, na.rm = TRUE), 0),
    SEMaxNPerString = ifelse(NumStrings > 1, SDMaxNPerString / sqrt(NumStrings), 0),
    
    # Biomass metrics with variation
    TotalBiomass_kg = sum(StringTotalBiomass_kg, na.rm = TRUE),
    MeanBiomassPerString = mean(StringTotalBiomass_kg, na.rm = TRUE),
    SDBiomassPerString = ifelse(NumStrings > 1, sd(StringTotalBiomass_kg, na.rm = TRUE), 0),
    SEBiomassPerString = ifelse(NumStrings > 1, SDBiomassPerString / sqrt(NumStrings), 0),
    
    # Species richness metrics with variation
    MeanSpeciesRichness = mean(StringSpeciesRichness, na.rm = TRUE),
    SDSpeciesRichness = ifelse(NumStrings > 1, sd(StringSpeciesRichness, na.rm = TRUE), 0),
    SESpeciesRichness = ifelse(NumStrings > 1, SDSpeciesRichness / sqrt(NumStrings), 0),
    
    # Family richness metrics with variation
    MeanFamilyRichness = mean(StringFamilyRichness, na.rm = TRUE),
    SDFamilyRichness = ifelse(NumStrings > 1, sd(StringFamilyRichness, na.rm = TRUE), 0),
    SEFamilyRichness = ifelse(NumStrings > 1, SDFamilyRichness / sqrt(NumStrings), 0),
    
    # Overall site richness
    TotalSpeciesRichness = n_distinct(string_level_combined$Binomial[string_level_combined$Site == first(Site)]),
    TotalFamilyRichness = n_distinct(string_level_combined$Family[string_level_combined$Site == first(Site)]),
    
    .groups = 'drop'
  ) %>%
  arrange(Zone, Site)

# View the result
print(site_metrics_safe)

# ---------------------------------------------------------
# Step 6: Calculate diversity indices by site
# ---------------------------------------------------------
# Create a species x site matrix for diversity calculations
species_by_site_matrix <- string_level_combined %>%
  # Take the mean MaxN per string for each species at each site
  group_by(Site, Binomial) %>%
  summarize(
    MeanMaxNPerString = sum(MaxN_string) / n_distinct(string_level_combined$String[string_level_combined$Site == first(Site)]),
    .groups = 'drop'
  ) %>%
  # Convert to wide format with species as columns
  pivot_wider(
    names_from = Binomial,
    values_from = MeanMaxNPerString,
    values_fill = 0
  ) %>%
  # Set the site as row names for use with vegan functions
  column_to_rownames("Site")

# Calculate diversity indices
diversity_indices <- data.frame(
  Site = rownames(species_by_site_matrix),
  # Shannon diversity index (H')
  ShannonIndex = diversity(species_by_site_matrix, index = "shannon"),
  # Simpson's diversity index (1-D)
  SimpsonIndex = diversity(species_by_site_matrix, index = "simpson"),
  # Species evenness
  Evenness = diversity(species_by_site_matrix, index = "shannon") / log(specnumber(species_by_site_matrix))
)

# Add zone information back
diversity_indices <- diversity_indices %>%
  left_join(
    bruv_data %>% select(Site, Zone) %>% distinct(),
    by = "Site"
  )

# View the diversity indices
print(diversity_indices)

# ---------------------------------------------------------
# Step 7: Calculate species metrics by site
# ---------------------------------------------------------
# Calculate metrics for each species at each site
species_metrics_by_site <- string_level_combined %>%
  group_by(Site, Zone, Family, Binomial, `Common.name`) %>%
  summarize(
    # Count strings where the species was present
    NumStringsPresent = n(),
    # Calculate occurrence frequency (% of strings where species was observed)
    OccurrenceFrequency = NumStringsPresent / n_distinct(string_level_combined$String[string_level_combined$Site == first(Site)]) * 100,
    
    # Calculate total and mean abundance
    TotalMaxN = sum(MaxN_string, na.rm = TRUE),
    MeanMaxNPerString = TotalMaxN / n_distinct(string_level_combined$String[string_level_combined$Site == first(Site)]),
    
    # Calculate total and mean biomass
    TotalBiomass_kg = sum(Biomass_kg_string, na.rm = TRUE),
    MeanBiomassPerString = TotalBiomass_kg / n_distinct(string_level_combined$String[string_level_combined$Site == first(Site)]),
    
    .groups = 'drop'
  ) %>%
  arrange(Site, desc(MeanMaxNPerString))

# View the result
head(species_metrics_by_site)

# ---------------------------------------------------------
# Step 8: Find dominant species by site
# ---------------------------------------------------------
# Identify the top species by MaxN at each site
dominant_species <- species_metrics_by_site %>%
  group_by(Site) %>%
  # Get the top 5 species by mean MaxN per string
  slice_max(order_by = MeanMaxNPerString, n = 5) %>%
  ungroup()

# View the dominant species by site
print(dominant_species)

# ---------------------------------------------------------
# Step 9: Export results
# ---------------------------------------------------------
# Save the results to CSV files for further analysis
write.csv(string_level_combined, "string_level_data.csv", row.names = FALSE)
write.csv(site_metrics, "site_metrics.csv", row.names = FALSE)
write.csv(diversity_indices, "diversity_indices.csv", row.names = FALSE)
write.csv(species_metrics_by_site, "species_metrics_by_site.csv", row.names = FALSE)
write.csv(dominant_species, "dominant_species_by_site.csv", row.names = FALSE)

# Print a summary message
cat("\nData aggregation complete. Results exported to CSV files.\n")


