# PNMS BRUV Analysis - data aggregations - outliers removed
# -------------------------------------------

# Load required packages
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(vegan)     # For diversity indices

# ---------------------------------------------------------
# Step 1: Load the data
# ---------------------------------------------------------

#Load data - load data without outliers
bruv_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /PNMS_BRUV_2022_ouliers removed.csv", stringsAsFactors = FALSE)

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
# Step 5: Calculate string totals for variation metrics
# ---------------------------------------------------------
# Calculate total MaxN and Biomass for each string
string_totals <- string_level_combined %>%
  group_by(String, Site, Zone) %>%
  summarize(
    StringTotalMaxN = sum(MaxN_string, na.rm = TRUE),
    StringTotalBiomass = sum(Biomass_kg_string, na.rm = TRUE),
    StringSpeciesRichness = n_distinct(Binomial),
    StringFamilyRichness = n_distinct(Family),
    .groups = 'drop'
  )

# Export string-level totals for future reference
write.csv(string_totals, "string_level_totals.csv", row.names = FALSE)

# ---------------------------------------------------------
# Step 6: Calculate site-level metrics with variation
# ---------------------------------------------------------
# Calculate site-level metrics from string-level data

site_metrics <- string_totals %>%
  # Group by site
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
    TotalBiomass_kg = sum(StringTotalBiomass, na.rm = TRUE),
    MeanBiomassPerString = mean(StringTotalBiomass, na.rm = TRUE),
    SDBiomassPerString = sd(StringTotalBiomass, na.rm = TRUE),
    SEBiomassPerString = SDBiomassPerString / sqrt(NumStrings),
    
    # Species richness metrics with variation
    MeanSpeciesRichness = mean(StringSpeciesRichness, na.rm = TRUE),
    SDSpeciesRichness = sd(StringSpeciesRichness, na.rm = TRUE),
    SESpeciesRichness = SDSpeciesRichness / sqrt(NumStrings),
    
    # Family richness metrics with variation
    MeanFamilyRichness = mean(StringFamilyRichness, na.rm = TRUE),
    SDFamilyRichness = sd(StringFamilyRichness, na.rm = TRUE),
    SEFamilyRichness = SDFamilyRichness / sqrt(NumStrings),
    
    .groups = 'drop'
  ) %>%
  # Handle cases where there's only one string (NA for SD and SE)
  mutate(across(
    c(SDMaxNPerString, SEMaxNPerString, 
      SDBiomassPerString, SEBiomassPerString,
      SDSpeciesRichness, SESpeciesRichness,
      SDFamilyRichness, SEFamilyRichness),
    ~ifelse(is.na(.) | NumStrings < 2, 0, .)
  )) %>%
  arrange(Zone, Site)

# Calculate total species richness at each site (across all strings)
total_richness <- string_level_combined %>%
  group_by(Site) %>%
  summarize(
    TotalSpeciesRichness = n_distinct(Binomial),
    TotalFamilyRichness = n_distinct(Family),
    .groups = 'drop'
  )

# Add total richness to site metrics
site_metrics <- site_metrics %>%
  left_join(total_richness, by = "Site")

# View the result
print(site_metrics)

# Export site-level metrics
write.csv(site_metrics, "site_metrics_with_se.csv", row.names = FALSE)

# ---------------------------------------------------------
# Step 7: Calculate diversity indices by site
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

# Calculate standard errors for diversity indices
# This requires bootstrapping - simplified version
diversity_ses <- NULL
for(site in unique(string_level_combined$Site)) {
  # Get all strings for this site
  site_strings <- unique(string_level_combined$String[string_level_combined$Site == site])
  
  # Skip if there's only one string
  if(length(site_strings) < 2) {
    diversity_ses <- rbind(diversity_ses, data.frame(
      Site = site,
      SEShannon = 0,
      SESimpson = 0
    ))
    next
  }
  
  # Calculate diversity for each string
  string_diversities <- data.frame(Shannon = numeric(), Simpson = numeric())
  for(str in site_strings) {
    # Create species abundance matrix for this string
    string_matrix <- string_level_combined %>%
      filter(String == str) %>%
      select(Binomial, MaxN_string) %>%
      pivot_wider(names_from = Binomial, values_from = MaxN_string, values_fill = 0) %>%
      as.data.frame()
    
    # Calculate diversity indices
    if(ncol(string_matrix) > 1) {  # Skip if no species
      string_diversities <- rbind(string_diversities, data.frame(
        Shannon = diversity(string_matrix[,-1], index = "shannon"),
        Simpson = diversity(string_matrix[,-1], index = "simpson")
      ))
    } else {
      string_diversities <- rbind(string_diversities, data.frame(
        Shannon = 0,
        Simpson = 0
      ))
    }
  }
  
  # Calculate standard errors
  diversity_ses <- rbind(diversity_ses, data.frame(
    Site = site,
    SEShannon = sd(string_diversities$Shannon) / sqrt(length(site_strings)),
    SESimpson = sd(string_diversities$Simpson) / sqrt(length(site_strings))
  ))
}

# Add standard errors to diversity indices
diversity_indices <- diversity_indices %>%
  left_join(diversity_ses, by = "Site") %>%
  # Fill in zeros for sites with one string
  mutate(
    SEShannon = ifelse(is.na(SEShannon), 0, SEShannon),
    SESimpson = ifelse(is.na(SESimpson), 0, SESimpson)
  )

# View the diversity indices
print(diversity_indices)

# Export diversity indices
write.csv(diversity_indices, "diversity_indices_with_se.csv", row.names = FALSE)

# ---------------------------------------------------------
# Step 8: Calculate species metrics by site
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

# Calculate standard errors for species metrics
species_ses <- NULL
for(site in unique(string_level_combined$Site)) {
  for(sp in unique(string_level_combined$Binomial[string_level_combined$Site == site])) {
    # Get all strings for this site
    site_strings <- unique(string_level_combined$String[string_level_combined$Site == site])
    
    # Skip if there's only one string
    if(length(site_strings) < 2) {
      species_ses <- rbind(species_ses, data.frame(
        Site = site,
        Binomial = sp,
        SEMaxN = 0,
        SEBiomass = 0
      ))
      next
    }
    
    # Calculate MaxN and Biomass for each string
    string_values <- data.frame(MaxN = numeric(), Biomass = numeric())
    for(str in site_strings) {
      # Get data for this species and string
      sp_data <- string_level_combined %>%
        filter(String == str, Binomial == sp)
      
      # Add values (0 if species not present in this string)
      if(nrow(sp_data) > 0) {
        string_values <- rbind(string_values, data.frame(
          MaxN = sp_data$MaxN_string,
          Biomass = sp_data$Biomass_kg_string
        ))
      } else {
        string_values <- rbind(string_values, data.frame(
          MaxN = 0,
          Biomass = 0
        ))
      }
    }
    
    # Calculate standard errors
    species_ses <- rbind(species_ses, data.frame(
      Site = site,
      Binomial = sp,
      SEMaxN = sd(string_values$MaxN) / sqrt(length(site_strings)),
      SEBiomass = sd(string_values$Biomass) / sqrt(length(site_strings))
    ))
  }
}

# Add standard errors to species metrics
species_metrics_by_site <- species_metrics_by_site %>%
  left_join(species_ses, by = c("Site", "Binomial")) %>%
  # Fill in zeros for NAs
  mutate(
    SEMaxN = ifelse(is.na(SEMaxN), 0, SEMaxN),
    SEBiomass = ifelse(is.na(SEBiomass), 0, SEBiomass)
  )

# View the species metrics
head(species_metrics_by_site)

# Export species metrics
write.csv(species_metrics_by_site, "species_metrics_by_site_with_se.csv", row.names = FALSE)

# ---------------------------------------------------------
# Step 9: Find dominant species by site
# ---------------------------------------------------------
# Identify the top species by MaxN at each site
dominant_species <- species_metrics_by_site %>%
  group_by(Site) %>%
  # Get the top 5 species by mean MaxN per string
  slice_max(order_by = MeanMaxNPerString, n = 5) %>%
  ungroup()

# View the dominant species by site
print(dominant_species)

# Export dominant species data
write.csv(dominant_species, "dominant_species_by_site.csv", row.names = FALSE)

# Print a summary message
cat("\nData aggregation complete with standard error calculations.\n")
cat("All results have been exported to CSV files for further analysis and visualization.\n")
