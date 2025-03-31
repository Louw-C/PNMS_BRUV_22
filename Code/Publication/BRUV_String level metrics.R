# Revised PNMS BRUV Analysis with String-Level Data
# ----------------------------------------------

# Load required packages
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(vegan)     # For diversity indices
library(ggpubr)    # For combining plots
library(viridis)   # For better color palettes

# ---------------------------------------------------------
# Step 1: Load the corrected string-level data
# ---------------------------------------------------------

# Load the string-level data that properly accounts for double-counting
string_level_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /BRUV_Aggregated_No Outliers/PNMS_BRUV_2022_string_level_MaxN.csv", stringsAsFactors = FALSE)

# Check the structure of the data
str(string_level_data)
head(string_level_data)

# Basic data cleaning and preparation
# Avoid scientific notation
options(scipen = 999)

# Set a consistent color palette
zone_colors <- c("PNMS North" = "#0088FE", "DFZ West" = "#00C49F", "PNMS South" = "#FFBB28")

# Set factor levels to ensure consistent zone ordering
string_level_data$Zone <- factor(string_level_data$Zone, levels = c("PNMS North", "DFZ West", "PNMS South"))

# Extract string number for ordering
string_level_data$StringNumber <- as.numeric(gsub("PAL22_", "", string_level_data$String))
string_level_data <- string_level_data %>% arrange(StringNumber)

# ---------------------------------------------------------
# Step 2: Calculate string-level summaries
# ---------------------------------------------------------

# Calculate total MaxN, biomass, and species richness for each string
string_summary <- string_level_data %>%
  group_by(String, Site, Zone) %>%
  summarize(
    StringTotalMaxN = sum(MaxN_string, na.rm = TRUE),
    StringTotalBiomass_kg = sum(Total_biomass_kg, na.rm = TRUE),
    StringSpeciesRichness = n_distinct(Binomial),
    StringFamilyRichness = n_distinct(Family),
    .groups = 'drop'
  )

# Export string-level summaries
write.csv(string_summary, "revised_string_summaries.csv", row.names = FALSE)

# ---------------------------------------------------------
# Step 3: Calculate site-level metrics
# ---------------------------------------------------------

# Calculate site-level metrics with variation measures
site_metrics <- string_summary %>%
  group_by(Site, Zone) %>%
  summarize(
    # Sample size (number of strings)
    NumStrings = n(),
    
    # MaxN metrics
    TotalMaxN = sum(StringTotalMaxN, na.rm = TRUE),
    MeanMaxNPerString = mean(StringTotalMaxN, na.rm = TRUE),
    MedianMaxNPerString = median(StringTotalMaxN, na.rm = TRUE),
    SDMaxNPerString = sd(StringTotalMaxN, na.rm = TRUE),
    SEMaxNPerString = SDMaxNPerString / sqrt(NumStrings),
    
    # Biomass metrics
    TotalBiomass_kg = sum(StringTotalBiomass_kg, na.rm = TRUE),
    MeanBiomassPerString = mean(StringTotalBiomass_kg, na.rm = TRUE),
    MedianBiomassPerString = median(StringTotalBiomass_kg, na.rm = TRUE),
    SDBiomassPerString = sd(StringTotalBiomass_kg, na.rm = TRUE),
    SEBiomassPerString = SDBiomassPerString / sqrt(NumStrings),
    
    # Species richness metrics
    TotalSpeciesRichness = n_distinct(string_level_data$Binomial[string_level_data$Site == first(Site)]),
    MeanSpeciesRichness = mean(StringSpeciesRichness, na.rm = TRUE),
    MedianSpeciesRichness = median(StringSpeciesRichness, na.rm = TRUE),
    SDSpeciesRichness = sd(StringSpeciesRichness, na.rm = TRUE),
    SESpeciesRichness = SDSpeciesRichness / sqrt(NumStrings),
    
    # Family richness metrics
    TotalFamilyRichness = n_distinct(string_level_data$Family[string_level_data$Site == first(Site)]),
    MeanFamilyRichness = mean(StringFamilyRichness, na.rm = TRUE),
    MedianFamilyRichness = median(StringFamilyRichness, na.rm = TRUE),
    SDFamilyRichness = sd(StringFamilyRichness, na.rm = TRUE),
    SEFamilyRichness = SDFamilyRichness / sqrt(NumStrings),
    
    .groups = 'drop'
  ) %>%
  # Handle single-string sites (NA for SD and SE)
  mutate(across(
    c(SDMaxNPerString, SEMaxNPerString, 
      SDBiomassPerString, SEBiomassPerString,
      SDSpeciesRichness, SESpeciesRichness,
      SDFamilyRichness, SEFamilyRichness),
    ~ifelse(is.na(.) | NumStrings < 2, 0, .)
  ))

# Export site-level metrics
write.csv(site_metrics, "revised_site_metrics.csv", row.names = FALSE)

# ---------------------------------------------------------
# Step 4: Calculate zone-level metrics
# ---------------------------------------------------------

# Calculate summary metrics by zone
zone_metrics <- string_summary %>%
  group_by(Zone) %>%
  summarize(
    # Sample size
    NumSites = n_distinct(Site),
    NumStrings = n(),
    
    # MaxN metrics
    TotalMaxN = sum(StringTotalMaxN, na.rm = TRUE),
    MeanMaxNPerString = mean(StringTotalMaxN, na.rm = TRUE),
    SEMaxNPerString = sd(StringTotalMaxN, na.rm = TRUE) / sqrt(NumStrings),
    
    # Biomass metrics
    TotalBiomass_kg = sum(StringTotalBiomass_kg, na.rm = TRUE),
    MeanBiomassPerString = mean(StringTotalBiomass_kg, na.rm = TRUE),
    SEBiomassPerString = sd(StringTotalBiomass_kg, na.rm = TRUE) / sqrt(NumStrings),
    
    # Species richness metrics
    TotalSpeciesRichness = n_distinct(string_level_data$Binomial[string_level_data$Zone == first(Zone)]),
    MeanSpeciesRichness = mean(StringSpeciesRichness, na.rm = TRUE),
    SESpeciesRichness = sd(StringSpeciesRichness, na.rm = TRUE) / sqrt(NumStrings),
    
    # Family richness metrics
    TotalFamilyRichness = n_distinct(string_level_data$Family[string_level_data$Zone == first(Zone)]),
    MeanFamilyRichness = mean(StringFamilyRichness, na.rm = TRUE),
    SEFamilyRichness = sd(StringFamilyRichness, na.rm = TRUE) / sqrt(NumStrings),
    
    .groups = 'drop'
  )

# Export zone-level metrics
write.csv(zone_metrics, "revised_zone_metrics.csv", row.names = FALSE)

# ---------------------------------------------------------
# Step 5: Calculate diversity indices
# ---------------------------------------------------------

# Create species-by-site matrix for diversity calculations
species_by_site_matrix <- string_level_data %>%
  # Calculate mean MaxN per string at each site for each species
  group_by(Site, Binomial) %>%
  summarize(
    MeanMaxN = mean(MaxN_string, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Convert to wide format
  pivot_wider(
    names_from = Binomial,
    values_from = MeanMaxN,
    values_fill = 0
  ) %>%
  column_to_rownames("Site")

# Calculate diversity indices
diversity_indices <- data.frame(
  Site = rownames(species_by_site_matrix),
  ShannonIndex = diversity(species_by_site_matrix, index = "shannon"),
  SimpsonIndex = diversity(species_by_site_matrix, index = "simpson"),
  PielouEvenness = diversity(species_by_site_matrix, index = "shannon") / log(specnumber(species_by_site_matrix))
)

# Add zone information
diversity_indices <- diversity_indices %>%
  left_join(
    distinct(string_level_data %>% select(Site, Zone)),
    by = "Site"
  )

# Export diversity indices
write.csv(diversity_indices, "revised_diversity_indices.csv", row.names = FALSE)

# ---------------------------------------------------------
# Step 6: Identify dominant species
# ---------------------------------------------------------

# Calculate species metrics by site
species_metrics <- string_level_data %>%
  group_by(Zone, Site, Family, Binomial, `Common.name`) %>%
  summarize(
    # Abundance metrics
    MaxN_string = sum(MaxN_string, na.rm = TRUE),
    Mean_MaxN = mean(MaxN_string, na.rm = TRUE),
    
    # Biomass metrics
    Total_biomass_kg = sum(Total_biomass_kg, na.rm = TRUE),
    Mean_biomass_kg = mean(Total_biomass_kg, na.rm = TRUE),
    
    # Occurrence metrics
    NumStringsObserved = n_distinct(String),
    NumStringsInSite = n_distinct(string_level_data$String[string_level_data$Site == first(Site)]),
    OccurrenceFrequency = NumStringsObserved / NumStringsInSite * 100,
    
    .groups = 'drop'
  ) %>%
  # Calculate relative abundance and biomass
  group_by(Site) %>%
  mutate(
    RelativeMaxN = MaxN_string / sum(MaxN_string, na.rm = TRUE) * 100,
    RelativeBiomass = Total_biomass_kg / sum(Total_biomass_kg, na.rm = TRUE) * 100
  ) %>%
  ungroup()

# Find top 5 dominant species by MaxN at each site
dominant_species <- species_metrics %>%
  group_by(Site) %>%
  slice_max(order_by = MaxN_string, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(Zone, Site, desc(MaxN_string))

# Export species metrics and dominant species
write.csv(species_metrics, "revised_species_metrics.csv", row.names = FALSE)
write.csv(dominant_species, "revised_dominant_species.csv", row.names = FALSE)