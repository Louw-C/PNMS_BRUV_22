# Revised PNMS BRUV Analysis with Functional Diversity and Trophic Level Analysis
# ----------------------------------------------


# Load required packages
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(vegan)     # For diversity indices
library(ggpubr)    # For combining plots
library(viridis)   # For better color palettes
library(FD)        # For functional diversity metrics (install if needed)

# ---------------------------------------------------------
# Step 1: Load the corrected string-level data
# Taxa specific MaxN and Biomass on a string level
# ---------------------------------------------------------

# Load the string-level data that properly accounts for double-counting
# Load the latest data with functional groups and TR included
string_level_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /BRUV_Aggregated_No Outliers/PNMS_BRUV_2022_string_level_MaxN_with_FuncGroups.csv", stringsAsFactors = FALSE)

# Check the structure of the data
str(string_level_data)
head(string_level_data)

# Basic data cleaning and preparation
# Avoid scientific notation
options(scipen = 999)

# Set a consistent color palette
zone_colors <- c("PNMS North" = "#0088FE", "DFZ West" = "#00C49F", "PNMS South" = "#FFBB28")

# Set factor levels to ensure consistent zone ordering
# Order from North to West to South
string_level_data$Zone <- factor(string_level_data$Zone, levels = c("PNMS North", "DFZ West", "PNMS South"))
# Order from North to West to South
string_level_data$Site <- factor(string_level_data$Site, levels = c("N1", "N3", "N4", "W1", "W2", "W3", "W4", "S1", "S2", "S3", "S4"))

# Extract numeric part of string ID for ordering
string_summary <- string_summary %>%
  mutate(StringNumber = as.numeric(gsub("PAL22_", "", String))) %>%
  arrange(StringNumber)

# Create factor with levels in the correct order
string_summary$String <- factor(string_summary$String, 
                                levels = string_summary$String[order(string_summary$StringNumber)])

# ---------------------------------------------------------
# Step 2: Calculate string-level summaries
# Totals of MaxN, Biomass and Species Richness for all taxa on a string
# ---------------------------------------------------------

# Calculate total MaxN, biomass, and species richness for each string
string_summary <- string_level_data %>%
  group_by(String, Site, Zone) %>%
  summarize(
    StringTotalMaxN = sum(MaxN_string, na.rm = TRUE),
    StringTotalBiomass_kg = sum(Total_biomass_kg, na.rm = TRUE),
    StringSpeciesRichness = n_distinct(Binomial),
    StringFamilyRichness = n_distinct(Family),
    StringMeanTR = weighted.mean(TR, w = MaxN_string, na.rm = TRUE),
    .groups = 'drop'
  )

# Create a basic bar plot of StringTotalMaxN by String, colored by Zone
plot_maxn <- ggplot(string_summary, aes(x = Site, y = StringTotalMaxN, fill = Zone)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("PNMS North" = "#0088FE", "DFZ West" = "#00C49F", "PNMS South" = "#FFBB28")) +
  labs(title = "Total MaxN by Site",
       x = NULL, 
       y = "Total MaxN") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_maxn

# Create a similar plot for biomass
plot_biomass <- ggplot(string_summary, aes(x = Site, y = StringTotalBiomass_kg, fill = Zone)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("PNMS North" = "#0088FE", "DFZ West" = "#00C49F", "PNMS South" = "#FFBB28")) +
  labs(title = "Total Biomass by Site",
       x = NULL, 
       y = "Total Biomass (kg)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_biomass

# Create a plot for species richness
plot_richness <- ggplot(string_summary, aes(x = Site, y = StringSpeciesRichness, fill = Zone)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("PNMS North" = "#0088FE", "DFZ West" = "#00C49F", "PNMS South" = "#FFBB28")) +
  labs(title = "Species Richness by Site",
       x = NULL, 
       y = "Number of Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_richness

# Export string-level summaries
write.csv(string_summary, "revised_string_summaries.csv", row.names = FALSE)

# ---------------------------------------------------------
# Step 3: Calculate functional group metrics at string level
# Contribution of different functional groups to a string - both MaxN and Biomass
# ---------------------------------------------------------

# Calculate functional group composition at string level
fg_string_level <- string_level_data %>%
  filter(!is.na(Functional_Group)) %>%
  group_by(String, Site, Zone, Functional_Group) %>%
  summarize(
    FG_MaxN = sum(MaxN_string, na.rm = TRUE),
    FG_Biomass_kg = sum(Total_biomass_kg, na.rm = TRUE),
    FG_SpeciesRichness = n_distinct(Binomial),
    FG_MeanTR = weighted.mean(TR, w = MaxN_string, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Calculate percentages
  group_by(String, Site, Zone) %>%
  mutate(
    StringTotalMaxN = sum(FG_MaxN),
    StringTotalBiomass_kg = sum(FG_Biomass_kg),
    FG_PercentMaxN = FG_MaxN / StringTotalMaxN * 100,
    FG_PercentBiomass = FG_Biomass_kg / StringTotalBiomass_kg * 100
  ) %>%
  ungroup()



# Create a stacked bar plot of functional group composition by site (abundance)
plot_fg_maxn <- ggplot(fg_site_level, aes(x = Site, y = FG_Percent_MaxN, fill = Functional_Group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(~Zone, scales = "free_x", space = "free") +
  labs(title = "Functional Group Composition by Site (Abundance)",
       x = "Site", 
       y = "Percentage of MaxN (%)",
       fill = "Functional Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        strip.background = element_rect(fill = "lightgray"),
        strip.text = element_text(face = "bold"))
plot_fg_maxn

# Create a stacked bar plot of functional group composition by site (biomass)
plot_fg_biomass <- ggplot(fg_site_level, aes(x = Site, y = FG_Percent_Biomass, fill = Functional_Group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(~Zone, scales = "free_x", space = "free") +
  labs(title = "Functional Group Composition by Site (Biomass)",
       x = "Site", 
       y = "Percentage of Biomass (%)",
       fill = "Functional Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        strip.background = element_rect(fill = "lightgray"),
        strip.text = element_text(face = "bold"))

# Export functional group string-level data
write.csv(fg_string_level, "functional_group_string_level.csv", row.names = FALSE)

# ---------------------------------------------------------
# Step 4: Calculate site-level metrics
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
    
    # Trophic level metrics
    MeanTrophicLevel = mean(StringMeanTR, na.rm = TRUE),
    SDTrophicLevel = sd(StringMeanTR, na.rm = TRUE),
    SETrophicLevel = SDTrophicLevel / sqrt(NumStrings),
    
    .groups = 'drop'
  ) %>%
  # Handle single-string sites (NA for SD and SE)
  mutate(across(
    c(SDMaxNPerString, SEMaxNPerString, 
      SDBiomassPerString, SEBiomassPerString,
      SDSpeciesRichness, SESpeciesRichness,
      SDFamilyRichness, SEFamilyRichness,
      SDTrophicLevel, SETrophicLevel),
    ~ifelse(is.na(.) | NumStrings < 2, 0, .)
  ))

# Export site-level metrics
write.csv(site_metrics, "revised_site_metrics.csv", row.names = FALSE)

# ---------------------------------------------------------
# Step 5: Calculate functional group metrics at site level
# ---------------------------------------------------------

# Calculate functional group composition at site level
fg_site_level <- fg_string_level %>%
  group_by(Site, Zone, Functional_Group) %>%
  summarize(
    FG_TotalMaxN = sum(FG_MaxN, na.rm = TRUE),
    FG_TotalBiomass_kg = sum(FG_Biomass_kg, na.rm = TRUE),
    FG_SpeciesRichness = n_distinct(string_level_data$Binomial[
      string_level_data$Site == first(Site) & 
        string_level_data$Functional_Group == first(Functional_Group)
    ]),
    FG_MeanTR = weighted.mean(FG_MeanTR, w = FG_MaxN, na.rm = TRUE),
    NumStrings = n_distinct(String),
    .groups = 'drop'
  ) %>%
  # Calculate percentages
  group_by(Site, Zone) %>%
  mutate(
    SiteTotalMaxN = sum(FG_TotalMaxN),
    SiteTotalBiomass_kg = sum(FG_TotalBiomass_kg),
    FG_PercentMaxN = FG_TotalMaxN / SiteTotalMaxN * 100,
    FG_PercentBiomass = FG_TotalBiomass_kg / SiteTotalBiomass_kg * 100
  ) %>%
  ungroup() %>%
  arrange(Zone, Site, desc(FG_PercentMaxN))

# Export functional group site-level data
write.csv(fg_site_level, "functional_group_site_level.csv", row.names = FALSE)

# ---------------------------------------------------------
# Step 6: Calculate zone-level metrics
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
    
    # Trophic level metrics
    MeanTrophicLevel = mean(StringMeanTR, na.rm = TRUE),
    SETrophicLevel = sd(StringMeanTR, na.rm = TRUE) / sqrt(NumStrings),
    
    .groups = 'drop'
  )

# Export zone-level metrics
write.csv(zone_metrics, "revised_zone_metrics.csv", row.names = FALSE)

# ---------------------------------------------------------
# Step 7: Calculate functional group metrics at zone level (Fixed)
# ---------------------------------------------------------

# Calculate functional group composition at zone level
fg_zone_level <- string_level_data %>%
  filter(!is.na(Functional_Group)) %>%
  # Calculate directly from string-level data to avoid the weighted mean error
  group_by(Zone, Functional_Group) %>%
  summarize(
    FG_TotalMaxN = sum(MaxN_string, na.rm = TRUE),
    FG_TotalBiomass_kg = sum(Total_biomass_kg, na.rm = TRUE),
    FG_SpeciesRichness = n_distinct(Binomial),
    # Calculate mean TR directly from string-level data
    FG_MeanTR = weighted.mean(TR, w = MaxN_string, na.rm = TRUE),
    NumStrings = n_distinct(String),
    NumSites = n_distinct(Site),
    .groups = 'drop'
  ) %>%
  # Calculate percentages
  group_by(Zone) %>%
  mutate(
    ZoneTotalMaxN = sum(FG_TotalMaxN),
    ZoneTotalBiomass_kg = sum(FG_TotalBiomass_kg),
    FG_PercentMaxN = FG_TotalMaxN / ZoneTotalMaxN * 100,
    FG_PercentBiomass = FG_TotalBiomass_kg / ZoneTotalBiomass_kg * 100
  ) %>%
  ungroup() %>%
  arrange(Zone, desc(FG_PercentMaxN))

# Export functional group zone-level data
write.csv(fg_zone_level, "functional_group_zone_level.csv", row.names = FALSE)

# ---------------------------------------------------------
# Step 8: Calculate diversity indices including functional diversity
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

# Calculate taxonomic diversity indices
taxonomic_diversity <- data.frame(
  Site = rownames(species_by_site_matrix),
  ShannonIndex = diversity(species_by_site_matrix, index = "shannon"),
  SimpsonIndex = diversity(species_by_site_matrix, index = "simpson"),
  PielouEvenness = diversity(species_by_site_matrix, index = "shannon") / log(specnumber(species_by_site_matrix))
)

# Create functional group-by-site matrix for functional diversity calculations
fg_by_site_matrix <- string_level_data %>%
  filter(!is.na(Functional_Group)) %>%
  # Calculate mean MaxN per string at each site for each functional group
  group_by(Site, Functional_Group) %>%
  summarize(
    MeanMaxN = mean(MaxN_string, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Convert to wide format
  pivot_wider(
    names_from = Functional_Group,
    values_from = MeanMaxN,
    values_fill = 0
  ) %>%
  column_to_rownames("Site")

# Calculate functional diversity indices
functional_diversity <- data.frame(
  Site = rownames(fg_by_site_matrix),
  FG_ShannonIndex = diversity(fg_by_site_matrix, index = "shannon"),
  FG_SimpsonIndex = diversity(fg_by_site_matrix, index = "simpson"),
  FG_PielouEvenness = diversity(fg_by_site_matrix, index = "shannon") / log(ncol(fg_by_site_matrix)),
  FG_Richness = specnumber(fg_by_site_matrix)
)

# Combine diversity indices and add zone information
all_diversity_indices <- taxonomic_diversity %>%
  left_join(functional_diversity, by = "Site") %>%
  left_join(
    distinct(string_level_data %>% select(Site, Zone)),
    by = "Site"
  )

# Export diversity indices
write.csv(all_diversity_indices, "revised_diversity_indices.csv", row.names = FALSE)

# ---------------------------------------------------------
# Step 9: Calculate trophic level distribution by site
# ---------------------------------------------------------

# Calculate the distribution of biomass across trophic levels at each site
trophic_distribution <- string_level_data %>%
  filter(!is.na(TR)) %>%
  # Create trophic level bins
  mutate(
    TrophicBin = case_when(
      TR < 3.0 ~ "Low (<3.0)",
      TR >= 3.0 & TR < 3.5 ~ "Mid-Low (3.0-3.5)",
      TR >= 3.5 & TR < 4.0 ~ "Mid (3.5-4.0)",
      TR >= 4.0 & TR < 4.5 ~ "Mid-High (4.0-4.5)",
      TR >= 4.5 ~ "High (≥4.5)",
      TRUE ~ "Unknown"
    ),
    TrophicBin = factor(TrophicBin, levels = c(
      "Low (<3.0)", "Mid-Low (3.0-3.5)", "Mid (3.5-4.0)", 
      "Mid-High (4.0-4.5)", "High (≥4.5)", "Unknown"
    ))
  ) %>%
  # Summarize by site and trophic bin
  group_by(Zone, Site, TrophicBin) %>%
  summarize(
    BinMaxN = sum(MaxN_string, na.rm = TRUE),
    BinBiomass_kg = sum(Total_biomass_kg, na.rm = TRUE),
    BinSpeciesCount = n_distinct(Binomial),
    .groups = 'drop'
  ) %>%
  # Calculate percentages
  group_by(Zone, Site) %>%
  mutate(
    TotalMaxN = sum(BinMaxN),
    TotalBiomass_kg = sum(BinBiomass_kg),
    PercentMaxN = BinMaxN / TotalMaxN * 100,
    PercentBiomass = BinBiomass_kg / TotalBiomass_kg * 100
  ) %>%
  ungroup() %>%
  arrange(Zone, Site, TrophicBin)

# Export trophic distribution data
write.csv(trophic_distribution, "trophic_distribution_by_site.csv", row.names = FALSE)

# ---------------------------------------------------------
# Step 10: Identify dominant species with functional groups and trophic info
# ---------------------------------------------------------

# Calculate species metrics by site
species_metrics <- string_level_data %>%
  group_by(Zone, Site, Family, Binomial, `Common.name`, Functional_Group, TR) %>%
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

# ---------------------------------------------------------
# Step 11: Calculate functional diversity metrics using FD package (Simplified)
# ---------------------------------------------------------

# This section uses the FD package to calculate functional diversity metrics
# Only run if the FD package is installed and you want these additional metrics

if(requireNamespace("FD", quietly = TRUE)) {
  # Print package version in a safe way
  cat("FD package is available\n")
  
  # Create a traits matrix with trophic level as a trait
  species_traits <- string_level_data %>%
    select(Binomial, Functional_Group, TR) %>%
    distinct() %>%
    filter(!is.na(Functional_Group) & !is.na(TR)) %>%
    mutate(
      # Convert functional group to numeric representation (one-hot encoding)
      FG_ApexPelagicPredator = ifelse(Functional_Group == "Apex Pelagic Predator", 1, 0),
      FG_PelagicMacropredator = ifelse(Functional_Group == "Pelagic Macropredator", 1, 0),
      FG_Zooplanktivore = ifelse(Functional_Group == "Zooplanktivore" | Functional_Group == "Zooplanktivore ", 1, 0),
      FG_OceanicOmnivore = ifelse(Functional_Group == "Oceanic Omnivore", 1, 0)
    ) %>%
    select(-Functional_Group) %>%
    column_to_rownames("Binomial")
  
  # Create community data matrix
  community_matrix <- string_level_data %>%
    filter(Binomial %in% rownames(species_traits)) %>%
    group_by(Site, Binomial) %>%
    summarize(
      Abundance = sum(MaxN_string, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = Binomial,
      values_from = Abundance,
      values_fill = 0
    ) %>%
    column_to_rownames("Site")
  
  # Ensure that the species in the traits matrix match those in the community matrix
  species_traits <- species_traits[intersect(rownames(species_traits), colnames(community_matrix)), ]
  community_matrix <- community_matrix[, intersect(colnames(community_matrix), rownames(species_traits))]
  
  # Calculate basic functional diversity metrics only - most compatible approach
  fd_indices <- tryCatch({
    cat("Calculating functional diversity indices...\n")
    
    # Use only the most basic and widely supported parameters
    fd_results <- FD::dbFD(
      x = species_traits,
      a = community_matrix
    )
    
    # Create dataframe with results
    fd_data <- data.frame(
      Site = rownames(community_matrix)
    )
    
    # Add metrics that should be available in all versions
    if(!is.null(fd_results$FRic)) fd_data$FunctionalRichness <- fd_results$FRic
    if(!is.null(fd_results$FEve)) fd_data$FunctionalEvenness <- fd_results$FEve
    if(!is.null(fd_results$FDiv)) fd_data$FunctionalDivergence <- fd_results$FDiv
    
    # Add zone information
    fd_data %>%
      left_join(
        distinct(string_level_data %>% select(Site, Zone)),
        by = "Site"
      )
  }, error = function(e) {
    cat("FD analysis failed with error:", e$message, "\n")
    # Return empty dataframe with just site and zone if all approaches fail
    string_level_data %>% 
      select(Site, Zone) %>% 
      distinct()
  })
  
  # Export functional diversity metrics
  write.csv(fd_indices, "functional_diversity_metrics.csv", row.names = FALSE)
  
  cat("Functional diversity analysis completed\n")
} else {
  cat("FD package is not available. Skipping functional diversity analysis.\n")
}

# ---------------------------------------------------------
# Step 12: Create visualizations
# ---------------------------------------------------------

# 1. Trophic level distribution by zone
tr_distribution_plot <- ggplot(trophic_distribution, 
                               aes(x = Site, y = PercentBiomass, fill = TrophicBin)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(limits = c(0, 100)) +  # Force y-axis to max at 100%
  scale_fill_viridis_d(option = "plasma", direction = -1) +
  labs(title = "Distribution of Biomass Across Trophic Levels",
       x = NULL, y = "Percentage of Biomass (%)",
       fill = "Trophic Level") +
  theme_minimal() +
  facet_wrap(~Zone)  # Separate by zone for clearer visualization

tr_distribution_plot

# 2. Functional group composition by zone
fg_zone_plot <- ggplot(fg_zone_level, 
                       aes(x = Zone, y = FG_PercentMaxN, fill = Functional_Group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Functional Group Composition by Zone",
       x = NULL, y = "Percentage of MaxN (%)",
       fill = "Functional Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "right")

# 3. Mean trophic level by site
tr_site_plot <- ggplot(site_metrics, 
                       aes(x = Site, y = MeanTrophicLevel, fill = Zone)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = MeanTrophicLevel - SETrophicLevel, 
                    ymax = MeanTrophicLevel + SETrophicLevel), width = 0.2) +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Mean Trophic Level by Site",
       x = NULL, y = "Mean Trophic Level",
       fill = "Zone") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine plots
combined_plots <- ggarrange(
  tr_distribution_plot, fg_zone_plot, tr_site_plot,
  ncol = 1, nrow = 3,
  common.legend = FALSE,
  legend = "right"
)

# Save the combined plot
ggsave("trophic_and_functional_analysis_plots.png", combined_plots, width = 10, height = 12, dpi = 300)

# 4. Trophic level vs. functional group distribution (bubble plot)
fg_tr_bubble <- string_level_data %>%
  filter(!is.na(Functional_Group) & !is.na(TR)) %>%
  group_by(Zone, Functional_Group, TR) %>%
  summarize(
    TotalMaxN = sum(MaxN_string, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = Functional_Group, y = TR, size = TotalMaxN, color = Zone)) +
  geom_point(alpha = 0.7) +
  scale_size_area(max_size = 15) +
  scale_color_manual(values = zone_colors) +
  labs(title = "Relationship Between Functional Groups and Trophic Levels",
       x = "Functional Group", y = "Trophic Level",
       size = "Total MaxN", color = "Zone") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the bubble plot
ggsave("trophic_functional_bubble_plot.png", fg_tr_bubble, width = 10, height = 8, dpi = 300)

# Print completion message
cat("\nAnalysis complete! New metrics for functional groups and trophic levels have been calculated and exported.\n")
cat("Generated files include:\n")
cat("- Functional group metrics at string, site, and zone levels\n")
cat("- Trophic level distribution data\n")
cat("- Functional diversity indices\n")
cat("- Visualizations of trophic and functional patterns\n")