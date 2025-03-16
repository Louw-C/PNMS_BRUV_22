# BRUV Data Visualization
# This script creates visualizations for the BRUV data after aggregation

# Load required packages
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(viridis) # For nice color palettes

# ---------------------------------------------------------
# Step 1: Load the aggregated data (assuming it was saved from previous script)
# If you're running this as a standalone script, uncomment and modify these lines
# ---------------------------------------------------------
# Read the aggregated data from CSV files
site_metrics <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /BRUV_Aggregated_No Outliers/site_metrics_with_se.csv", stringsAsFactors = FALSE)
string_level_combined <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /BRUV_Aggregated_No Outliers/string_level_totals.csv", stringsAsFactors = FALSE)
diversity_indices <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /BRUV_Aggregated_No Outliers/diversity_indices_with_se.csv", stringsAsFactors = FALSE)
species_metrics_by_site <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /BRUV_Aggregated_No Outliers/species_metrics_by_site_with_se.csv", stringsAsFactors = FALSE)

# If you don't have the CSV files, you can run the aggregation code before this script
# and use the data frames directly


# ---------------------------------------------------------
# Step 2: Prepare data for visualizations
# ---------------------------------------------------------
# Sort sites by mean MaxN for consistent ordering in plots
site_order <- site_metrics %>%
  arrange(desc(MeanMaxNPerString)) %>%
  pull(Site)

# Update factor levels for consistent ordering
site_metrics$Site <- factor(site_metrics$Site, levels = site_order)
diversity_indices$Site <- factor(diversity_indices$Site, levels = site_order)

# Create a zone color palette
zone_colors <- setNames(
  viridis(n = length(unique(site_metrics$Zone)), option = "D"),
  unique(site_metrics$Zone)
)

# ---------------------------------------------------------
# Step 3: Create visualizations for site-level metrics
# ---------------------------------------------------------

# Plot 1: Mean MaxN per String by Site with Error Bars
p1 <- ggplot(site_metrics, aes(x = Site, y = MeanMaxNPerString, fill = Zone)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = MeanMaxNPerString - SEMaxNPerString, 
                    ymax = MeanMaxNPerString + SEMaxNPerString),
                width = 0.2) +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Mean MaxN per String by Site",
       subtitle = "Error bars show ± 1 standard error",
       x = "Site", 
       y = "Mean MaxN per String") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1

# Plot 2: Mean Biomass per String by Site with Error Bars
p2 <- ggplot(site_metrics, aes(x = Site, y = MeanBiomassPerString, fill = Zone)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = MeanBiomassPerString - SEBiomassPerString, 
                    ymax = MeanBiomassPerString + SEBiomassPerString),
                width = 0.2) +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Mean Biomass per String by Site",
       subtitle = "Error bars show ± 1 standard error",
       x = "Site", 
       y = "Mean Biomass (kg) per String") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2

# Plot 3: Species Richness by Site
p3 <- ggplot(site_metrics, aes(x = Site, y = TotalSpeciesRichness, fill = Zone)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Species Richness by Site",
       x = "Site", 
       y = "Number of Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p3

# Plot 4: Diversity Indices by Site
p4 <- ggplot(diversity_indices, aes(x = Site)) +
  geom_bar(aes(y = ShannonIndex, fill = Zone), stat = "identity", alpha = 0.7) +
  geom_point(aes(y = SimpsonIndex), size = 3, color = "black") +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Diversity Indices by Site",
       x = "Site", 
       y = "Index Value",
       caption = "Bars = Shannon Index, Points = Simpson Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p4

# Arrange the site-level plots in a grid
site_plots <- grid.arrange(p1, p2, p3, p4, ncol = 2)

# Save the plots
ggsave("site_level_metrics.png", site_plots, width = 14, height = 10)

# ---------------------------------------------------------
# Step 4: Create zone comparison plots
# ---------------------------------------------------------

# Calculate mean metrics by zone
zone_metrics <- site_metrics %>%
  group_by(Zone) %>%
  summarize(
    NumSites = n(),
    MeanMaxN = mean(MeanMaxNPerString),
    SEMaxN = sd(MeanMaxNPerString) / sqrt(NumSites),
    MeanBiomass = mean(MeanBiomassPerString),
    SEBiomass = sd(MeanBiomassPerString) / sqrt(NumSites),
    MeanSpeciesRichness = mean(TotalSpeciesRichness),
    SESpeciesRichness = sd(TotalSpeciesRichness) / sqrt(NumSites)
  )

# Plot 5: Zone Comparisons
p5 <- ggplot(zone_metrics, aes(x = Zone, y = MeanMaxN, fill = Zone)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = MeanMaxN - SEMaxN, ymax = MeanMaxN + SEMaxN), width = 0.2) +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Mean MaxN by Zone",
       subtitle = "Error bars show ± 1 standard error",
       x = NULL, 
       y = "Mean MaxN per String") +
  theme_minimal()+
  guides(fill = "none")
p5

p6 <- ggplot(zone_metrics, aes(x = Zone, y = MeanBiomass, fill = Zone)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = MeanBiomass - SEBiomass, ymax = MeanBiomass + SEBiomass), width = 0.2) +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Mean Biomass by Zone",
       subtitle = "Error bars show ± 1 standard error",
       x = NULL, 
       y = "Mean Biomass (kg) per String") +
  theme_minimal()+
  guides(fill = "none")
p6

p7 <- ggplot(zone_metrics, aes(x = Zone, y = MeanSpeciesRichness, fill = Zone)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = MeanSpeciesRichness - SESpeciesRichness, 
                    ymax = MeanSpeciesRichness + SESpeciesRichness), width = 0.2) +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Mean Species Richness by Zone",
       subtitle = "Error bars show ± 1 standard error",
       x = NULL, 
       y = "Mean Species Richness") +
  theme_minimal()+
  guides(fill = "none")
p7

# Arrange the zone comparison plots
zone_plots <- grid.arrange(p5, p6, p7, ncol = 3)

# Save the zone comparison plots
ggsave("zone_comparisons.png", zone_plots, width = 18, height = 6)

# ---------------------------------------------------------
# Step 5: Create species composition plots
# ---------------------------------------------------------

# Get the top 10 species across all sites by total MaxN
top_species <- species_metrics_by_site %>%
  group_by(Binomial, `Common.name`) %>%
  summarize(TotalMaxN = sum(TotalMaxN), .groups = 'drop') %>%
  arrange(desc(TotalMaxN)) %>%
  slice_head(n = 10) %>%
  pull(Binomial)

# Filter data for top species
species_metrics_by_site <- species_metrics_by_site %>%
  filter(Binomial != "None")

library(patchwork)

# Plot 8: Top Species by Site
p8 <- ggplot(species_metrics_by_site, aes(x = Site, y = MeanMaxNPerString, fill = Binomial)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  labs(title = "Species Composition by Site",
       x = NULL, 
       y = "Mean MaxN per String") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
p8

# Plot 9: Top Species by Zone
p9 <- species_metrics_by_site %>%
  group_by(Zone, Family, Binomial) %>%
  summarize(MeanMaxN = mean(MeanMaxNPerString), .groups = 'drop') %>%
  ggplot(aes(x = Zone, y = MeanMaxN, fill = Binomial)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  labs(title = "Species Composition by Zone",
       x = NULL, 
       y = "Mean MaxN per String") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(face = "italic"))
p9

# Combine plots with a shared legend at the bottom
combined_plot <- p8 / p9 + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(face = "italic"))

# Display the combined plot
combined_plot

# Save the species composition plots
ggsave("species_composition.png", species_plots, width = 14, height = 12)

# ---------------------------------------------------------
# Step 8: Create box plots to show distribution of values
# ---------------------------------------------------------

# Prepare string-level data for boxplots
string_data_for_boxplot <- string_level_combined %>%
  group_by(Site, Zone, String) %>%
  summarize(
    TotalMaxN = sum(StringTotalMaxN),
    TotalBiomass = sum(StringTotalBiomass),
    .groups = 'drop'
  )

# Box plots of MaxN and Biomass by Site
p11 <- ggplot(string_data_for_boxplot, aes(x = Site, y = TotalMaxN, fill = Zone)) +
  geom_boxplot() +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Distribution of MaxN Values by Site",
       x = "Site", 
       y = "Total MaxN per String") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p11

p12 <- ggplot(string_data_for_boxplot, aes(x = Site, y = TotalBiomass, fill = Zone)) +
  geom_boxplot() +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Distribution of Biomass Values by Site",
       x = "Site", 
       y = "Total Biomass (kg) per String") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p12

# Arrange the box plots
boxplots <- grid.arrange(p11, p12, ncol = 1)

# Save the box plots
ggsave("distribution_boxplots.png", boxplots, width = 14, height = 10)

# Print completion message
cat("\nVisualization complete. All plots have been saved to the working directory.\n")