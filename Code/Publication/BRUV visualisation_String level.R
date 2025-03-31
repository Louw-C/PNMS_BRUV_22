# Revised BRUV Data Visualization
# This script creates visualizations using the corrected string-level data

# Load required packages
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(viridis)  # For nice color palettes
library(vegan)    # For NMDS analysis
library(patchwork) # For combining plots

# ---------------------------------------------------------
# Step 1: Load the revised string-level data
# ---------------------------------------------------------
# Read the revised data from CSV files
site_metrics <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /BRUV_Aggregated_No Outliers/revised_site_metrics.csv", stringsAsFactors = FALSE)
string_summaries <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /BRUV_Aggregated_No Outliers/revised_string_summaries.csv", stringsAsFactors = FALSE)
diversity_indices <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /BRUV_Aggregated_No Outliers/revised_diversity_indices.csv", stringsAsFactors = FALSE)
species_metrics <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /BRUV_Aggregated_No Outliers/revised_species_metrics.csv", stringsAsFactors = FALSE)
string_level_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /BRUV_Aggregated_No Outliers/PNMS_BRUV_2022_string_level_MaxN.csv", stringsAsFactors = FALSE)

# ---------------------------------------------------------
# Step 2: Prepare data for visualizations
# ---------------------------------------------------------
# Define your specific site order as in the original code
site_order <- c("N1", "N3", "N4", "W1", "W2", "W3", "W4", "S1", "S2", "S3", "S4")

# Update factor levels for consistent ordering
site_metrics$Site <- factor(site_metrics$Site, levels = site_order)
diversity_indices$Site <- factor(diversity_indices$Site, levels = site_order)
string_summaries$Site <- factor(string_summaries$Site, levels = site_order)
species_metrics$Site <- factor(species_metrics$Site, levels = site_order)

# Define your specific zone order
zone_order <- c("PNMS North", "DFZ West", "PNMS South")

# Create a zone color palette with ordered zones
zone_colors <- setNames(
  viridis(n = length(zone_order), option = "D"),
  zone_order
)

# Make sure the Zone variable is also a factor with the correct ordering
site_metrics$Zone <- factor(site_metrics$Zone, levels = zone_order)
diversity_indices$Zone <- factor(diversity_indices$Zone, levels = zone_order)
string_summaries$Zone <- factor(string_summaries$Zone, levels = zone_order)
species_metrics$Zone <- factor(species_metrics$Zone, levels = zone_order)

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
  labs(title = "A: Mean MaxN per String by Site",
       subtitle = "Error bars show ± 1 standard error",
       x = NULL, 
       y = "Mean MaxN per String") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p1)

# Plot 2: Mean Biomass per String by Site with Error Bars
p2 <- ggplot(site_metrics, aes(x = Site, y = MeanBiomassPerString, fill = Zone)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = MeanBiomassPerString - SEBiomassPerString, 
                    ymax = MeanBiomassPerString + SEBiomassPerString),
                width = 0.2) +
  scale_fill_manual(values = zone_colors) +
  labs(title = "B: Mean Biomass per String by Site",
       subtitle = "Error bars show ± 1 standard error",
       x = NULL, 
       y = "Mean Biomass (kg) per String") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2)

# Plot 3: Species Richness by Site
p3 <- ggplot(site_metrics, aes(x = Site, y = TotalSpeciesRichness, fill = Zone)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = zone_colors) +
  labs(title = "C: Species Richness by Site",
       x = NULL, 
       y = "Number of Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p3)

# Plot 4: Diversity Indices by Site
p4 <- ggplot(diversity_indices, aes(x = Site)) +
  geom_bar(aes(y = ShannonIndex, fill = Zone), stat = "identity", alpha = 0.7) +
  geom_point(aes(y = SimpsonIndex), size = 3, color = "black") +
  scale_fill_manual(values = zone_colors) +
  labs(title = "D: Diversity Indices by Site",
       x = NULL, 
       y = "Index Value",
       caption = "Bars = Shannon Index, Points = Simpson Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p4)

# Arrange the site-level plots in a grid
site_plots <- grid.arrange(p1, p2, p3, p4, ncol = 2)
print(site_plots)

# Save the plots
ggsave("revised_site_level_metrics.png", site_plots, width = 14, height = 10)

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

# Plot 5: Mean MaxN by Zone
p5 <- ggplot(zone_metrics, aes(x = Zone, y = MeanMaxN, fill = Zone)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = MeanMaxN - SEMaxN, ymax = MeanMaxN + SEMaxN), width = 0.2) +
  scale_fill_manual(values = zone_colors) +
  labs(title = "A: Mean MaxN by Zone",
       subtitle = "Error bars show ± 1 standard error",
       x = NULL, 
       y = "Mean MaxN per String") +
  theme_minimal() +
  guides(fill = "none")
print(p5)

# Plot 6: Mean Biomass by Zone
p6 <- ggplot(zone_metrics, aes(x = Zone, y = MeanBiomass, fill = Zone)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = MeanBiomass - SEBiomass, ymax = MeanBiomass + SEBiomass), width = 0.2) +
  scale_fill_manual(values = zone_colors) +
  labs(title = "B: Mean Biomass by Zone",
       subtitle = "Error bars show ± 1 standard error",
       x = NULL, 
       y = "Mean Biomass (kg) per String") +
  theme_minimal() +
  guides(fill = "none")
print(p6)

# Plot 7: Mean Species Richness by Zone
p7 <- ggplot(zone_metrics, aes(x = Zone, y = MeanSpeciesRichness, fill = Zone)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = MeanSpeciesRichness - SESpeciesRichness, 
                    ymax = MeanSpeciesRichness + SESpeciesRichness), width = 0.2) +
  scale_fill_manual(values = zone_colors) +
  labs(title = "C: Mean Species Richness by Zone",
       subtitle = "Error bars show ± 1 standard error",
       x = NULL, 
       y = "Mean Species Richness") +
  theme_minimal() +
  guides(fill = "none")
print(p7)

# Arrange the zone comparison plots
zone_plots <- grid.arrange(p5, p6, p7, ncol = 3)
print(zone_plots)

# Save the zone comparison plots
ggsave("revised_zone_comparisons.png", zone_plots, width = 18, height = 6)

# ---------------------------------------------------------
# Step 5: Create species composition plots
# ---------------------------------------------------------

# Get the top 10 species across all sites by total MaxN
top_species <- species_metrics %>%
  group_by(Binomial, Common.name) %>%
  summarize(TotalMaxN = sum(MaxN_string), .groups = 'drop') %>%
  arrange(desc(TotalMaxN)) %>%
  slice_head(n = 10) %>%
  pull(Binomial)

# Filter data for top species
top_species_data <- species_metrics %>%
  filter(Binomial %in% top_species) %>%
  # Make sure Binomial is a factor with levels ordered by overall abundance
  mutate(Binomial = factor(Binomial, levels = top_species))

# Plot 8: Top Species by Site
p8 <- ggplot(species_metrics, aes(x = Site, y = Mean_MaxN, fill = Binomial)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  labs(title = "Species Composition by Site",
       x = NULL, 
       y = "Mean MaxN per String") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
print(p8)

# Plot 9: Top Species by Zone
# Aggregate species data by zone
zone_species_data <- species_metrics %>%
  group_by(Zone, Family, Binomial, Common.name) %>%
  summarize(MeanMaxN = mean(Mean_MaxN, na.rm = TRUE), .groups = 'drop') %>%
  # Ensure Zone ordering is preserved after summarizing
  mutate(Zone = factor(Zone, levels = zone_order))

p9 <- ggplot(zone_species_data, aes(x = Zone, y = MeanMaxN, fill = Binomial)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  labs(title = "Species Composition by Zone",
       x = NULL, 
       y = "Mean MaxN per String") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(face = "italic"))
print(p9)

# Combine plots with a shared legend at the bottom using patchwork
combined_plot <- p8 / p9 + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(face = "italic"))
print(combined_plot)

# Save the species composition plots
ggsave("revised_species_composition.png", combined_plot, width = 14, height = 12)

# ---------------------------------------------------------
# Step 6: Create box plots to show distribution of values
# ---------------------------------------------------------

# Plot 11: Box plot of MaxN by Site
p11 <- ggplot(string_summaries, aes(x = Site, y = StringTotalMaxN, fill = Zone)) +
  geom_boxplot() +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Distribution of MaxN Values by Site",
       x = "Site", 
       y = "Total MaxN per String") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p11)

# Plot 12: Box plot of Biomass by Site
p12 <- ggplot(string_summaries, aes(x = Site, y = StringTotalBiomass_kg, fill = Zone)) +
  geom_boxplot() +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Distribution of Biomass Values by Site",
       x = "Site", 
       y = "Total Biomass (kg) per String") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p12)

# Arrange the box plots
boxplots <- grid.arrange(p11, p12, ncol = 1)
print(boxplots)

# Save the box plots
ggsave("revised_distribution_boxplots.png", boxplots, width = 14, height = 10)

# ---------------------------------------------------------
# Step 7: NMDS Analysis for BRUV Fish Data
# ---------------------------------------------------------

# Fix for NMDS Plot with Proper Zone Coloring

library(tidyverse)
library(vegan)
library(ggplot2)

# ---------------------------------------------------------
# Load data and prepare for NMDS
# ---------------------------------------------------------

# Load string level data
string_level_data <- read.csv("PNMS_BRUV_2022_string_level_MaxN.csv", stringsAsFactors = FALSE)

# Define zone order and colors
zone_order <- c("PNMS North", "DFZ West", "PNMS South")
zone_colors <- c("PNMS North" = "#0072B2", "DFZ West" = "#009E73", "PNMS South" = "#D55E00")

# Ensure Zone is a factor with proper ordering
string_level_data$Zone <- factor(string_level_data$Zone, levels = zone_order)

# Create deployment metadata
deployment_metadata <- string_level_data %>%
  select(String, Zone, Site) %>%
  distinct()

# Prepare community matrix
community_matrix <- string_level_data %>%
  group_by(String, Binomial) %>%
  summarize(MaxN = sum(MaxN_string, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Binomial, values_from = MaxN, values_fill = 0)

# Extract deployment metadata and ensure it's properly associated with the community data
nmds_metadata <- community_matrix %>%
  select(String) %>%
  left_join(select(deployment_metadata, String, Zone, Site), by = "String")

# Ensure Zone is a factor with proper ordering
nmds_metadata$Zone <- factor(nmds_metadata$Zone, levels = zone_order)

# Prepare community data for NMDS
community_data <- community_matrix %>%
  select(-String) %>%
  as.matrix()
row.names(community_data) <- community_matrix$String

# ---------------------------------------------------------
# Perform NMDS analysis
# ---------------------------------------------------------

# Remove rows with all zeros (empty deployments)
zero_rows <- which(rowSums(community_data) == 0)
if(length(zero_rows) > 0) {
  community_data <- community_data[-zero_rows, ]
  nmds_metadata <- nmds_metadata[-zero_rows, ]
  cat("Removed", length(zero_rows), "empty deployments from the analysis.\n")
}

# Calculate Bray-Curtis dissimilarity
bc_dist <- vegdist(community_data, method = "bray")

# Perform NMDS
set.seed(123) # For reproducibility
nmds_result <- metaMDS(bc_dist, k = 2, trymax = 100)

# Extract NMDS scores
nmds_scores <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_scores$String <- row.names(nmds_scores)

# Join with metadata to get Zone information
nmds_scores <- nmds_scores %>%
  left_join(nmds_metadata, by = "String")

# Double-check Zone is a factor with proper ordering
nmds_scores$Zone <- factor(nmds_scores$Zone, levels = zone_order)

# Display the actual zone values in nmds_scores to confirm
print(table(nmds_scores$Zone))

# ---------------------------------------------------------
# Create the improved NMDS plot
# ---------------------------------------------------------

# Create NMDS plot with proper zone coloring
p <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Zone)) +
  geom_point(size = 3.5, alpha = 0.8) +
  scale_color_manual(values = zone_colors) +
  stat_ellipse(aes(group = Zone), linetype = 2) +
  labs(title = "NMDS Ordination of Fish Communities",
       subtitle = paste("Stress =", round(nmds_result$stress, 3)),
       x = "NMDS1", y = "NMDS2") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

# Print the plot to screen
print(p)

# Save the plot with higher resolution
ggsave("revised_nmds_plot_fixed.png", p, width = 8, height = 6, dpi = 300)

# ---------------------------------------------------------
# Additional NMDS visualization with site labels
# ---------------------------------------------------------

# Create a version with site labels
p_with_labels <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Zone)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(aes(label = Site), hjust = -0.3, vjust = 0.3, size = 3) +
  scale_color_manual(values = zone_colors) +
  stat_ellipse(aes(group = Zone), linetype = 2) +
  labs(title = "NMDS Ordination with Site Labels",
       subtitle = paste("Stress =", round(nmds_result$stress, 3)),
       x = "NMDS1", y = "NMDS2") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

# Print the labeled plot
print(p_with_labels)

# Save the labeled plot
ggsave("revised_nmds_plot_with_labels.png", p_with_labels, width = 8, height = 6, dpi = 300)

# ---------------------------------------------------------
# Show PERMANOVA results
# ---------------------------------------------------------

# Perform PERMANOVA test
permanova_result <- adonis2(bc_dist ~ Zone, data = nmds_metadata, permutations = 999)
print("PERMANOVA results:")
print(permanova_result)
# ---------------------------------------------------------
# Step 8: Create additional plots for in-depth analysis
# ---------------------------------------------------------

# Plot 13: Family composition by zone
family_data <- string_level_data %>%
  group_by(Zone, Family) %>%
  summarize(TotalMaxN = sum(MaxN_string, na.rm = TRUE), .groups = "drop") %>%
  group_by(Zone) %>%
  mutate(Percentage = TotalMaxN / sum(TotalMaxN) * 100) %>%
  ungroup()

# Keep only families that represent at least 5% in any zone
significant_families <- family_data %>%
  group_by(Family) %>%
  summarize(MaxPercentage = max(Percentage), .groups = "drop") %>%
  filter(MaxPercentage >= 5) %>%
  pull(Family)

# Combine minor families into "Other"
family_data_plot <- family_data %>%
  mutate(Family = ifelse(Family %in% significant_families, Family, "Other")) %>%
  group_by(Zone, Family) %>%
  summarize(Percentage = sum(Percentage), .groups = "drop")

p13 <- ggplot(family_data_plot, aes(x = Zone, y = Percentage, fill = reorder(Family, -Percentage))) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  labs(title = "Family Composition by Zone",
       x = NULL,
       y = "Percentage of Total MaxN",
       fill = "Family") +
  theme_minimal() +
  theme(legend.position = "right")
print(p13)

# Save the family composition plot
ggsave("revised_family_composition.png", p13, width = 10, height = 7)

# Print completion message
cat("\nVisualization complete. All plots have been saved with 'revised_' prefix.\n")

