# Analysis of Plankton Functional Groups Across Zones
# ------------------------------------------------
# This script analyzes how functional plankton groups vary across different zones
# Note: Sampling is balanced with 8 samples per zone

# Load required packages
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(vegan)      # For diversity calculations
library(patchwork)  # For combining plots

# Load the classified plankton data
plankton_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /PAL_2022_Zooplankton_Classified.csv")

# Step 1: Verify balanced sampling across zones
# --------------------------------------------
samples_per_zone <- plankton_data %>%
  select(Zone, Sample) %>%
  distinct() %>%
  count(Zone, name = "NumSamples")

print("Samples per zone:")
print(samples_per_zone)

# Verify site-level sampling
samples_per_site <- plankton_data %>%
  select(Zone, Site, Sample) %>%
  distinct() %>%
  count(Zone, Site, name = "NumSamples") %>%
  arrange(Zone, Site)

print("Samples per site:")
print(samples_per_site)

# Step 2: Basic summary of the data
# --------------------------------
cat("Dataset summary:\n")
cat("Total records:", nrow(plankton_data), "\n")
cat("Total plankton count:", sum(plankton_data$`Total Count`), "\n")
cat("Unique samples:", length(unique(plankton_data$Sample)), "\n")

# Step 3: Analyze functional groups by zone
# ---------------------------------------

# Create a summary by functional group and zone
fg_by_zone <- plankton_data %>%
  group_by(Zone, FunctionalGroup) %>%
  summarize(
    TotalCount = sum(`Total Count`, na.rm = TRUE),
    NumRecords = n(),
    NumSpecimenTypes = n_distinct(`Specimen Type`),
    .groups = "drop"
  ) %>%
  # Calculate percentage within each zone
  group_by(Zone) %>%
  mutate(
    ZoneTotal = sum(TotalCount),
    Percentage = TotalCount / ZoneTotal * 100
  ) %>%
  ungroup() %>%
  arrange(Zone, desc(Percentage))

# Display the summary
print(fg_by_zone)

# Since we have equal samples per zone, we can directly compare zone totals
# but we'll calculate mean per sample for reference
fg_by_zone <- fg_by_zone %>%
  left_join(samples_per_zone, by = "Zone") %>%
  mutate(
    MeanCountPerSample = TotalCount / NumSamples
  )

# ---------------------------------------------------------
# Prepare plankton data for visualizations with consistent ordering
# ---------------------------------------------------------

# Load required packages for colors
library(viridis)

# Define your specific site order
site_order <- c("N1", "N3", "N4", "W1", "W2", "W3", "W4", "S1", "S2", "S3", "S4")

# Define your specific zone order
zone_order <- c("North", "West", "South")  # Adjust to match your plankton data zone names

# Create a zone color palette with ordered zones
zone_colors <- setNames(
  viridis(n = length(zone_order), option = "D"),
  zone_order
)

# Update factor levels in plankton data for consistent ordering
plankton_data$Site <- factor(plankton_data$Site, levels = site_order)
plankton_data$Zone <- factor(plankton_data$Zone, levels = zone_order)

# Apply consistent ordering to any derived data frames
fg_by_zone$Zone <- factor(fg_by_zone$Zone, levels = zone_order)
site_fg_composition$Zone <- factor(site_fg_composition$Zone, levels = zone_order)
site_fg_composition$Site <- factor(site_fg_composition$Site, levels = site_order)
sample_fg_composition$Zone <- factor(sample_fg_composition$Zone, levels = zone_order)
sample_fg_composition$Site <- factor(sample_fg_composition$Site, levels = site_order)
diversity_by_zone$Zone <- factor(diversity_by_zone$Zone, levels = zone_order)

# Define a consistent functional group color palette (if needed)
functional_group_order <- c(
  "Small Crustacean Plankton",
  "Large Crustacean Plankton", 
  "Gelatinous Plankton",
  "Fish Larvae & Eggs",
  "Benthic Invertebrate Larvae"
)

# Ensure functional group is also properly ordered
plankton_data$FunctionalGroup <- factor(plankton_data$FunctionalGroup, levels = functional_group_order)
fg_by_zone$FunctionalGroup <- factor(fg_by_zone$FunctionalGroup, levels = functional_group_order)
site_fg_composition$FunctionalGroup <- factor(site_fg_composition$FunctionalGroup, levels = functional_group_order)

# Update your ggplot color scales to use this consistent palette
# For example:
# scale_fill_manual(values = zone_colors)  # For fill aesthetics
# scale_color_manual(values = zone_colors)  # For color aesthetics


# Step 4: Create visualizations
# ----------------------------

# 1. Stacked bar chart of functional group composition by zone (percentage)
plot1 <- ggplot(fg_by_zone, aes(x = Zone, y = Percentage, fill = FunctionalGroup)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Plankton Functional Group Composition by Zone",
       x = NULL, 
       y = "Percentage (%)",
       fill = "Functional Group") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
        legend.position = "right")
plot1

# 2. Bar chart of total abundance by zone (direct comparison due to equal sampling)
plot2 <- ggplot(fg_by_zone, aes(x = Zone, y = TotalCount, fill = FunctionalGroup)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Abundance of Functional Groups by Zone",
       subtitle = "Direct comparison with equal sampling effort (8 samples per zone)",
       x = "Zone", 
       y = "Total Count",
       fill = "Functional Group") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
        legend.position = "right")

# 3. Faceted bar chart to compare functional groups across zones
plot3 <- ggplot(fg_by_zone, aes(x = Zone, y = TotalCount, fill = Zone)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ FunctionalGroup, scales = "free_y") +
  labs(title = "Functional Group Abundance Across Zones",
       subtitle = "Direct comparison with equal sampling effort (8 samples per zone)",
       x = "Zone", 
       y = "Total Count",
       fill = "Zone") +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "lightgray"),
        strip.text = element_text(face = "bold"))

# Save the plots
ggsave("Plankton_Composition_by_Zone_Percentage.png", plot1, width = 10, height = 6)
ggsave("Plankton_Abundance_by_Zone.png", plot2, width = 10, height = 6)
ggsave("Functional_Groups_Across_Zones.png", plot3, width = 12, height = 8)

# Step 5: Statistical analysis
# --------------------------

# Calculate Shannon diversity of functional groups for each zone
diversity_by_zone <- fg_by_zone %>%
  select(Zone, FunctionalGroup, TotalCount) %>%
  group_by(Zone) %>%
  summarize(
    TotalCount = sum(TotalCount),
    NumFunctionalGroups = n_distinct(FunctionalGroup),
    # Calculate diversity using vegan's diversity function
    ShannonDiversity = diversity(TotalCount, index = "shannon"),
    SimpsonDiversity = diversity(TotalCount, index = "simpson"),
    # Calculate evenness
    Evenness = ShannonDiversity / log(NumFunctionalGroups),
    .groups = "drop"
  )

print("Diversity of functional groups by zone:")
print(diversity_by_zone)

# Create a diversity comparison plot
plot4 <- ggplot(diversity_by_zone, aes(x = Zone, y = ShannonDiversity, fill = Zone)) +
  geom_bar(stat = "identity") +
  labs(title = "Diversity of Functional Groups by Zone",
       subtitle = "Shannon diversity index",
       x = "Zone", 
       y = "Shannon Diversity",
       fill = "Zone") +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none")

# Evenness comparison plot
plot5 <- ggplot(diversity_by_zone, aes(x = Zone, y = Evenness, fill = Zone)) +
  geom_bar(stat = "identity") +
  labs(title = "Evenness of Functional Groups by Zone",
       x = "Zone", 
       y = "Evenness",
       fill = "Zone") +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none")

# Combine diversity plots
diversity_plots <- plot4 + plot5
ggsave("Functional_Group_Diversity_by_Zone.png", diversity_plots, width = 12, height = 5)

# Step 6: Sample-level analysis to look at variability
# --------------------------------------------------

# Calculate functional group composition for each sample
sample_fg_composition <- plankton_data %>%
  group_by(Sample, Zone, Site, FunctionalGroup) %>%
  summarize(
    TotalCount = sum(`Total Count`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculate total and percentage for each sample
  group_by(Sample, Zone, Site) %>%
  mutate(
    SampleTotal = sum(TotalCount),
    Percentage = TotalCount / SampleTotal * 100
  ) %>%
  ungroup()

# Calculate mean and standard error for each functional group by zone
zone_fg_stats <- sample_fg_composition %>%
  group_by(Zone, FunctionalGroup) %>%
  summarize(
    NumSamples = n_distinct(Sample),
    MeanPercentage = mean(Percentage, na.rm = TRUE),
    SD = sd(Percentage, na.rm = TRUE),
    SE = SD / sqrt(NumSamples),
    .groups = "drop"
  ) %>%
  arrange(Zone, desc(MeanPercentage))

# Plot with error bars
plot6 <- ggplot(zone_fg_stats, aes(x = Zone, y = MeanPercentage, fill = FunctionalGroup)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_errorbar(aes(ymin = MeanPercentage - SE, ymax = MeanPercentage + SE), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Mean Functional Group Composition by Zone",
       subtitle = "Error bars show standard error; based on 8 samples per zone",
       x = "Zone", 
       y = "Mean Percentage (%)",
       fill = "Functional Group") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

ggsave("Functional_Group_Composition_with_Error.png", plot6, width = 10, height = 7)

# Step 7: Site-level comparison
# --------------------------

# Create site-level summary (note: sites have uneven sampling)
site_fg_composition <- plankton_data %>%
  group_by(Zone, Site, FunctionalGroup) %>%
  summarize(
    TotalCount = sum(`Total Count`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculate total and percentage for each site
  group_by(Zone, Site) %>%
  mutate(
    SiteTotal = sum(TotalCount),
    Percentage = TotalCount / SiteTotal * 100
  ) %>%
  ungroup()

# For site-level analysis, we should account for unequal sampling
site_fg_composition <- site_fg_composition %>%
  left_join(samples_per_site, by = c("Zone", "Site")) %>%
  mutate(
    # Calculate mean count per sample at each site
    MeanCountPerSample = TotalCount / NumSamples
  )

# Create a heatmap of functional group composition by site
plot7 <- ggplot(site_fg_composition, aes(x = Site, y = FunctionalGroup, fill = Percentage)) +
  geom_tile() +
  facet_grid(. ~ Zone, scales = "free_x", space = "free") +
  labs(title = "Functional Group Composition by Site",
       x = "Site", 
       y = "Functional Group",
       fill = "Percentage (%)") +
  theme_minimal() +
  scale_fill_viridis_c() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Functional_Group_Composition_by_Site.png", plot7, width = 14, height = 8)

# Step 8: Dominant functional groups
# --------------------------------

# Identify the dominant functional group for each site
dominant_fg <- site_fg_composition %>%
  group_by(Zone, Site) %>%
  slice_max(order_by = Percentage, n = 1) %>%
  ungroup() %>%
  select(Zone, Site, FunctionalGroup, Percentage) %>%
  arrange(Zone, Site)

print("Dominant functional group by site:")
print(dominant_fg)

# Create a summary of dominant groups
dominant_summary <- dominant_fg %>%
  count(Zone, FunctionalGroup) %>%
  pivot_wider(names_from = Zone, values_from = n, values_fill = 0)

print("Number of sites dominated by each functional group across zones:")
print(dominant_summary)

# Step 9: Multivariate analysis of functional group composition
# -----------------------------------------------------------

# Create a wide-format matrix of functional group composition by site
fg_matrix <- site_fg_composition %>%
  select(Zone, Site, FunctionalGroup, Percentage) %>%
  pivot_wider(
    id_cols = c(Zone, Site),
    names_from = FunctionalGroup,
    values_from = Percentage,
    values_fill = 0
  )

# Extract metadata and community matrix
fg_metadata <- fg_matrix %>% select(Zone, Site)
fg_comm <- fg_matrix %>% select(-Zone, -Site) %>% as.matrix()
rownames(fg_comm) <- paste(fg_metadata$Zone, fg_metadata$Site, sep = "_")

# Run NMDS
set.seed(123)
nmds_result <- metaMDS(fg_comm, distance = "bray", k = 2, trymax = 100)

# Extract scores
nmds_scores <- as.data.frame(scores(nmds_result))
nmds_scores$Site <- rownames(nmds_scores)
nmds_scores$Zone <- fg_metadata$Zone

# Create NMDS plot
plot8 <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Zone, label = Site)) +
  geom_point(size = 3) +
  geom_text(hjust = -0.3, vjust = 0.3, size = 3) +
  stat_ellipse(aes(group = Zone), type = "t", linetype = 2) +
  labs(title = "NMDS Ordination of Functional Group Composition by Site",
       subtitle = paste("Stress =", round(nmds_result$stress, 3)),
       color = "Zone") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2")

ggsave("NMDS_Functional_Groups.png", plot8, width = 10, height = 8)

# Run PERMANOVA to test for differences between zones
permanova_result <- adonis2(fg_comm ~ fg_metadata$Zone, permutations = 999, method = "bray")
print("PERMANOVA results for functional group composition between zones:")
print(permanova_result)

# Step 10: Create a comprehensive summary report
# --------------------------------------------
sink("Plankton_Functional_Groups_Summary.txt")

cat("PLANKTON FUNCTIONAL GROUPS ACROSS ZONES\n")
cat("======================================\n\n")

cat("SAMPLING DESIGN\n")
cat("--------------\n")
cat("- Equal sampling across zones: 8 samples per zone\n")
cat("- Unequal sampling across sites within zones\n\n")

cat("SUMMARY STATISTICS\n")
cat("-----------------\n")
cat("Total plankton count:", sum(plankton_data$`Total Count`), "\n")
cat("Number of samples:", length(unique(plankton_data$Sample)), "\n")
cat("Number of sites:", length(unique(plankton_data$Site)), "\n\n")

cat("Zone summary:\n")
zone_summary <- plankton_data %>%
  group_by(Zone) %>%
  summarize(
    NumSamples = n_distinct(Sample),
    NumSites = n_distinct(Site),
    TotalCount = sum(`Total Count`, na.rm = TRUE),
    .groups = "drop"
  )
print(zone_summary)
cat("\n")

cat("FUNCTIONAL GROUP COMPOSITION BY ZONE\n")
cat("---------------------------------\n")
fg_zone_summary <- fg_by_zone %>%
  select(Zone, FunctionalGroup, TotalCount, Percentage) %>%
  arrange(Zone, desc(Percentage))

for (zone in unique(fg_zone_summary$Zone)) {
  cat("\n", zone, "Zone:\n")
  zone_data <- fg_zone_summary %>% filter(Zone == zone)
  for (i in 1:nrow(zone_data)) {
    cat(sprintf("  %s: %.1f%% (%d individuals)\n", 
                zone_data$FunctionalGroup[i],
                zone_data$Percentage[i],
                zone_data$TotalCount[i]))
  }
}

cat("\nDIVERSITY METRICS\n")
cat("---------------\n")
print(diversity_by_zone)
cat("\n")

cat("DOMINANT FUNCTIONAL GROUPS BY SITE\n")
cat("-------------------------------\n")
for (zone in unique(dominant_fg$Zone)) {
  cat("\n", zone, "Zone:\n")
  zone_dominants <- dominant_fg %>% filter(Zone == zone)
  for (i in 1:nrow(zone_dominants)) {
    cat(sprintf("  Site %s: %s (%.1f%%)\n", 
                zone_dominants$Site[i],
                zone_dominants$FunctionalGroup[i],
                zone_dominants$Percentage[i]))
  }
}

cat("\nSTATISTICAL ANALYSIS\n")
cat("------------------\n")
cat("PERMANOVA results (testing for differences in functional group composition between zones):\n")
capture.output(permanova_result) %>% cat(sep = "\n")
cat("\n")

if (permanova_result[1, "Pr(>F)"] < 0.05) {
  cat("The analysis indicates significant differences in functional group composition between zones.\n")
} else {
  cat("The analysis does not indicate significant differences in functional group composition between zones.\n")
}

cat("\nKEY FINDINGS\n")
cat("-----------\n")

# Identify the most abundant functional group overall and by zone
overall_top_fg <- fg_by_zone %>%
  group_by(FunctionalGroup) %>%
  summarize(TotalCount = sum(TotalCount), .groups = "drop") %>%
  slice_max(order_by = TotalCount, n = 1)

cat("1. ", overall_top_fg$FunctionalGroup, " is the most abundant functional group overall,\n", 
    "   accounting for ", round(overall_top_fg$TotalCount / sum(fg_by_zone$TotalCount) * 100, 1), 
    "% of all plankton individuals.\n", sep = "")

# Find the zone with most diverse functional group composition
most_diverse_zone <- diversity_by_zone %>%
  slice_max(order_by = ShannonDiversity, n = 1)

cat("2. ", most_diverse_zone$Zone, " zone has the most diverse functional group composition\n", 
    "   with a Shannon diversity index of ", round(most_diverse_zone$ShannonDiversity, 2), ".\n", sep = "")

# Identify significant differences in functional group proportions
cat("3. Notable differences in functional group composition between zones include:\n")

# Get the most distinctive functional group for each zone
for (zone in unique(fg_by_zone$Zone)) {
  # Find the functional group with the greatest difference from other zones
  zone_fg <- fg_by_zone %>%
    filter(Zone == zone) %>%
    select(FunctionalGroup, Percentage)
  
  other_zones_fg <- fg_by_zone %>%
    filter(Zone != zone) %>%
    group_by(FunctionalGroup) %>%
    summarize(AvgPercentage = mean(Percentage), .groups = "drop")
  
  comparison <- zone_fg %>%
    left_join(other_zones_fg, by = "FunctionalGroup") %>%
    mutate(Difference = Percentage - AvgPercentage) %>%
    arrange(desc(abs(Difference))) %>%
    head(1)
  
  if (comparison$Difference > 0) {
    comparison_text <- "higher"
  } else {
    comparison_text <- "lower"
  }
  
  cat(sprintf("   - %s zone has %.1f%% %s abundance of %s compared to other zones (%.1f%%)\n", 
              zone, 
              comparison$Percentage, 
              comparison_text, 
              comparison$FunctionalGroup, 
              comparison$AvgPercentage))
}

cat("\nIMPLICATIONS FOR FISH-PLANKTON RELATIONSHIPS\n")
cat("------------------------------------------\n")
cat("Based on the distribution of functional plankton groups across zones, we can infer:\n\n")

cat("1. Fish feeding ecology: The dominance of certain plankton functional groups in\n")
cat("   specific zones may influence the distribution of fish species that preferentially\n")
cat("   feed on those groups. For example:\n")

# Find zones with high abundance of key prey groups
small_crust_zone <- fg_by_zone %>%
  filter(FunctionalGroup == "Small Crustacean Plankton") %>%
  slice_max(order_by = Percentage, n = 1)

cat(sprintf("   - %s zone has the highest proportion of Small Crustacean Plankton (%.1f%%),\n", 
            small_crust_zone$Zone, small_crust_zone$Percentage))
cat("     which may attract planktivorous fishes like scads (Decapterus sp.) and\n")
cat("     driftfish (Psenes sp.).\n")

large_crust_zone <- fg_by_zone %>%
  filter(FunctionalGroup == "Large Crustacean Plankton") %>%
  slice_max(order_by = Percentage, n = 1)

cat(sprintf("   - %s zone has the highest proportion of Large Crustacean Plankton (%.1f%%),\n", 
            large_crust_zone$Zone, large_crust_zone$Percentage))
cat("     which may attract predatory carangids and other larger pelagic predators.\n\n")

cat("2. Spatial patterns: The observed differences in functional group composition suggest\n")
cat("   potential spatial patterns in pelagic food web structure across the study area,\n")
cat("   which may influence the distribution and abundance of pelagic fish communities.\n\n")

cat("3. Future research: These findings suggest that further analysis of the relationship\n")
cat("   between specific fish species in the BRUV data and functional plankton groups\n")
cat("   could yield insights into how plankton community structure influences pelagic\n")
cat("   fish distribution and abundance in this ecosystem.\n")

sink()

cat("\nAnalysis complete!\n")
cat("Output files created:\n")
cat("1. Plankton_Composition_by_Zone_Percentage.png - Stacked bar chart of functional group percentages\n")
cat("2. Plankton_Abundance_by_Zone.png - Bar chart of absolute abundance by zone\n")
cat("3. Functional_Groups_Across_Zones.png - Faceted comparison of functional groups\n")
cat("4. Functional_Group_Diversity_by_Zone.png - Diversity and evenness comparisons\n")
cat("5. Functional_Group_Composition_with_Error.png - Composition with standard error bars\n")
cat("6. Functional_Group_Composition_by_Site.png - Heatmap of composition by site\n")
cat("7. NMDS_Functional_Groups.png - Multivariate analysis of community composition\n")
cat("8. Plankton_Functional_Groups_Summary.txt - Comprehensive summary report\n")