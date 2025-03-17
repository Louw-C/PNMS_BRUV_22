# PNMS Zooplankton Visualization and Diversity Analysis
# ---------------------------------------------
# Load required packages
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(vegan)     # For diversity indices
library(readxl)    # For reading Excel files
library(viridis)   # For color palettes
library(reshape2)  # For data reshaping
library(RColorBrewer)  # For additional color palettes
library(patchwork)  # For combining plots

# Avoid scientific notation
options(scipen = 999)

# Set a consistent color palette based on the data
zone_colors <- c("North" = "#0088FE", "West" = "#00C49F", "South" = "#FFBB28")

# ---------------------------------------------
# Step 1: Data Loading and Preparation
# ---------------------------------------------
# Load the zooplankton data
zooplankton_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /PAL_2022_Zooplankton_Data_with_BRUV.csv")

# Ensure all categorical variables are factors with correct levels
zooplankton_data <- zooplankton_data %>%
  mutate(
    Zone = factor(Zone, levels = c("North", "West", "South")),
    Site = factor(Site),
    Plankton_Category = factor(Plankton_Category),
    Specimen_Type = factor(Specimen_Type),
    Morphotype = factor(Morphotype)
  )

# Create a sample metadata table
sample_metadata <- zooplankton_data %>%
  select(Sample, Zone, Site, BRUVString) %>%
  distinct()

# Display sample counts by zone
cat("Number of samples by zone:\n")
print(table(sample_metadata$Zone))

# ---------------------------------------------
# Step 2: Basic Summary Statistics
# ---------------------------------------------
# Calculate total counts by sample and category
summary_by_sample <- zooplankton_data %>%
  group_by(Sample, Zone, Site) %>%
  summarize(
    Total_Count = sum(Total_Count),
    Category_Count = n_distinct(Plankton_Category),
    Specimen_Type_Count = n_distinct(Specimen_Type),
    Morphotype_Count = n_distinct(Morphotype),
    .groups = "drop"
  )

# Calculate means by zone
zone_summary <- summary_by_sample %>%
  group_by(Zone) %>%
  summarize(
    Mean_Total_Count = mean(Total_Count),
    SE_Total_Count = sd(Total_Count) / sqrt(n()),
    Mean_Category_Count = mean(Category_Count),
    SE_Category_Count = sd(Category_Count) / sqrt(n()),
    Mean_Specimen_Count = mean(Specimen_Type_Count),
    SE_Specimen_Count = sd(Specimen_Type_Count) / sqrt(n()),
    Mean_Morphotype_Count = mean(Morphotype_Count),
    SE_Morphotype_Count = sd(Morphotype_Count) / sqrt(n()),
    .groups = "drop"
  )

# Create a version of zone_summary with counts included
zone_summary_with_n <- zone_summary %>%
  left_join(
    summary_by_sample %>%
      group_by(Zone) %>%
      summarize(n = n(), .groups = "drop"),
    by = "Zone"
  )

print("Summary statistics by zone:")
print(zone_summary)

# ---------------------------------------------
# Step 3: Visualizing Abundance Data
# ---------------------------------------------

# 1. Total zooplankton count by zone (bar plot with error bars)
p1 <- ggplot(zone_summary_with_n, aes(x = Zone, y = Mean_Total_Count, fill = Zone)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Mean_Total_Count - SE_Total_Count, 
                    ymax = Mean_Total_Count + SE_Total_Count), 
                width = 0.25) +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Mean Total Zooplankton Abundance by Zone",
       x = "Zone", y = "Mean Count (±SE)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5))
p1

# Calculate mean total count per site 

site_means <- zooplankton_data %>%
  # First, get the total count for each sample at each site
  group_by(Sample, Zone, Site) %>%
  summarize(
    Sample_Total_Count = sum(Total_Count),
    .groups = "drop"
  ) %>%
  # Then calculate the mean and SE of these totals for each site
  group_by(Zone, Site) %>%
  summarize(
    Mean_Count = mean(Sample_Total_Count),
    SE_Count = sd(Sample_Total_Count) / sqrt(n()),
    Sample_Count = n(),  # Number of samples per site
    .groups = "drop"
  )

# Create site order by zone
site_means <- site_means %>%
  arrange(Zone, Site) %>%
  mutate(Site = factor(Site, levels = unique(Site)))

# Create the improved plot with means and error bars
p2 <- ggplot(site_means, aes(x = Site, y = Mean_Count, fill = Zone)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Mean_Count - SE_Count, 
                    ymax = Mean_Count + SE_Count), 
                width = 0.25) +
  scale_fill_manual(values = zone_colors) +
  geom_text(aes(label = paste("N =", Sample_Count)), 
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3.5) +
  labs(title = "Mean Zooplankton Abundance by Site",
       x = "Site", y = "Mean Count per Sample (±SE)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 12, face = "bold"))

p2

# 3. Distribution of plankton categories by zone
category_by_zone <- zooplankton_data %>%
  group_by(Zone, Plankton_Category) %>%
  summarize(
    Total_Count = sum(Total_Count),
    .groups = "drop"
  ) %>%
  group_by(Zone) %>%
  mutate(Percentage = Total_Count / sum(Total_Count) * 100)

p3 <- ggplot(category_by_zone, aes(x = Zone, y = Percentage, fill = Plankton_Category)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Plankton Category Composition by Zone",
       x = NULL, y = "Percentage", fill = "Plankton Category") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"))
p3

# Identify the top specimen types across all zones
top_specimens <- zooplankton_data %>%
  group_by(Specimen_Type) %>%
  summarize(
    Total_Count = sum(Total_Count),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Count)) %>%
  head(15) %>%  # Increased to 15 for more comprehensive view
  pull(Specimen_Type)

# Prepare data for heatmap with the top specimen types
heatmap_data <- zooplankton_data %>%
  filter(Specimen_Type %in% top_specimens) %>%
  group_by(Zone, Specimen_Type) %>%
  summarize(
    Total_Count = sum(Total_Count),
    .groups = "drop"
  ) %>%
  # Calculate percentage within each zone
  group_by(Zone) %>%
  mutate(
    Percentage = Total_Count / sum(Total_Count) * 100,
    # For log transformation (adding 1 to handle zeros)
    Log_Count = log10(Total_Count + 1)
  ) %>%
  ungroup()

# Create heatmap
p_heatmap <- ggplot(heatmap_data, aes(x = Zone, y = Specimen_Type, fill = Log_Count)) +
  geom_tile(color = "white", linewidth = 0.2) +
  # Add count text to each tile
  geom_text(aes(label = round(Total_Count, 0)), color = "black", size = 3) +
  # Use viridis color palette for better perception
  scale_fill_viridis_c(name = "Log10(Count+1)", option = "viridis") +
  # Make sure specimen types are ordered by overall abundance
  scale_y_discrete(limits = top_specimens) +
  labs(title = "Top Specimen Types by Zone",
       x = NULL, y = "Specimen Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 9)
  )

# Print the heatmap
p_heatmap

# Combine the plots using patchwork
basic_plots <- (p1 | p2) / (p3 | p_heatmap)
print(basic_plots)

# Save the combined plot
ggsave("zooplankton_abundance_plots.png", basic_plots, width = 14, height = 10, dpi = 300)

# ---------------------------------------------
# Step 4: Diversity Analysis
# ---------------------------------------------

# 1. Calculate diversity indices for each sample
# Prepare data in wide format for diversity calculation
diversity_data <- zooplankton_data %>%
  select(Sample, Zone, Site, Specimen_Type, Total_Count) %>%
  group_by(Sample, Zone, Site, Specimen_Type) %>%
  summarize(Total_Count = sum(Total_Count), .groups = "drop") %>%
  pivot_wider(names_from = Specimen_Type, values_from = Total_Count, values_fill = 0)

# Extract the abundance matrix
abundance_matrix <- diversity_data %>%
  select(-Sample, -Zone, -Site) %>%
  as.matrix()

# Calculate diversity indices
sample_diversity <- data.frame(
  Sample = diversity_data$Sample,
  Zone = diversity_data$Zone,
  Site = diversity_data$Site,
  Species_Richness = specnumber(abundance_matrix),
  Shannon_Diversity = diversity(abundance_matrix, index = "shannon"),
  Simpson_Diversity = diversity(abundance_matrix, index = "simpson"),
  Pielou_Evenness = diversity(abundance_matrix, index = "shannon") / log(specnumber(abundance_matrix))
)

# Replace NaN values (when richness is 0) with 0
sample_diversity$Pielou_Evenness[is.nan(sample_diversity$Pielou_Evenness)] <- 0

# Summary of diversity by zone
diversity_summary <- sample_diversity %>%
  group_by(Zone) %>%
  summarize(
    Mean_Richness = mean(Species_Richness),
    SE_Richness = sd(Species_Richness) / sqrt(n()),
    Mean_Shannon = mean(Shannon_Diversity),
    SE_Shannon = sd(Shannon_Diversity) / sqrt(n()),
    Mean_Simpson = mean(Simpson_Diversity),
    SE_Simpson = sd(Simpson_Diversity) / sqrt(n()),
    Mean_Evenness = mean(Pielou_Evenness),
    SE_Evenness = sd(Pielou_Evenness) / sqrt(n()),
    .groups = "drop"
  )

print("Diversity indices by zone:")
print(diversity_summary)

# 2. Visualize diversity metrics
# Richness by zone
p5 <- ggplot(diversity_summary, aes(x = Zone, y = Mean_Richness, fill = Zone)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Mean_Richness - SE_Richness, 
                    ymax = Mean_Richness + SE_Richness), 
                width = 0.25) +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Species Richness by Zone",
       x = NULL, y = "Mean Species Richness (±SE)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, face = "bold"))
p5

# Shannon diversity by zone
p6 <- ggplot(diversity_summary, aes(x = Zone, y = Mean_Shannon, fill = Zone)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Mean_Shannon - SE_Shannon, 
                    ymax = Mean_Shannon + SE_Shannon), 
                width = 0.25) +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Shannon Diversity by Zone",
       x = NULL, y = "Mean Shannon Index (±SE)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, face = "bold"))
p6

# Evenness by zone
p7 <- ggplot(diversity_summary, aes(x = Zone, y = Mean_Evenness, fill = Zone)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Mean_Evenness - SE_Evenness, 
                    ymax = Mean_Evenness + SE_Evenness), 
                width = 0.25) +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Species Evenness by Zone",
       x = NULL, y = "Mean Pielou's Evenness (±SE)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, face = "bold"))
p7

# Combine diversity plots
diversity_plots <- p5 | p6 | p7
print(diversity_plots)

# Save the diversity plots
ggsave("zooplankton_diversity_plots.png", diversity_plots, width = 12, height = 6, dpi = 300)

# ---------------------------------------------
# Step 5: Site-Level Analysis
# ---------------------------------------------

# Calculate diversity metrics by site
site_diversity <- sample_diversity %>%
  group_by(Zone, Site) %>%
  summarize(
    Mean_Richness = mean(Species_Richness),
    SE_Richness = sd(Species_Richness) / sqrt(n()),
    Mean_Shannon = mean(Shannon_Diversity),
    SE_Shannon = sd(Shannon_Diversity) / sqrt(n()),
    Samples = n(),
    .groups = "drop"
  )

# Order sites by zone
site_diversity <- site_diversity %>%
  arrange(Zone, Site) %>%
  mutate(Site = factor(Site, levels = unique(Site)))

# Plot richness by site
p8 <- ggplot(site_diversity, aes(x = Site, y = Mean_Richness, fill = Zone)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Mean_Richness - SE_Richness, 
                    ymax = Mean_Richness + SE_Richness), 
                width = 0.25) +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Species Richness by Site",
       x = NULL, y = "Mean Species Richness (±SE)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 12, face = "bold"))
p8

# Plot Shannon diversity by site
p9 <- ggplot(site_diversity, aes(x = Site, y = Mean_Shannon, fill = Zone)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Mean_Shannon - SE_Shannon, 
                    ymax = Mean_Shannon + SE_Shannon), 
                width = 0.25) +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Shannon Diversity by Site",
       x = NULL, y = "Mean Shannon Index (±SE)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 12, face = "bold"))
p9

# Combine site-level plots
site_plots <- p8 / p9
print(site_plots)

# Save the site-level plots
ggsave("zooplankton_site_plots.png", site_plots, width = 10, height = 8, dpi = 300)

# ---------------------------------------------
# Step 6: Specimen Type Composition Analysis
# ---------------------------------------------

# Calculate the top 10 specimen types by total count
top10_specimens <- zooplankton_data %>%
  group_by(Specimen_Type) %>%
  summarize(Total_Count = sum(Total_Count)) %>%
  arrange(desc(Total_Count)) %>%
  head(10) %>%
  pull(Specimen_Type)

# Filter for only the top 10 specimen types
top10_data <- zooplankton_data %>%
  filter(Specimen_Type %in% top10_specimens) %>%
  mutate(Specimen_Type = factor(Specimen_Type, levels = top10_specimens))

# Calculate percentages by zone for the top 10 specimen types
top10_zone_composition <- top10_data %>%
  group_by(Zone, Specimen_Type) %>%
  summarize(Total_Count = sum(Total_Count), .groups = "drop") %>%
  group_by(Zone) %>%
  mutate(Percentage = Total_Count / sum(Total_Count) * 100)

# Create stacked bar chart of top 10 specimen types by zone
p10 <- ggplot(top10_zone_composition, aes(x = Zone, y = Percentage, fill = Specimen_Type)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.7) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Composition of Top 10 Specimen Types by Zone",
       x = NULL, y = "Percentage", fill = "Specimen Type") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"))
p10

# Create a heatmap of the top 10 specimen types across sites
top10_site_matrix <- top10_data %>%
  group_by(Zone, Site, Specimen_Type) %>%
  summarize(Total_Count = sum(Total_Count), .groups = "drop") %>%
  ungroup() %>%
  # Create combined Site label with Zone prefix
  mutate(SiteZone = paste(Zone, Site, sep = "-")) %>%
  # Order by Zone and Site
  arrange(Zone, Site) %>%
  # Convert to matrix format
  select(SiteZone, Specimen_Type, Total_Count) %>%
  pivot_wider(names_from = Specimen_Type, values_from = Total_Count, values_fill = 0)

# Extract site order
site_order <- top10_site_matrix$SiteZone
top10_site_matrix <- top10_site_matrix %>% 
  select(-SiteZone) %>% 
  as.matrix()
rownames(top10_site_matrix) <- site_order

# Convert to long format for heatmap
heatmap_data <- melt(top10_site_matrix) %>%
  rename(SiteZone = Var1, Specimen_Type = Var2, Count = value) %>%
  # Extract Zone from SiteZone
  mutate(Zone = str_extract(SiteZone, "^[^-]+"))

# Create heatmap
p11 <- ggplot(heatmap_data, aes(x = Specimen_Type, y = SiteZone, fill = log1p(Count))) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_viridis(name = "Log(Count+1)", option = "viridis") +
  labs(title = "Heatmap of Top 10 Specimen Types Across Sites",
       x = "Specimen Type", y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 12, face = "bold"))
p11

# Combine composition plots
composition_plots <- p10 / p11
print(composition_plots)

# Save the composition plots
ggsave("zooplankton_composition_plots.png", composition_plots, width = 10, height = 12, dpi = 300)

# Calculate NMDS ordination based on Bray-Curtis dissimilarity
# Prepare community matrix (samples x morphotypes)
community_matrix <- zooplankton_data %>%
  group_by(Sample, Morphotype) %>%
  summarize(Total_Count = sum(Total_Count), .groups = "drop") %>%
  pivot_wider(names_from = Morphotype, values_from = Total_Count, values_fill = 0)

# Extract sample metadata
nmds_metadata <- community_matrix %>%
  select(Sample) %>%
  left_join(select(sample_metadata, Sample, Zone, Site), by = "Sample")

# Make sure Site is a factor with limited levels for proper shape mapping
nmds_metadata$Site <- factor(nmds_metadata$Site)

# Prepare community data for NMDS
community_data <- community_matrix %>%
  select(-Sample) %>%
  as.matrix()

# Check if there's enough variation in the data for NMDS
if(sum(rowSums(community_data) > 0) > 3) {
  # Calculate Bray-Curtis dissimilarity
  bc_dist <- vegdist(community_data, method = "bray")
  
  # Perform NMDS
  set.seed(123) # For reproducibility
  nmds_result <- metaMDS(bc_dist, k = 2, trymax = 100)
  
  # Extract NMDS scores
  nmds_scores <- as.data.frame(scores(nmds_result, display = "sites"))
  nmds_scores$Sample <- nmds_metadata$Sample
  nmds_scores$Zone <- nmds_metadata$Zone
  nmds_scores$Site <- nmds_metadata$Site
  
  # Create a simplified plot without site shapes 
  # (since you have too many sites for distinct shapes)
  p12 <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Zone)) +
    geom_point(size = 3, alpha = 0.8) +
    scale_color_manual(values = zone_colors) +
    labs(title = paste("NMDS Ordination of Zooplankton Communities"),
         subtitle = paste("Stress =", round(nmds_result$stress, 3)),
         x = "NMDS1", y = "NMDS2") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"))
  
  # Draw ellipses around zones
  p12 <- p12 + 
    stat_ellipse(aes(group = Zone), linetype = 2)
  
  print(p12)
  
  # Save the NMDS plot
  ggsave("zooplankton_nmds_plot.png", p12, width = 8, height = 6, dpi = 300)
  
  # Perform PERMANOVA test
  permanova_result <- adonis2(bc_dist ~ Zone, data = nmds_metadata, permutations = 999)
  print("PERMANOVA results:")
  print(permanova_result)
  
  # Perform pairwise PERMANOVA if overall is significant
  if(permanova_result["Zone", "Pr(>F)"] < 0.05) {
    # Make sure Zone is a factor with proper levels
    nmds_metadata$Zone <- factor(nmds_metadata$Zone)
    zone_pairs <- combn(levels(nmds_metadata$Zone), 2, simplify = FALSE)
    
    # Empty list to store results
    pairwise_results <- list()
    
    # Try-catch block to handle potential errors
    for(i in seq_along(zone_pairs)) {
      pair <- zone_pairs[[i]]
      tryCatch({
        sub_meta <- nmds_metadata[nmds_metadata$Zone %in% pair, ]
        
        # Make sure we have the right rows from community_data
        matched_rows <- match(sub_meta$Sample, nmds_metadata$Sample)
        sub_data <- community_data[matched_rows, ]
        
        # Calculate distance matrix
        sub_dist <- vegdist(sub_data, method = "bray")
        
        # Run pairwise PERMANOVA
        result <- adonis2(sub_dist ~ Zone, data = sub_meta, permutations = 999)
        
        pairwise_results[[i]] <- data.frame(
          comparison = paste(pair, collapse = " vs "),
          p_value = result["Zone", "Pr(>F)"]
        )
      }, error = function(e) {
        message("Error in pairwise comparison ", paste(pair, collapse = " vs "), ": ", e$message)
      })
    }
    
    # Combine results if any exist
    if(length(pairwise_results) > 0) {
      pairwise_df <- do.call(rbind, pairwise_results)
      print("Pairwise PERMANOVA results:")
      print(pairwise_df)
    } else {
      print("No pairwise comparisons could be completed due to errors.")
    }
  }
} else {
  cat("Not enough variation in the data for NMDS analysis.\n")
}

