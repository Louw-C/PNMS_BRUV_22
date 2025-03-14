# PNMS BRUV Analysis with Outlier Item Removal
# -------------------------------------------

# Load required packages
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(vegan)     # For diversity indices

#Load data
bruv_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /PNMS_BRUV_2022.csv", stringsAsFactors = FALSE)

# Set a consistent color palette
zone_colors <- c("PNMS North" = "#0088FE", "DFZ West" = "#00C49F", "PNMS South" = "#FFBB28")

# Set factor levels to ensure consistent zone ordering
bruv_data$Zone <- factor(bruv_data$Zone, levels = c("PNMS North", "DFZ West", "PNMS South"))

# 1. Identify Outliers in Raw Data
# -------------------------------

# Function to find outliers using Tukey's rule
find_outlier_thresholds <- function(x, multiplier = 1.5) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - multiplier * iqr
  upper_bound <- q3 + multiplier * iqr
  return(list(lower = lower_bound, upper = upper_bound, q1 = q1, q3 = q3, iqr = iqr))
}

# Find outliers in biomass data
biomass_thresholds <- find_outlier_thresholds(bruv_data$Biomass_kg)
cat("Biomass quartiles (kg):", biomass_thresholds$q1, biomass_thresholds$q3, "\n")
cat("Biomass IQR (kg):", biomass_thresholds$iqr, "\n")
cat("Biomass outlier thresholds (kg):", biomass_thresholds$lower, biomass_thresholds$upper, "\n")

# Flag outliers in the data
bruv_data$biomass_outlier <- bruv_data$Biomass_kg < biomass_thresholds$lower | 
  bruv_data$Biomass_kg > biomass_thresholds$upper

# Find extreme outliers (3 * IQR)
extreme_thresholds <- find_outlier_thresholds(bruv_data$Biomass_kg, multiplier = 3)
cat("Extreme biomass outlier thresholds (kg):", extreme_thresholds$lower, extreme_thresholds$upper, "\n")

bruv_data$extreme_outlier <- bruv_data$Biomass_kg < extreme_thresholds$lower | 
  bruv_data$Biomass_kg > extreme_thresholds$upper

# Identify outliers
biomass_outliers <- bruv_data %>% 
  filter(biomass_outlier) %>% 
  arrange(desc(Biomass_kg)) %>%
  select(Sample, String, Site, Zone, Binomial, Biomass_kg)

extreme_outliers <- bruv_data %>% 
  filter(extreme_outlier) %>% 
  arrange(desc(Biomass_kg)) %>%
  select(Sample, String, Site, Zone, Binomial, Biomass_kg)

# Print outlier summary
cat("Number of biomass outliers:", nrow(biomass_outliers), "\n")
cat("Number of extreme biomass outliers:", nrow(extreme_outliers), "\n")

print("Top biomass outliers:")
print(head(biomass_outliers, 5))

print("Extreme biomass outliers:")
print(extreme_outliers)

# Create a dataset with extreme outliers removed
extreme_outlier_samples <- extreme_outliers$Sample
bruv_data_no_extreme <- bruv_data %>%
  filter(!(Sample %in% extreme_outlier_samples))

cat("Original data rows:", nrow(bruv_data), "\n")
cat("Rows after removing extreme outliers:", nrow(bruv_data_no_extreme), "\n")
cat("Number of items removed:", nrow(bruv_data) - nrow(bruv_data_no_extreme), "\n")

cat("\nDetailed list of extreme outlier items removed:\n")
print(extreme_outliers %>% 
        select(Sample, String, Site, Zone, Binomial, Biomass_kg) %>%
        arrange(String, Sample))

# Summarize what was removed by species
cat("\nSummary of species removed as extreme outliers:\n")
print(extreme_outliers %>%
        group_by(Binomial) %>%
        summarize(
          Count = n(),
          Total_Biomass_kg = sum(Biomass_kg),
          Mean_Biomass_kg = mean(Biomass_kg),
          Zones = paste(unique(Zone), collapse = ", ")
        ) %>%
        arrange(desc(Total_Biomass_kg)))

# Summarize what was removed by zone
cat("\nSummary of biomass removed by zone:\n")
print(extreme_outliers %>%
        group_by(Zone) %>%
        summarize(
          Items_Removed = n(),
          Total_Biomass_Removed_kg = sum(Biomass_kg),
          Percent_of_Outlier_Biomass = Total_Biomass_Removed_kg / sum(extreme_outliers$Biomass_kg) * 100
        ) %>%
        arrange(desc(Total_Biomass_Removed_kg)))

# 2. Calculate metrics by sample set (String)
# ------------------------------------------

# Function to calculate metrics from raw data
calculate_sample_metrics <- function(data) {
  # Get unique sample sets
  sample_sets <- unique(data$String)
  
  # Initialize results data frame
  results <- data.frame()
  
  for (set in sample_sets) {
    # Get data for this sample set
    set_data <- data %>% filter(String == set)
    
    # If no data for this set (all removed as outliers), skip
    if (nrow(set_data) == 0) next
    
    # Get zone and site
    zone <- set_data$Zone[1]
    site <- set_data$Site[1]
    
    # Count deployments - in this case 1 per sample set
    deployments <- 1
    
    # Calculate Taxonomic Richness (TR) - count of unique taxa
    taxa_count <- length(unique(set_data$Binomial[!is.na(set_data$Binomial)]))
    mean_tr <- taxa_count / deployments
    
    # Calculate Total Abundance (TA) - sum of MaxN
    total_maxn <- sum(set_data$MaxN, na.rm = TRUE)
    mean_ta <- total_maxn / deployments
    
    # Calculate Total Biomass (TB) - sum of biomass in kg
    total_biomass <- sum(set_data$Biomass_kg, na.rm = TRUE)
    mean_tb <- total_biomass / deployments
    
    # Add to results dataframe
    results <- rbind(results, data.frame(
      SampleSet = set,
      Site = site,
      Zone = zone,
      Deployments = deployments,
      TaxonomicRichness = mean_tr,
      TotalAbundance = mean_ta,
      TotalBiomass_kg = mean_tb
    ))
  }
  
  # Ensure Zone is a factor with correct levels
  results$Zone <- factor(results$Zone, levels = c("PNMS North", "DFZ West", "PNMS South"))
  
  return(results)
}

# Calculate metrics for both original data and data with extreme outliers removed
sample_metrics_original <- calculate_sample_metrics(bruv_data)
sample_metrics_no_extreme <- calculate_sample_metrics(bruv_data_no_extreme)

# 3. Compare the metrics from both datasets
# ----------------------------------------

# Function to identify affected sample sets
find_affected_sets <- function(metrics1, metrics2) {
  affected <- metrics1 %>%
    full_join(metrics2, by = "SampleSet", suffix = c("_orig", "_clean")) %>%
    filter(TotalBiomass_kg_orig != TotalBiomass_kg_clean |
             TaxonomicRichness_orig != TaxonomicRichness_clean |
             TotalAbundance_orig != TotalAbundance_clean) %>%
    mutate(
      Biomass_Diff = TotalBiomass_kg_orig - TotalBiomass_kg_clean,
      Biomass_Pct_Change = (TotalBiomass_kg_clean / TotalBiomass_kg_orig - 1) * 100,
      TR_Diff = TaxonomicRichness_orig - TaxonomicRichness_clean,
      TA_Diff = TotalAbundance_orig - TotalAbundance_clean
    ) %>%
    select(SampleSet, Zone_orig, Zone_clean, 
           TaxonomicRichness_orig, TaxonomicRichness_clean, TR_Diff,
           TotalAbundance_orig, TotalAbundance_clean, TA_Diff,
           TotalBiomass_kg_orig, TotalBiomass_kg_clean, Biomass_Diff, Biomass_Pct_Change)
  
  return(affected)
}

affected_sets <- find_affected_sets(sample_metrics_original, sample_metrics_no_extreme)

cat("Number of sample sets affected by removing extreme outliers:", nrow(affected_sets), "\n")
print("Summary of affected sample sets:")
print(affected_sets)

# Create data summaries grouped by zone
summarize_by_zone <- function(metrics) {
  summary <- metrics %>%
    group_by(Zone) %>%
    summarize(
      N = n(),
      TR_Mean = mean(TaxonomicRichness, na.rm = TRUE),
      TR_SD = sd(TaxonomicRichness, na.rm = TRUE),
      TR_Median = median(TaxonomicRichness, na.rm = TRUE),
      TA_Mean = mean(TotalAbundance, na.rm = TRUE),
      TA_SD = sd(TotalAbundance, na.rm = TRUE),
      TA_Median = median(TotalAbundance, na.rm = TRUE),
      TB_Mean = mean(TotalBiomass_kg, na.rm = TRUE),
      TB_SD = sd(TotalBiomass_kg, na.rm = TRUE),
      TB_Median = median(TotalBiomass_kg, na.rm = TRUE)
    )
  
  return(summary)
}

zone_summary_original <- summarize_by_zone(sample_metrics_original)
zone_summary_no_extreme <- summarize_by_zone(sample_metrics_no_extreme)

# Print zone summaries
cat("\nZone summary with original data:\n")
print(zone_summary_original)

cat("\nZone summary with extreme outlier items removed:\n")
print(zone_summary_no_extreme)

# 4. Visualizations comparing both approaches
# ------------------------------------------

# Function to create boxplots
create_boxplot <- function(data_orig, data_no_extreme, y_var, y_label, title) {
  # Combine datasets with a new column to indicate the dataset
  data_orig$Dataset <- "Original Data"
  data_no_extreme$Dataset <- "Extreme Outliers Removed"
  
  combined_data <- rbind(data_orig, data_no_extreme)
  combined_data$Dataset <- factor(combined_data$Dataset, 
                                  levels = c("Original Data", "Extreme Outliers Removed"))
  
  # Create the plot
  p <- ggplot(combined_data, aes_string(x = "Zone", y = y_var, fill = "Zone")) +
    geom_boxplot(outlier.shape = NA) +  # Hide default outliers
    geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
    scale_fill_manual(values = zone_colors) +
    facet_wrap(~ Dataset) +
    labs(title = title,
         x = "Zone",
         y = y_label) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  return(p)
}

# Create plots for each metric
p1 <- create_boxplot(sample_metrics_original, sample_metrics_no_extreme, 
                     "TaxonomicRichness", "Taxonomic Richness", 
                     "Taxonomic Richness by Zone")

p2 <- create_boxplot(sample_metrics_original, sample_metrics_no_extreme, 
                     "TotalAbundance", "Total Abundance (MaxN)", 
                     "Total Abundance by Zone")

# For biomass, also create a log-scaled version due to the wide range
p3a <- create_boxplot(sample_metrics_original, sample_metrics_no_extreme, 
                      "TotalBiomass_kg", "Total Biomass (kg)", 
                      "Total Biomass by Zone")

# Log-scaled version
p3b <- ggplot(rbind(
  mutate(sample_metrics_original, Dataset = "Original Data"),
  mutate(sample_metrics_no_extreme, Dataset = "Extreme Outliers Removed")
), aes(x = Zone, y = TotalBiomass_kg, fill = Zone)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  scale_y_log10() +
  scale_fill_manual(values = zone_colors) +
  facet_wrap(~ Dataset) +
  labs(title = "Total Biomass by Zone (log scale)",
       x = "Zone",
       y = "Total Biomass (kg, log scale)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Display plots
print(p1)
print(p2)
print(p3a)
print(p3b)

# 5. Create publication-ready tables
# ---------------------------------

create_publication_table <- function(summary_data, label = "") {
  publication_table <- summary_data %>%
    mutate(
      TR = sprintf("%.2f ± %.2f (%.1f)", TR_Mean, TR_SD, TR_Median),
      TA = sprintf("%.2f ± %.2f (%.1f)", TA_Mean, TA_SD, TA_Median),
      TB = sprintf("%.2f ± %.2f (%.2f)", TB_Mean, TB_SD, TB_Median)
    ) %>%
    select(Zone, N, TR, TA, TB)
  
  names(publication_table) <- c(
    "Zone", 
    "N", 
    "Taxonomic Richness Mean ± SD (Median)", 
    "Total Abundance Mean ± SD (Median)", 
    "Total Biomass (kg) Mean ± SD (Median)"
  )
  
  return(publication_table)
}

pub_table_original <- create_publication_table(zone_summary_original, "Original Data")
pub_table_no_extreme <- create_publication_table(zone_summary_no_extreme, "Without Extreme Outliers")

# Print publication tables
cat("\nSummary Table for Publication (Original Data):\n")
print(pub_table_original)

cat("\nSummary Table for Publication (With Extreme Outlier Items Removed):\n")
print(pub_table_no_extreme)

# 4. Additional visualizations focusing only on data with outliers removed
# ---------------------------------------------------------------------

# Create standalone plots for cleaned data only
create_clean_plot <- function(data, y_var, y_label, title) {
  p <- ggplot(data, aes_string(x = "Zone", y = y_var, fill = "Zone")) +
    geom_boxplot() +
    geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
    scale_fill_manual(values = zone_colors) +
    labs(title = title,
         x = "Zone",
         y = y_label) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  return(p)
}

# Create focused plots for cleaned data
p_clean1 <- create_clean_plot(sample_metrics_no_extreme, 
                              "TaxonomicRichness", 
                              "Taxonomic Richness", 
                              "Taxonomic Richness by Zone (Outliers Removed)")

p_clean2 <- create_clean_plot(sample_metrics_no_extreme, 
                              "TotalAbundance", 
                              "Total Abundance (MaxN)", 
                              "Total Abundance by Zone (Outliers Removed)")

p_clean3 <- create_clean_plot(sample_metrics_no_extreme, 
                              "TotalBiomass_kg", 
                              "Total Biomass (kg)", 
                              "Total Biomass by Zone (Outliers Removed)")

# Log-scaled version for biomass
p_clean4 <- ggplot(sample_metrics_no_extreme, aes(x = Zone, y = TotalBiomass_kg, fill = Zone)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  scale_y_log10() +
  scale_fill_manual(values = zone_colors) +
  labs(title = "Total Biomass by Zone (Outliers Removed, Log Scale)",
       x = "Zone",
       y = "Total Biomass (kg, log scale)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Create bar plots with error bars for cleaned data
create_barplot_with_error <- function(summary_data, value_col, error_col, y_label, title) {
  p <- ggplot(summary_data, aes_string(x = "Zone", y = value_col, fill = "Zone")) +
    geom_bar(stat = "identity", width = 0.6) +
    geom_errorbar(aes_string(ymin = paste0(value_col, "-", error_col), 
                             ymax = paste0(value_col, "+", error_col)),
                  width = 0.2) +
    scale_fill_manual(values = zone_colors) +
    labs(title = title,
         x = "Zone",
         y = y_label) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  return(p)
}

# Create bar plots with error bars
p_bar1 <- create_barplot_with_error(zone_summary_no_extreme,
                                    "TR_Mean", "TR_SD",
                                    "Mean Taxonomic Richness (± SD)",
                                    "Mean Taxonomic Richness by Zone (Outliers Removed)")

p_bar2 <- create_barplot_with_error(zone_summary_no_extreme,
                                    "TA_Mean", "TA_SD",
                                    "Mean Total Abundance (± SD)",
                                    "Mean Total Abundance by Zone (Outliers Removed)")

p_bar3 <- create_barplot_with_error(zone_summary_no_extreme,
                                    "TB_Mean", "TB_SD",
                                    "Mean Total Biomass (kg) (± SD)",
                                    "Mean Total Biomass by Zone (Outliers Removed)")

# Display cleaned data plots
print(p_clean1)
print(p_clean2)
print(p_clean3)
print(p_clean4)
print(p_bar1)
print(p_bar2)
print(p_bar3)

# Create a combined panel of cleaned data plots
cleaned_boxplots <- grid.arrange(p_clean1, p_clean2, p_clean3, ncol = 2)
cleaned_barplots <- grid.arrange(p_bar1, p_bar2, p_bar3, ncol = 2)

print(cleaned_boxplots)
print(cleaned_barplots)

# 6. Statistical tests comparing zones
# ----------------------------------
# Using the dataset with extreme outliers removed for more robust comparisons

# ANOVA tests for zone differences
tr_aov <- aov(TaxonomicRichness ~ Zone, data = sample_metrics_no_extreme)
tr_tukey <- TukeyHSD(tr_aov)

ta_aov <- aov(TotalAbundance ~ Zone, data = sample_metrics_no_extreme)
ta_tukey <- TukeyHSD(ta_aov)

# Log-transform biomass for better normality in statistical tests
tb_aov <- aov(log1p(TotalBiomass_kg) ~ Zone, data = sample_metrics_no_extreme)
tb_tukey <- TukeyHSD(tb_aov)

# Print ANOVA results
cat("\nANOVA for Taxonomic Richness by Zone (extreme outliers removed):\n")
print(summary(tr_aov))
print(tr_tukey)

cat("\nANOVA for Total Abundance by Zone (extreme outliers removed):\n")
print(summary(ta_aov))
print(ta_tukey)

cat("\nANOVA for Total Biomass (log-transformed) by Zone (extreme outliers removed):\n")
print(summary(tb_aov))
print(tb_tukey)

# Non-parametric Kruskal-Wallis tests as an alternative
tr_kw <- kruskal.test(TaxonomicRichness ~ Zone, data = sample_metrics_no_extreme)
ta_kw <- kruskal.test(TotalAbundance ~ Zone, data = sample_metrics_no_extreme)
tb_kw <- kruskal.test(TotalBiomass_kg ~ Zone, data = sample_metrics_no_extreme)

cat("\nKruskal-Wallis test for Taxonomic Richness by Zone (extreme outliers removed):\n")
print(tr_kw)

cat("\nKruskal-Wallis test for Total Abundance by Zone (extreme outliers removed):\n")
print(ta_kw)

cat("\nKruskal-Wallis test for Total Biomass by Zone (extreme outliers removed):\n")
print(tb_kw)

# 7. Save outputs
# -------------
# Create directories if they don't exist
dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# Save tables
write.csv(pub_table_original, "results/metrics_original_data.csv", row.names = FALSE)
write.csv(pub_table_no_extreme, "results/metrics_extreme_items_removed.csv", row.names = FALSE)
write.csv(affected_sets, "results/affected_sample_sets.csv", row.names = FALSE)
write.csv(extreme_outliers, "results/extreme_outlier_items.csv", row.names = FALSE)

# Save plots
ggsave("figures/taxonomic_richness_comparison.png", plot = p1, width = 10, height = 6)
ggsave("figures/abundance_comparison.png", plot = p2, width = 10, height = 6)
ggsave("figures/biomass_comparison.png", plot = p3a, width = 10, height = 6)
ggsave("figures/biomass_log_comparison.png", plot = p3b, width = 10, height = 6)

# Summary and conclusions
# ---------------------
cat("\n=== SUMMARY AND RECOMMENDATIONS ===\n")
cat("1. Identified", nrow(extreme_outliers), "extreme biomass outlier items that could be removed\n")
cat("2. These items affected", nrow(affected_sets), "out of", nrow(sample_metrics_original), "sample sets\n")
cat("3. Removing these items substantially changed the biomass estimates, especially in DFZ West and PNMS South\n")
cat("4. Recommended approach: Report results both with and without extreme outlier items removed\n")
cat("5. Present both original data (ecological reality including rare large predators) and cleaned data (typical community structure)\n")