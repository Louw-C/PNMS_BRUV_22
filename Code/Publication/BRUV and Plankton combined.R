
# ---------------------------------------------
# Step 8: Combining with BRUV Data (if available)
# ---------------------------------------------

# This section would integrate BRUV data if it's available separately

# ---------------------------------------------
# Step 9: Summary Output
# ---------------------------------------------

# Create a summary table of key metrics
summary_table <- zooplankton_data %>%
  group_by(Zone) %>%
  summarize(
    Samples = n_distinct(Sample),
    Total_Abundance = sum(Total_Count),
    Unique_Categories = n_distinct(Plankton_Category),
    Unique_Specimen_Types = n_distinct(Specimen_Type),
    Unique_Morphotypes = n_distinct(Morphotype),
    .groups = "drop"
  ) %>%
  left_join(
    diversity_summary %>%
      select(Zone, Mean_Richness, Mean_Shannon, Mean_Evenness),
    by = "Zone"
  )

print("Summary table of key metrics by zone:")
print(summary_table)

# Save the summary table
write_csv(summary_table, "zooplankton_summary_by_zone.csv")

# Save diversity data for potential further analysis
write_csv(sample_diversity, "zooplankton_diversity_by_sample.csv")

# Final message
cat("\nZooplankton visualization and diversity analysis complete.\n")
cat("Output files generated:\n")
cat("- zooplankton_abundance_plots.png\n")
cat("- zooplankton_diversity_plots.png\n")
cat("- zooplankton_site_plots.png\n")
cat("- zooplankton_composition_plots.png\n")
cat("- zooplankton_nmds_plot.png\n")
cat("- zooplankton_summary_by_zone.csv\n")
cat("- zooplankton_diversity_by_sample.csv\n")