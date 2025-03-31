# Specimen-Level Plankton Outlier Detection
# This script analyzes the plankton dataset to identify potential outliers
# at the individual specimen type level within samples

# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(robustbase) # For robust statistics
library(gridExtra)  # For arranging multiple plots

# Load the zooplankton data
plankton_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /PAL_2022_Zooplankton_Data_with_BRUV.csv")

# Initial data exploration
cat("Dataset dimensions:", dim(plankton_data), "\n")
cat("Number of unique samples:", length(unique(plankton_data$Sample)), "\n")
cat("Number of specimen types:", length(unique(plankton_data$Specimen_Type)), "\n")

# ----- PART 1: EXAMINE THE RAW DATA STRUCTURE -----

# Look at distribution of counts by specimen type
specimen_summary <- plankton_data %>%
  # We need to group by more variables to maintain individual observations
  group_by(Specimen_Type, Sample, BRUVString, Morphotype) %>%
  # First summarize to get total counts per specimen type per sample per morphotype
  summarize(Count = sum(Total_Count, na.rm = TRUE), .groups = "drop") %>%
  # Then group only by Specimen_Type for the final statistics
  group_by(Specimen_Type) %>%
  summarize(
    Total_Count = sum(Count, na.rm = TRUE),
    Mean_Count = mean(Count, na.rm = TRUE),
    Median_Count = median(Count, na.rm = TRUE),
    Max_Count = max(Count, na.rm = TRUE),
    Min_Count = min(Count, na.rm = TRUE),
    SD_Count = sd(Count, na.rm = TRUE),
    Records = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Count))

# Print summary of specimen types
cat("\n--- Summary of specimen types ---\n")
print(specimen_summary)

# ----- PART 2: OUTLIER DETECTION FOR EACH SPECIMEN TYPE -----

# Function to calculate modified z-scores (more robust to outliers)
calculate_modified_z_scores <- function(x) {
  # Use median and MAD (Median Absolute Deviation) instead of mean and SD
  # Add check for mad = 0 to avoid division by zero
  mad_value <- mad(x, na.rm = TRUE)
  if (mad_value == 0) return(rep(0, length(x)))
  0.6745 * (x - median(x, na.rm = TRUE)) / mad_value
}

# Function to detect outliers for a specimen type
detect_specimen_outliers <- function(data, specimen_type, threshold = 3.5) {
  # Filter data for this specimen type
  spec_data <- data %>%
    filter(Specimen_Type == specimen_type)
  
  # Skip if too few observations
  if(nrow(spec_data) < 5) return(NULL)
  
  # Calculate modified z-scores
  spec_data <- spec_data %>%
    mutate(ModZ_Count = calculate_modified_z_scores(Total_Count))
  
  # Check which columns are available
  cols_to_select <- c("Sample", "BRUVString", "Zone", "Site", "Specimen_Type", 
                      "Morphotype", "Total_Count", "ModZ_Count")
  
  # Filter to include only columns that exist in the data
  cols_to_use <- intersect(cols_to_select, names(spec_data))
  
  # Identify outliers - WITHOUT using all_of()
  outliers <- spec_data %>%
    filter(abs(ModZ_Count) > threshold)
  
  # Manually select only the columns we want
  outliers <- outliers[, cols_to_use, drop = FALSE]
  
  # Sort by ModZ_Count
  outliers <- outliers %>%
    arrange(desc(abs(ModZ_Count)))
  
  return(outliers)
}

# Get list of common specimen types (with at least 5 observations)
common_specimen_types <- plankton_data %>%
  group_by(Specimen_Type) %>%
  summarize(
    Records = n(),
    .groups = 'drop'
  ) %>%
  filter(Records >= 5) %>%
  pull(Specimen_Type)

# Set outlier threshold
mod_z_threshold <- 3.5

# Initialize list to store outliers
all_specimen_outliers <- list()

# Process each specimen type
cat("\n--- Outlier detection by specimen type ---\n")
for(spec_type in common_specimen_types) {
  # Get data for this specimen type
  outliers <- detect_specimen_outliers(plankton_data, spec_type, mod_z_threshold)
  
  # Store and report results
  if(!is.null(outliers) && nrow(outliers) > 0) {
    all_specimen_outliers[[spec_type]] <- outliers
    cat("\nSpecimen type:", spec_type, "\n")
    cat("Found", nrow(outliers), "potential outliers\n")
    print(outliers)
  }
}

# Combine all outliers into a single dataframe (if any were found)
if(length(all_specimen_outliers) > 0) {
  combined_outliers <- bind_rows(all_specimen_outliers)
  
  # Print summary of outliers found
  cat("\n--- Specimen-level outliers detected ---\n")
  cat("Total outliers found:", nrow(combined_outliers), "\n")
  cat("Number of specimen types with outliers:", length(all_specimen_outliers), "\n")
  
  # Print the top outliers
  cat("\n--- Top outliers by modified z-score ---\n")
  print(combined_outliers %>% 
          arrange(desc(abs(ModZ_Count))) %>% 
          head(20))
} else {
  cat("\nNo specimen-level outliers detected with the given threshold.\n")
}

# ----- PART 3: VISUALIZE OUTLIERS FOR KEY SPECIMEN TYPES -----

# Function to create boxplot for a specific specimen type
create_specimen_boxplot <- function(data, spec_type) {
  spec_data <- data %>% 
    filter(Specimen_Type == spec_type)
  
  # Detect outliers for this specimen type
  outliers <- detect_specimen_outliers(data, spec_type, mod_z_threshold)
  
  p <- ggplot(spec_data, aes(x = 1, y = Total_Count)) +
    geom_boxplot(fill = "lightblue") +
    geom_jitter(width = 0.2, alpha = 0.6) +
    labs(title = paste("Distribution of", spec_type, "Counts"),
         x = "", y = "Count") +
    theme_minimal()
  
  # Add outlier labels if outliers exist
  if(!is.null(outliers) && nrow(outliers) > 0) {
    p <- p + 
      geom_text(data = outliers, 
                aes(x = 1, y = Total_Count, label = BRUVString),
                hjust = -0.3, size = 3)
  }
  
  return(p)
}

# Select top specimen types by total count for visualization
top_specimen_types <- specimen_summary %>% 
  head(4) %>% 
  pull(Specimen_Type)

# Create boxplots for top specimen types
par(mfrow=c(2,2))
for(spec_type in top_specimen_types) {
  print(create_specimen_boxplot(plankton_data, spec_type))
}
par(mfrow=c(1,1))

# ----- PART 4: CREATE DATASET WITH OUTLIERS REMOVED OR FLAGGED -----

# Create dataset with outlier flags
if(exists("combined_outliers") && nrow(combined_outliers) > 0) {
  # Mark outliers in the original dataset
  # First create a composite key for matching
  combined_outliers <- combined_outliers %>%
    mutate(match_key = paste(Sample, Specimen_Type, Morphotype, sep = "||"))
  
  plankton_data_with_flags <- plankton_data %>%
    mutate(match_key = paste(Sample, Specimen_Type, Morphotype, sep = "||"),
           Is_Outlier = match_key %in% combined_outliers$match_key) %>%
    select(-match_key)
  
  # Create cleaned dataset with outliers removed
  plankton_data_no_outliers <- plankton_data_with_flags %>%
    filter(!Is_Outlier)
  
  # Summary statistics
  cat("\n--- Impact of removing specimen-level outliers ---\n")
  cat("Original dataset:", nrow(plankton_data), "records\n")
  cat("Records flagged as outliers:", sum(plankton_data_with_flags$Is_Outlier), "\n")
  cat("Cleaned dataset:", nrow(plankton_data_no_outliers), "records\n")
  cat("Percentage of records removed:", 
      round(sum(plankton_data_with_flags$Is_Outlier) / nrow(plankton_data) * 100, 2), "%\n")
  
  # Save the flagged and cleaned datasets
  write_csv(plankton_data_with_flags, "Plankton_Data_With_Outlier_Flags.csv")
  write_csv(plankton_data_no_outliers, "Plankton_Data_No_Outliers.csv")
  
  cat("\nAnalysis complete. Results saved to CSV files.\n")
} else {
  cat("\nNo outliers detected with the current threshold, so no modified datasets were created.\n")
}

# ----- PART 5: ANALYZE IMPACT OF OUTLIER REMOVAL ON BRUV TOTALS -----

if(exists("plankton_data_no_outliers")) {
  # Original BRUV totals
  original_bruv_totals <- plankton_data %>%
    group_by(BRUVString, Zone, Site) %>%
    summarize(
      Total_Count = sum(Total_Count, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # BRUV totals with outliers removed
  cleaned_bruv_totals <- plankton_data_no_outliers %>%
    group_by(BRUVString, Zone, Site) %>%
    summarize(
      Total_Count = sum(Total_Count, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Compare the totals
  bruv_comparison <- original_bruv_totals %>%
    left_join(cleaned_bruv_totals, 
              by = c("BRUVString", "Zone", "Site"),
              suffix = c("_Original", "_Cleaned")) %>%
    mutate(
      Difference = Total_Count_Original - Total_Count_Cleaned,
      Percent_Change = (Difference / Total_Count_Original) * 100
    ) %>%
    arrange(desc(abs(Percent_Change)))
  
  cat("\n--- Impact of specimen-level outlier removal on BRUV string totals ---\n")
  print(bruv_comparison)
  
  # Save the comparison
  write_csv(bruv_comparison, "BRUV_Totals_After_Outlier_Removal.csv")
}
