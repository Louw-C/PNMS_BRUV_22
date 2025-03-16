# Script to add BRUV String column to Zooplankton Data
# ----------------------------------------------

# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Step 1: Define the mapping between plankton strings and BRUV strings
mapping <- tibble::tribble(
  ~PlanktonString, ~BRUVString,
  "PAL22_N1_1_S", "PAL22_1",
  "PAL22_N1_2_S", "PAL22_2",
  "PAL22_N1_3_S", "PAL22_3",
  "PAL22_N3_1_S", "PAL22_4",
  "PAL22_N3_2_S", "PAL22_5",
  "PAL22_N3_3_S", "PAL22_6",
  "PAL22_N4_1_S", "PAL22_7",
  "PAL22_N4_2_S", "PAL22_8",
  "PAL22_S1_1_S", "PAL22_26",
  "PAL22_S1_3_S", "PAL22_27",
  "PAL22_S2_2_S", "PAL22_17",
  "PAL22_S2_3_S", "PAL22_18",
  "PAL22_S3_2_S", "PAL22_20",
  "PAL22_S3_3_S", "PAL22_21",
  "PAL22_S4_1_S", "PAL22_23",
  "PAL22_S4_2_S", "PAL22_24",
  "PAL22_W1_1_S", "PAL22_9",
  "PAL22_W1_2_S", "PAL22_10",
  "PAL22_WA_1_S", "PAL22_11",
  "PAL22_WA_2_S", "PAL22_12",
  "PAL22_W3_1_S", "PAL22_13",
  "PAL22_W4_1_S", "PAL22_14",
  "PAL22_W4_2_S", "PAL22_15",
  "PAL22_W4_3_S", "PAL22_16"
)

# Step 2: Load the zooplankton data
zooplankton_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /PAL_2022_Zooplankton Data_RAW.csv")
# Step 3: Add the BRUV string column to the zooplankton data
zooplankton_data_with_bruv <- zooplankton_data %>%
  left_join(mapping, by = c("String" = "PlanktonString"))

# Check if there are any NAs in the BRUVString column
if (any(is.na(zooplankton_data_with_bruv$BRUVString))) {
  warning("Some plankton strings could not be matched to a BRUV string.")
  
  # Identify unmatched plankton strings
  unmatched_strings <- zooplankton_data_with_bruv %>%
    filter(is.na(BRUVString)) %>%
    pull(String) %>%
    unique()
  
  cat("Unmatched plankton strings:", paste(unmatched_strings, collapse = ", "), "\n")
}

# Step 4: Save the updated zooplankton data
write_csv(zooplankton_data_with_bruv, "PAL_2022_Zooplankton_Data_with_BRUV.csv")

# Step 5: Print a summary
cat("\nZooplankton data with BRUV string column has been created.\n")
cat("Original data had", nrow(zooplankton_data), "rows and", ncol(zooplankton_data), "columns.\n")
cat("Updated data has", nrow(zooplankton_data_with_bruv), "rows and", ncol(zooplankton_data_with_bruv), "columns.\n")
