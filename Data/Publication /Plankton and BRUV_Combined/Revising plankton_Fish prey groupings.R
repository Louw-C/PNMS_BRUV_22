# Plankton Functional Group Classification
# ----------------------------------------------
# This script adds ecological functional groups to the plankton dataset
# based on feeding relationships with pelagic fish

# Load required packages
library(tidyverse)
library(readr)


# Step 1: Load the plankton data
# ------------------------------
plankton_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /PAL_2022_Zooplankton_Publication.csv")

# Check the first few rows
head(plankton_data)

# Step 2: Create a lookup table for functional groups
# --------------------------------------------------
# This table assigns each specimen type to an ecological functional group
# based on known feeding relationships with pelagic fish

functional_groups <- tribble(
  ~`Specimen Type`, ~FunctionalGroup, ~Reasoning,
  
  # Group 1: Small Crustacean Plankton
  "Copepod", "Small Crustacean Plankton", "Primary prey for planktivorous fish; small-sized; numerically dominant in most pelagic systems",
  "Evadne", "Small Crustacean Plankton", "Small cladoceran consumed by planktivorous fish",
  "Ostracod", "Small Crustacean Plankton", "Small crustaceans commonly consumed by planktivorous fish",
  
  # Group 2: Large Crustacean Plankton
  "Krill", "Large Crustacean Plankton", "Larger crustacean prey targeted by many pelagic predators",
  "Amphipod", "Large Crustacean Plankton", "Larger crustacean prey important for carangids and other predatory fish",
  "Shrimp", "Large Crustacean Plankton", "Larger crustacean prey important for many pelagic predators",
  "Crab", "Large Crustacean Plankton", "Larger crustacean prey (likely larvae) consumed by larger predatory fish",
  "Stomatopod", "Large Crustacean Plankton", "Larger crustacean prey consumed by predatory fish",
  "Isopod", "Large Crustacean Plankton", "Larger crustacean prey consumed by various pelagic fish",
  
  # Group 3: Gelatinous Plankton
  "Other Gelatinous", "Gelatinous Plankton", "Soft-bodied prey requiring specialized feeding strategies",
  "Jelly", "Gelatinous Plankton", "Soft-bodied prey requiring specialized feeding strategies",
  "Salp", "Gelatinous Plankton", "Soft-bodied prey requiring specialized feeding strategies",
  "Chaetognath", "Gelatinous Plankton", "Arrow worms; soft-bodied predators consumed by larger fish",
  "Pteropod", "Gelatinous Plankton", "Sea butterflies; soft-bodied prey often grouped with gelatinous organisms",
  
  # Group 4: Fish Larvae & Eggs
  "Fish", "Fish Larvae & Eggs", "Likely fish larvae; high-value prey for piscivorous fish",
  "Huge Hairy Egg", "Fish Larvae & Eggs", "Likely fish or invertebrate eggs; important food resource",
  "Embryo", "Fish Larvae & Eggs", "Developing embryos; high-nutrition prey for predators",
  
  # Group 5: Benthic Invertebrate Larvae
  "Benthic Snail", "Benthic Invertebrate Larvae", "Temporary planktonic stages of benthic organisms",
  "Echinoderm", "Benthic Invertebrate Larvae", "Temporary planktonic stages of benthic organisms",
  "Nudibranch", "Benthic Invertebrate Larvae", "Temporary planktonic stages of benthic organisms",
  "Barnacle", "Benthic Invertebrate Larvae", "Temporary planktonic stages of benthic organisms",
  "Polychaete", "Benthic Invertebrate Larvae", "Temporary planktonic stages of benthic organisms",
  "Sipunculan", "Benthic Invertebrate Larvae", "Temporary planktonic stages of benthic organisms",
  "Bivalve", "Benthic Invertebrate Larvae", "Temporary planktonic stages of benthic organisms",
  "Limpet", "Benthic Invertebrate Larvae", "Temporary planktonic stages of benthic organisms",
  "Cnidaria", "Benthic Invertebrate Larvae", "Temporary planktonic stages of benthic organisms",
  "Bryozoan", "Benthic Invertebrate Larvae", "Temporary planktonic stages of benthic organisms",
  "Porifera", "Benthic Invertebrate Larvae", "Temporary planktonic stages of benthic organisms",
  "Platyhelminth", "Benthic Invertebrate Larvae", "Temporary planktonic stages of benthic organisms",
  "Hemichordate", "Benthic Invertebrate Larvae", "Temporary planktonic stages of benthic organisms",
  "Phoronid", "Benthic Invertebrate Larvae", "Temporary planktonic stages of benthic organisms",
  "Deciduous Larva", "Benthic Invertebrate Larvae", "Temporary planktonic stages of benthic organisms",
  "Ascidean Tadpole", "Benthic Invertebrate Larvae", "Temporary planktonic stages of benthic organisms"
)

# Check for any specimen types that might not be in our lookup table
missing_specimens <- setdiff(unique(plankton_data$`Specimen Type`), functional_groups$`Specimen Type`)
if(length(missing_specimens) > 0) {
  cat("Warning: The following specimen types are not in the lookup table and will not be classified:\n")
  cat(paste(missing_specimens, collapse = ", "), "\n")
}

# Step 3: Add functional group classification to the plankton data
# ---------------------------------------------------------------
# Join with our lookup table to add the functional group classification
plankton_data_classified <- plankton_data %>%
  left_join(functional_groups %>% select(`Specimen Type`, FunctionalGroup), 
            by = "Specimen Type")

# Check if any specimens weren't classified
unclassified <- plankton_data_classified %>% 
  filter(is.na(FunctionalGroup))

if(nrow(unclassified) > 0) {
  cat("Warning:", nrow(unclassified), "records could not be classified into functional groups.\n")
  
  # Show the unclassified specimen types
  unclassified_types <- unique(unclassified$`Specimen Type`)
  cat("Unclassified specimen types:", paste(unclassified_types, collapse = ", "), "\n")
  
  # Assign these to "Other" category
  plankton_data_classified <- plankton_data_classified %>%
    mutate(FunctionalGroup = if_else(is.na(FunctionalGroup), "Other", FunctionalGroup))
}

# Step 4: Summarize the data by functional group
# ----------------------------------------------
# Summarize abundance by functional group
functional_group_summary <- plankton_data_classified %>%
  group_by(FunctionalGroup) %>%
  summarize(
    Total_Count = sum(`Total Count`, na.rm = TRUE),
    Specimen_Types = n_distinct(`Specimen Type`),
    Records = n()
  ) %>%
  mutate(
    Percent_Abundance = Total_Count / sum(Total_Count) * 100
  ) %>%
  arrange(desc(Total_Count))

# Display the summary
print(functional_group_summary)

# Step 5: Save the classified data
# -------------------------------
# Save the dataset with functional groups
write_csv(plankton_data_classified, "PAL_2022_Zooplankton_Classified.csv")

# Create a detailed documentation file explaining the classification
sink("Plankton_Classification_Documentation.txt")

cat("PLANKTON FUNCTIONAL GROUP CLASSIFICATION\n")
cat("=========================================\n\n")

cat("Overview:\n")
cat("This document explains the ecological functional grouping of plankton specimens\n")
cat("based on their relevance as prey for pelagic fish species observed in BRUV surveys.\n\n")

cat("Classification Rationale:\n")
cat("Without direct size measurements, we created functional groups based on known\n")
cat("feeding relationships between pelagic fish and plankton taxa. These groups reflect\n")
cat("both taxonomic relationships and ecological roles in pelagic food webs.\n\n")

cat("Functional Groups:\n\n")

cat("1. Small Crustacean Plankton\n")
cat("   - Taxa included: Copepods, Evadne, Ostracods\n")
cat("   - Ecological role: Primary prey for planktivorous fish like scads (Decapterus sp.),\n")
cat("     driftfish (Psenes sp.), and juvenile triggerfishes\n")
cat("   - Characteristics: Generally small-sized crustaceans that form the base of many\n")
cat("     pelagic food webs; typically filter-fed by planktivorous fish\n\n")

cat("2. Large Crustacean Plankton\n")
cat("   - Taxa included: Krill, Amphipods, Shrimp, Crab, Stomatopod, Isopod\n")
cat("   - Ecological role: Prey for larger pelagic predators like jacks (Caranx sp.),\n")
cat("     amberjacks (Seriola spp.), and other carangids\n")
cat("   - Characteristics: Larger crustaceans that are actively pursued by visual predators;\n")
cat("     often higher in energy content than smaller zooplankton\n\n")

cat("3. Gelatinous Plankton\n")
cat("   - Taxa included: Other Gelatinous, Jelly, Salp, Chaetognath, Pteropod\n")
cat("   - Ecological role: Specialized prey requiring specific feeding adaptations;\n")
cat("     consumed by some filefish species and other specialized feeders\n")
cat("   - Characteristics: Soft-bodied organisms with high water content; generally lower\n")
cat("     energy density but may be abundant and accessible to specialized feeders\n\n")

cat("4. Fish Larvae & Eggs\n")
cat("   - Taxa included: Fish larvae, Huge Hairy Egg, Embryo\n")
cat("   - Ecological role: High-value prey for piscivorous and predatory pelagic fish\n")
cat("   - Characteristics: High in protein and lipids; actively targeted by many predatory fish\n\n")

cat("5. Benthic Invertebrate Larvae\n")
cat("   - Taxa included: Various larvae of benthic invertebrates (snails, echinoderms, etc.)\n")
cat("   - Ecological role: Temporary members of the plankton community during larval stages\n")
cat("   - Characteristics: Diverse group representing the planktonic phase of primarily\n")
cat("     benthic organisms; opportunistically consumed by many pelagic fish\n\n")

cat("Literature Support:\n")
cat("This classification is supported by numerous studies of pelagic fish feeding ecology,\n")
cat("including:\n")
cat("- Young et al. (2009) - Trophic ecology of tuna and their prey\n")
cat("- Choy et al. (2015) - Functional prey categories in pelagic food webs\n")
cat("- Brodeur et al. (2019) - Size-based functional groups in marine ecosystems\n\n")

cat("Limitations:\n")
cat("This classification system has several limitations:\n")
cat("1. Lack of direct size measurements for specimens (size is inferred based on taxonomy)\n")
cat("2. No differentiation between life stages within taxonomic groups\n")
cat("3. Some overlap between categories (some taxa could potentially fit multiple groups)\n")
cat("4. Simplified compared to more detailed classification schemes possible with additional data\n\n")

cat("Usage:\n")
cat("When analyzing relationships between fish and plankton, consider using these functional\n")
cat("groups rather than individual taxa, as they provide a more ecologically meaningful framework\n")
cat("for understanding trophic relationships in the pelagic ecosystem.\n")

sink()

# Step 6: Create a summary visualization
# -------------------------------------
# Create a bar chart of abundance by functional group
library(ggplot2)

# Plot the data
abundance_plot <- ggplot(functional_group_summary, 
                         aes(x = reorder(FunctionalGroup, Total_Count), 
                             y = Total_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Abundance of Plankton Functional Groups",
       x = "Functional Group",
       y = "Total Count") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)
abundance_plot

# Save the plot
ggsave("Plankton_Functional_Groups_Abundance.png", abundance_plot, 
       width = 8, height = 6, dpi = 300)

# Create a detailed table of specimen types within each functional group
specimen_details <- plankton_data_classified %>%
  group_by(FunctionalGroup, `Specimen Type`) %>%
  summarize(
    Total_Count = sum(`Total Count`, na.rm = TRUE),
    Records = n(),
    .groups = "drop"
  ) %>%
  arrange(FunctionalGroup, desc(Total_Count))
specimen_details 

# Save the detailed table
write_csv(specimen_details, "Plankton_Functional_Groups_Details.csv")

cat("\nAnalysis complete!\n")
cat("Created files:\n")
cat("1. PAL_2022_Zooplankton_Classified.csv - Original data with functional groups added\n")
cat("2. Plankton_Classification_Documentation.txt - Detailed explanation of classification\n")
cat("3. Plankton_Functional_Groups_Abundance.png - Visualization of functional group abundance\n")
cat("4. Plankton_Functional_Groups_Details.csv - Detailed breakdown of specimens within groups\n")