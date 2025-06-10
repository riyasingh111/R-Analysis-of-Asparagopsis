# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(tibble)
library(farver)

# Step 2: Define raw data
data_raw <- tibble(
  Strain = c(rep("KU1196", 6), rep("CCAP", 6), rep("KU1884", 6), rep("KU1885", 6)),
  Media_Type = c(rep(c("F/2", "Cell Hi Bio"), each = 3), rep(c("F/2", "Cell Hi Bio"), each = 3),
                 rep(c("F/2", "Cell Hi Bio"), each = 3), rep(c("F/2", "Cell Hi Bio"), each = 3)),
  Replicate = rep(c("A1", "A2", "A3", "B1", "B2", "B3"), 4),
  Carbohydrate = c(20.4, 19.6, 19.6, 24.6, 25.4, 24.4, 32.0, 33.0, 34.0, 31.6, 31.6, 31.6,
                   30.4, 32.0, 30.4, 32.0, 31.4, 30.4, 33.0, 33.6, 35.4, 30.4, 30.6, 31.4),
  Protein = c(21.55, 21.38, 21.42, 20.33, 20.52, 20.62, 18.95, 18.82, 18.69, 11.61, 11.47, 11.53,
              10.67, 10.56, 10.74, 13.29, 13.10, 13.60, 17.91, 17.95, 17.99, 13.99, 14.45, 14.29),
  Lipid = c(5.45, 5.15, 5.15, 6.98, 6.27, 6.91, 6.68, 6.05, 6.41, 7.50, 7.45, 7.43,
            5.10, 5.68, 5.21, 7.68, 7.46, 7.10, 7.05, 7.28, 7.92, 6.10, 6.17, 6.46),
  Ash = c(26.11, 24.69, 24.39, 33.54, 34.96, 33.19, 46.64, 48.41, 45.18, 35.93, 35.93, 35.93,
          43.81, 46.64, 43.81, 36.64, 35.58, 33.81, 38.41, 39.47, 42.66, 33.81, 34.16, 35.58)
)

# Reshape data
data_long <- data_raw %>%
  pivot_longer(cols = c(Carbohydrate, Protein, Lipid, Ash),
               names_to = "Component", values_to = "Percentage")

data_long$Component <- factor(data_long$Component,
                              levels = c("Carbohydrate", "Protein", "Lipid", "Ash"))

data_long <- data_long %>%
  mutate(Component_Strain = factor(paste(Component, Strain, sep = "_"),
                                   levels = c(
                                     "Carbohydrate_KU1196", "Carbohydrate_CCAP", "Carbohydrate_KU1884", "Carbohydrate_KU1885",
                                     "Protein_KU1196", "Protein_CCAP", "Protein_KU1884", "Protein_KU1885",
                                     "Lipid_KU1196", "Lipid_CCAP", "Lipid_KU1884", "Lipid_KU1885",
                                     "Ash_KU1196", "Ash_CCAP", "Ash_KU1884", "Ash_KU1885"
                                   )))

# Summary statistics
summary_data <- data_long %>%
  group_by(Strain, Media_Type, Component, Component_Strain) %>%
  summarise(
    Mean_Percentage = mean(Percentage),
    SD_Percentage = sd(Percentage),
    .groups = 'drop'
  )

# Define custom colors
custom_colors <- c(
  "Carbohydrate_KU1196" = "#E5F5E0", "Carbohydrate_CCAP" = "#C7E9C0",
  "Carbohydrate_KU1884" = "#A1D99B", "Carbohydrate_KU1885" = "#74C476",
  "Protein_KU1196" = "#EFEDF5", "Protein_CCAP" = "#DADAEB",
  "Protein_KU1884" = "#BCBDDC", "Protein_KU1885" = "#9E9AC8",
  "Lipid_KU1196" = "#DEEBF7", "Lipid_CCAP" = "#BDD7E7",
  "Lipid_KU1884" = "#9ECAE1", "Lipid_KU1885" = "#6BAED6",
  "Ash_KU1196" = "#FEE6CE", "Ash_CCAP" = "#FDD0A2",
  "Ash_KU1884" = "#FDAE6B", "Ash_KU1885" = "#FD8D3C"
)

# Plot
ggplot(summary_data, aes(x = interaction(Strain, Media_Type, sep = "\n"),
                         y = Mean_Percentage,
                         fill = Component_Strain)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, color = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = Mean_Percentage - SD_Percentage,
                    ymax = Mean_Percentage + SD_Percentage),
                width = 0.1,
                color = "darkgrey",
                position = "identity") +
  scale_fill_manual(
    values = custom_colors,
    name = "Biochemical Component (Strain)",
    breaks = levels(summary_data$Component_Strain),
    labels = c(
      "Carbohydrate (KU1196)", "Carbohydrate (CCAP)", "Carbohydrate (KU1884)", "Carbohydrate (KU1885)",
      "Protein (KU1196)", "Protein (CCAP)", "Protein (KU1884)", "Protein (KU1885)",
      "Lipid (KU1196)", "Lipid (CCAP)", "Lipid (KU1884)", "Lipid (KU1885)",
      "Ash (KU1196)", "Ash (CCAP)", "Ash (KU1884)", "Ash (KU1885)"
    )) +
  labs(
    title = "Biochemical Composition (% dry weight) of Asparagopsis strains",
    subtitle = "Cultivated under F/2 and Cell-Hi Bio nutrient regimes",
    x = "Strain and Nutrient Regime",
    y = "Biochemical Composition (% Dry Weight)"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey", linetype = "dotted"),
    panel.grid.minor.y = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(t = 30, r = 20, b = 20, l = 20)
  )
