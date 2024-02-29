rm(list = ls())

# Getting data
source("functions.R")

install.packages("readxl")
install.packages("tidyr")
install.packages("dplyr")
install.packages("broom")
library(dplyr)
library(tidyr)
library(broom)
library("readxl")

vr_self <- read_excel("Daten/Fragebogendaten_Mittelwerte_Seminar_B_Team_1.xlsx", sheet = "VR_Meeting")

zoom_self <- read_excel("Daten/Fragebogendaten_Mittelwerte_Seminar_B_Team_1.xlsx", sheet = "Zoom-Meeting")

vr_external <- read_excel("Daten/Entitativity _VR_Team 1.xlsx", sheet = "VR_Meeting_Gesamt")

zoom_external <- read_excel("Daten/Entitativity _Zoom_Team 1.xlsx", sheet = "Zoom_Meeting_Gesamt")

# Getting clean data

clean_vr_self<-clean_data_self(vr_self)

clean_zoom_self<-clean_data_self(zoom_self)

clean_vr_external <- clean_data_external(vr_external)

clean_zoom_external<- clean_data_external(zoom_external)

zoom_interact <- clean_data_interact("Daten/Ausgewertet_Sem B_Team 6_Zoom.xls")

vr_interact<- clean_data_interact("Daten/Ausgewertet_Sem B_Team 6_VR.xls")

# Calculating ES for phase one vs phase two
# Calculate Cohen's d for clean_vr_group1
cohen_d_vr <- calculate_cohens_d(clean_vr_self, 9, 10)
cat("Cohen's d for VR data:", cohen_d_vr, "\n")

# Calculate Cohen's d for clean_zoom_group1
cohen_d_zoom <- calculate_cohens_d(clean_zoom_self, 9, 10)
cat("Cohen's d for Zoom data:", cohen_d_zoom, "\n")

# Testing the external Ratings for normal distribution
## Apply the function to clean_zoom_external and clean_vr_external
shapiro_test_zoom <- calculate_shapiro_test(clean_zoom_external)
shapiro_test_vr <- calculate_shapiro_test(clean_vr_external)

## Output the results
print("Shapiro-Wilk test results for Zoom data:")
print(shapiro_test_zoom)
print("Shapiro-Wilk test results for VR data:")
print(shapiro_test_vr)

# Ratings: Self vs. External: VR
rows_self <- 5:8  # Rows containing self-ratings in clean_..._group1
rows_external <- 1:4  # Rows containing external ratings in clean_..._external
perform_paired_t_tests(clean_vr_self, clean_vr_external, rows_self, rows_external)



# Ratings: Self vs. External: Zoom
perform_paired_t_tests(clean_zoom_self, clean_zoom_external, rows_self, rows_external)
