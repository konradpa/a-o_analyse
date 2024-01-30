rm( )

# Getting data
source("functions.R")

install.packages("readxl")
library("readxl")
excel_file_path <- "Daten/Fragebogendaten_Mittelwerte_Seminar_B_Team_1.xlsx"

vr_questionaire_group1 <- read_excel(excel_file_path, sheet = "VR_Meeting")

zoom_questionaire_group1 <- read_excel(excel_file_path, sheet = "Zoom-Meeting")

# Cleaning Data up

vr_questionaire_group1<-clean_data(vr_data)

zoom_questionaire_group1<-clean_data(zoom_data)
