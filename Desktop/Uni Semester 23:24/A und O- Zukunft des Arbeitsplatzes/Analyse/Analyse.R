rm( )

# Getting data
source("functions.R")

install.packages("readxl")
library("readxl")

vr_group1 <- read_excel("Daten/Fragebogendaten_Mittelwerte_Seminar_B_Team_1.xlsx", sheet = "VR_Meeting")

zoom_group1 <- read_excel("Daten/Fragebogendaten_Mittelwerte_Seminar_B_Team_1.xlsx", sheet = "Zoom-Meeting")

vr_external <- read_excel("Daten/Entitativity _VR_Team 1.xlsx", sheet = "VR_Meeting_Gesamt")

zoom_external <- read_excel("Daten/Entitativity _Zoom_Team 1.xlsx", sheet = "Zoom_Meeting_Gesamt")

zoom_interact <- clean_data_interact("Daten/Ausgewertet_Sem B_Team 6_Zoom.xls")

# Cleaning Data up

clean_vr_group1<-clean_data_group1(vr_group1)

clean_zoom_group1<-clean_data_group1(zoom_group1)

clean_vr_external <- clean_data_external(vr_external)

clean_zoom_external <- clean_data_external(zoom_external)

# Ratings: External: Zoom vs. VR

plot_mean_gesamt(clean_zoom_external, clean_vr_external, "Zoom external", "VR external ratings")

plot_mean_gesamt(clean_zoom_group1, clean_vr_group1,"Zoom internal", "VR internal ratings" )




