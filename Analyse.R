# Getting data

install.packages("readxl")
library("readxl")
excel_file_path <- "Daten/Fragebogendaten_Mittelwerte_Seminar_B_Team_1.xlsx"

vr_data <- read_excel(excel_file_path, sheet = "VR_Meeting")

zoom_data <- read_excel(excel_file_path, sheet = "Zoom-Meeting")

# Cleaning Data up
clean_data <- function(df) {
  # Rename columns
  colnames(df) <- c("variable", "mean", "SD", "Skala")
  
  # Remove rows with missing values in the 'mean' column
  df <- df[complete.cases(df$mean), ]
  
  # Remove the first row
  df <- df[-1,]
  
  # Convert 'mean' and 'SD' columns to numeric
  df$mean <- as.numeric(as.character(df$mean))
  df$SD <- as.numeric(as.character(df$SD))
  
  # Return the cleaned dataframe
  return(df)
}


vr_data<-clean_data(vr_data)

zoom_data<-clean_data(zoom_data)


