clean_data_group1 <- function(df) {
  # Rename columns
  colnames(df) <- c("variable", "mean_gesamt", "SD", "Skala")
  
  # Remove rows with missing values in the 'mean' column
  df <- df[complete.cases(df$mean_gesamt), ]
  
  # Remove the first row
  df <- df[-1,]
  
  # Convert 'mean' and 'SD' columns to numeric
  df$mean_gesamt <- as.numeric(as.character(df$mean_gesamt))
  df$SD <- as.numeric(as.character(df$SD))
  
  # Return the cleaned dataframe
  return(df)
}

clean_data_external <- function(df) {
  # Rename columns
  colnames(df) <- c("variable", "mean_erste_5_minuten", "mean_zweite_5_minuten", "mean_dritte_5_minuten", "mean_vierte_5_minuten", "mean_gesamt", "SD")
  
  # Remove rows with missing values in the 'mean_gesamt' column
  df <- df[complete.cases(df$mean_gesamt), ]
  
  # Remove the first row
  df <- df[-1,]
  
  # Convert 'mean' and 'SD' columns to numeric
  df$mean_gesamt <- as.numeric(as.character(df$mean_gesamt))
  df$SD <- as.numeric(as.character(df$SD))
  
  # Assign specific names to the "variable" column
  df$variable <- c("Entitativität", "Ähnlichkeit", "Interaktivität", "Geteilte Ziele")
  
  return(df)
}

# Load required library
library(readxl)

# Load required libraries
library(readxl)

clean_data_interact <- function(file_path) {
  # Read all sheets from the Excel file
  sheets <- excel_sheets(file_path)
  
  # Initialize an empty list to store data frames
  data_list <- list()
  
  # Read data from each sheet, remove the first two columns, and store it in the list
  for (sheet in sheets) {
    data <- read_excel(file_path, sheet = sheet)
    data <- data[, -c(1, 2)]  # Remove the first two columns
    data_list[[sheet]] <- data
  }
  
  # Combine all data frames into a single data frame
  combined_data <- do.call(rbind, data_list)
  
  # Return the combined data frame
  return(combined_data)
}

# Function to create a bar plot
plot_mean_gesamt <- function(df1, df2, df1_name, df2_name, variable_columns = c("Entitativität", "Ähnlichkeit", "Interaktivität", "Geteilte Ziele")) {
  # Extract only the necessary columns from data frames
  df1_subset <- df1[df1$variable %in% variable_columns, ]
  df2_subset <- df2[df2$variable %in% variable_columns, ]
  
  # Merge data frames for plotting
  merged_data <- merge(df1_subset, df2_subset, by = "variable")
  
  # Create a matrix for barplot
  bar_matrix <- matrix(
    c(merged_data$mean_gesamt.x, merged_data$mean_gesamt.y),
    nrow = 2, byrow = TRUE,
    dimnames = list(c(df1_name, df2_name), merged_data$variable)
  )
  
  # Bar plot
  barplot(
    bar_matrix,
    col = c("blue", "red"),  # Adjust colors as needed
    beside = TRUE,
    ylim = c(0, 7),  # Set y-axis limits to 0-7
    xlab = "Variables",
    ylab = "Mean Gesamt",
    main = paste("Mean of", df1_name, "and", df2_name)  # Add a main title
  )
  
  # Add legend with smaller text size (cex)
  legend("topright", legend = c(df1_name, df2_name), fill = c("blue", "red"), cex = 0.5)
}



