clean_data_questionaire_group1 <- function(df) {
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