clean_data_self <- function(df) {
  if (is.data.frame(df)) {
    # Single dataframe provided
    # Rename columns
    colnames(df) <- c("variable", "mean_gesamt", "SD", "Skala")
    
    # Remove rows with missing values in the 'mean' column
    df <- df[complete.cases(df$mean_gesamt), ]
    
    # Remove the first row
    df <- df[-1,]
    
    # Convert 'mean' and 'SD' columns to numeric
    df$mean_gesamt <- as.numeric(as.character(df$mean_gesamt))
    df$SD <- as.numeric(as.character(df$SD))
    
    return(as.data.frame(df))
  }
}



clean_data_external <- function(df) {
  # Rename columns
  colnames(df) <- c("variable", "mean_erste_minuten", "mean_zweite_minuten",  "mean_dritte_minuten", 
                    "mean_vierte_minuten", "mean_gesamt", "SD" )
  
  # Remove rows with missing values in the 'mean_total' column
  df <- df[complete.cases(df$mean_gesamt), ]
  
  # Remove the first row
  df <- df[-1,]
  
  # Convert 'mean' and 'SD' columns to numeric
  df[, 2:7] <- lapply(df[, 2:7], as.numeric)
  
  specific_names <- c("Entitativität", "Ähnlichkeit", "Interaktivität", "Geteilte Ziele")
  df$variable <- specific_names
  

  
  return(as.data.frame(df))
  
}


# Load required library
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

# Function to do t tests
perform_paired_t_tests <- function(dataframe_self, dataframe_external, rows_self, rows_external, alpha = 0.05) {
  # Check if lengths of rows_self and rows_external are equal
  if (length(rows_self) != length(rows_external)) {
    stop("Lengths of rows_self and rows_external must be equal.")
  }
  
  # Initialize results table
  results_table <- data.frame(Category = character(length(rows_self)),
                              Variable = character(length(rows_self)),
                              T_statistic = numeric(length(rows_self)),
                              Degrees_of_freedom = numeric(length(rows_self)),
                              Critical_t_value = numeric(length(rows_self)),
                              Hypothesis_conclusion = character(length(rows_self)),
                              P_value = numeric(length(rows_self)))
  
  # Loop through each row
  for (i in 1:length(rows_self)) {
    # Extract data for the current row
    variable_name <- dataframe_self[rows_self[i], "variable"]
    mean_self <- as.numeric(dataframe_self[rows_self[i], "mean_gesamt"]) # Ensure numeric type
    mean_external <- as.numeric(dataframe_external[rows_external[i], "mean_gesamt"]) # Ensure numeric type
    sd_self <- as.numeric(dataframe_self[rows_self[i], "SD"]) # Ensure numeric type
    sd_external <- as.numeric(dataframe_external[rows_external[i], "SD"]) # Ensure numeric type
    
    # Calculate the difference in means
    difference <- mean_self - mean_external
    
    # Calculate the standard deviation of the differences 
    SD_d<-sqrt((sd_self^2 + sd_external^2) / 3)
    
    # Calculate the standard error of the mean difference
    SE_d <- SD_d / sqrt(3)
    
    # Calculate the t-statistic
    t_statistic <- difference / SE_d
    
    # Determine the degrees of freedom
    df <- 2
    
    # Find the critical t-value
    critical_t <- qt(1 - alpha / 2, df)
    
    # Compare the t-statistic to the critical t-value
    if (abs(t_statistic) > critical_t) {
      hypothesis <- "Reject null hypothesis"
    } else {
      hypothesis <- "Fail to reject null hypothesis"
    }
    
    # Calculate the p-value
    p_value <- 2 * pt(abs(t_statistic), df, lower.tail = FALSE)
    
    # Store the results in the table
    results_table[i, ] <- list(Category = rownames(dataframe_self)[rows_self[i]],
                               Variable = variable_name,
                               T_statistic = t_statistic,
                               Degrees_of_freedom = df,
                               Critical_t_value = critical_t,
                               Hypothesis_conclusion = hypothesis,
                               P_value = p_value)
  }
  
  # Return the results table
  return(results_table)
}




# Cohens D

calculate_cohens_d <- function(dataframe, phase1_row_index, phase2_row_index) {
  # Extract mean and standard deviation for Phase one and Phase two
  mean_phase1 <- dataframe$mean_gesamt[phase1_row_index]
  mean_phase2 <- dataframe$mean_gesamt[phase2_row_index]
  sd_phase1 <- dataframe$SD[phase1_row_index]
  sd_phase2 <- dataframe$SD[phase2_row_index]
  
  # Calculate Cohen's d
  cohen_d <- (mean_phase1 - mean_phase2) / sqrt(((sd_phase1^2) + (sd_phase2^2)) / 2)
  
  return(cohen_d)
}

calculate_cohens_d_multiple <- function(df1, df2, vars) {
  d_values <- numeric(length(vars))
  
  for (i in 1:length(vars)) {
    cat("Calculating Cohen's d for variable:", vars[i], "\n")
    
    # Extract the relevant rows for the variable in df1
    df1_var <- df1[df1$variable == vars[i], ]
    
    # Extract the relevant rows for the variable in df2
    df2_var <- df2[df2$variable == vars[i], ]
    
    # Check if there's data available for this variable in both data frames
    if (nrow(df1_var) == 0 || nrow(df2_var) == 0) {
      warning(paste("No data available for variable", vars[i]))
      d_values[i] <- NA
    } else {
      # Get mean and standard deviation for df1
      mean_x <- df1_var$mean_gesamt
      sd_x <- df1_var$SD
      
      # Get mean and standard deviation for df2
      mean_y <- df2_var$mean_gesamt
      sd_y <- df2_var$SD
      
      cat("Mean (df1):", mean_x, "\n")
      cat("Standard Deviation (df1):", sd_x, "\n")
      cat("Mean (df2):", mean_y, "\n")
      cat("Standard Deviation (df2):", sd_y, "\n")
      
      # Calculate Cohen's d without pooling standard deviations
      d_values[i] <- abs(mean_x - mean_y) / sqrt((sd_x^2 + sd_y^2) / 2)
      
      cat("Cohen's d:", d_values[i], "\n")
    }
    cat("\n")
  }
  
  result_df <- data.frame(Variable = vars, Cohen_d = d_values)
  return(result_df)
}


# Example usage:
# Assuming you have two data frames df1 and df2, and vars contains the names of the variables you want to compare
# df1 <- data.frame(variable = c("Entitativität", "Ähnlichkeit", "Interaktivität", "geteilte ziele", "Irrelevant1", "Irrelevant2"),
#                   mean_gesamt = c(1, 2, 3, 4, 5, 6), SD = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
# df2 <- data.frame(variable = c("Entitativität", "Ähnlichkeit",


# Shapiro Test 
calculate_shapiro_test <- function(dataframe) {
  category_names <- dataframe$variable  # Extract category names from the 'variable' column
  result <- lapply(1:nrow(dataframe), function(i) {
    row <- as.numeric(dataframe[i, 2:5])
    shapiro_result <- shapiro.test(row)
    return(data.frame(Category = category_names[i],
                      p_value = shapiro_result$p.value,
                      W_value = shapiro_result$statistic))
  })
  return(do.call(rbind, result))
}


# T- Sum
perform_tsum_test <- function(external_df, self_df, rows_external, rows_self) {
  # Initialize a list to store the results
  t_results_list <- list()
  
  # Iterate through the indices
  for (i in 1:length(rows_external)) {
    # Extract the indices for the current combination
    idx_external <- rows_external[i]
    idx_self <- rows_self[i]
    
    # Perform tsum.test for the current combination of rows
    t_results <- tsum.test(
      mean.x = external_df[idx_external, 6],
      s.x = external_df[idx_external, 7],
      n.x = 3,
      mean.y = self_df[idx_self, 2],
      s.y = self_df[idx_self, 3],
      n.y = 3,
      alternative = "two.sided",
      mu = 0,
      var.equal = FALSE,
      conf.level = 0.95
    )
    
    # Store the results in the list
    t_results_list[[paste0("t_results_", idx_external, "_", idx_self)]] <- t_results
  }
  
  # Return the list of results
  return(t_results_list)
