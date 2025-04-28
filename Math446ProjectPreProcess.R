# Load necessary libraries
library(tseries)  # For adf.test()
library(forecast)
library(zoo)  # For NA handling

# List of stock files
stock_files <- c("Aselsan_stock_data.csv", "Broadcom_stock_data.csv", "Intel_stock_data.csv", 
                 "Microsoft_stock_data.csv", "Northrop Grumman_stock_data.csv", 
                 "Oracle_stock_data.csv", "Qualcomm_stock_data.csv", 
                 "Saab AB_stock_data.csv", "Suncore Energy_stock_data.csv", 
                 "TechnipFMC_stock_data.csv", "Woodside Energy_stock_data.csv")

all_log_returns_list <- list()

# Loop through each stock file and process it
for (stock_file in stock_files) {
  
  # Read the stock data
  stock <- read.csv(paste0("C:/Users/Baron/Downloads/stock_data/content/stock_data/", stock_file))
  
  # Extract the 'Close' column and remove the first two values
  closing_prices <- stock$Close
  closing_prices <- closing_prices[3:length(closing_prices)]  # Remove the first two values
  
  # Convert to numeric
  closing_prices_numeric <- as.numeric(closing_prices)
  
  closing_prices_interpolated <- approx(seq_along(closing_prices_numeric), closing_prices_numeric, 
                                        xout = seq_along(closing_prices_numeric), method = "linear")$y
  
  # Compute the difference between consecutive closing prices
  log_returns <- diff(log(closing_prices_interpolated))
  
  log_returns = tail(log_returns, 3947)
  
  # Perform ADF test on original closing prices
  adf_result_original <- adf.test(closing_prices_interpolated, alternative = "stationary")
  
  # Extract relevant ADF test results for the original data
  df_stat_original <- adf_result_original$statistic
  p_value_original <- adf_result_original$p.value
  lag_order_original <- adf_result_original$lags
  
  # Print the ADF test result for the original data
  cat("\nADF test result for", stock_file, "on original data:\n")
  cat("Dickey-Fuller = ", df_stat_original, ", Lag order = ", lag_order_original, ", p-value = ", p_value_original, "\n")
  
  # Perform ADF test on the log-differenced prices
  adf_result_log_diff <- adf.test(log_returns, alternative = "stationary")
  
  # Extract relevant ADF test results for the log-differenced data
  df_stat_log_diff <- adf_result_log_diff$statistic
  p_value_log_diff <- adf_result_log_diff$p.value
  lag_order_log_diff <- adf_result_log_diff$lags
  
  # Print the ADF test result for the log-differenced data
  cat("\nADF test result for", stock_file, "on log-differenced data:\n")
  cat("Dickey-Fuller = ", df_stat_log_diff, ", Lag order = ", lag_order_log_diff, ", p-value = ", p_value_log_diff, "\n")
  print(length(log_returns))
  
  all_log_returns_list[[stock_file]] <- log_returns
  

  cat("\n----------------------------------------------------\n")
}

all_log_returns_df <- as.data.frame(do.call(rbind, all_log_returns_list))

# Add stock names as row identifiers
all_log_returns_df <- cbind(Stock = rownames(all_log_returns_df), all_log_returns_df)

write.csv(all_log_returns_df, "all_log_returns_wide_format.csv", row.names = FALSE)

# Optional: Plots of Closing Prices and Log-Differenced Prices
if (FALSE) {
  
  dev.new()
  plot(closing_prices_numeric, type = "l", col = "blue", 
       xlab = "Time (Days)", ylab = "Closing Price", 
       main = "Original Closing Prices Over Time")
  
  # Open a new page for the second plot
  dev.new()
  plot(log_diff_prices_clean, type = "l", col = "red", 
       xlab = "Time (Days)", ylab = "Log-Differenced Price", 
       main = "Log-Transformed Differenced Closing Prices Over Time")

  windows()
  plot(closing_prices_interpolated, type = "l", col = "green",
       xlab = "Time (Days)", ylab = "Closing Price",
       main = paste("Closing Prices for", stock_file))
  
  windows()
  plot(log_returns, type = "l", col = "orange",
       xlab = "Time (Days)", ylab = "Log Returns",
       main = paste("Log Returns for", stock_file))
  
  
  
  }
