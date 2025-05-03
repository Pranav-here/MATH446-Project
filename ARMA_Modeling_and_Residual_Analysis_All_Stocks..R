# Load required libraries for time series modeling and diagnostics
library(tseries)     # For time series tests like ADF
library(forecast)    # For ARIMA model fitting
library(lmtest)      # For Box-Ljung test
library(FinTS)       # For ARCH test (heteroskedasticity)

# Load pre-processed log-return data for multiple stocks
data <- read.csv("all_log_returns_wide_format.csv")

# Identify all unique stock names (each row is a stock, columns are returns)
stock_list <- unique(data$Stock)

# Create a directory to save diagnostic plots if it doesn't already exist
if (!dir.exists("Stock_Plots")) {
  dir.create("Stock_Plots")
}

# Initialize a dataframe to store model comparison results for each stock
results_summary <- data.frame(
  Stock = character(),
  Best_Model = character(),
  AIC_17_2 = numeric(),
  AIC_12_3 = numeric(),
  AIC_7_4 = numeric(),
  Shapiro_pvalue = numeric(),
  BoxLjung_pvalue = numeric(),
  ArchTest_pvalue = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each stock and perform ARMA modeling and diagnostics
for (stock in stock_list) {
  cat("\nProcessing:", stock, "\n")
  
  # Extract the return series for the current stock
  stock_row <- data[data$Stock == stock, ]
  if (nrow(stock_row) == 0) next
  stock_returns <- as.numeric(stock_row[,-1])  # Remove stock name column
  
  # Skip if all values are NA
  if (all(is.na(stock_returns))) {
    cat("Skipping", stock, "(all NA values)\n")
    next
  }
  
  # Define training and test split (last 80 points reserved for future validation)
  test_size <- 80
  n <- length(stock_returns)
  if (n <= test_size + 20) {
    cat("Skipping", stock, "(insufficient data)\n")
    next
  }
  train_data <- stock_returns[1:(n - test_size)]
  
  # Plot and save ACF/PACF for visual stationarity checks
  png(filename = paste0("Stock_Plots/", stock, "_ACF_PACF.png"))
  par(mfrow = c(1, 2))
  acf(train_data, main = paste("ACF -", stock))
  pacf(train_data, main = paste("PACF -", stock))
  dev.off()
  
  # Fit 3 candidate ARMA models with varying complexity
  model_17_2 <- tryCatch(arima(train_data, order = c(17, 0, 2)), error = function(e) NULL)
  model_12_3 <- tryCatch(arima(train_data, order = c(12, 0, 3)), error = function(e) NULL)
  model_7_4  <- tryCatch(arima(train_data, order = c(7, 0, 4)), error = function(e) NULL)
  
  # Skip if any model fails to converge
  if (is.null(model_17_2) | is.null(model_12_3) | is.null(model_7_4)) {
    cat("Skipping", stock, "(model fitting failed)\n")
    next
  }
  
  # Compare AIC scores and select the best-fitting model
  aics <- c(AIC(model_17_2), AIC(model_12_3), AIC(model_7_4))
  names(aics) <- c("ARMA(17,2)", "ARMA(12,3)", "ARMA(7,4)")
  best_model_name <- names(which.min(aics))
  
  # Extract residuals of the best model for diagnostics
  best_model <- switch(best_model_name,
                       "ARMA(17,2)" = model_17_2,
                       "ARMA(12,3)" = model_12_3,
                       "ARMA(7,4)"  = model_7_4)
  residuals_best <- residuals(best_model)
  
  # Run diagnostic tests on residuals
  shapiro_p <- tryCatch(shapiro.test(residuals_best)$p.value, error = function(e) NA)  # Normality
  boxljung_p <- tryCatch(Box.test(residuals_best, lag = 20, type = "Ljung-Box")$p.value, error = function(e) NA)  # Autocorrelation
  archtest_p <- tryCatch(ArchTest(residuals_best, lags = 12)$p.value, error = function(e) NA)  # Heteroskedasticity
  
  # Plot residuals time series and boxplot
  png(filename = paste0("Stock_Plots/", stock, "_Residuals.png"))
  par(mfrow = c(1, 2))
  ts.plot(residuals_best, main = paste("Residuals -", stock))
  boxplot(residuals_best, main = paste("Boxplot of Residuals -", stock))
  dev.off()
  
  # Plot histogram of residuals with density curve overlay
  png(filename = paste0("Stock_Plots/", stock, "_Histogram.png"))
  hist(residuals_best, breaks = 30, probability = TRUE,
       main = paste("Histogram of Residuals -", stock),
       xlab = "Residuals", col = "lightblue", border = "black")
  lines(density(residuals_best), col = "red", lwd = 2)
  dev.off()
  
  # Plot squared residuals to visualize volatility clustering
  png(filename = paste0("Stock_Plots/", stock, "_SquaredResiduals.png"))
  ts.plot(residuals_best^2, main = paste("Squared Residuals -", stock), ylab = "Squared Residuals")
  dev.off()
  
  # Append results to summary dataframe
  results_summary <- rbind(results_summary, data.frame(
    Stock = stock,
    Best_Model = best_model_name,
    AIC_17_2 = aics["ARMA(17,2)"],
    AIC_12_3 = aics["ARMA(12,3)"],
    AIC_7_4  = aics["ARMA(7,4)"],
    Shapiro_pvalue = shapiro_p,
    BoxLjung_pvalue = boxljung_p,
    ArchTest_pvalue = archtest_p
  ))
}

# Export summary of results for all stocks
write.csv(results_summary, "arma_modeling_results_summary.csv", row.names = FALSE)
print(results_summary)

