# Load libraries
library(tseries)
library(forecast)
library(lmtest)
library(FinTS)

# Read the data
data <- read.csv("all_log_returns_wide_format.csv")

# List of all stock names
stock_list <- unique(data$Stock)

# Create folder to save plots if not exist
if (!dir.exists("Stock_Plots")) {
  dir.create("Stock_Plots")
}

# Initialize result storage
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

# Process each stock one-by-one
for (stock in stock_list) {
  cat("\nProcessing:", stock, "\n")
  
  # Step 1: Extract stock returns
  stock_row <- data[data$Stock == stock, ]
  if (nrow(stock_row) == 0) next
  stock_returns <- as.numeric(stock_row[,-1])
  if (all(is.na(stock_returns))) {
    cat("Skipping", stock, "(all NA)\n")
    next
  }
  
  # Step 2: Split into training and testing
  test_size <- 80
  n <- length(stock_returns)
  if (n <= test_size + 20) {
    cat("Skipping", stock, "(not enough data)\n")
    next
  }
  train_data <- stock_returns[1:(n - test_size)]
  test_data <- stock_returns[(n - test_size + 1):n]
  
  # Step 3: Plot ACF and PACF for training data
  png(filename=paste0("Stock_Plots/", stock, "_ACF_PACF.png"))
  par(mfrow=c(1,2))
  acf(train_data, main=paste("ACF -", stock))
  pacf(train_data, main=paste("PACF -", stock))
  dev.off()
  
  # Step 4: Fit ARMA models
  model_17_2 <- tryCatch(arima(train_data, order = c(17,0,2)), error = function(e) NULL)
  model_12_3 <- tryCatch(arima(train_data, order = c(12,0,3)), error = function(e) NULL)
  model_7_4  <- tryCatch(arima(train_data, order = c(7,0,4)), error = function(e) NULL)
  
  if (is.null(model_17_2) | is.null(model_12_3) | is.null(model_7_4)) {
    cat("Skipping", stock, "(model fitting failed)\n")
    next
  }
  
  # Step 5: Compare AICs
  aics <- c(AIC(model_17_2), AIC(model_12_3), AIC(model_7_4))
  names(aics) <- c("ARMA(17,2)", "ARMA(12,3)", "ARMA(7,4)")
  best_model_name <- names(which.min(aics))
  
  best_model <- switch(best_model_name,
                       "ARMA(17,2)" = model_17_2,
                       "ARMA(12,3)" = model_12_3,
                       "ARMA(7,4)"  = model_7_4)
  residuals_best <- residuals(best_model)
  
  # Step 6: Residual analysis
  shapiro_p <- tryCatch(shapiro.test(residuals_best)$p.value, error = function(e) NA)
  boxljung_p <- tryCatch(Box.test(residuals_best, lag=20, type="Ljung-Box")$p.value, error = function(e) NA)
  archtest_p <- tryCatch(ArchTest(residuals_best, lags=12)$p.value, error = function(e) NA)
  
  # Plot residuals and boxplot
  png(filename=paste0("Stock_Plots/", stock, "_Residuals.png"))
  par(mfrow=c(1,2))
  ts.plot(residuals_best, main=paste("Residuals -", stock))
  boxplot(residuals_best, main=paste("Boxplot of Residuals -", stock))
  dev.off()
  
  # Plot Histogram instead of QQ plot
  png(filename=paste0("Stock_Plots/", stock, "_Histogram.png"))
  hist(residuals_best, breaks=30, probability=TRUE, 
       main=paste("Histogram of Residuals -", stock),
       xlab="Residuals", col="lightblue", border="black")
  lines(density(residuals_best), col="red", lwd=2)
  dev.off()
  
  
  # Step 7: Save results
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

# Final Step: Save everything
write.csv(results_summary, "arma_modeling_results_summary.csv", row.names = FALSE)
print(results_summary)



# 1. Data Processing
#     For each stock, I separated the data into training (older points) and test (last ~80 points representing 2024).
#     I used only the training data to fit the models.

# 2. Model Selection
#     Plotted ACF and PACF for each stock to observe correlation patterns.
#     Fitted three ARMA models: ARMA(17,2), ARMA(12,3), ARMA(7,4).
#     Chose the best model based on the lowest AIC.

# 3. Residual Analysis
#     Shapiro-Wilk Test checked for normality of residuals.
#     Box-Ljung Test checked for no autocorrelation (good if p-value > 0.05).
#     ARCH Test checked for heteroskedasticity (constant variance).
#
#     In almost all cases:
#         Residuals are not perfectly normal (p-values very small, expected for financial returns).
#         No autocorrelation in residuals (Box-Ljung p-values mostly > 0.95).
#         Heteroskedasticity exists (very low ARCH p-values — again expected for stocks).

# 4. Visuals
#     Created plots for each stock:
#     ACF + PACF plot of training data.

# Residuals + Boxplot.
#     All saved in the Stock_Plots/ folder.



# Plot             | Good sign that model good      | Bad sign
# ACF             | Bars die off fast               | Bars stay high for a long time
# PACF            | Bars cut off cleanly            | Bars stay random everywhere
# Residuals plot  | Random ups and downs around 0   | Patterns or waves
# Boxplot         | Centered around 0               | Huge outliers or very uneven box
# QQ plot         | Points on a straight line       | Points curve away, fat tails (but normal for stocks)
