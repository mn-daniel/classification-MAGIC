library(sgt)
library(dplyr)
library(ggplot2)

# Load data
data <- read.csv("spx_data.csv", header = FALSE)
colnames(data) <- c("Date", "Close")
data$Date <- as.Date(data$Date, format="%Y-%m-%d")

# Calculate daily returns

data <- data %>%
  mutate(Returns = (log(Close) - log(lag(Close))) * 100)
data <- data[-1,] # Remove first row (NA)

# Fit SGT distributions
all_returns <- data$Returns
last_200_returns <- tail(all_returns, 200)
last_50_returns <- tail(all_returns, 50)
last_5_returns <- tail(all_returns, 5)

# Fit SGT distributions
# Fit SGT distributions
fit_all <- dsgt(all_returns, mean.cent = TRUE, var.adj = TRUE)
fit_200 <- dsgt(last_200_returns, mean.cent = TRUE, var.adj = TRUE)
fit_50 <- dsgt(last_50_returns, mean.cent = TRUE, var.adj = TRUE)
fit_5 <- dsgt(last_5_returns, mean.cent = TRUE, var.adj = TRUE)

pdf("all_plots_II.pdf")
# Generate histograms using SGT distribution fits
hist_all <- ggplot(data, aes(x = Returns)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, fill = "blue", color = "black") +
  stat_function(fun = dsgt, args = fit_all, color = "blue", size = 1) +
  labs(title = "Histogram of All Returns", x = "Returns", y = "Density") +
  theme_minimal()

hist_200 <- ggplot(data.frame(Returns = last_200_returns), aes(x = Returns)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, fill = "red", color = "black") +
  stat_function(fun = dsgt, args = fit_200, color = "red", size = 1) +
  labs(title = "Histogram of Last 200 Returns", x = "Returns", y = "Density") +
  theme_minimal()

hist_50 <- ggplot(data.frame(Returns = last_50_returns), aes(x = Returns)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, fill = "green", color = "black") +
  stat_function(fun = dsgt, args = fit_50, color = "green", size = 1) +
  labs(title = "Histogram of Last 50 Returns", x = "Returns", y = "Density") +
  theme_minimal()

hist_5 <- ggplot(data.frame(Returns = last_5_returns), aes(x = Returns)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, fill = "purple", color = "black") +
  stat_function(fun = dsgt, args = fit_5, color = "purple", size = 1) +
  labs(title = "Histogram of Last 5 Returns", x = "Returns", y = "Density") +
  theme_minimal()

# Suppress warning messages
suppressWarnings({
  print(hist_all)
  print(hist_200)
  print(hist_50)
  print(hist_5)
})

# Create a function to calculate dsgt parameters for a given time window
get_dsgt_params <- function(returns) {
  fit <- dsgt(returns, mean.cent = FALSE, var.adj = FALSE)
  return(c(fit[1], fit[2], fit[3], fit[4]))
}

# Calculate dsgt parameters for all time scales
params_all <- data.frame(matrix(ncol = 16, nrow = 0))
colnames(params_all) <- c(paste0("all_", c("location", "scale", "p", "q")),
                          paste0("200_", c("location", "scale", "p", "q")),
                          paste0("50_", c("location", "scale", "p", "q")),
                          paste0("5_", c("location", "scale", "p", "q")))

for (i in 1:(nrow(data) - 205)) {
  all_returns <- data$Returns[i:(i + 204)]
  last_200_returns <- data$Returns[(i + 5):(i + 204)]
  last_50_returns <- data$Returns[(i + 155):(i + 204)]
  last_5_returns <- data$Returns[(i + 200):(i + 204)]
  
  params_all[i, ] <- c(get_dsgt_params(all_returns),
                       get_dsgt_params(last_200_returns),
                       get_dsgt_params(last_50_returns),
                       get_dsgt_params(last_5_returns))
}

# Add the SPX values five days ahead as the response variable
params_all$SPX_5_days_ahead <- data$Close[206:nrow(data)]

# Fit the multivariate regression model
model <- lm(SPX_5_days_ahead ~ ., data = params_all)

# Print the model summary
summary(model)

# Install and load the gt package
if (!requireNamespace("gt", quietly = TRUE)) {
  install.packages("gt")
}
library(gt)

# Create a table with the model summary
summary_table <- as.data.frame(summary(model)$coefficients)
colnames(summary_table) <- c("Estimate", "Std.Error", "t_value", "Pr(>|t|)")

# Add a new column for the coefficient names
summary_table$Coefficient <- rownames(summary_table)

# Customize the table appearance
summary_gt <- summary_table %>%
  gt() %>%
  cols_label(
    Coefficient = "Coefficient",
    Estimate = "Estimate",
    Std.Error = "Std. Error",
    t_value = "t value",
    `Pr(>|t|)` = "Pr(>|t|)"
  ) %>%
  fmt_number(
    columns = c("Estimate", "Std.Error", "t_value", "Pr(>|t|)"),
    decimals = 4
  ) %>%
  tab_header(
    title = "Summary of Linear Model"
  )

# Print the table
print(summary_gt)

# Calculate the residuals (observed - predicted)
residuals <- params_all$SPX_5_days_ahead - predict(model, newdata = params_all)

# Create a histogram of the residuals
hist_res <- ggplot(data.frame(residuals = residuals), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Density") +
  theme_minimal()

# Print the histogram
print(hist_res)

# Extract the current coefficients
current_all <- get_dsgt_params(data$Returns)
current_200 <- get_dsgt_params(tail(data$Returns, 200))
current_50 <- get_dsgt_params(tail(data$Returns, 50))
current_5 <- get_dsgt_params(tail(data$Returns, 5))

# Create a new data frame with the current coefficients
current_params <- data.frame(matrix(ncol = 16, nrow = 1))

colnames(current_params) <- c(paste0("all_", c("location", "scale", "p", "q")),
                              paste0("200_", c("location", "scale", "p", "q")),
                              paste0("50_", c("location", "scale", "p", "q")),
                              paste0("5_", c("location", "scale", "p", "q")))

current_params[1, ] <- c(current_all, current_200, current_50, current_5)

# Make the prediction for the SPX value five days ahead
prediction <- predict(model, newdata = current_params)
print(prediction)

predict_spx_5_days_ahead <- function(current_date, data) {
  # Extract the current coefficients
  current_all <- get_dsgt_params(data$Returns)
  current_200 <- get_dsgt_params(tail(data$Returns, 200))
  current_50 <- get_dsgt_params(tail(data$Returns, 50))
  current_5 <- get_dsgt_params(tail(data$Returns, 5))
  
  # Create a new data frame with the current coefficients
  current_params <- data.frame(matrix(ncol = 16, nrow = 1))
  
  colnames(current_params) <- c(paste0("all_", c("location", "scale", "p", "q")),
                                paste0("200_", c("location", "scale", "p", "q")),
                                paste0("50_", c("location", "scale", "p", "q")),
                                paste0("5_", c("location", "scale", "p", "q")))
  
  current_params[1, ] <- c(current_all, current_200, current_50, current_5)
  
  # Make the prediction for the SPX value five days ahead
  prediction <- predict(model, newdata = current_params)
  
  # Create a simple bar chart to visualize the predicted SPX value
  prediction_chart <- ggplot() +
    geom_bar(aes(x = paste0("Prediction on ", current_date), y = prediction), stat = "identity", fill = "steelblue") +
    labs(title = "Predicted SPX Value 5 Days Ahead", x = "", y = "SPX Value") +
    theme_minimal()
  
  print(prediction_chart)
  
  return(prediction)
}

# Example: Call the function with the current date and data
current_date <- as.Date("2014-01-07")
prediction <- predict_spx_5_days_ahead(current_date, data)
print(prediction)
dev.off()

