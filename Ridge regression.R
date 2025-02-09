# Load required libraries
library(glmnet)
library(dplyr)

GDPusage_ridge <- GDPusage_lag_scaled %>% 
  ungroup() %>%
  select(c(2, 4:8))
View(GDPusage_ridge)

# Prepare the data

# Create the design matrix 
x <- model.matrix(GDP_per_capita ~ ., data = GDPusage_ridge)  

# Define the response variable
y <- GDPusage_ridge$GDP_per_capita

best_lambda
# Fit a ridge regression model
set.seed(123)  # For reproducibility
ridge_model <- glmnet(x, y, alpha = 0)  # alpha = 0 specifies ridge regression

# Display a summary of the model
print(ridge_model)

# Choose the optimal lambda (penalty term) using cross-validation
cv_ridge <- cv.glmnet(x, y, alpha = 0)

# Plot cross-validation results to find the best lambda
plot(cv_ridge)

# Optimal lambda
best_lambda <- cv_ridge$lambda.min
cat("Optimal lambda:", best_lambda, "\n")

best_lambda

# Refit ridge regression with the optimal lambda
final_ridge_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)


# Coefficients of the final ridge model
ridge_coefficients <- coef(final_ridge_model)
print(ridge_coefficients)

library(glmnet)
library(dplyr)
library(MASS)  # For the true R-squared calculation



# Assuming 'GDPusage_lag_scaled' is your original data frame and 'ridge_model' is fitted

# Make predictions on the training set
predictions <- predict(final_ridge_model, s = best_lambda, newx = x)

# Compute RMSE
actual_values <- GDPusage_lag_scaled$GDP_per_capita
predicted_values <- as.vector(predictions)
rmse <- sqrt(mean((actual_values - predicted_values)^2))
cat("RMSE:", rmse, "\n")

# Compute R-squared
rss <- sum((actual_values - predicted_values)^2)
tss <- sum((actual_values - mean(actual_values))^2)
r_squared <- 1 - (rss / tss)
cat("R-squared:", r_squared, "\n")

# Cross-validation results using cv.glmnet
cv_ridge <- cv.glmnet(x, y, alpha = 0)

# Plot cross-validation results
plot(cv_ridge)

# Best lambda value from cross-validation
best_lambda <- cv_ridge$lambda.min
cat("Optimal lambda:", best_lambda, "\n")

# View the cross-validation results
cat("Cross-validation mean MSE at optimal lambda:", mean(cv_ridge$cvm[cv_ridge$lambda == best_lambda]), "\n")

# Compute the residuals
residuals <- actual_values - predicted_values

# Compute the estimated residual variance
residual_variance <- var(residuals)
cat("Estimated residual variance:", residual_variance, "\n")

## I get optimal lamda = 0.05592575 
## and estimated residual variance: 0.4927652
## these correspond to normal prior variance for betas of 
# tau = 0.4927652 / 0.05592575 = 8.811061


# Calculate the standard deviation (square root of variance)
std_dev <- sqrt(8.811061)

# Create a sequence of x values
x <- seq(-4*std_dev, 4*std_dev, length.out = 200)

# Calculate the probability density function (PDF)
y <- dnorm(x, mean = 0, sd = std_dev)

# Plot the normal distribution
plot(x, y, 
     type = "l", 
     main = "Normal Distribution (μ = 0, σ² = 8.811061)",
     xlab = "Value", 
     ylab = "Probability Density",
     col = "blue",
     lwd = 2)



