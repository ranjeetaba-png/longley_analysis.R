# Load the dataset from the datasets package
library(datasets)
data(longley)

# ==========================================
# STEP 1 & 2: Correlation and Plotting
# ==========================================
# Compute correlations with 'Employed'
correlations <- cor(longley)[, "Employed"]
# Remove the correlation of 'Employed' with itself
correlations <- correlations[names(correlations) != "Employed"]

# Sort by absolute correlation value to get the top 3
top_3_vars <- names(sort(abs(correlations), decreasing = TRUE)[1:3])
print("Top 3 variables most correlated to Employed:")
print(correlations[top_3_vars])

# Plot Employed against each variable
par(mfrow=c(1, 3)) # Set plotting grid to 1 row, 3 columns
for (var in top_3_vars) {
  plot(longley[[var]], longley$Employed, 
       xlab = var, ylab = "Employed", 
       main = paste("Employed vs", var),
       col = "blue", pch = 16)
}

# ==========================================
# STEP 3: Create Models and Select Champion
# ==========================================
# Create the three simple linear regression models
model1 <- lm(Employed ~ GNP, data = longley)
model2 <- lm(Employed ~ Year, data = longley)
model3 <- lm(Employed ~ GNP.deflator, data = longley)

# Compare model performances (Adjusted R-squared)
print("Adjusted R-squared Values:")
print(paste("GNP:", summary(model1)$adj.r.squared))
print(paste("Year:", summary(model2)$adj.r.squared))
print(paste("GNP.deflator:", summary(model3)$adj.r.squared))

# Selection of the Champion Model (GNP has the highest Adjusted R-squared)
champion_model <- model1
print("Champion Model Summary:")
print(summary(champion_model))

# ==========================================
# STEP 4: Create Model Matrices
# ==========================================
# Extract the response vector Y
Y <- as.matrix(longley$Employed)

# Create the Design Matrix X (includes the Intercept column of 1s)
X <- model.matrix(Employed ~ GNP, data = longley)

# Print snapshots of the matrices to verify they exist
print("Design Matrix X (First 5 rows):")
print(head(X, 5))

print("Response Vector Y (First 5 rows):")
print(head(Y, 5))

# ==========================================
# STEP 5: Recalculate Parameters (Matrix Algebra)
# ==========================================
# 1. Recalculate Beta Parameters using: beta = (X^T * X)^-1 * X^T * Y
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
print("Recalculated Regression Parameters (Matrix Algebra):")
print(beta_hat)

# Compare with the original lm() output to verify accuracy
print("Original lm() coefficients:")
print(coef(champion_model))

# 2. Recalculate Predicted Values using: Y_hat = X * beta
Y_hat <- X %*% beta_hat
print("First 5 Recalculated Predicted Values:")
print(head(Y_hat, 5))

# Compare with the original lm() fitted values
print("First 5 Original lm() fitted values:")
print(head(fitted(champion_model), 5))
