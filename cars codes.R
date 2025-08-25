# Load necessary libraries
library(readr)       # For reading CSV files
library(dplyr)       # For data manipulation
library(ggplot2)     # For data visualization
library(ggcorrplot)  # New package for correlation visualization
library(corrplot)    # For correlation matrix visualization
library(GGally)
library(ggplot2)

# Load dataset
cars <- read_csv("C:/Users/danca/Downloads/Project/Final/cars.csv")

# View first few rows
head(cars)

# Check structure of the dataset
str(cars)

glimpse(cars)

# Summary statistics
summary(cars)

# Data Cleaning: Handling missing values
cars_cleaned <- cars %>%
  filter(!is.na(`Engine HP`) & !is.na(MSRP))  # Removing rows with missing HP or price

# Convert categorical variables to factors
cars_cleaned <- cars_cleaned %>%
  mutate(
    `Transmission Type` = as.factor(`Transmission Type`),
    `Driven_Wheels` = as.factor(`Driven_Wheels`),
    `Vehicle Size` = as.factor(`Vehicle Size`)
  )

# Exploratory Data Analysis (EDA)
## Distribution of MSRP (Price)
ggplot(cars_cleaned, aes(x = MSRP)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Car Prices (MSRP)", x = "Price (MSRP)", y = "Count") +
  theme_minimal()

ggpairs_plot <- ggpairs(
  cars,
  columns = c("Engine HP", "MSRP", "combined mpg", "Engine Cylinders"),
  lower = list(continuous = wrap("smooth", method = "lm", se = FALSE)),
  title = "Pairwise Relationships in Car Dataset"
)

# Show the plot
print(ggpairs_plot)

## Relationship between Engine HP and Price
ggplot(cars_cleaned, aes(x = `Engine HP`, y = MSRP)) +
  geom_point(alpha = 0.5, color = "red") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Engine Horsepower vs Price", x = "Engine HP", y = "Price (MSRP)") +
  theme_minimal()

## Boxplot of Price by Transmission Type
ggplot(cars_cleaned, aes(x = `Transmission Type`, y = MSRP, fill = `Transmission Type`)) +
  geom_boxplot() +
  labs(title = "Car Price Distribution by Transmission Type", x = "Transmission Type", y = "Price (MSRP)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Correlation Analysis
## Selecting numeric columns
numeric_columns <- cars_cleaned %>%
  select(`Engine HP`, `Engine Cylinders`, `highway MPG`, `city mpg`, `combined mpg`, MSRP)

## Compute correlation matrix
cor_matrix <- cor(numeric_columns, use = "complete.obs")

## Visualizing Correlation Matrix
ggcorrplot(cor_matrix, method = "circle", type = "lower", lab = TRUE, lab_size = 3)

# Linear Regression Model: Predicting Price (MSRP)
model <- lm(MSRP ~ `Engine HP` + `highway MPG` + `city mpg`, data = cars_cleaned)
summary(model)

anova(model)

# Model diagnostics: Residual plot
ggplot(data = data.frame(Fitted = fitted(model), Residuals = resid(model)), aes(x = Fitted, y = Residuals)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Apply log transformation to MSRP
cars_cleaned$log_MSRP <- log(cars_cleaned$MSRP)

# Fit linear regression model with log-transformed MSRP
log_model <- lm(log_MSRP ~ `Engine HP` + `highway MPG` + `city mpg`, data = cars_cleaned)

# Summary of the log-transformed model
summary(log_model)

# ANOVA for the log-transformed model
anova(log_model)

# Set up 2x2 plotting area
par(mfrow = c(2,2))

# Plot diagnostic plots for the log-transformed model
plot(log_model)



