# Load necessary library
library(ggplot2)
library(here)

# Determine the directory of the script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Construct the path to the data file
data_file_path <- file.path(script_dir, "..", "data", "Marker Run SEC.csv")

# Load data
data <- read.csv(data_file_path, sep = ";", dec = ",", header = TRUE)

# Select only rows 1-4
data_subset <- data[1:4, ]

# Fit an exponential model (log-transform y for linearity)
exp_model <- lm(log(MW) ~ ml, data = data_subset)

# Extract coefficients
intercept <- exp(coef(exp_model)[1])  # Convert back from log scale
slope <- coef(exp_model)[2]

# Generate predicted values
data_subset$MW_pred <- exp(predict(exp_model, newdata = data_subset))

# Create equation as a string, replacing "ml" with "x"
equation <- paste0("MW = ", round(intercept, 3), " * e^(", round(slope, 3), " * x)")

# Define position for the equation (adjust x and y for better centering)
x_center <- mean(range(data_subset$ml))  # Center of x-axis
y_center <- mean(range(data_subset$MW))  # Center of y-axis (log scale will affect this)

# Create the plot
plot <- ggplot(data_subset, aes(x = ml, y = MW)) +
  geom_point() +  # Scatter plot
  geom_text(aes(label = X), vjust = -1, size = 5) +  # Add labels from column "X"
  geom_line(aes(y = MW_pred), color = "red", linetype = "dashed") +  # Regression line
  scale_y_log10() +  # Set y-axis to logarithmic scale
  labs(x = "Volume [mL]", y = "Molecular Weight [kDa]") +
  annotate("text", x = x_center, y = y_center,
           label = equation, hjust = 0.5, vjust = -0.5, size = 5) +  # Centered equation
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13)
  )

# Add vertical line and horizontal line from the intersection
if (nrow(data) >= 6) {
  ml_value_row6 <- data$ml[6]
  predicted_mw_row6 <- exp(predict(exp_model, newdata = data.frame(ml = ml_value_row6)))
  
  # Add vertical dotted line at the intersection point
  plot <- plot +
    annotate("segment", x = ml_value_row6, xend = ml_value_row6,
             y = 1, yend = predicted_mw_row6, linetype = "dotted")
  
  # Add horizontal line from the intersection to the y-axis (x = 0)
  plot <- plot +
    annotate("segment", x = 100, xend = ml_value_row6,
             y = predicted_mw_row6, yend = predicted_mw_row6, linetype = "dotted")              
  plot <- plot +
    annotate("text", x = ml_value_row6, y = 1,
             label = paste("Volume [mL] =", ml_value_row6), vjust = 1.5, hjust = 0.5, size = 5) +
    annotate("text", x = min(data$ml, na.rm = TRUE), y = predicted_mw_row6,
             label = paste("MW [kDa] =", round(predicted_mw_row6, 2)), hjust = 1.0, vjust = -0.5, size = 5)
}

# Display the plot
print(plot) 