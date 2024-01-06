################################################################################
# Load required libraries
################################################################################
library(randomForest)
library(caret)
library(FactoMineR)
library(fastDummies)
library(dplyr)
library(ggplot2)
library(reshape2)
library(MASS)
if (!require(nnet)) install.packages("nnet")
library(nnet)
if (!require(ggfortify)) install.packages("ggfortify")
library(ggfortify)
library(moments)

################################################################################
# Load the data
################################################################################
data=read.csv('/Users/kellyliu/Documents/00. McGill MMA/## MGSC661 R/Final Project/Automobile/Automobile.csv')

################################################################################
# Preprocess and Feature Engineering
################################################################################
# Replace "?" with NA for all variables in the dataset
data=data %>%
  mutate(across(where(is.character), ~na_if(., "?")))

# Remove rows with NA values
data=na.omit(data)  

# Convert all character columns to factors
data=data %>%
  mutate(across(where(is.character), as.factor))

# Possible columns that can be converted to numeric
table(num.of.doors)
table(num.of.cylinders)

# Convert 'num.of.doors' from text to discrete integer
# Map the text to numbers
doors_map=c("four" = "4", "two" = "2")

# Replace the text with numbers using the map
data$num.of.doors=sapply(data$num.of.doors, function(x) doors_map[x])

# Convert 'num.of.cylinders' from text to numeric
# Map the text to numbers
cylinders_map=c("eight" = 8, "five" = 5, "four" = 4, "six" = 6, "three" = 3)
data$num.of.cylinders=as.numeric(sapply(data$num.of.cylinders, function(x) cylinders_map[x]))

# Check the results
table(data$num.of.doors)
table(data$num.of.cylinders)

# Ensure numeric variables are in numeric
# Columns to convert to numeric
columns_to_convert=c("normalized.losses", "price", "bore", "stroke", 
                        "horsepower", "peak.rpm", "symboling","num.of.doors","num.of.cylinders")

# Loop through each column and convert to numeric
for (col in columns_to_convert) {
  data[[col]]=as.numeric(as.character(data[[col]]))
}

################################################################################
# Check Distribution
################################################################################
summary(data)

################################################################################
# Check Skewness
################################################################################
skewness_values = c()
skewness_degrees = c()
mydata = data
num_columns = sapply(mydata, is.numeric) 
for (column in names(mydata[, num_columns])) {
  skew_value=skewness(mydata[[column]])
  if (skew_value >= -0.5 && skew_value <= 0.5) {
    skew_degree="Symmetric"
  } else if (skew_value > 0.5 && skew_value <= 1 || skew_value < -0.5 && skew_value >= -1) {
    skew_degree="Moderated Skewed"
  } else if (skew_value < -1 || skew_value > 1) {
    skew_degree="Highly Skewed"
  }
  skewness_values=c(skewness_values, skew_value)  
  skewness_degrees=c(skewness_degrees, skew_degree) 
}
skewness_results=data.frame(
  Column = names(mydata[, num_columns]),
  Skewness = skewness_values,
  Skewness_Degree = skewness_degrees
)
print(skewness_results)

#Convert the result into a chart
library(gt)
# Define your table as before
skewness_table=gt(data = skewness_results) %>%
  tab_header(
    title = "Skewness Analysis of Numeric Columns"
  ) %>%
  cols_label(
    Column = "Column Name",
    Skewness = "Skewness Value",
    Skewness_Degree = "Degree of Skewness"
  ) %>%
  fmt_number(
    columns = c("Skewness"),
    decimals = 2  # Adjust the number of decimal places as needed
  ) %>%
  tab_style(
    style = cell_text(align = 'center'),
    locations = cells_body(
      columns = c("Skewness")
    )
  ) %>%
  # Set 'Avenir' as the base font for the table
  opt_table_font(
    font = list("Avenir", default_fonts())
  ) %>%
  tab_options(
    column_labels.font.weight = "bold",
    heading.title.font.size = px(20),
    table.font.size = px(12)  
  )

skewness_table

################################################################################
# Correlation of all numeric data
################################################################################
# Select only numeric columns for correlation
numeric_data=data[sapply(data, is.numeric)]

# Compute correlation matrix
correlation_matrix=cor(numeric_data, method = "pearson")
print(correlation_matrix)
# Melt the correlation matrix for ggplot2
melted_corr_matrix=melt(correlation_matrix)
# Create a heatmap
ggplot(melted_corr_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "navy", high = "salmon", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()) +
  coord_fixed()

# Find highly correlated pairs of variables
correlation_matrix=cor(data %>% select_if(is.numeric))
highlyCorrelated=findCorrelation(correlation_matrix, cutoff = 0.8)
names(data)[highlyCorrelated]

data$symboling=as.factor(data$symboling)

################################################################################
# Check Relationships
################################################################################
# Set up theme for the plots to avoid repetitive customization for each element
custom_theme=theme_classic(base_family = "Avenir") +
  theme(plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(color = "black", size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "none")

# Define the custom color palette
symboling_levels=levels(data$symboling)
custom_colors=colorRampPalette(colors = c("navy", "white", "salmon"))(length(symboling_levels))

# Create a named vector of colors for each symboling value
colors_for_symboling=setNames(custom_colors, symboling_levels)

# Histogram for Symboling
ggplot(data, aes(x = symboling, fill = factor(symboling))) +
  geom_bar(color = "darkgrey", position = "identity", alpha = 0.6) +
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Symboling Distribution") +
  xlab("Symboling") + ylab("Frequency") +
  custom_theme

# Histogram for make
ggplot(data, aes(x = make, fill = factor(symboling))) +
  geom_bar(color = "darkgrey", position = "identity", alpha = 0.6) +
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Car Make Distribution") +
  xlab("Make") + ylab("Frequency") +
  custom_theme +
  theme(legend.position = "right")

# Histogram for Fuel Type
ggplot(data, aes(x = fuel.type, fill = factor(symboling))) +
  geom_bar(color = "darkgrey", position = "identity", alpha = 0.6) +
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Fuel Type Distribution") +
  xlab("Fuel Type") + ylab("Frequency") +
  custom_theme +
  theme(legend.position = "right")

# Histogram for Fuel System 
ggplot(data, aes(x = fuel.system, fill = factor(symboling))) +
  geom_bar(color = "darkgrey", position = "identity", alpha = 0.6) +
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Fuel System Distribution") +
  xlab("Fuel System") + ylab("Frequency") +
  custom_theme +
  theme(legend.position = "right")

# Histogram for Aspiration 
ggplot(data, aes(x = aspiration, fill = factor(symboling))) +
  geom_bar(color = "darkgrey", position = "identity", alpha = 0.6) +
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Aspiration  Distribution") +
  xlab("Aspiration") + ylab("Frequency") +
  custom_theme +
  theme(legend.position = "right")

# Histogram for Engine Type 
ggplot(data, aes(x = engine.type, fill = factor(symboling))) +
  geom_bar(color = "darkgrey", position = "identity", alpha = 0.6) +
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Engine Type  Distribution") +
  xlab("Engine Type") + ylab("Frequency") +
  custom_theme +
  theme(legend.position = "right")

# Boxplot for Normalized Losses Distribution by Symboling
ggplot(data, aes(x = symboling, y = normalized.losses, fill = symboling)) +
  geom_boxplot(color = "darkgrey") + 
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Normalized Losses by Symboling Distribution") +
  xlab("Symboling Types") + ylab("Normalized Losses") +
  custom_theme

# Boxplot for Price Distribution by Symboling
ggplot(data, aes(x = symboling, y = price, fill = symboling)) +
  geom_boxplot(color = "darkgrey") + 
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Price by Symboling Distribution") +
  xlab("Symboling Types") + ylab("Price") +
  custom_theme

# Boxplot for Width Distribution by Symboling
ggplot(data, aes(x = symboling, y = width, fill = symboling)) +
  geom_boxplot(color = "darkgrey") + 
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Width by Symboling Distribution") +
  xlab("Symboling Types") + ylab("Width") +
  custom_theme

# Boxplot for Length Distribution by Symboling
ggplot(data, aes(x = symboling, y = length, fill = symboling)) +
  geom_boxplot(color = "darkgrey") + 
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Length by Symboling Distribution") +
  xlab("Symboling Types") + ylab("Length") +
  custom_theme

# Boxplot for Height Distribution by Symboling
ggplot(data, aes(x = symboling, y = height, fill = symboling)) +
  geom_boxplot(color = "darkgrey") + 
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Height by Symboling Distribution") +
  xlab("Symboling Types") + ylab("Height") +
  custom_theme

# Boxplot for Wheel Base Distribution by Symboling
ggplot(data, aes(x = symboling, y = wheel.base, fill = symboling)) +
  geom_boxplot(color = "darkgrey") + 
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Wheel Base by Symboling Distribution") +
  xlab("Symboling Types") + ylab("Wheel Base") +
  custom_theme

# Boxplot for Peak RPM Distribution by Symboling
ggplot(data, aes(x = symboling, y = peak.rpm, fill = symboling)) +
  geom_boxplot(color = "darkgrey") + 
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Peak RPM by Symboling Distribution") +
  xlab("Symboling Types") + ylab("Peak RPM") +
  custom_theme

# Boxplot for Number of Cylinders Distribution by Symboling
ggplot(data, aes(x = symboling, y = num.of.cylinders, fill = symboling)) +
  geom_boxplot(color = "darkgrey") + 
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Number of Cylinders by Symboling Distribution") +
  xlab("Symboling Types") + ylab("Number of Cylinders") +
  custom_theme

# Boxplot for City MPG Distribution by Symboling
ggplot(data, aes(x = symboling, y = city.mpg, fill = symboling)) +
  geom_boxplot(color = "darkgrey") + 
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("City MPG by Symboling Distribution") +
  xlab("Symboling Types") + ylab("City MPG") +
  custom_theme


# Boxplot for Number of Doors Distribution by Symboling
ggplot(data, aes(x = symboling, y = num.of.doors, fill = symboling)) +
  geom_boxplot(color = "darkgrey") + 
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Number of Doors by Symboling Distribution") +
  xlab("Symboling Types") + ylab("Number of Doors") +
  custom_theme

# Boxplot for Horsepower Distribution by Symboling
ggplot(data, aes(x = symboling, y = horsepower, fill = symboling)) +
  geom_boxplot(color = "darkgrey") + 
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Horsepower by Symboling Distribution") +
  xlab("Symboling Types") + ylab("Horsepower") +
  custom_theme

# Boxplot for Compression Ratio Distribution by Symboling
ggplot(data, aes(x = symboling, y = compression.ratio, fill = symboling)) +
  geom_boxplot(color = "darkgrey") + 
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Compression Ratio by Symboling Distribution") +
  xlab("Symboling Types") + ylab("Compression Ratio") +
  custom_theme

# Boxplot for Engine Size Distribution by Symboling
ggplot(data, aes(x = symboling, y = engine.size, fill = symboling)) +
  geom_boxplot(color = "darkgrey") + 
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Engine Size by Symboling Distribution") +
  xlab("Symboling Types") + ylab("Engine Size") +
  custom_theme

# Boxplot for Curb Weight Distribution by Symboling
ggplot(data, aes(x = symboling, y = curb.weight, fill = symboling)) +
  geom_boxplot(color = "darkgrey") + 
  scale_fill_manual(values = colors_for_symboling) +
  ggtitle("Curb Weight by Symboling Distribution") +
  xlab("Symboling Types") + ylab("Curb Weight") +
  custom_theme

################################################################################
# Feature Selection
################################################################################
target_variable=data[, 1]
data_features=data[, -1]

# Wrapper Method: Recursive Feature Elimination (RFE)
set.seed(123) # for reproducibility
control=rfeControl(functions = rfFuncs, method = "cv", number = 10)
results=rfe(data_features, target_variable, sizes = c(1:10), rfeControl = control)
# Get the subset of selected features
feature_selected=data_features[, results$optVariables]
#feature_selected

extra_columns=c("peak.rpm","price","length","horsepower","city.mpg",
                   "num.of.cylinders","compression.ratio","engine.size",
                   "curb.weight")
feature_selected_1=cbind(feature_selected, data[, extra_columns])

################################################################################
# Correlation of feature selected
################################################################################
# Select only numeric columns for correlation
numeric_data=feature_selected_1[sapply(feature_selected_1, is.numeric)]

# Compute correlation matrix
correlation_matrix=cor(numeric_data, method = "pearson")
print(correlation_matrix)
# Melt the correlation matrix for ggplot2
melted_corr_matrix=melt(correlation_matrix)
# Create a heatmap
ggplot(melted_corr_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "navy", high = "salmon", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()) +
  coord_fixed()

# Find highly correlated pairs of variables
highlyCorrelated=findCorrelation(correlation_matrix, cutoff = 0.8)
names(feature_selected_1)[highlyCorrelated]

################################################################################
# PCA and Visualization with symboling types
################################################################################

# Install ggfortify if not already installed
if (!require(ggfortify)) install.packages("ggfortify")
library(ggfortify)

# Ensure symboling is numeric
data$symboling=as.numeric(as.character(data$symboling))

# Select only numeric columns for PCA
numeric_data=feature_selected_1 %>% select_if(is.numeric)

# Run PCA
pca_result=prcomp(numeric_data, center = TRUE, scale. = TRUE)
pca_result

# Display pve
pve=(pca_result$sdev^2)/sum(pca_result$sdev^2) 
pve
par(mfrow=c(1,2)) 
plot(pve, ylim=c(0,1)) 
plot(cumsum(pve), ylim=c(0,1))
pve

# Prepare data for plotting (merge PCA scores with brand data)
# Plotting the first and second principle component
pca_scores=as.data.frame(pca_result$x)
pca_scores$symboling=data$symboling

# Visualize PCA with different colors for each symboling
autoplot(pca_result, data = pca_scores, colour = 'symboling', loadings = TRUE, loadings.label = TRUE) +
  scale_colour_gradient2(low = "navy", high = "salmon", mid = "white", midpoint = median(pca_scores$symboling, na.rm = TRUE)) +
  theme_minimal() +
  labs(colour = "Symboling")

if (!require(gt)) install.packages("gt")
library(gt)

rotation_df=as.data.frame(pca_result$rotation[, 1:2])
rotation_df=data.frame(Variable = rownames(rotation_df), rotation_df)

# Create a gt table with formatting similar to the skewness_table
rotation_table_gt=gt(rotation_df) %>%
  tab_header(
    title = "Loadings for the First Two Principal Components"
  ) %>%
  cols_label(
    Variable = "Variable",
    PC1 = "PC1",
    PC2 = "PC2"
  ) %>%
  fmt_number(
    columns = vars(PC1, PC2),
    decimals = 4
  ) %>%
  tab_style(
    style = cell_text(align = 'center'),
    locations = cells_body(
      columns = c("PC1", "PC2")
    )
  ) %>%
  opt_table_font(
    font = list("Avenir", default_fonts())
  ) %>%
  tab_options(
    column_labels.font.weight = "bold",
    heading.title.font.size = px(20),
    table.font.size = px(12)
  )

# Print the gt-format table
print(rotation_table_gt)

################################################################################
# Predicting 'symboling' with feature_selected_1
################################################################################

# Ensure 'symboling' is treated as a factor (categorical variable)
data$symboling=as.factor(data$symboling)

# Adding the 'symboling' column back to the selected features
final_data=cbind(symboling = data$symboling, feature_selected_1)

# Split data into training and test sets for symboling prediction
set.seed(42)
train_fraction=0.8
train_sample=sample(seq_len(nrow(final_data)), size = floor(train_fraction * nrow(final_data)))
dataTrain_symboling=final_data[train_sample, ]
dataTest_symboling=final_data[-train_sample, ]

# Train Random Forest for predicting symboling
library(randomForest)

#rf_model_symboling=randomForest(symboling ~ ., data = dataTrain_symboling, ntree = 500)

# Train Random Forest for predicting symboling without 'normalized.losses'
rf_model_symboling = randomForest(symboling ~ . - normalized.losses, 
                                  data = dataTrain_symboling, ntree = 500)

# Predict 'symboling' on the test set
symboling_predictions=predict(rf_model_symboling, newdata = dataTest_symboling)

# Optional: Calculate accuracy or other performance metrics
confusionMatrix=table(dataTest_symboling$symboling, symboling_predictions)
accuracy=sum(diag(confusionMatrix)) / sum(confusionMatrix)
print(accuracy)

# Optional: View variable importance for the symboling model
importance_scores=importance(rf_model_symboling)

# Convert importance matrix to a data frame
importance_df=as.data.frame(importance_scores)
importance_df$Feature=rownames(importance_df)

# Sort by importance
importance_sorted=importance_df[order(-importance_df$MeanDecreaseGini), ]
print(importance_sorted)

# Load the ggplot2 library
library(ggplot2)

# Plotting Feature Importance
ggplot(importance_sorted, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flips the axes for horizontal bars
  theme_minimal() +
  labs(title = "Feature Importance in Random Forest Model",
       x = "Feature",
       y = "Importance")+
  custom_theme

