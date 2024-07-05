healthcare_data <- read.csv("healthcare_dataset.csv", stringsAsFactors = FALSE)
#Converting Categorical Variables to Factors
healthcare_data$Gender <- factor(healthcare_data$Gender)
healthcare_data$Blood.Type <- factor(healthcare_data$Blood.Type)
healthcare_data$Admission.Type <- factor(healthcare_data$Admission.Type)
healthcare_data$Test.Results <- factor(healthcare_data$Test.Results)
healthcare_data$Medical.Condition <- factor(healthcare_data$Medical.Condition)

head(healthcare_data)
# Check for missing values
missing_values <- colSums(is.na(healthcare_data))
print(missing_values)

# Boxplot to detect outliers in numeric variables
numeric_columns <- sapply(healthcare_data, is.numeric)
boxplot(healthcare_data[, numeric_columns])

# Handling outliers by capping (Winsorizing)
for(col in colnames(healthcare_data[, numeric_columns])) {
  Q1 <- quantile(healthcare_data[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(healthcare_data[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  healthcare_data[[col]] <- ifelse(healthcare_data[[col]] < (Q1 - 1.5 * IQR), 
                                   Q1 - 1.5 * IQR, healthcare_data[[col]])
  healthcare_data[[col]] <- ifelse(healthcare_data[[col]] > (Q3 + 1.5 * IQR), 
                                   Q3 + 1.5 * IQR, healthcare_data[[col]])
}