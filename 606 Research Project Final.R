library(readr)
library(dplyr)
library(caret)
library(survival)
library(survminer)




BreastCancerData <- read_delim("Downloads/BreatCancerProjectSpring2025.txt", delim = "\t")


colnames(BreastCancerData)

glimpse(BreastCancerData)

nrow(BreastCancerData)

BreastCancerData[BreastCancerData == "Unknown"] <- NA
BreastCancerData[BreastCancerData == "Blank(s)"] <- NA
BreastCancerData[BreastCancerData == "Not Available"] <- NA
BreastCancerData[BreastCancerData == "Not Applicable"] <- NA


colSums(is.na(BreastCancerData))

missing_percent <- colSums(is.na(BreastCancerData)) / nrow(BreastCancerData) * 100
missing_percent[missing_percent > 0]


# Save column names before dropping
before_cols <- colnames(BreastCancerData)

# Drop columns with 100% missing
BreastCancerData <- BreastCancerData[, colSums(is.na(BreastCancerData)) != nrow(BreastCancerData)]

# Save column names after dropping
after_cols <- colnames(BreastCancerData)

# Find out which columns were dropped
dropped_cols <- setdiff(before_cols, after_cols)
print(dropped_cols)


# Identify numeric columns
numeric_cols <- sapply(BreastCancerData, is.numeric)

# Impute missing values with the median for numeric columns
BreastCancerData[numeric_cols] <- lapply(BreastCancerData[numeric_cols], function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
})


# Identify categorical columns
categorical_cols <- sapply(BreastCancerData, is.factor) | sapply(BreastCancerData, is.character)

# Impute missing values with the mode for categorical columns
BreastCancerData[categorical_cols] <- lapply(BreastCancerData[categorical_cols], function(x) {
  mode_value <- names(sort(table(x), decreasing = TRUE))[1]
  x[is.na(x)] <- mode_value
  return(x)
})



# Check for any remaining missing values in the dataset
missing_values <- colSums(is.na(BreastCancerData))

# Display columns with remaining missing values
missing_values[missing_values > 0]

glimpse(BreastCancerData)

BreastCancerData$`Survival months` <- as.numeric(BreastCancerData$`Survival months`)

# Create binary target: TRUE if survived 60 months or more, else FALSE
BreastCancerData$survived_5yr <- ifelse(BreastCancerData$`Survival months` >= 60, 1, 0)

table(BreastCancerData$survived_5yr)

sum(is.na(BreastCancerData$survived_5yr))
BreastCancerData <- BreastCancerData %>% filter(!is.na(survived_5yr))

sum(is.na(BreastCancerData$survived_5yr))  # should return 0


sum(is.na(BreastCancerData))  # total NAs in all columns
      
colnames(BreastCancerData)

# Check the columns of train_data
colnames(train_data)

# Check the first few rows of train_data
head(train_data)


# Ensure the 'event' variable is correctly created
train_data$event <- ifelse(train_data$`Vital status recode (study cutoff used)` == "Dead", 1, 0)

# Check for missing values in train_data
sum(is.na(train_data))


columns_to_drop <- c(
  "Race recode (W, B, AI, API)",
  "Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)",
  "Race/ethnicity",
  "Site recode ICD-O-3/WHO 2008 (for SIRs)",
  "Site recode - rare tumors",
  "COD to site recode",
  "COD to site rec KM",
  "COD to site recode ICD-O-3 2023 Revision",
  "COD to site recode ICD-O-3 2023 Revision Expanded (1999+)",
  "Patient ID",
  "Record number recode",
  "Sequence number",
  "Scope of reg lymph nd surg (1998-2002)",
  "RX Summ--Reg LN Examined (1998-2002)",
  "Surgery of oth reg/dis sites (1998-2002)",
  "Site specific surgery (1973-1997 varying detail by year and site)",
  "Radiation to Brain or CNS Recode (1988-1997)",
  "Survival months flag",
  "SEER cause-specific death classification",
  "SEER other cause of death classification",
  "IHS Link",
  "Year of follow-up recode",
  "Year of death recode",
  "Primary by international rules"
)

BreastCancerData <- BreastCancerData %>% select(-all_of(columns_to_drop))


colnames(trainData)








# Extract the lower age from the string (e.g., "60-64 years" -> 60)
BreastCancerData$Age_numeric <- as.numeric(sub("^(\\d+)-.*", "\\1", BreastCancerData$`Age recode with <1 year olds`))

unique(BreastCancerData$`Age recode with <1 year olds`)

age_raw <- BreastCancerData$`Age recode with <1 year olds`

# Convert to lower bound of age group
BreastCancerData$Age_numeric <- as.numeric(
  ifelse(grepl("^[0-9]+\\+[ ]*years$", age_raw), 
         sub("\\+.*", "", age_raw),                             # e.g., "85+ years" -> "85"
         sub("^(\\d+).*", "\\1", age_raw))                      # e.g., "60-64 years" -> "60"
)

summary(BreastCancerData$Age_numeric)
table(is.na(BreastCancerData$Age_numeric))  # should be mostly FALSE

hist(BreastCancerData$Age_numeric,
     main = "Distribution of Age",
     xlab = "Approximate Age (Years)",
     col = "skyblue",
     border = "white",
     breaks = 20)

options(scipen = 999)
barplot(table(trainData$Sex),
        main = "Gender Distribution",
        col = "orchid",
        ylab = "Count",
        ylim = c(0, 500000))


# Check the first few rows of the data to understand the structure
head(trainData[c("survived_5yr")])

# Check for any missing values in those columns
summary(trainData[c("Survival_months", "Year_of_death_recode")])

colnames(trainData)

table(trainData$survived_5yr)


# Calculate survival counts
survival_count <- table(trainData$survived_5yr)

# Calculate survival counts
survival_count <- table(trainData$survived_5yr)

# Rename categories for clarity
names(survival_count) <- c("Did Not Survive", "Survived")

# Calculate percentages
percentages <- round(100 * survival_count / sum(survival_count), 1)

# Create labels with percentages
labels <- paste0(names(survival_count), ": ", percentages, "%")

# Plot pie chart
pie(survival_count,
    labels = labels,
    col = c("lightblue", "palevioletred1"),
    main = "5-Year Survival Distribution")


# Create a frequency table
death_counts <- table(BreastCancerData$`Year of death recode`)

# Convert to a data frame for better handling
death_df <- as.data.frame(death_counts)
colnames(death_df) <- c("Year", "Count")

# Convert 'Year' to a factor with proper order
death_df$Year <- factor(death_df$Year, levels = death_df$Year)


# Set plot margins and label positioning
par(mar = c(8, 6, 4, 2),      # Margins: bottom, left, top, right
    mgp = c(4, 1, 0))         # Move axis labels further from axis: (label, tick labels, line)

# Create the barplot
barplot(height = death_df$Count,
        names.arg = death_df$Year,
        las = 2,
        col = ifelse(death_df$Year == "Alive at last contact", "aquamarine", "deeppink3"),
        main = "Year of Death Distribution",
        ylab = "Number of Patients",
        xlab = "Year",
        cex.names = 0.7,
        ylim = c(0, max(death_df$Count) + 20000))



table(BreastCancerData$`Year of death recode`)

# Select numerical variables
num_data <- BreastCancerData[, sapply(BreastCancerData, is.numeric)]
num_data <- na.omit(num_data)

# Compute correlation
cor_matrix <- cor(num_data)

# Plot
library(corrplot)
corrplot::corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7, title = "Correlation Heatmap")



library(dplyr)

# Load the required library
library(dplyr)

# Calculate the marital status distribution
marital_dist <- BreastCancerData %>%
  group_by(`Marital status at diagnosis`) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# View the result
print(marital_dist)


# Create labels with percentages
labels <- paste0(marital_dist$`Marital status at diagnosis`, 
                 ": ", round(marital_dist$percentage, 1), "%")

# Plot pie chart
pie(marital_dist$count, 
    labels = labels,
    col = rainbow(length(labels)), 
    main = "Marital Status at Diagnosis Distribution")
















 




# Find columns with only one unique value
cols_with_one_level <- sapply(trainData, function(col) length(unique(col)) == 1)

# View them
names(trainData)[cols_with_one_level]

drop_cols <- c("Site recode ICD-O-3/WHO 2008", "RX Summ--Scope Reg LN Sur (2003+)")

trainData <- trainData[, !(names(trainData) %in% drop_cols)]
testData <- testData[, !(names(testData) %in% drop_cols)]





# Check the column names after renaming
colnames(trainData)

# Rename columns to match the model formula
colnames(trainData)[colnames(trainData) == "Age_recode_with_<1_year_olds"] <- "Age_recode_with_1_year_olds"
colnames(trainData)[colnames(trainData) == "Breast_Subtype_(2010+)"] <- "Breast_Subtype_2010"
# Fit logistic regression model again
log_model <- glm(survived_5yr ~ Age_recode_with_1_year_olds
                 + Breast_Subtype_2010, 
                 data = trainData, family = binomial())

# Summarize the model
summary(log_model)








# Rename columns to simplify formula usage
colnames(trainData)[colnames(trainData) == "Age_recode_with_<1_year_olds"] <- "Age_recode_with_1_year_olds"
colnames(trainData)[colnames(trainData) == "Breast_Subtype_(2010+)"] <- "Breast_Subtype_2010"
colnames(trainData)[colnames(trainData) == "Race_recode_(White,_Black,_Other)"] <- "Race_recode"
colnames(trainData)[colnames(trainData) == "Chemotherapy_recode_(yes,_no/unk)"] <- "Chemotherapy_recode"
colnames(trainData)[colnames(trainData) == "Grade_Recode_(thru_2017)"] <- "Grade_Recode"
colnames(trainData)[colnames(trainData) == "Mets_at_DX-Distant_LN_(2016+)"] <- "Mets_DX_Distant_LN"
colnames(trainData)[colnames(trainData) == "Mets_at_DX-Other_(2016+)"] <- "Mets_DX_Other"
colnames(trainData)[colnames(trainData) == "Marital_status_at_diagnosis"] <- "Marital_status"

# Fit logistic regression model with more variables
log_model <- glm(survived_5yr ~ Age_recode_with_1_year_olds +
                            Breast_Subtype_2010 +
                            Race_recode +
                            Sex +
                            Grade_Recode +
                            Chemotherapy_recode +
                            Radiation_recode +
                            Mets_DX_Distant_LN +
                            Mets_DX_Other +
                            Marital_status,
                          data = trainData, family = binomial())

# View summary of the model
summary(log_model)



# Predict probabilities from the model
predicted_probs <- predict(log_model, type = "response")

# Convert probabilities to 0 or 1 using a threshold (like 0.5)
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Load the library
library(caret)
#  Confusion Matrix
conf_matrix <- table(Predicted = predicted_classes, Actual = trainData$survived_5yr)

# View results
print(conf_matrix)

#  Calculate Accuracy
accuracy <- mean(predicted_classes == trainData$survived_5yr)
print(paste("Accuracy of Logistic Regression:", round(accuracy, 4)))

#  Precision, Recall, and F1-Score
precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print precision, recall, and F1-score
print(paste("Precision: ", round(precision, 4)))
print(paste("Recall: ", round(recall, 4)))
print(paste("F1-Score: ", round(f1_score, 4)))





















library(nnet)

# Fit Neural Network Model
nn_model <- nnet(survived_5yr ~ Age_recode_with_1_year_olds +
                   Breast_Subtype_2010 +
                   Race_recode +
                   Sex +
                   Grade_Recode +
                   Chemotherapy_recode +
                   Radiation_recode +
                   Mets_DX_Distant_LN +
                   Mets_DX_Other +
                   Marital_status,
                 data = trainData, size = 5, maxit = 200, decay = 0.01)

# Predict probabilities
nn_probs <- predict(nn_model, type = "raw")

# Convert to classes (0 or 1)
nn_pred_classes <- ifelse(nn_probs > 0.5, 1, 0)

# Confusion Matrix
nn_conf <- table(Predicted = nn_pred_classes, Actual = trainData$survived_5yr)
print(nn_conf)

# Accuracy
nn_accuracy <- mean(nn_pred_classes == trainData$survived_5yr)
print(paste("Accuracy of Neural Network:", round(nn_accuracy, 4)))

# Precision, Recall, F1
nn_precision <- nn_conf[2, 2] / sum(nn_conf[2, ])
nn_recall <- nn_conf[2, 2] / sum(nn_conf[, 2])
nn_f1 <- 2 * (nn_precision * nn_recall) / (nn_precision + nn_recall)

# Print metrics
print(paste("Precision: ", round(nn_precision, 4)))
print(paste("Recall: ", round(nn_recall, 4)))
print(paste("F1-Score: ", round(nn_f1, 4)))









# Load necessary libraries
library(rpart)
library(rpart.plot)
library(caret)

# Step 1: Fit the Decision Tree Model
tree_model <- rpart(survived_5yr ~ Age_recode_with_1_year_olds + 
                      Breast_Subtype_2010 + Race_recode + 
                      Sex + Grade_Recode + Chemotherapy_recode + 
                      Radiation_recode + Mets_DX_Distant_LN + 
                      Mets_DX_Other + Marital_status, 
                    data = trainData, method = "class")

# Step 2: Predict on Training Data
tree_pred <- predict(tree_model, type = "class")

# Step 3: Confusion Matrix
tree_conf <- table(Predicted = tree_pred, Actual = trainData$survived_5yr)
print(tree_conf)

# Step 4: Calculate Performance Metrics
# Accuracy
tree_accuracy <- mean(tree_pred == trainData$survived_5yr)
print(paste("Accuracy: ", round(tree_accuracy, 4)))

# Precision
tree_precision <- tree_conf[2, 2] / sum(tree_conf[2, ])
print(paste("Precision: ", round(tree_precision, 4)))

# Recall
tree_recall <- tree_conf[2, 2] / sum(tree_conf[, 2])
print(paste("Recall: ", round(tree_recall, 4)))

# F1-Score
tree_f1 <- 2 * (tree_precision * tree_recall) / (tree_precision + tree_recall)
print(paste("F1-Score: ", round(tree_f1, 4)))


# Step 6: Visualize the Tree
rpart.plot(tree_model, type = 3, extra = 102, cex = 0.8)






















# Install and load required library
install.packages("pROC")  # Only if not already installed
library(pROC)










# Load pROC package
library(pROC)

# --- LOGISTIC REGRESSION ---
logit_probs <- predict(logit_model, type = "response")
roc_logit <- roc(trainData$survived_5yr, logit_probs)

# --- NEURAL NETWORK ---
nn_probs <- predict(nn_model, type = "raw")
roc_nn <- roc(trainData$survived_5yr, nn_probs)

# --- DECISION TREE ---
tree_probs <- predict(tree_model, type = "prob")[, 2]
roc_tree <- roc(trainData$survived_5yr, tree_probs)

# --- Plotting All ROC Curves ---
plot(roc_logit, col = "blue", main = "ROC Curves Comparison", lwd = 2, xlab = "1 - Specificity")
plot(roc_nn, col = "red", add = TRUE, lwd = 2)
plot(roc_tree, col = "green", add = TRUE, lwd = 2)
legend("bottomright", legend = c(
  paste("Logistic Regression (AUC =", round(auc(roc_logit), 4), ")"),
  paste("Neural Network (AUC =", round(auc(roc_nn), 4), ")"),
  paste("Decision Tree (AUC =", round(auc(roc_tree), 4), ")")
), col = c("blue", "red", "green"), lwd = 2)














