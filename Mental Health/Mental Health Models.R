library(readr)
library(dplyr)
library(caret)
library(pROC)
library(ggplot2)
library(reshape2)

# Load the dataset
data <- read_csv("data/FF_allwaves_2024v1.csv", na = c("-9 Not in wave", "-3 Missing", "-2 Don't know"))

# Convert the variables to the correct factor levels and adjust rankings
data <- data %>%
  mutate(
    p6b10 = factor(p6b10, levels = c("2 No", "1 Yes"), labels = c("1 No", "3 Yes")),
    p6b5 = factor(p6b5, levels = c("2 No", "1 Yes"), labels = c("1 No", "3 Yes")),
    k6d1a = factor(k6d1a, levels = c("1 Not true", "2 Sometimes true", "3 Often true")),
    k6d1e = factor(k6d1e, levels = c("1 Not true", "2 Sometimes true", "3 Often true")),
    k6d1c = factor(k6d1c, levels = c("1 Not true", "2 Sometimes true", "3 Often true"))
  )

# Define outcomes and predictors
outcomes <- c("k6d1a", "k6d1e", "k6d1c")
predictors <- c("p6b10", "p6b5")

# Run models using a loop and 5-fold cross-validation
results <- list()
for (outcome in outcomes) {
  # Prepare data for this specific outcome (filter NA only in relevant columns)
  relevant_columns <- c(predictors, outcome)
  outcome_data <- data %>%
    select(all_of(relevant_columns)) %>%
    na.omit()  # Only omit rows where the relevant columns for the current model are NA
  
  # Set up 5-fold cross-validation
  set.seed(123)
  folds <- createFolds(outcome_data[[outcome]], k = 5)
  
  model_stats <- list()
  for (i in seq_along(folds)) {
    # Subset data for training and testing
    train <- outcome_data[-folds[[i]],]
    test <- outcome_data[folds[[i]],]
    
    # Fit logistic regression model
    model <- glm(formula(paste(outcome, "~", paste(predictors, collapse = "+"))), data = train, family = binomial())
    
    # Predict on test set and calculate statistics
    preds_prob <- predict(model, newdata = test, type = "response")
    preds_class <- ifelse(preds_prob > 0.5, "3 Often true", ifelse(preds_prob > 0.33, "2 Sometimes true", "1 Not true"))
    cm <- confusionMatrix(as.factor(preds_class), as.factor(test[[outcome]]))
    
    # Store model statistics
    model_stats[[i]] <- list(model = model, confusionMatrix = cm$table)
  }
  results[[outcome]] <- model_stats
}

# Plotting Confusion Matrices as Heatmaps
for (outcome in names(results)) {
  cat("Outcome:", outcome, "\n")
  for (i in seq_along(results[[outcome]])) {
    cat("Fold", i, "\n")
    # Extracting the confusion matrix
    cm_matrix <- results[[outcome]][[i]]$confusionMatrix
    # Melting the confusion matrix for plotting
    cm_data <- reshape2::melt(cm_matrix)
    names(cm_data) <- c("Prediction", "Reference", "Count")  # Correcting column names
    
    # Plotting
    p <- ggplot(cm_data, aes(x = Reference, y = Prediction, fill = Count)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "blue", high = "red") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold")
      ) +
      labs(title = paste("Confusion Matrix Heatmap for", outcome, "- Fold", i),
           x = "Predicted Class",
           y = "Actual Class",
           fill = "Count")
    print(p)
  }
}
