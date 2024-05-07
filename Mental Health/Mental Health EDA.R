library(tidyverse)

# Read the dataset
data <- read.csv("data/FF_allwaves_2024v1.csv")
str(data)
summary(data)

# Creating a subset for sociability-related variables
sociability_vars <- data %>% select(k6d1a, k6d1b, k6d2g, k6d1e, k6d1d, k6d1f, k6c40, k6d1c, k6d2af)

# Replace coded missing and other non-response values with NA for clarity in visualization
non_response_codes <- c(-1, -2, -3, -9)  # include all codes that denote non-response or missing data
sociability_vars[sociability_vars %in% non_response_codes] <- NA

# Plotting the distribution of sociability responses
sociability_vars_long <- pivot_longer(sociability_vars, cols = everything(), names_to = "Variable", values_to = "Response")

# Generate a bar plot for the distribution of responses
ggplot(sociability_vars_long, aes(x = Response, fill = as.factor(Response))) +
  geom_bar() +
  facet_wrap(~Variable, scales = "free_y") +
  labs(title = "Distribution of Sociability-Related Responses",
       x = "Response Code",
       y = "Count of Responses",
       fill = "Response Code") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("sociability_response_distribution.png", width = 12, height = 8)
