# Function to calculate average mileage by brand
average_mileage_by_brand <- function(data) {
  data %>%
    group_by(vehicle_brand) %>%
    summarise(avg_mileage = mean(mileage_km, na.rm = TRUE)) %>%
    arrange(desc(avg_mileage))
}

# Function to count sellers by type
seller_type_distribution <- function(data) {
  data %>%
    count(seller_type) %>%
    arrange(desc(n))
}

extract_numeric <- function(text) {
  as.numeric(str_replace_all(text, "[^0-9]", ""))
}

aggregate_price <- function(data, group_col) {
  data %>%
    group_by({{ group_col }}) %>%
    summarize(avg_price = mean(as.numeric(str_replace(price, " ", "")), na.rm = TRUE))
}

clean_column <- function(column) {
  str_trim(column)
}

#Function to load data
load_data <- function(file_path) {
  truck_data <- read.csv(file_path)
  return(truck_data)
}
#Add column that defines segment (LCV,MCV,HCV)
    #segmented based on HP or cc
assign_segment <- function(truck_data) {
  truck_data$segment <- ifelse(truck_data$engine_capacity_num < 3000, "LCV", 
                               ifelse(truck_data$engine_capacity_num < 7500, "MCV", "HCV"))
  return(truck_data)
}
# Add column defined (2025-year of production)
    #truck_data$Age <- 2025 - truck_data$Year_of_Production
calculate_age <- function(truck_data) {
  truck_data$Age <- 2025 - truck_data$Year_of_Production
  return(truck_data)
}
# Exploratory Data Analysis
perform_eda <- function(truck_data) {
  # Summary statistics
  print(summary(truck_data))
  
  # Plot distributions
  hist(truck_data$Price, main = "Price Distribution",
       xlab = "Price", col = "lightblue")
  
  # Correlation matrix for numerical variables
  cor_matrix <- cor(truck_data[, sapply(truck_data, is.numeric)])
  print(cor_matrix)
}

# Identify key features impacting price
    #Random Forest, Linear Regression, Correlation Analysis
identify_key_factors <- function(truck_data) {
  # Random Forest Model
  library(randomForest)
  rf_model <- randomForest(Price ~ ., data = truck_data, importance = TRUE)
  print(rf_model)
  
  # Linear Regression Model
  lm_model <- lm(Price ~ ., data = truck_data)
  summary(lm_model)
  
  # Correlation Matrix
  cor_matrix <- cor(truck_data[, sapply(truck_data, is.numeric)])
  print(cor_matrix)
}

# Visualising the results
    #Bar chart of factors importance,
    #Price vs factor scatter plot,
    #Comparison of segments
visualize_results <- function(truck_data, rf_model) {
  # Feature Importance - Bar chart for Random Forest
  importance_data <- importance(rf_model)
  barplot(importance_data[, 1], main = "Feature Importance",
          col = "lightblue", las = 2)
  
  # Price vs Key Factors - Scatter plot
  plot(truck_data$Age, truck_data$Price, main = "Price vs Age",
       xlab = "Age", ylab = "Price", col = "blue")
  plot(truck_data$Weight, truck_data$Price, main = "Price vs Weight",
       xlab = "Weight", ylab = "Price", col = "green")
  
  # Compare Segments - Boxplot for price comparison
  boxplot(Price ~ Segment, data = truck_data, main = "Price by Segment",
          xlab = "Segment", ylab = "Price",
          col = c("lightgreen", "lightblue", "lightpink"))
}
