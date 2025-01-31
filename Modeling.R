cleaned_data <- read.csv("cleaned_truck_market_data.csv")
glimpse(cleaned_data)

# Function for LR and correlation analysis
analyze_subset <- function(data, segment_name) {
  cat("\n### Analysis for", segment_name, "###\n")
  
  # Remove NA rows
  data_clean <- na.omit(data)
  
  if (nrow(data_clean) == 0) {
    cat("No data available after removing NAs for", segment_name, "\n")
    return(NULL)
  }
  
  # Perform Linear Regression
  lm_model <- lm(price_num ~ ., data = data_clean)
  print(summary(lm_model))
  
  # Compute Correlation Matrix
  cor_matrix <- cor(data_clean[, sapply(data_clean, is.numeric)], use = "complete.obs")
  
  if ("price_num" %in% rownames(cor_matrix)) {
    cor_price <- sort(cor_matrix["price_num", ], decreasing = TRUE)
    print(cor_price)
  } else {
    cor_price <- NA
    cat("price_num column not found in correlation matrix for", segment_name, "\n")
  }
  
  return(list(lm_model = lm_model, cor_price = cor_price))
}

# Define subsets
subsets <- list(
  All_Data = subset(cleaned_data, select = c("price_num", "mileage_km_num", "power_num", "engine_capacity_num", "years")), # Entire dataset
  LCV = subset(cleaned_data, segment == "LCV", select = c("price_num", "mileage_km_num", "power_num", "engine_capacity_num", "years")),
  MCV = subset(cleaned_data, segment == "MCV", select = c("price_num", "mileage_km_num", "power_num", "engine_capacity_num", "years")),
  HCV = subset(cleaned_data, segment == "HCV", select = c("price_num", "mileage_km_num", "power_num", "engine_capacity_num", "years"))
)

# Run analysis for each segment and whole dataset
results <- lapply(names(subsets), function(segment) analyze_subset(subsets[[segment]], segment))


