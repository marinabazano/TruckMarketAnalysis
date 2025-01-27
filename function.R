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
