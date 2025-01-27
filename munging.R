library(dplyr)
library(stringi)


raw_data <- read.csv("truck_market_data_with_prices.csv", stringsAsFactors = FALSE)

# country names
country_list <- c(
  "Polska", "Niemcy", "Francja", "Włochy", "Hiszpania", "Norwegia", "Szwecja", 
  "Finlandia", "Dania", "Holandia", "Belgia", "Czechy", "Słowacja", "Węgry", 
  "Austria", "Litwa", "Łotwa", "Estonia", "Szwajcaria", "Wielka Brytania", 
  "Irlandia", "Rumunia", "Bułgaria", "Turcja", "Ukraina", "Białoruś", 
  "Rosja", "Chorwacja", "Słowenia", "Serbia", "Bośnia i Hercegowina", 
  "Macedonia", "Grecja", "Portugalia", "Chiny", "Japonia", "Korea Południowa", 
  "USA", "Kanada", "Brazylia"
)


cleaned_data <- raw_data %>%
  mutate(
    # 1. Initialize weight_kg column by extracting weights from country_of_origin
    weight_kg = ifelse(grepl("kg$", country_of_origin), 
                       stri_extract_first_regex(country_of_origin, "\\d+"), NA),  # Extract weight
    country_of_origin = ifelse(grepl("kg$", country_of_origin), NA, country_of_origin),  # Remove weights from country_of_origin
    
    # 2. Handle misplaced values in engine_capacity
    engine_capacity_moved = ifelse(
      grepl("KM$", engine_capacity), engine_capacity,  # Move KM to power
      ifelse(grepl("kg$", engine_capacity), engine_capacity,  # Move kg to weight_kg
             ifelse(engine_capacity %in% country_list, engine_capacity, NA)  # Move countries to country_of_origin
      )
    ),
    engine_capacity = ifelse(
      grepl("cm3$", engine_capacity), engine_capacity, NA  # Keep only valid cm3
    ),
    
    # 3. Handle misplaced values in power
    power_moved = ifelse(
      grepl("cm3$", power), power,  # Move cm3 to engine_capacity
      ifelse(grepl("kg$", power), power,  # Move kg to weight_kg
             ifelse(power %in% country_list, power, NA)  # Move countries to country_of_origin
      )
    ),
    power = ifelse(
      grepl("KM$", power), power, NA  # Keep only valid KM
    ),
    
    # 4. Append `engine_capacity_moved` and `power_moved` to appropriate columns
    country_of_origin = ifelse(
      is.na(country_of_origin) & !is.na(engine_capacity_moved) & engine_capacity_moved %in% country_list,
      engine_capacity_moved,
      ifelse(is.na(country_of_origin) & !is.na(power_moved) & power_moved %in% country_list,
             power_moved, country_of_origin
      )
    ),
    
    # Handle misplaced weights in `engine_capacity_moved` and `power_moved`
    weight_kg = ifelse(
      is.na(weight_kg) & grepl("kg$", engine_capacity_moved), 
      stri_extract_first_regex(engine_capacity_moved, "\\d+"),
      ifelse(is.na(weight_kg) & grepl("kg$", power_moved),
             stri_extract_first_regex(power_moved, "\\d+"), weight_kg
      )
    ),
    
    # 5. Reassign engine_capacity
    engine_capacity = ifelse(
      is.na(engine_capacity) & grepl("cm3$", power_moved),
      power_moved,
      ifelse(is.na(engine_capacity) & grepl("cm3$", engine_capacity_moved),
             engine_capacity_moved, engine_capacity
      )
    ),
    
    # 6. Reassign power
    power = ifelse(
      is.na(power) & grepl("KM$", engine_capacity_moved),
      engine_capacity_moved,
      ifelse(is.na(power) & grepl("KM$", power_moved),
             power_moved, power
      )
    )
  ) %>%
  select(-engine_capacity_moved, -power_moved)  # Drop intermediate columns used for swapping


write.csv(cleaned_data, "cleaned_truck_market_data.csv", row.names = FALSE)

glimpse(cleaned_data)
