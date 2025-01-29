library(dplyr)
library(stringi)
library(stringr)


raw_data <- read.csv("truck_market_data_with_prices.csv", stringsAsFactors = FALSE)

# country names
country_list <- c(
  "Polska", "Niemcy", "Francja", "Włochy", "Hiszpania", "Norwegia", "Szwecja", 
  "Finlandia", "Dania", "Holandia", "Belgia", "Czechy", "Słowacja", "Węgry", 
  "Austria", "Litwa", "Łotwa", "Estonia", "Szwajcaria", "Wielka Brytania", 
  "Irlandia", "Rumunia", "Bułgaria", "Turcja", "Ukraina", "Białoruś", 
  "Rosja", "Chorwacja", "Słowenia", "Serbia", "Bośnia i Hercegowina", 
  "Macedonia", "Grecja", "Portugalia", "Chiny", "Japonia", "Korea", 
  "USA", "Kanada", "Brazylia"
)


cleaned_data <- raw_data %>%
  mutate(
    weight_kg = ifelse(grepl("kg$", country_of_origin), 
                       stri_extract_first_regex(country_of_origin, "\\d+"), NA),
    country_of_origin = ifelse(grepl("kg$", country_of_origin), NA, country_of_origin), 
    
    engine_capacity_moved = ifelse(
      grepl("KM$", engine_capacity), engine_capacity,
      ifelse(grepl("kg$", engine_capacity), engine_capacity,
             ifelse(engine_capacity %in% country_list, engine_capacity, NA) 
      )
    ),
    engine_capacity = ifelse(
      grepl("cm3$", engine_capacity), engine_capacity, NA 
    ),
    
    power_moved = ifelse(
      grepl("cm3$", power), power,
      ifelse(grepl("kg$", power), power, 
             ifelse(power %in% country_list, power, NA)
      )
    ),
    power = ifelse(
      grepl("KM$", power), power, NA
    ),
    
    country_of_origin = ifelse(
      is.na(country_of_origin) & !is.na(engine_capacity_moved) & engine_capacity_moved %in% country_list,
      engine_capacity_moved,
      ifelse(is.na(country_of_origin) & !is.na(power_moved) & power_moved %in% country_list,
             power_moved, country_of_origin
      )
    ),
    
    weight_kg = ifelse(
      is.na(weight_kg) & grepl("kg$", engine_capacity_moved), 
      stri_extract_first_regex(engine_capacity_moved, "\\d+"),
      ifelse(is.na(weight_kg) & grepl("kg$", power_moved),
             stri_extract_first_regex(power_moved, "\\d+"), weight_kg
      )
    ),
    
    engine_capacity = ifelse(
      is.na(engine_capacity) & grepl("cm3$", power_moved),
      power_moved,
      ifelse(is.na(engine_capacity) & grepl("cm3$", engine_capacity_moved),
             engine_capacity_moved, engine_capacity
      )
    ),
    
    power = ifelse(
      is.na(power) & grepl("KM$", engine_capacity_moved),
      engine_capacity_moved,
      ifelse(is.na(power) & grepl("KM$", power_moved),
             power_moved, power
      )
    )
  ) %>%
  select(-engine_capacity_moved, -power_moved)

cleaned_data <- cleaned_data %>%
  mutate(
    description = stri_replace_all_regex(description, "\\.ooa-[^;]*;\\}|\\.ooa-[^\\{]*\\{[^\\}]*\\}", ""),
    description = stri_replace_all_regex(description, "<[^>]*>", ""),
    description = gsub("\n", " ", description),
    description = trimws(description)
  )

cleaned_data <- cleaned_data %>%
  mutate(
    power_num =  as.integer(stri_replace_all_regex(stri_extract(power, "\\d[\\d\\s]*"), "\\s+", "")),
    engine_capacity_num = as.integer(stri_replace_all_regex(stri_extract(engine_capacity, "\\d[\\d\\s]*"), "\\s+", "")),
    mileage_km_num = as.integer(stri_replace_all_regex(stri_extract(mileage_km, "\\d[\\d\\s]*"), "\\s+", "")),
    weight_kg_num = as.integer(weight_kg)
  )

cleaned_data <- cleaned_data %>%
  mutate(
    years = 2025 - production_year
  )

write.csv(cleaned_data, "cleaned_truck_market_data.csv", row.names = FALSE)

glimpse(cleaned_data)



