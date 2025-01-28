library(httr)
library(rvest)
library(dplyr)
library(stringi)
library(tidyverse)
library(xml2)
# data set
truck_data <- data.frame(
  offer_name = character(),
  price = character(),
  mileage_km = character(),
  fuel_type = character(),
  transmission = character(),
  engine_capacity = character(),
  power = character(),
  country_of_origin = character(),
  description = character(),
  vehicle_brand = character(),
  vehicle_model = character(),
  color = character(),
  production_year = character(),
  seller_name = character(),
  seller_type = character(),
  seller_time_on_site = character(),
  location = character(),
  stringsAsFactors = FALSE
)

# iterating over the pages
all_offers <- c()
for (i in 1:2) {
  tryCatch({
    
    url <- paste0("https://www.otomoto.pl/ciezarowe/mazowieckie?search%5Bfilter_enum_damaged%5D=0&page=", i)
    
    response <- GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:108.0) Gecko/20100101 Firefox/108.0"))
    
    page <- content(response)
    
    offers <- page %>%
      html_elements('h2 a') %>%
      html_attr('href')
    
    all_offers <- c(all_offers, offers)
    
    print(paste("Page", i, "scraped. Found", length(offers), "offers."))
    
    Sys.sleep(3)
  }, error = function(e) {
    print(paste("Error scraping page:", i, "Error:", e$message))
  })
}

all_offers <- unique(all_offers)
print(paste("Total unique offers:", length(all_offers)))

# iterating over the offers
for (offer_link in all_offers[10:15]) {
  tryCatch({
    response <- GET(offer_link, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:108.0) Gecko/20100101 Firefox/108.0"))
    Sys.sleep(3)
    
    offer_page <- content(response)
    
    offer_name <- offer_page %>%
      html_element('h1') %>%
      html_text(trim = TRUE)
    
    price <- tryCatch({
      offer_page %>%
        html_element('h3.offer-price__number') %>%
        html_text(trim = TRUE)
    }, error = function(e) {
      print("Error extracting price")
      NA
    })
    
    details <- offer_page %>%
      html_elements('div[data-testid="detail"]') %>% 
      html_element('p:nth-of-type(2)') %>% 
      html_text(trim = TRUE)
  
    
    mileage_km <- details[1]
    fuel_type <- details[2]
    transmission <- details[3]
    engine_capacity <- details[4]
    power <- details[5]
    country_of_origin <- details[6]
    
    # Extract description
    description <- offer_page %>%
      html_element('div[data-testid="textWrapper"]') %>%
      html_text(trim = TRUE)
    
    # Extract specifications
    specs <- offer_page %>%
      html_elements('div[data-testid="basic_information"] p') %>%  
      html_text(trim = TRUE)
    
    vehicle_brand <- specs[3]
    vehicle_model <- specs[5]
    color <- specs[7]
    production_year <- specs[9]
    
    # Extract seller name
    seller_name <- offer_page %>%
      html_element('div[data-testid="content-seller-area-section"] p:first-child') %>%
      html_text(trim = TRUE)
    
    # Extract seller information
    seller_info <- offer_page %>%
      html_elements('div[data-testid="content-seller-area-section"]') %>%
      html_text(trim = TRUE)
    
    seller_type <- str_extract(seller_info, "Osoba prywatna|Firma")
    seller_time_on_site <- str_extract(seller_info, "Sprzedający na OTOMOTO od \\d{4}")
    
    # Extract location
    location <- str_extract(seller_info, "[^}]+$")
    
    truck_data <- rbind(truck_data, data.frame(
      offer_name = offer_name,
      price = price,
      mileage_km = mileage_km,
      fuel_type = fuel_type,
      transmission = transmission,
      engine_capacity = engine_capacity,
      power = power,
      country_of_origin = country_of_origin,
      description = description,
      vehicle_brand = vehicle_brand,
      vehicle_model = vehicle_model,
      color = color,
      production_year = production_year,
      seller_name = seller_name,
      seller_type = seller_type,
      seller_time_on_site = seller_time_on_site,
      location = location,
      stringsAsFactors = FALSE
    ))
    
    print(paste("Scraped data from:", offer_link))
    
  }, error = function(e) {
    print(paste("Error scraping:", offer_link, "Error:", e$message))
  })
}



code <- paste(as.character(offer_page), collapse = "\n")

write.table(code, 
            file='code_result.html', 
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE)


####################


write.csv(truck_data, "truck_market_data.csv", row.names = FALSE)
print("Scraping completed. Data saved to 'truck_market_data.csv'.")

glimpse(truck_data)

truck_data$location
