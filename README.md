# Truck Market Analysis
## Decoding the Price Tag: How do various factors influence truck pricing across different weight categories? Mazowieckie Voivodeship Case Analysis

### About the Project
This project is part of the requirements for a Web Scraping Master's course at the Faculty of Management of Politechnika Wrocławska.

### Prerequisites
The code for the scraping and the cleaning of the raw data was done with an R script executed on RStudio.

### Relevant Files on this Repository
- scraper.R -> on this file you can find the extraction by request of all the truck listings on Otomoto website for the Mazowieckie Voivodeship and the deatils of each listing.
- truck_market_data.csv -> file cointaining the raw data extracted.
- munging.R -> this file focuses on taking the scraped raw data for the cleaning and fixing of some fields, and also saves the cleaned data on a separated file.
- cleaned_truck_market_data.csv -> file containing the resulting cleaned data.
- function.R -> the functions needed for an extended analysis will be found on this file.
- Bazano_Sammartino_Mandziuk.Rmd -> this is the report file with the details of the study, it has to be knitted to obtain the html file.
- Bazano_Sammartino_Mandziuk.html -> the html knitted file resulting from the .Rmd file.

### Contributors
Marina Bazano Sammartino  
Mateusz Mandziuk
