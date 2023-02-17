
library(tidyverse)

# Setup -------------------------------------------------------------------

source("00-variable_info.R", local = TRUE)
source("00-helper_functions.R", local = TRUE)


dta <- readRDS("data/raw_cleaned.rds")

missing_removed <- readxl::read_xlsx("AACN_survey-missing_values.xlsx", sheet = "MissingRemoved")

vars_remove <- c("StartDate", "EndDate", "Status", "IPAddress", "id_part")

dta <- dta %>% 
  filter(id %in% missing_removed$Responseid) %>% 
  select( -any_of(vars_remove) )

labels <- labelled::var_label(dta)


country <- "ALL_Countries-missing_not_removed"

country_data <- dta %>% 
  #filter(country == !!country) %>% 
  select( -any_of(vars_remove) )

country_data_fct <- country_data %>% 
  mutate(
    across(where(labelled::is.labelled), as_factor)
  )

haven::write_sav(country_data, glue::glue("data/FULL DATA/Country-data-{country}.sav"))
saveRDS(country_data, glue::glue("data/FULL DATA/Country-data-{country}.rds"))
writexl::write_xlsx(country_data, glue::glue("data/FULL DATA/Country-data-{country}.xlsx"))


#dictionary <- labelled::generate_dictionary(spain) 
#writexl::write_xlsx(dictionary, "data/dictionary-spain.xlsx")


vtable::vtable(dta, file = "data/dictionary-numericValues-in-Reponses.html")
vtable::vtable(dta, file = "data/dictionary-textValues-in-Responses.html")

