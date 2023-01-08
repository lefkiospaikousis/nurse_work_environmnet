
library(gtsummary)
library(haven)
library(labelled)
library(flextable)
library(glue)
library(tidyverse)

# Setup -------------------------------------------------------------------

source("00-variable_info.R", local = TRUE)
source("00-helper_functions.R", local = TRUE)

gtsummary::set_gtsummary_theme(theme_gtsummary)

flextable::set_flextable_defaults(
  table.layout = "autofit"
)

dta <- readRDS("data/raw_cleaned.rds")

missing_removed <- readxl::read_xlsx("AACN_survey-missing_values.xlsx", sheet = "MissingRemoved")

vars_remove <- c("StartDate", "EndDate", "Status", "IPAddress", "id_part")

dta <- dta %>% 
  filter(id %in% missing_removed$Responseid) %>% 
  select( -any_of(vars_remove) )

labels <- labelled::var_label(dta)


country <- "Cyprus"

country_data <- dta %>% 
  filter(country == !!country) %>% 
  select( -any_of(vars_remove) )

country_data_fct <- country_data %>% 
  mutate(
    across(where(is.labelled), as_factor)
  )

write_sav(country_data, glue::glue("data/Country-data-{country}.sav"))
saveRDS(country_data, glue::glue("data/Country-data-{country}.rds"))
writexl::write_xlsx(country_data, glue::glue("data/Country-data-{country}.xlsx"))


#dictionary <- labelled::generate_dictionary(spain) 
#writexl::write_xlsx(dictionary, "data/dictionary-spain.xlsx")


vtable::vtable(dta, file = "data/dictionary-numericValues-in-Reponses.html")
vtable::vtable(dta, file = "data/dictionary-textValues-in-Responses.html")

