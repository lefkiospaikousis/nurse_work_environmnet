
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

labels <- labelled::var_label(dta)


vars_remove <- c("StartDate", "EndDate", "Status", "IPAddress", "id_part")


spain <- 
  dta %>% 
  filter(country == "Spain") %>% 
  select( -any_of(vars_remove) )


write_sav(spain, "data/Country-data-Spain.sav")
saveRDS(spain, "data/Country-data-Spain.rds")


spain_fct <- spain %>% 
  mutate(
    across(where(is.labelled), as_factor)
  )

writexl::write_xlsx(spain_fct, "data/Country-data-Spain.xlsx")


dictionary <- labelled::generate_dictionary(spain) 

writexl::write_xlsx(dictionary, "data/dictionary-spain.xlsx")


vtable::vtable(spain, file = "data/dictionary-numeric.html")
vtable::vtable(spain_fct, file = "data/dictionary-text.html")

