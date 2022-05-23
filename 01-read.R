# Script to Read the datafiles and do some first processing
# 
# There are 2 datasets. One is the result of online submitting responses
# and the other '.. -copy' is the result of the manual input of  hard copy questionnaires


# Libraries ---------------------------------------------------------------

library(gtsummary)
library(haven)
library(labelled)
library(tidyverse)

# Read data ---------------------------------------------------------------

path_orgl <- "data/HWE_March+23,+2022_09.27.sav"
path_copy <- "data/HWE+-+Copy+2_March+23,+2022_09.28.sav"

dta1 <- read_sav(path_orgl) 
dta2 <- read_sav(path_copy) 

dta <- bind_rows(dta1, dta2)

# some var labels are not kept. Specifically character columns. WHY?
# Fix
dta <- dta %>% copy_labels_from(dta1)

# Carriage returns in some long labels
#  e.g.
var_label(dta$C11_1_1)

# Remove carriage returns and store to an object
var_label(dta) <- lapply(var_label(dta), str_squish)


# Variable Dictionary -----------------------------------------------------


# Labels - Dictionary
# dictionary <- labelled::generate_dictionary(dta)
# 
# writexl::write_xlsx(dictionary, "data/temp_dictionary.xlsx") %>% browseURL()


# The survey was launched at 29/12/2020. Also, no `EN` language
# Remember 'language' is the selected language by user
# and 'UserLanguage' most probably is the browser's detected language

dta <- filter(dta, 
              RecordedDate > as.Date("2020-12-29"),
              language != "EN",
              Status != 1 # Preview response, not an actual response
)

# Completion Rate -------------------------------------------------------

dta <- dta %>% 
  mutate(
    # Section they have reached
    section = case_when(
      Progress <= 9 ~ "Introduction/Participant Code",
      Progress <= 18 ~ "Section A",
      Progress <= 43 ~ "Section B",
      Progress <= 100 ~ "Section C",
      TRUE ~ NA_character_
    ),
    # Do we have demographic information (Progress == 100)
    with_demo = Progress == 100
  ) %>% 
  set_variable_labels(
    section   = "Section reached",
    with_demo = "With demographic variables"
  ) 

dta %>% count(section, with_demo)

# Have a look
dta %>% 
  filter(Progress >= 9) %>% 
  select(section, language) %>% 
  tbl_summary(by = language) %>% 
  modify_header(label ~ "") 


# Progress = 9 means they dropped out at the participant questions

dta <- dta %>% filter(Progress > 9)


# Fix column names and labels ---------------------------------------------

dictionary <- readxl::read_xlsx("data/dictionary.xlsx")

# Rename column names safely using a vector of names in the form
# New name = Old name
col_names <- dictionary[, c("name", "old_name" )] %>% deframe()

# have a look
col_names[21:23]

dta <- dta %>% rename(!!!col_names)


# list of column names with labels as values
# add labels safely with labelled::var_label()
labels <- dictionary[, c("name", "label" )] %>% deframe() %>% as.list()
labelled::var_label(dta) <- labels
  

# Unnecessary Columns
cols_remove <- c("RecipientLastName", 
                 "RecipientFirstName", 
                 "RecipientEmail", 
                 "ExternalReference", 
                 "LocationLatitude", 
                 "LocationLongitude", 
                 "DistributionChannel",
                 "Create_New_Field_or_Choose_From_Dropdown..."
)

dta <- dta %>% select(-any_of(cols_remove))

# Remove other incomplete responses

dta <- dta %>% 
  filter(!id %in% c("R_2rHOqIPcoC6EFTU", # incomplete in Section B despite 43% progress
                    "R_RVxixGVSb4O8j8l" # this is a test form SPAIN
                     
                    )
         )

var_label(dta) %>% keep(is.null)


# Save as RDS file

saveRDS(dta, "data/raw_processed.rds")
