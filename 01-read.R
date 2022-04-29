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
              language != "EN"
)

dta %>% count(language)

# Completion Rate -------------------------------------------------------

dta <- 
  dta %>% 
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
    section = "Section reached",
    with_demo = "With demographic variables"
  ) 


dta %>% count(section, with_demo)

# Have a look
dta %>% 
  filter(Progress >= 9) %>% 
  select(section, language) %>% 
  tbl_summary(
    by = language
  ) %>% 
  modify_header(label ~ "") 


# Progress = 9 means they dropped out at the participant questions
# Remove them
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

# Remove ids that are incomplete in Section B despite 43% progress
# Where he fuck did this come from?

dta <- dta %>% 
  filter(!id %in% c("R_2rHOqIPcoC6EFTU"))




# Locate Tests - Retests -----------------------------------------------------------------

# Create an identification variable (id_part)
dta <- dta %>% 
  mutate(across(starts_with("part"), str_trim)) %>% 
  mutate(id_part = paste(part1, part2, part3, part4, sep = "-"),
         id_part = tolower(id_part))

# test retest mainly for Section B - the HWE scale. That is at 43% progress

dta_compinations <- 
  dta %>% 
  filter(!is.na(part2)) %>% 
  filter(progress>=43) %>% 
  count(country, residence, id_part,  sort = TRUE) %>% 
  # Not sure about the n > 2, so I keep only the instances with 2 concurrences
  filter(n == 2) %>% 
  # a better combination id using the resedence as well
  mutate(id_comb = paste(country, residence, id_part, sep = ":")) 


dta_compinations %>% count(country)


# Now filter the data using this id_comb to get the response ids (id)
dta_retests <-  dta %>% 
  mutate(id_comb = paste(country, residence, id_part, sep = ":")) %>% 
  filter(id_comb %in% dta_compinations$id_comb) %>% 
  select(country, id, date, id_comb) %>% 
  group_by(country, id_comb) %>% 
  # same date repsonss hardly are retests
  arrange(date, .by_group = TRUE) %>% 
  mutate(same_date = as.Date(date) == lag(as.Date(date))) %>% 
  fill(same_date, .direction = "up") %>% 
  filter(!same_date) %>% 
  mutate(test = c("test", "retest")) %>% 
  ungroup()

# How many retest is each country
dta_retests %>% 
  filter(test == "retest") %>% 
  count(country)


retest_ids <- dta_retests %>% 
  filter(test == "retest") %>% 
  pull(id)

test_ids <- dta_retests %>% 
  filter(test == "test") %>% 
  pull(id)


dta <-  filter(dta, !id %in% retest_ids)

var_label(dta) %>% keep(is.null)

# Save as RDS file

saveRDS(dta, "data/raw_processed.rds")

saveRDS(dta_retests, "data/dta_retests.rds")
