# Script to CLEAN the dataset
# 

# Libraries ---------------------------------------------------------------

library(gtsummary)
library(haven)
library(labelled)
library(tidyverse)

# Setup -------------------------------------------------------------------

source("00-variable_info.R", local = TRUE)
source("00-helper_functions.R", local = TRUE)

dta_orgl <- readRDS("data/raw_processed.rds")

labels <- var_label(dta_orgl)


# Demographics -------------------------------------------------------------
dta <- dta_orgl

dta <- dta %>% 
  mutate(country = factor(country, 
                          levels = c("EL", "ES-ES", "HR", "PL", "RO"),
                          labels = c("Cyprus", "Spain", "Croatia", "Poland", "Romania"))
  ) 

# 1 RN stated -1 as experience
dta <- dta %>% 
  mutate(
    across(c(experience, experience_icu), abs)
  )

# Section A ---------------------------------------------------------------

# fix coding values by Qualtrics
dta <- dta %>% 
  mutate(
    aware_hwes = recode(aware_hwes, '23' = 1, '24' = 0),
    aware_hwes = ifelse(is.na(aware_hwes), 0, aware_hwes)
  ) %>% 
  mutate(
    across(c(satisfied_nurse, advice_nurse),
           ~ifelse(is.na(.) , round(median(., na.rm = TRUE), 0), .)
    )
  ) %>% 
  mutate(
    quality_care = na_if(quality_care, 99),
    quality_care = ifelse(is.na(quality_care) , 
                          round(median(quality_care, na.rm = TRUE), 0), 
                          quality_care)
  ) %>% 
  mutate(
    across(c(implement_hwes_unit, implement_hwes_org),
           ~ ifelse(is.na(.), 17, .)
    )
  ) %>% 
  copy_labels_from(dta) %>% 
  set_value_labels(
    aware_hwes = c("Yes" = 1, "No" = 0)
  )



# Section B ---------------------------------------------------------------

# Ratings are scored 1,2,4,5. need to do it 1,2,3,4 

dta <- dta %>% 
  mutate(
    across(all_of(c(vars_hwes_org, vars_hwes_unit)),
           ~ ifelse(. %in% c(4:5), . -1, .)
    )
  ) %>% 
  mutate(
    across(
      all_of(c(vars_hwes_org, vars_hwes_unit)),
      ~ labelled(., labels = c("Strongly agree" = 1,
                               "Agree" = 2,
                               "Disagree" = 3,
                               "Strongly disagree" = 4)
      )
    )
  )


# Section C ---------------------------------------------------------------


## C1,C2,C3 ----------------------------------------------------------------------

# How would you rate the quality of communication in your unit among the following?
# How would you rate the quality of collaboration in your unit among the following?
# In your unit how would you rate the respect for nurses by each of the following?


dta %>% count(collab_nurses)
dta %>% count(commun_nurses)
dta %>% count(respect_by_nurses)

# Fix. All these variables were coded 6 to 9 in the SPSS
# DO it in 1 to 4

dta <- dta %>% 
  mutate(
    across(all_of(c(vars_collaboration, vars_communication, vars_respect_by)), ~ .-5)
  ) %>% 
  mutate(
    across(all_of(c(vars_collaboration, 
                    vars_communication, 
                    vars_respect_by)
    ), 
    ~ labelled(., labels = c(Excellent = 1,
                             Good = 2,
                             Fair = 3,
                             Poor = 4)))
  ) 


dta %>% 
  filter(section == "Section C") %>% 
  select(all_of(c(vars_collaboration, vars_communication, vars_respect_by))) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarise(median = median(value, na.rm = TRUE),
            n = n(),
            n_miss = sum(is.na(value))
  )

# Very few missing. Replace them with median

dta <- dta %>% 
  mutate(
    across(all_of(c(vars_collaboration, vars_communication, vars_respect_by)),
           ~ifelse(section == "Section C" & is.na(.), round(median(., na.rm = TRUE), 0), .)
    )
  ) %>%
  copy_labels_from(dta)

## C4, C5 Moral Distress, Right Staffing -----------------------------------

dta %>% 
  filter(section == "Section C") %>% 
  #count(moral_distress)
  count(staffing_right)

# Fix. Few missing . Fill with median  
dta <-     
  dta %>% 
  mutate(
    across(c(staffing_right, moral_distress),
           ~ifelse(section == "Section C" & is.na(.), round(median(., na.rm = TRUE), 0), .)
    )
  ) %>%
  copy_labels_from(dta) 


## C6, C7 - Skills -------------------------------------------------------------

# Very few missing. Replace them with median


dta %>% 
  filter(section == "Section C") %>% 
  select(all_of(c(vars_skills_manager, vars_skills_admin))) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarise(median = median(value, na.rm = TRUE),
            n = n(), 
            n_miss = sum(is.na(value))
  )

dta <- dta %>% 
  mutate(
    across(all_of(c(vars_skills_manager, vars_skills_admin)),
           ~ifelse(section == "Section C" & is.na(.), round(median(., na.rm = TRUE), 0), .)
    )
  ) %>%
  copy_labels_from(dta) 


## C8, C9 ------------------------------------------------------------------
# Does your organization have a zero tolerance policy on verbal abuse? 
# Does your organization have a zero tolerance policy on physical abuse?

dta %>% 
  filter(section == "Section C") %>% 
  select(starts_with("tolerance"), country) %>% 
  #mutate_all(as_factor) %>% 
  count(country, tolerance_verbal)

# Why missing in this questions? Perhaps they didn't know?

dta <- dta %>% 
  mutate(
    across(c(tolerance_verbal, tolerance_physical),
           ~ifelse(section == "Section C" & is.na(.), 3, .)
    )
  ) %>%
  # put labels back on
  mutate(
    across(c(tolerance_verbal, tolerance_physical),
           ~labelled(., labels = c("Yes" = 1, "No" = 2, "Don't know" = 3))
    )
  )


## C10 - Experiences -------------------------------------------------------

# What are the main factors that keep you working in your current organization?

# Fix 1. Selection of the factor is indicated with 1 and no selection with NA
# Turn to 1 and 0
dta <- dta %>% 
  mutate(across(all_of(vars_factors_keep_work), 
                ~ifelse(section == "Section C" & is.na(.), 0, .))) %>% 
  copy_labels_from(dta)


## C11 - Experiences -------------------------------------------------------

# In the past year, in your work as a nurse, please indicate the number of 
# times you personally experienced each of the following?

# as.numeric() will keep the numbers and make NA all the texts they input

# This was answered by everyone who concluded Section C. 
# So, needs to be 0 for those who answered here [Section C only] 
# and NA for the others

# eg.
parse_number(dta$harass_patients)

get_parsing_problems <- function(var){
  res <- parse_number(dta[[var]])
  attr(res, "problems")
}

get_parsing_problems("harass_manager")

problems <- map(vars_incident_all %>% set_names, get_parsing_problems) 

issues <- 
  map(problems, ~ .[["actual"]]) %>% enframe("variable", "value") %>% 
  unnest(cols = c(value)) %>% 
  count(value, sort = TRUE)

#writexl::write_xlsx(issues, "data/temp_parsing_issues.xlsx")
issues <- readxl::read_xlsx("data/parsing_issues.xlsx")

# New = Old style of vector
recode_scheme <- issues %>% 
  select(incidents, value) %>% 
  deframe()


# 1. recode the text answers to numbers using the scheme
# 2. character vector needed (the fct_recode turns it into a factor)
# 3. Purse numbers
dta <- dta %>% 
  mutate(
    # Before parsing numbers
    # if the user noted x, or X then will be considered as 1 incident
    across(
      all_of(vars_incident_all), ~ifelse(section == "Section C" & . %in% c("x" ,"X"), "1", .)
    )
  )%>%
  mutate(across(all_of(vars_incident_all), 
                function(x) {
                  
                  step1 <- fct_recode(x, !!!recode_scheme) 
                  step2 <- as.character(step1)
                  
                  readr::parse_number(step2)
                  
                })
  ) %>%
  mutate(
    across(
      all_of(vars_incident_all), ~if_else(is.na(.) & section == "Section C", 0, .)
    )
  )

# for id == "R_O9zjHk5qlOXHGTv"
dta %>% filter(discrim_physician == 100000) %>% pull(id)
# we have 100,000 incidents.

dta <- dta %>% 
  mutate(
    across(c(discrim_physician, abuse_verb_physician), ~if_else(. == 100000, 100, .))
  )

# Any harassment r abuse? If count is > 0
dta <- dta %>% 
  rowwise() %>% 
  mutate(
    incident_harass     = as.numeric(sum(c_across(all_of(vars_incident_harass))) > 0),
    incident_discrim    = as.numeric(sum(c_across(all_of(vars_incident_discrim))) > 0),
    incident_abuse_verb = as.numeric(sum(c_across(all_of(vars_incident_abuse_verbal))) > 0),
    incident_abuse_phys = as.numeric(sum(c_across(all_of(vars_incident_abuse_physical))) > 0),
    
    incident_any        = as.numeric(sum(c_across(all_of(vars_incident_all))) > 0)
  ) %>% 
  ungroup()


incident_labels <- list(
  
  incident_harass = "At least one incident of Harassment",
  incident_discrim = "At least one incident of Discrimination",
  incident_abuse_verb = "At least one incident of Verbal Abuse",
  incident_abuse_phys = "At least one incident of Physical Abuse",
  incident_any = "At least one incident (Harassment/ Discrimination/ Verbal or Physical Abuse)"
  
)

var_label(dta) <- incident_labels


# n = 1104 for Section C

# Fix 1. any incident and NA n reported, then reported = NO, none of them
dta %>% 
  filter(section == "Section C") %>% 
  count(incident_any, reported_incident) 

dta <- dta %>% 
  mutate(
    reported_incident = ifelse(incident_any == 1 & is.na(reported_incident), 0, reported_incident)
  ) %>%
  labelled::copy_labels_from(dta) 

# Fix 2
# 215 responses where no incident, but answered whether they reported it
dta %>% 
  filter(section == "Section C") %>% 
  count(incident_any, reported_incident) 

## C12 ---------------------------------------------------------------------

dta %>% 
  filter(section == "Section C") %>% 
  count(reported_incident, report_after)



## C13, C14 ---------------------------------------------------------------------
# fix the values for value_health_safety
dta %>% 
  filter(section == "Section C") %>% 
  #count(recognition_meaningful) %>% 
  count(value_health_safety) %>% 
  {.}

dta <- dta %>% 
  mutate(
    value_health_safety = recode(value_health_safety, "1" = 1, "4" = 2, "5" = 3, "6" = 4),
    value_health_safety = ifelse(is.na(value_health_safety) & section == "Section C", 
                                 median(value_health_safety, na.rm = T), 
                                 value_health_safety),
    # add labels now
    value_health_safety = labelled(value_health_safety, 
                                   labels = c('Strongly agree' = 1,
                                              'Agree' = 2,
                                              'Disagree' = 3,
                                              'Strongly disagree' = 4))
  )

## C15 ---------------------------------------------------------------------
# At the end of a typical shift, to what degree do you get the following work done?


dta %>% count(work_done_1)

dta %>% 
  filter(section == "Section C") %>% 
  select(all_of(c(vars_work_done))) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarise(median = median(value, na.rm = TRUE),
            n = n(),
            n_miss = sum(is.na(value))
  )



## C16, C17, C18 ----------------------------------------------------------------------


dta %>% 
  filter(section == "Section C") %>% 
  count(satisfied_overall)


# Replace missing only for satisfied overall
dta <- dta %>% 
  mutate(
    # C16
    satisfied_overall = recode(satisfied_overall, "1" = 1, "4" = 2, "5" = 3, "6" = 4),
    satisfied_overall = ifelse(is.na(satisfied_overall) & section == "Section C", 
                               median(satisfied_overall, na.rm = T), 
                               satisfied_overall),
    # add labels now
    satisfied_overall = labelled(satisfied_overall, 
                                 labels = c('Very satisfied' = 1,
                                            'Somewhat satisfied' = 2,
                                            'Somewhat dissatisfied' = 3,
                                            'Very dissatisfied' = 4))
  ) %>% 
  # C17
  mutate(
    evaluate_contrib_hwe = recode(evaluate_contrib_hwe, "1" = 1, "4" = 2, "5" = 3, "6" = 4),
    evaluate_contrib_hwe = labelled(evaluate_contrib_hwe, 
                                    labels = c('Plays a major part in my evaluation' = 1,
                                               'Plays a substantial part in my evaluation' = 2,
                                               'Plays a minor part in my evaluation' = 3,
                                               'Plays no part in my evaluation' = 4))
  )  %>% 
  # C18
  
  mutate(
    plan_leave = recode(plan_leave, "1" = 2, "4" = 1, "5" = 3),
    plan_leave = ifelse(is.na(plan_leave) & section == "Section C", 3, plan_leave),
    plan_leave = labelled(plan_leave, labels = c(
      'Yes, within the next 3 years' = 1,
      'Yes, within the next 12 months' = 2,
      'No plans to leave within the next 3 years' = 3
    ))
  )  


## C18 -------------------------------------------------------------------

# lots of missing values

dta %>% 
  filter(section == "Section C") %>% 
  select(all_of(c(vars_influence))) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarise(median = median(value, na.rm = TRUE),
            n = n(),
            n_miss = sum(is.na(value))
  )

dta %>% 
  filter(section == "Section C") %>% 
  count(plan_leave, influence_2)

dta %>% 
  filter(section == "Section C") %>%
  filter(text_influence_other != "") %>% 
  count(country, text_influence_other) %>% 
  # writexl::write_xlsx("data/temp_C8_influence_to_leave.xlsx") %>% 
  {.}

# Many missing. Do not replace!! 
# TODO Remember to use the fct_explicit_na when plotting and for counting %


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
dta_rt <-  dta %>% 
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
dta_rt %>% 
  filter(test == "retest") %>% 
  count(country)


retest_ids <- dta_rt %>% 
  filter(test == "retest") %>% 
  pull(id)

test_ids <- dta_rt %>% 
  filter(test == "test") %>% 
  pull(id)


dta_retest <- 
  dta %>% 
  filter(id %in% dta_rt$id) %>% 
  left_join(
    dta_rt[c("id", "id_comb", "test")]
    , by= "id"
  )

var_label(dta_retest) <- labels
# remove the retests for the main dataset
dta <-  filter(dta, !id %in% retest_ids)

var_label(dta) %>% keep(is.null)



# Create AACN Dimensions ----------------------------------------------------

dta <- dta %>% 
  mutate(
    skilled_communication_unit  = score(vars_hwes_unit[1:2]),
    true_collaboration_unit     = score(vars_hwes_unit[3:5]),
    effective_decison_unit      = score(vars_hwes_unit[6:9]),
    appropriate_stuffing_unit   = score(vars_hwes_unit[10:11]),
    meaningful_recognition_unit = score(vars_hwes_unit[12:13]),
    authentic_leadership_unit   = score(vars_hwes_unit[14:16])
  ) %>% 
  mutate(
    skilled_communication_org  = score(vars_hwes_org[1:2]),
    true_collaboration_org     = score(vars_hwes_org[3:5]),
    effective_decison_org      = score(vars_hwes_org[6:9]),
    appropriate_stuffing_org   = score(vars_hwes_org[10:11]),
    meaningful_recognition_org = score(vars_hwes_org[12:13]),
    authentic_leadership_org   = score(vars_hwes_org[14:16])
  ) 

var_label(dta) <- scale_labels

# Fix labels again --------------------------------------------------------

# Mutates remove the labels from the variables

var_label(dta) <- labels

# Do all have labels?
var_label(dta) %>% keep(is.null)
var_label(dta_retest) %>% keep(is.null)

# Missing values ----
library(naniar)

secA <- dta %>% 
  select(country, id, section, all_of(vars_sectionA)) %>% 
  add_n_miss(label = "SECTION_A") %>% 
  select(id, section, ends_with("_all"))

secB <- dta %>% 
  select(country,id, section, all_of(vars_sectionB)) %>% 
  add_n_miss(label = "SECTION_B") %>% 
  select(id, section, ends_with("_all"))


# Sec C
temp <- dta %>% 
  relocate(country,id, section) %>% 
  select(-all_of(c(vars_sectionA, vars_sectionB))) %>% 
  select(-c(StartDate:date), -c(browser_language:part4)) %>% 
  select(-ends_with("other")) %>% 
  select(-starts_with("text_")) %>% 
  select(-ends_with("_after")) %>% 
  select(-all_of(c(vars_demo, "icu_type_2", "with_demo",
                   "institution_name", "residence", #"id_part", 
                   "no_report_why"
  )
  )) 

n_cols_C <- ncol(temp) - 2

secC <- temp %>% 
  #select(id, section, all_of(vars_sectionC)) %>% 
  add_n_miss(label = "SECTION_C") %>% 
  select(country,id, section, ends_with("_all"))

final <- reduce(
  list(secA, secB, secC), left_join
) 
# %>% 
#   replace_na(
#     list(
#       SECTION_A_all = length(vars_sectionA),
#       SECTION_B_all = length(vars_sectionB),
#       SECTION_C_all = n_cols_C
#       )
#   )

writexl::write_xlsx(final, "missing_valuesImputed1.xlsx")

# Save as RDS file

saveRDS(dta, "data/raw_cleaned.rds")
saveRDS(dta_retest, "data/temp_dta_retest.rds")
