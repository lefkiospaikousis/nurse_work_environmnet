# This script holds dimension names, labels, and other global values
# that will be needed in the analysis or plotting


# Sys.setlocale("LC_CTYPE", locale = "Greek")

# Variables ---------------------------------------------------------------

vars_demo <- 
  c("gender", "age", "uni_education", "education", "training", 
    "experience", "experience_icu", "position", 
    "workplace", "institution_type", "icu_type")

text_vars <- c(
  "position_other", "workplace_other" , "residence", "institution_name"
  , "icu_type_2"
)


# Scales-------------------------------------------------------------------


## Section A ----

vars_sectionA <- c("satisfied_nurse", "advice_nurse", "quality_care", "aware_hwes",
                   
                   "implement_hwes_unit", "implement_hwes_org")

## Section B: Critical Elements of a Healthy Work Environment Scale ----

vars_hwes_org <- paste0("hwes_", 1:16, "_org")   # In your organisation
vars_hwes_unit <- paste0("hwes_", 1:16, "_unit")  # In your work unit

vars_sectionB <- c(vars_hwes_org, vars_hwes_unit)

## Section C ---- 

#' How would you rate the quality of `communication` in your unit among the following?
#' How would you rate the quality of `collaboration` in your unit among the following?

rate_who <- c("nurses", "physicians", "managers", "administration")

vars_communication <- paste0("commun_", rate_who)
vars_collaboration <- paste0("collab_", rate_who)


#' In your unit how would you rate the respect for nurses by each of the following?

respect_by <- c("nurses", "physicians", "colleagues", "managers", "administration")

vars_respect_by <- paste0("respect_by_", respect_by)


#'C6 - Please rate the skill of your unit Nursing managers in the following areas

skills <- c(
  "communication",
  "collaboaration",
  "staff",
  "supplies",
  "decision",
  "recognition",
  "leadership",
  "provision",
  "professional",
  "effectiveness")


vars_skills_manager <- paste0("mangr_", skills)
vars_skills_admin <- paste0("admin_", skills)


#' C10 - What are the main factors that keep you working in your current organization?

factors_keep <- c(
  "people", "salary", "patients", "manager", "reputation", "environment", 
  "support", "staffing", "recognition", "development", "advancement", 
  "location", "schedule", "other")

vars_factors_keep_work <- paste0("fact_", factors_keep)


#' C11
#' In the past year, in your work as a nurse, please indicate the number 
#' of times you personally experienced each of the following?


incident_by_who <- c("patients", "families", "nurse", "physician",
                     "manager", "administration", "other")


vars_incident_harass         <- paste0("harass_", incident_by_who)
vars_incident_discrim        <- paste0("discrim_", incident_by_who)
vars_incident_abuse_verbal   <- paste0("abuse_verb_", incident_by_who)
vars_incident_abuse_physical <- paste0("abuse_phys_", incident_by_who)

vars_incident_all <- c(
  vars_incident_harass, vars_incident_discrim, vars_incident_abuse_verbal,
  vars_incident_abuse_physical
)

#' C15
#' At the end of a typical shift, to what degree do you get the following work done?

vars_work_done <- paste0("work_done_", 1:5)


#' C18 Influence
#' 

vars_influence <- paste0("influence_", c(1:11)) #, "Other"))

# Scales labels-------------------------------------------------------------


scale_labels <- list(
  
  # Unit
  skilled_communication_unit  = "Skilled Communication",
  true_collaboration_unit     = "True Collaboration",
  effective_decison_unit      = "Effective Decision-Making",
  appropriate_stuffing_unit   = "Appropriate Staffing",
  meaningful_recognition_unit = "Meaningful Recognition",
  authentic_leadership_unit   = "Authentic Leadership",
  
  # Organisation
  skilled_communication_org  = "Skilled Communication",
  true_collaboration_org     = "True Collaboration",
  effective_decison_org      = "Effective Decision-Making",
  appropriate_stuffing_org   = "Appropriate Staffing",
  meaningful_recognition_org = "Meaningful Recognition",
  authentic_leadership_org   = "Authentic Leadership"
  
)

# dimensions

dim_scales <- names(scale_labels)

dim__scales_unit <- dim_scales[grepl("_unit$", dim_scales)]
dim__scales_org  <- dim_scales[grepl("_org$", dim_scales)]


# Theme gtsummary

theme_gtsummary <-   
  list(
    "tbl_summary-str:continuous_stat" = "{mean} ({sd})",
    "tbl_summary-arg:digits" = list(all_continuous() ~ c(1, 1)),
    "add_p.tbl_summary-attr:test.continuous_by2" = "t.test",
    "add_p.tbl_summary-attr:test.continuous" = "aov",
    "add_p.tbl_summary-attr:test.categorical" = "chisq.test",
   # "add_p.tbl_summary-attr:test.categorical" = "fisher.test",
    "pkgwide-str:theme_name" = "Improvast Theme"
  )

