# This script contains helper functions to enable table creation


#' Sets the Greek locale 
set_greek_locale <- function(){
  Sys.setlocale(locale = "greek")
  
}

#' Score likert scales items
#' 
#' @details So many thanks to Mattan! (thanks dude)
#' https://gist.github.com/mattansb/eb7d5dbe92e129f462c2f1288efd1615
#' 
#' @param ... Variables to score. Using tidyselect way
score <- function(...,  Means = TRUE, na.rm =  TRUE){
  if(Means){
    rowMeans(dplyr::across(...), na.rm = na.rm)
  } else {
    rowSums(dplyr::across(...), na.rm = na.rm)
  }
}

#' Create wide table of frequencies (%) . SPSS like 
#' 
#' Supply a dataset with likert scale items and get a `wide`table with the items as rows
#' and the likert respones as columns.
#' 
#' @details Make sure the dataset supplied is only the columns you want.
#' Similar usage to \code{tbl_summary} function of the \code{gtsummary} package
#' @param data The dataset
#' @param ... Other arguments passed to \code{tbl_summary} function
aggregate_wide <- function(data, ...){
  
  
  gt_table <- 
    data %>% 
    tbl_summary(...) 
  
  var_labels <- gt_table$meta_data$var_label
  
  miss <- gt_table$inputs$missing
  miss_text <- gt_table$inputs$missing_text
  
  wide_table <- 
    gt_table %>% 
    as_tibble() %>% 
    set_names("name","values") %>% 
    mutate(
      item  = if_else(name %in% var_labels,  name, NA_character_)
    ) %>% 
    fill(item) %>% 
    na.omit() %>% 
    mutate(name = fct_inorder(name),
           item = fct_inorder(item)) %>% 
    spread(name, values) %>% 
    mutate(item  = stringr::str_squish(item)) %>% 
    relocate(item)
  
  wide_table
  
  # TODO relocate does not work if "Unknown" (missing_text) is present
  # I think now its done .. :)
}



#' Create a cross correlation table between two groups of variables
#' 
#' This function takes two groups of variables and creates cross table of correlations
#' with the one group on the columns, and the other group on the rows. 
#' 
#' It supports \code{tidyselect} selection of variables
#' https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html
#' 
#' @param .data The dataset. A dataframe or a tibble
#' @param .colsx <tidy-select> Columns - This is the first group of columns
#' @param .colsx <tidy-select> Columns - This is the second group of columns
#' @param .method String length 1. The correlation method. One of c("pearson", "kendall", "spearman")
#' @param .digits Scalar. The number of digits for the correlation coefficient

tbl_cross_correlation <- function(.data, .colsx, .colsy, .method = "pearson", .digits = 2){
  
  stopifnot(inherits(.data, "data.frame"))
  stopifnot(length(.digits)==1 | is.integer(.digits))
  .method <- match.arg(.method, c("pearson", "kendall", "spearman"))
  
  exprX <- rlang::enquo(.colsx)
  exprY <- rlang::enquo(.colsy)
  
  posX <- tidyselect::eval_select(exprX, data = .data)
  posY <- tidyselect::eval_select(exprY, data = .data)
  
  
  if (length(posX) == 0) {
    rlang::abort(glue::glue("`.colsx` must select at least one column."))
  }
  if (length(posY) == 0) {
    rlang::abort(glue::glue("`.colsy` must select at least one column."))
  }
  
  
  poss <- c(posX, posY)
  
  # this also renames of you pas smthng like .colsx = c(New_name = old_name)
  dta <- rlang::set_names(.data[poss], names(poss))
  
  # Get labels (if any)
  lab_list <- dta %>% 
    lapply(function(x) attr(x, "label", exact = TRUE)) %>% 
    discard(is.null)
  
  
  out <- 
    list(colsx = names(posX), colsy = names(posY)) %>% 
    purrr::cross_df() %>% 
    mutate(
      cor_test = purrr::map2(colsx, colsy, ~ cor.test(dta[[.x]], dta[[.y]]), method = .method),
      r = purrr::map_dbl(cor_test, ~ .$estimate),
      p = purrr::map_dbl(cor_test, ~ .$p.value)
    ) 
  
  if(!purrr::is_empty(lab_list)){
    
    out <- out %>% 
      mutate(
        colsx = recode(colsx, !!!unlist(lab_list)),
        colsy = recode(colsy, !!!unlist(lab_list))
      )
  }
  
  
  out %>% 
    mutate(r = round(r, .digits)) %>% 
    mutate(sig = case_when(
      p <0.01 ~ "**",
      p <0.05 ~ "*",
      TRUE    ~ ""
    ),
    r = paste0(r, sig)) %>% 
    select(colsx, colsy, r) %>% 
    tidyr::pivot_wider(
      names_from = colsy,
      values_from = r
    )
  
}


#' Descriptives of CONTINUOUS variables by a factor
#' 
#' @param dta The dataset
#' @param group_var The variable to group_by. 
#' @param ... The variables the get the descriptives. You can use dplyr style for this
descr_by_group <- function(dta, group_var, ..., char_label = "") {
  
  dta  %>% 
    # TODO Use the `all_of` when using objects that contain columns in a character format
    select(
      {{group_var}}, ...
    ) %>%
    tbl_summary(
      by = {{group_var}},
      missing = "no"
      , type = list(all_continuous() ~ "continuous")
      #digits = all_continuous() ~ c(1, 1)
    ) %>% 
    add_overall(col_label = "**Συνολικά**, N = {N}") %>% 
    add_p(test = list(all_categorical() ~ "chisq.test"
                      #all_continuous() ~ "t.test"
    )
    ) %>% 
    modify_header(label = char_label) %>% 
    #as_tibble() %>% 
    {.}
  
}


#descriptives function---------------------------------------------------------

#' @param dta The dataset
#' 
descriptives_fun <- function(dta, dim){
  dta %>%
    select(all_of(dim)) %>% 
    pivot_longer(
      cols = everything(),
      names_to = "Scale",
      values_to = "value"
      
    ) %>%
    mutate(value = round(value, 1)) %>% 
    group_by(Scale) %>%
    summarise(`Mean (SD)` = paste(round(mean(value),digits = 1)," (",
                                  round(sd(value),digits = 1),")",sep = ""),
              ObservedRange=paste("[",range(value)[1],"-",range(value)[2],"]",sep = "")) %>%
    arrange(factor(Scale, levels = dim))
}

