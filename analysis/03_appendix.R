## Mikael Poul Johannesson (mikajoh@gmail.com)
## 2018

## Start matter ------------------------------------------------------

library(here)
library(tidyverse)
library(ggstance)
library(devtools)
library(secret)
library(huxtable)

## Contains functions for AMCE estimation as well as various utils
## functions for making the figures. See for instance
## `?descr2utils::amce`.
if (!require(descr2)) {
  devtools::install_github("mikajoh/descr2")
}

## Get data ----------------------------------------------------------

## ## The prepared and combined EIPS data.
## ## Md5sum: c1e3c33270986d8b0804f7f939c6277e
## ## tools::md5sum(here("data", "eips.csv"))
## eips_raw <- read.csv(
##   file = here("data", "eips.csv"),
##   stringsAsFactors = FALSE
## )

## The prepared and combined EIPS data.
## Contact Mikael (mikael.johannesson@uib.no) to enquire about a key.
## Md5sum: c1e3c33270986d8b0804f7f939c6277e
## tools::md5sum(here("data", "eips.csv"))
eips_raw <- get_secret(
  name = "eips_raw",
  vault = here("vault")
)

## We want the treatments in a certain order for the figures.
eips <-
  eips_raw %>%
  mutate(
    rsp_gender = fct_rev(rsp_gender),
    rsp_edu = fct_rev(rsp_edu),
    cnd_age = factor(cnd_age),
    cnd_edu = factor(cnd_edu),
    cnd_gender = fct_rev(cnd_gender),
    cnd_religion = lvls_reorder(cnd_religion, c(3, 5, 1, 4, 2)),
    cnd_class = lvls_reorder(cnd_class, c(3, 1, 2))
  )


## Treatment Assignments ---------------------------------------------



## Descriptive Statistics --------------------------------------------


## To see the latex pkgs needed, run
## report_latex_dependencies()

var_labels <- tribble(
  ~var,            ~variable,
  "rsp_country",   "Country",
  "rsp_gender",    "Gender",
  "rsp_age_cat_3", "Age Category",
  "rsp_edu",       "Education"
)

descr_cntry <-
  eips %>%
  filter(!duplicated(rsp_id)) %>%
  gather(var, value, rsp_gender, rsp_age_cat_3, rsp_edu) %>%
  left_join(var_labels, by = "var") %>%
  group_by(rsp_country, variable) %>%
  mutate(n_tot = n()) %>%
  group_by(rsp_country, variable, value) %>%
  summarize(
    n = n(),
    prop = round(n / n_tot[1], 2)
  ) %>%
  ungroup() %>%  
  gather(type, res, n, prop) %>%
  filter(!is.na(res)) %>%
  mutate(Country = paste(rsp_country, type)) %>%
  select(-type, -rsp_country) %>%
  spread(Country, res) %>%
  mutate(value = ifelse(is.na(value), "(n.a.)", value))

first_row <-
  names(descr_cntry) %>%
  str_replace(" n| prop|variable|value", "")
first_row <- ifelse(duplicated(first_row), "", first_row)
second_row <-
  names(descr_cntry) %>%
  str_replace("[A-Z][a-z]+", "") %>%
  str_trim() %>%
  str_replace("variable", "Variable") %>%
  str_replace("value", "Value") %>%
  str_replace("n", "N") %>%
  str_replace("prop", "%")

tbl_descr_01 <-
  descr_cntry %>%
  set_names(first_row) %>%  
  as_hux() %>%
  add_colnames() %>%
  insert_row(second_row, after = 1) %>%
  set_colspan(1, c(1, 3, 5, 7, 9, 11), 2)

tbl_descr_02 <-
  tbl_descr_01 %>%
  set_top_border(1, 1:ncol(.), 1) %>%
  set_bottom_border(2, 1:ncol(.), .75) %>%
  set_bottom_border(nrow(.), 1:ncol(.), 1)

tbl_descr_03 <-
  tbl_descr_02 %>%
  set_number_format(3:nrow(.), c(3, 5, 7, 9, 11), 0) %>%
  set_number_format(3:nrow(.), c(4, 6, 8, 10, 12), 2) %>%
  set_align(1, 1:ncol(.), "center") %>%
  set_align(1:nrow(.), 3:ncol(.), "right") %>%
  set_all_padding(1:nrow(.), 1:ncol(.), -2.5) %>%
  set_top_padding(c(2, 6, 10) + 1, 1:ncol(.), 12.5) %>%
  set_font_size(1:3,1:ncol(.), 8) %>%
  set_font_size(3:nrow(.),1:ncol(.), 8) %>%
  set_col_width(3:ncol(.), .04)

tbl_descr_04 <-
  tbl_descr_03 %>%    
  set_latex_float("h") %>%
  set_width(1) %>%
  set_position("center") %>%
  set_caption("Descriptive statistics for each country.") %>%
  set_caption_pos("bottom") %>%
  set_label("tbl_descr_cntry") %>%
  add_footnote(
    paste(
      "Note:"
    ),
    font_size = 7, top_padding = 0
  )

tbl_descr <- tbl_descr_04

tbl_descr %>%
  to_latex() %>%
  write_lines(here("output", "tbls", "tbl_descr_cntry.tex"))
