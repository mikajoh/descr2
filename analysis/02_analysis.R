## Mikael Poul Johannesson (mikajoh@gmail.com)
## 2018

## Start matter ------------------------------------------------------

library(here)
library(tidyverse)
library(ggstance)
library(devtools)
library(secret)

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
## Remeber to set the path to the private key by:
## Sys.setenv(USER_KEY = "path/to/private/key")
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

## SUB ONLY ----------------------------------------------------------

res_sub <-
  eips %>%
  amce(
    cnd_post, cnd_age, cnd_gender, cnd_edu, cnd_religion, cnd_class,
    subgroup = "exp_treat",
    cluster = "rsp_id"
  ) %>%
  add_labels()

fig_sub <-
  res_sub %>%
  plot_sub_by(".")
fig_sub

ggsave2(
  fig_sub,
  "fig_both",
  width = 4.5,
  height = 4.7
)

## SUB by SUB WITH DIFF ----------------------------------------------

res_sub_all_sub <-
  eips %>%
  amce(
    cnd_post, cnd_age, cnd_gender, cnd_edu, cnd_religion, cnd_class,
    subgroup = "exp_treat",
    cluster = "rsp_id"
  )

res_sub_all_diff <-
  eips %>%
  amce(
    cnd_post, cnd_age, cnd_gender, cnd_edu, cnd_religion, cnd_class,
    diff = "exp_treat",
    cluster = "rsp_id"
  ) %>%
  mutate(exp_treat = "Difference")

res_sub_all <-
  bind_rows(res_sub_all_sub, res_sub_all_diff) %>%
  add_labels() %>%
  mutate(exp_treat = lvls_reorder(exp_treat, c(1, 3, 2)))

fig_sub_all <-
  res_sub_all %>%
    ggplot(aes(
      x = estimate, y = value,
      xmin = estimate - (1.96 * std_error),
      xmax = estimate + (1.96 * std_error))) +
    facet_grid(
      treatment ~ exp_treat,
      scales = "free_y",
      space = "free_y") +
    ggstance::geom_errorbarh(
      width = 0,
      position = position_dodgev(.75)) +
    geom_point(position = position_dodgev(.75)) +
    geom_vline(aes(xintercept = 0), linetype = "dotted") +
    scale_x_continuous(
      limits = c(-.4, .4),
      breaks = round(seq(-.4, .4, .2), 2),
      expand = c(0, 0),
      labels = function(x) x * 100) +
    scale_y_discrete(
      labels = function(x) parse(text = as.character(x))) +
    labs(
      x = "Marginal effect, choosing candidate (%)",
      y = "Candidate attributes"
    ) +
    theme_m() 
fig_sub_all

ggsave2(
  fig_sub_all,
  "fig_both_full",
  width = 9.5,
  height = 4.7
)

## SUB by COUNTRY ----------------------------------------------------

res_sub_cntry <-
  eips %>%
  amce(
    cnd_post, cnd_age, cnd_gender, cnd_edu, cnd_religion, cnd_class,
    subgroup = c("rsp_country", "exp_treat"),
    cluster = "rsp_id"
  ) %>%
  add_labels()

fig_sub_cntry <-
  res_sub_cntry %>%
  plot_sub_by("rsp_country")
fig_sub_cntry

ggsave2(
  fig_sub_cntry,
  "fig_both_by_country",
  width = 14.5,
  height = 5
)

## SUB by AGE --------------------------------------------------------

res_sub_age <-
  eips %>%
  amce(
    cnd_post, cnd_age, cnd_gender, cnd_edu, cnd_religion, cnd_class,
    subgroup = c("rsp_age_cat_3", "exp_treat"),
    cluster = "rsp_id"
  ) %>%
  add_labels()

fig_sub_age <-
  res_sub_age %>%
  plot_sub_by("rsp_age_cat_3")

ggsave2(
  fig_sub_age,
  "fig_both_by_age",
  width = 9.5,
  height = 5
)

## SUB by GENDER -----------------------------------------------------

res_sub_gnd <-
  eips %>%
  amce(
    cnd_post, cnd_age, cnd_gender, cnd_edu, cnd_religion, cnd_class,
    subgroup = c("rsp_gender", "exp_treat"),
    cluster = "rsp_id"
  ) %>%
  add_labels()

fig_sub_gnd <-
  res_sub_gnd %>%
  plot_sub_by("rsp_gender")
fig_sub_gnd

ggsave2(
  fig_sub_gnd,
  "fig_both_by_gender",
  width = 7,
  height = 5
)

## SUB by EDUCATION --------------------------------------------------

res_sub_edu <-
  eips %>%
  amce(
    cnd_post, cnd_age, cnd_gender, cnd_edu, cnd_religion, cnd_class,
    subgroup = c("rsp_edu_2", "exp_treat"),
    cluster = "rsp_id"
  ) %>%
  add_labels()

fig_sub_edu <-
  res_sub_edu %>%
  plot_sub_by("rsp_edu_2")
fig_sub_edu

ggsave2(
  fig_sub_edu,
  "fig_both_by_edu",
  width = 7,
  height = 5
)

## DIFF ONLY ---------------------------------------------------------

res_diff <-
  eips %>%  
  amce(cnd_post, cnd_age, cnd_gender, cnd_edu, cnd_religion, cnd_class,
       diff = "exp_treat",
       cluster = "rsp_id") %>%
  add_labels()

fig_diff_all <-
  res_diff %>%
  ggplot(aes(
    x = estimate, y = value,
    xmin = estimate - (1.96 * std_error),
    xmax = estimate + (1.96 * std_error))) +
  facet_grid(
    treatment ~ .,
    scales = "free_y",
    space = "free_y") +
  ggstance::geom_errorbarh(
    width = 0,
    position = position_dodgev(.75)) +
  geom_point(position = position_dodgev(.75)) +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  scale_x_continuous(
    limits = c(-.4, .4),
    breaks = round(seq(-.4, .4, .2), 2),
    expand = c(0, 0),
    labels = function(x) x * 100) +
  scale_y_discrete(
    labels = function(x) parse(text = as.character(x))) +
  labs(
    x = paste0(
      "Difference in Marginal Effect, choosing candidate (%)\n",
      "(Closest - Prefered)"),
    y = "Candidate attributes"
  ) +
  theme_m()
fig_diff_all

ggsave2(
  fig_diff_all,
  "fig_diff",
  width = 4.5,
  height = 4.7
)

## DIFF by COUNTRY ---------------------------------------------------

res_diff_cntry <-
  eips %>%  
  amce(
    cnd_post, cnd_age, cnd_gender, cnd_edu, cnd_religion, cnd_class,
    subgroup = "rsp_country",
    diff = "exp_treat",
    cluster = "rsp_id"
  ) %>%
  add_labels()

fig_diff_cntry <-
  res_diff_cntry %>%
  plot_diff_by("rsp_country")
fig_diff_cntry

ggsave2(
  fig_diff_cntry,
  "fig_diff_by_country",
  width = 14.5,
  height = 4.7
)

## DIFF by AGE -------------------------------------------------------

res_diff_age <-
  eips %>%
  amce(
    cnd_post, cnd_age, cnd_gender, cnd_edu, cnd_religion, cnd_class,
    subgroup = "rsp_age_cat_3",
    diff = "exp_treat",
    cluster = "rsp_id"
  ) %>%
  add_labels()

fig_diff_age <-
  res_diff_age %>%
  plot_diff_by("rsp_age_cat_3")
fig_diff_age

ggsave2(
  fig_diff_age,
  "fig_diff_by_age",
  width = 9.5,
  height = 4.7
)


## DIFF by GENDER ----------------------------------------------------

res_diff_gnd <-
  eips %>%
  amce(
    cnd_post, cnd_age, cnd_gender, cnd_edu, cnd_religion, cnd_class,
    subgroup = "rsp_gender",
    diff = "exp_treat",
    cluster = "rsp_id"
  ) %>%
  add_labels()

fig_diff_gnd <-
  res_diff_gnd %>%
  plot_diff_by("rsp_gender")
fig_diff_gnd

ggsave2(
  fig_diff_gnd,
  "fig_diff_by_gender",
  width = 7,
  height = 4.7
)

## DIFF by EDUCATION -------------------------------------------------

res_diff_edu <-
  eips %>%
  amce(
    cnd_post, cnd_age, cnd_gender, cnd_edu, cnd_religion, cnd_class,
    subgroup = "rsp_edu_2",
    diff = "exp_treat",
    cluster = "rsp_id"
  ) %>%
  add_labels()

fig_diff_edu <-
  res_diff_edu %>%
  plot_diff_by("rsp_edu_2")
fig_diff_edu

ggsave2(
  fig_diff_edu,
  "fig_diff_by_edu",
  width = 7,
  height = 4.7
)

## DIFF by COUNTRY and AGE ----------------------------------------

res_diff_cntry_age <-
  eips %>%
  amce(
    cnd_post, cnd_age, cnd_gender, cnd_edu, cnd_religion, cnd_class,
    subgroup = c("rsp_country", "rsp_age_cat_3"),
    diff = "exp_treat",
    cluster = "rsp_id"
  ) %>%
  add_labels()

fig_diff_cntry_age <-
  res_diff_cntry_age %>%
  plot_diff_cntry_by("rsp_age_cat_3", "Respondents age")
fig_diff_cntry_age

ggsave2(
  fig_diff_cntry_age,
  "fig_diff_by_country_and_age",
  width = 14.5,
  height = 4.7
)

## DIFF by COUNTRY and GENDER ----------------------------------------

res_diff_cntry_gnd <-
  eips %>%
  amce(
    cnd_post, cnd_age, cnd_gender, cnd_edu, cnd_religion, cnd_class,
    subgroup = c("rsp_country", "rsp_gender"),
    diff = "exp_treat",
    cluster = "rsp_id"
  ) %>%
  add_labels()

fig_diff_cntry_gnd <-
  res_diff_cntry_gnd %>%
  plot_diff_cntry_by("rsp_gender", "Respondents gender")
fig_diff_cntry_gnd

ggsave2(
  fig_diff_cntry_gnd,
  "fig_diff_by_country_and_gnd",
  width = 14.5,
  height = 4.7
)

## DIFF by COUNTRY and EDUCATION -------------------------------------

res_diff_cntry_edu <-
  eips %>%
  amce(
    cnd_post, cnd_age, cnd_gender, cnd_edu, cnd_religion, cnd_class,
    subgroup = c("rsp_country", "rsp_edu_2"),
    diff = "exp_treat",
    cluster = "rsp_id"
  ) %>%
  add_labels()

fig_diff_cntry_edu <-
  res_diff_cntry_edu %>%
  plot_diff_cntry_by("rsp_edu_2", "Respondents education")
fig_diff_cntry_edu

ggsave2(
  fig_diff_cntry_edu,
  "fig_diff_by_country_and_edu",
  width = 14.5,
  height = 4.7
)
