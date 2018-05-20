## Mikael Poul Johannesson (mikajoh@gmail.com)
## 2018

## Start matter ------------------------------------------------------

library(here)
library(haven)
library(tidyverse)

## Some sanity tests for the finished data, should not be needed.
run_tests <- TRUE
if (run_tests) library(assertthat)

## Get data ----------------------------------------------------------

## EIPS Iceland.
## Md5sum: 492e6210db4b3fcb047819bdf0bd2955
## tools::md5sum(here("data", "EIPS_is.sav"))
is_raw <- read_sav(here("data", "EIPS_is.sav"))

## EIPS Netherlands.
## Md5sum: 2fcad34e4ddda3c8ff18d17f94878b8a
## tools::md5sum(here("data", "L_PanelCollaboration_wave4_4p_EN.sav"))
nl_raw <- read_sav(here("data", "L_PanelCollaboration_wave4_4p_EN.sav"))

## EIPS Norway.
## Md5sum: 74d22c43548599e69ea50b78f8630ce3
## tools::md5sum(here("data", "Norwegian Citizen Panel - wave 9 EN.sav"))
no_raw <- read_sav(here("data", "Norwegian Citizen Panel - wave 9 EN.sav"))

## EIPS Sweden.
## Md5sum: 2ebae375d080fb29e4d2ed3586144ce3
## tools::md5sum(here("data", "EIPS2017_Swedish_Citizen_Panel_20180112.dta"))
se_raw <- read_dta(here("data", "EIPS2017_Swedish_Citizen_Panel_20180112.dta"))

## EIPS France.
## Md5sum: d910c074786ce467fc4aa9293d1689f1
## tools::md5sum(here("data", "EIPS2017-France.sav"))
fr_raw <- read_sav(here("data", "EIPS2017-France.sav"))

## Prepare Iceland ---------------------------------------------------

is_01 <-
  is_raw %>%
  mutate(
    rsp_id = as.numeric(paste0(100, row_number())),
    rsp_country = "Iceland",
    rsp_gender = case_when(
      kyn == 1 ~ "Male",
      kyn == 2 ~ "Female"),
    rsp_gender = factor(rsp_gender),
    rsp_age = as.numeric(aldur),
    rsp_edu = case_when(
      menntun == 1 ~ "Lower",
      menntun == 2 ~ "Intermediate",
      menntun == 3 ~ "Intermediate",
      menntun == 4 ~ "Higher",
      menntun == 5 ~ "Higher",
      menntun == 6 ~ "Higher",
      menntun == 7 ~ "Higher",
      menntun == 8 ~ "Higher")
  )

is_02 <-
  is_01 %>%
  gather(var, value, matches("EIPS2017G_[12]_\\w\\w\\w.*"), na.rm = TRUE) %>%
  mutate(
    cnd_n = as.numeric(gsub("EIPS2017G_([12])_.*", "\\1", var)),
    var = gsub("EIPS2017G_[12]_", "", var)) %>%
  spread(var, value) %>%
  mutate(
    cnd_age = case_when(
      age == 1 ~ "28",
      age == 2 ~ "45",
      age == 3 ~ "71"),
    cnd_edu = case_when(
      edu == 0 ~ "High school",
      edu == 1 ~ "University level",
      edu == 2 ~ "University level"),
    cnd_gender = case_when(
      gender == 0 ~ "Male",
      gender == 1 ~ "Female"),
    cnd_religion = case_when(
      religion == 1 ~ "No religion",
      religion == 2 ~ "Christian",
      religion == 3 ~ "Non-practicing Christian",
      religion == 4 ~ "Muslim",
      religion == 5 ~ "Non-praciticing Muslim"),
    cnd_class = case_when(
      class == 1 ~ "Working class",
      class == 2 ~ "Middle class",
      class == 3 ~ "Upper class"),
    EIPS2017G_1_dv = ifelse(EIPS2017G_1_dv %in% 96:99, NA, EIPS2017G_1_dv),
    EIPS2017G_2_dv = ifelse(EIPS2017G_2_dv %in% 96:99, NA, EIPS2017G_2_dv),
    exp_post = ifelse(!is.na(EIPS2017G_1_dv), EIPS2017G_1_dv, EIPS2017G_2_dv),
    cnd_post = case_when(
      exp_post == cnd_n ~ 1,
      exp_post != cnd_n ~ 0),
    exp_treat = ifelse(!is.na(EIPS2017G_1_dv), "Closest candidate", "Prefered candidate"),
    exp_treat = ifelse(is.na(EIPS2017G_1_dv) & is.na(EIPS2017G_2_dv), NA, exp_treat)
  ) %>%
  select(matches("^rsp_"), matches("^exp_"), matches("^cnd_"))

is <- is_02

## Prepare Netherlands -----------------------------------------------

nl_01 <-
  nl_raw %>%
  mutate(
    rsp_id = as.numeric(paste0(200, nomem_encr)),
    rsp_country = "Netherlands",
    rsp_gender = case_when(
      geslacht == 1 ~ "Male",
      geslacht == 2 ~ "Female"),
    rsp_age = as.numeric(leeftijd),
    rsp_popdensity = 6 - as.numeric(sted),
    rsp_edu = case_when(
      oplcat == 1     ~ "Lower",
      oplcat %in% 2:4 ~ "Intermediate",
      oplcat %in% 5:6 ~ "Higher")
  )

nl_02 <-
  nl_01 %>%
  gather(var, value, matches("EIPS2017G_[12]_.*"), na.rm = TRUE) %>%
  mutate(
    cnd_n = as.numeric(gsub("EIPS2017G_([12])_.*", "\\1", var)),
    var = gsub("EIPS2017G_[12]_", "", var)
  ) %>%
  spread(var, value) %>%
  mutate(
    cnd_age = case_when(
      age == 1 ~ "28",
      age == 2 ~ "45",
      age == 3 ~ "71"),
    cnd_edu = case_when(
      edu == 1 ~ "High school",
      edu == 2 ~ "University level"),
    cnd_gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female"),
    cnd_religion = case_when(
      religion == 1 ~ "No religion",
      religion == 2 ~ "Christian",
      religion == 3 ~ "Non-practicing Christian",
      religion == 4 ~ "Muslim",
      religion == 5 ~ "Non-praciticing Muslim"),
    cnd_class = case_when(
      class == 1 ~ "Working class",
      class == 2 ~ "Middle class",
      class == 3 ~ "Upper class"),
    exp_post = ifelse(!is.na(EIPS2017G_1), EIPS2017G_1, EIPS2017G_2),
    cnd_post = case_when(
      exp_post == cnd_n ~ 1,
      exp_post != cnd_n ~ 0),
    exp_treat = ifelse(!is.na(EIPS2017G_1), "Closest candidate", "Prefered candidate"),
    exp_treat = ifelse(is.na(EIPS2017G_1) & is.na(EIPS2017G_2), NA, exp_treat)
  ) %>%
  select(matches("^rsp_"), matches("^exp_"), matches("^cnd_"))

nl <- nl_02

## Prepare Norway ----------------------------------------------------

no_01 <-
  no_raw %>%
  mutate(
    rsp_id = as.numeric(paste0(300, responseid)),
    rsp_country = "Norway",
    rsp_gender = case_when(
      R9P1 == 1 ~ "Male",
      R9P1 == 2 ~ "Female"),
    rsp_age_10a = case_when(
      R9P5_1 == 1 ~ "18-25",
      R9P5_1 == 2 ~ "26-35",
      R9P5_1 == 3 ~ "36-45",
      R9P5_1 == 4 ~ "46-55",
      R9P5_1 == 5 ~ "56-65",
      R9P5_1 == 6 ~ "66-75",
      R9P5_1 == 7 ~ "75 <"),
    rsp_edu = case_when(
      R9P4_1 == 1 ~ "Lower",
      R9P4_1 == 2 ~ "Intermediate",
      R9P4_1 == 3 ~ "Higher")
  )

no_02 <-
  no_01 %>%
  gather(var, value, matches("eips2017g_[12]_.*"), na.rm = TRUE) %>%
  mutate(
    cnd_n = as.numeric(gsub("eips2017g_([12])_.*", "\\1", var)),
    var = gsub("eips2017g_[12]_", "", var)
  ) %>%
  spread(var, value) %>%
  mutate(
    cnd_age = case_when(
      age == 1 ~ "28",
      age == 2 ~ "45",
      age == 3 ~ "71"),
    cnd_edu = case_when(
      edu == 0 ~ "High school",
      edu == 1 ~ "University level"),
    cnd_gender = case_when(
      gender == 0 ~ "Male",
      gender == 1 ~ "Female"),
    cnd_religion = case_when(
      religion == 1 ~ "No religion",
      religion == 2 ~ "Christian",
      religion == 3 ~ "Non-practicing Christian",
      religion == 4 ~ "Muslim",
      religion == 5 ~ "Non-praciticing Muslim"),
    cnd_class = case_when(
      class == 1 ~ "Working class",
      class == 2 ~ "Middle class",
      class == 3 ~ "Upper class"),
    eips2017g_1 = ifelse(eips2017g_1 %in% 97:98, NA, eips2017g_1),
    eips2017g_2 = ifelse(eips2017g_2 %in% 97:98, NA, eips2017g_2),
    exp_post = ifelse(!is.na(eips2017g_1), eips2017g_1, eips2017g_2),
    cnd_post = case_when(
      exp_post == cnd_n ~ 1,
      exp_post != cnd_n ~ 0),
    exp_treat = ifelse(!is.na(eips2017g_1), "Closest candidate", "Prefered candidate"),
    exp_treat = ifelse(is.na(eips2017g_1) & is.na(eips2017g_2), NA, exp_treat)
  ) %>%
  select(matches("^rsp_"), matches("^exp_"), matches("^cnd_"))

no <- no_02

## Prepare Sweden ----------------------------------------------------

se_01 <-
  se_raw %>%
  mutate(
    rsp_id = as.numeric(paste0(400, id)),
    rsp_country = "Sweden",
    rsp_gender = case_when(
      sex == 2 ~ "Male",
      sex == 1 ~ "Female"),
    rsp_age = as.numeric(age),
    rsp_edu = case_when(
      edu3 == 1 ~ "Lower",
      edu3 == 2 ~ "Intermediate",
      edu3 == 3 ~ "Higher")
  )

se_02 <-
  se_01 %>%
  select(-edu, -age) %>%
  gather(var, value, matches("EIPS2017G_[12]_\\w\\w\\w.*"), na.rm = TRUE) %>%
  mutate(
    cnd_n = as.numeric(gsub("EIPS2017G_([12])_.*", "\\1", var)),
    var = gsub("EIPS2017G_[12]_", "", var)) %>%
  spread(var, value) %>%
  mutate(
    cnd_age = case_when(
      age == 1 ~ "28",
      age == 2 ~ "45",
      age == 3 ~ "71"),
    cnd_edu = case_when(
      edu == 1 ~ "High school",
      edu == 2 ~ "University level"),
    cnd_gender = case_when(
      gender == 0 ~ "Male",
      gender == 1 ~ "Female"),
    cnd_religion = case_when(
      religion == 1 ~ "No religion",
      religion == 2 ~ "Christian",
      religion == 3 ~ "Non-practicing Christian",
      religion == 4 ~ "Muslim",
      religion == 5 ~ "Non-praciticing Muslim"),
    cnd_class = case_when(
      class == 1 ~ "Working class",
      class == 2 ~ "Middle class",
      class == 3 ~ "Upper class"),
    EIPS2017G_1_dv = ifelse(EIPS2017G_1_dv %in% 96:99, NA, EIPS2017G_1_dv),
    EIPS2017G_2_dv = ifelse(EIPS2017G_2_dv %in% 96:99, NA, EIPS2017G_2_dv),
    exp_post = ifelse(!is.na(EIPS2017G_1_dv), EIPS2017G_1_dv, EIPS2017G_2_dv),
    cnd_post = case_when(
      exp_post == cnd_n ~ 1,
      exp_post != cnd_n ~ 0),
    exp_treat = ifelse(!is.na(EIPS2017G_1_dv), "Closest candidate", "Prefered candidate"),
    exp_treat = ifelse(is.na(EIPS2017G_1_dv) & is.na(EIPS2017G_2_dv), NA, exp_treat)
  ) %>%
  select(matches("^rsp_"), matches("^exp_"), matches("^cnd_"))

se <- se_02

## Prepare France ----------------------------------------------------

fr_01 <-
  fr_raw %>%
  mutate(
    rsp_id = as.numeric(paste0(500, UID_pe03)),
    rsp_country = "France",
    rsp_gender = case_when(
      ea17_A1 == 1 ~ "Male",
      ea17_A1 == 2 ~ "Female"),
    rsp_age_5 = case_when(
      ea17_A2A_rec == 4  ~ "18-24",
      ea17_A2A_rec == 5  ~ "25-29",
      ea17_A2A_rec == 6  ~ "30-34",
      ea17_A2A_rec == 7  ~ "35-39",
      ea17_A2A_rec == 8  ~ "40-44",
      ea17_A2A_rec == 9  ~ "45-59",
      ea17_A2A_rec == 10 ~ "50-54",
      ea17_A2A_rec == 11 ~ "55-59",
      ea17_A2A_rec == 12 ~ "60-64",
      ea17_A2A_rec == 13 ~ "65-69",
      ea17_A2A_rec == 14 ~ "70 <"),
    rsp_age_10b = case_when(
      CAL_AGE10 == 1 ~ "18-22",
      CAL_AGE10 == 2 ~ "23-34",
      CAL_AGE10 == 3 ~ "35-44",
      CAL_AGE10 == 4 ~ "45-54",
      CAL_AGE10 == 5 ~ "55-64",
      CAL_AGE10 == 6 ~ "65-75",
      CAL_AGE10 == 7 ~ "76-79"),
    rsp_edu = case_when(
      CAL_DIPLOME == 1     ~ "Lower",
      CAL_DIPLOME == 2     ~ "Intermediate",
      CAL_DIPLOME %in% 3:4 ~ "Higher")
  )

fr_02 <-
  fr_01 %>%
  gather(
    var, value,
    matches("pe03_EIPS2017G_C[12]_\\w\\w\\w.*"),
    na.rm = TRUE
  ) %>%
  mutate(
    cnd_n = as.numeric(gsub("pe03_EIPS2017G_C([12])_.*", "\\1", var)),
    var = gsub("pe03_EIPS2017G_C[12]_", "", var),
    var = tolower(var)) %>%
  spread(var, value) %>%
  mutate(
    cnd_age = age,
    cnd_edu = case_when(
      edu == "N'a pas fait d'études supérieures" ~ "High school",
      edu == "A fait des études supérieures"     ~ "University level"),
    cnd_gender = case_when(
      gender == "Homme" ~ "Male",
      gender == "Femme" ~ "Female"),
    cnd_religion = case_when(
      religion == "Sans religion"           ~ "No religion",
      religion == "Chrétien"                ~ "Christian",
      religion == "Chrétien non-pratiquant" ~ "Non-practicing Christian",
      religion == "Musulman"                ~ "Muslim",
      religion == "Musulman non-pratiquant" ~ "Non-praciticing Muslim"),
    cnd_class = case_when(
      class == "Classe ouvrière"   ~ "Working class",
      class == "Classe moyenne"    ~ "Middle class",
      class == "Classe supérieure" ~ "Upper class"),
    exp_post = case_when(
      pe03_EIPS2017G_1_DV == 1 ~ 1,
      pe03_EIPS2017G_2_DV == 1 ~ 1,
      pe03_EIPS2017G_1_DV == 2 ~ 2,
      pe03_EIPS2017G_2_DV == 2 ~ 2),
    cnd_post = case_when(
      exp_post == cnd_n ~ 1,
      exp_post != cnd_n ~ 0),
    exp_treat = case_when(
      pe03_EIPS2017G_1_DV == 66 ~ "Prefered candidate",
      pe03_EIPS2017G_2_DV == 66 ~ "Closest candidate")
  ) %>%
  select(matches("^rsp_"), matches("^exp_"), matches("^cnd_"))

fr <- fr_02

## Combine EIPS ------------------------------------------------------

eips_raw <- bind_rows(is, nl, no, se, fr) %>%
  filter(!is.na(cnd_post)) %>%
  mutate(
    rsp_edu_2 = case_when(
      rsp_edu == "Higher" ~ "Higher",
      rsp_edu %in% c("Intermediate", "Lower") ~ "Lower"),
    rsp_age_cat_3 = case_when(
      rsp_age_10a %in% c("18-25", "26-35")          ~ "18-35",
      rsp_age_10b %in% c("18-22", "23-34")          ~ "18-35",
      rsp_age %in% 18:35                            ~ "18-35",
      rsp_age_10a %in% c("36-45", "46-55")          ~ "36-55",
      rsp_age_10b %in% c("35-44", "45-54")          ~ "36-55",
      rsp_age %in% 36:55                            ~ "36-55",
      rsp_age_10a %in% c("56-65", "66-75", "75 <")  ~ "55<",
      rsp_age_10b %in% c("55-64", "65-75", "76-79") ~ "55<",
      rsp_age >= 56                                 ~ "55<"),
    rsp_age_cat_4 = case_when(
      rsp_age_10a %in% c("18-25", "26-35") ~ "18-35",
      rsp_age_10b %in% c("18-22", "23-34") ~ "18-35",
      rsp_age %in% 18:35                   ~ "18-35",
      rsp_age_10a %in% c("36-45", "46-55") ~ "36-55",
      rsp_age_10b %in% c("35-44", "45-54") ~ "36-55",
      rsp_age %in% 36:55                   ~ "36-55",
      rsp_age_10a %in% c("56-65")          ~ "56-65",
      rsp_age_10b %in% c("55-64")          ~ "56-65",
      rsp_age %in% 56:65                   ~ "56-65",
      rsp_age_10a %in% c("66-75", "75 <")  ~ "65<",
      rsp_age_10b %in% c("65-75", "76-79") ~ "65<",
      rsp_age >= 66                        ~ "65<")
  )

## Run sanity checks -------------------------------------------------

if (run_tests) {
  
  assert_that(  
    are_equal(
      length(eips_raw$cnd_post == 1),
      length(eips_raw$cnd_post == 2)
    ),
    msg = "There are not an equal nr of 0/1 choice observations."
  )

  n_levels <- tribble(
    ~var,           ~lvls,
    "rsp_gender",    2,
    "rsp_edu",       3,
    "rsp_edu_2",     2,
    "cnd_age",       3,
    "cnd_edu",       2,
    "cnd_gender",    2,
    "cnd_religion",  5,
    "cnd_class",     3,
    "exp_treat",     2
  )

  for (i in 1:nrow(n_levels)) {
    .lvls <- length(levels(factor(eips_raw[[n_levels$var[i]]])))
    assert_that(
      are_equal(.lvls, n_levels$lvls[i]),
      msg = paste0(
        "There are ", .lvls, " levels in `", n_levels$var[i], "`, ",
        "but there should be ", n_levels$lvls[i], ". "
      )
    )
  }
  
  assert_that(
    all(table(eips_raw$rsp_id) == 2),
    msg = "There are more or less than 2 obs per `rsp_id`."
  )
}

## Write data to file ------------------------------------------------

write.csv(
  x = eips_raw,
  file = here("data", "eips.csv"),
  row.names = FALSE
)
