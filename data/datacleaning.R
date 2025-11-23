library(tidyverse)
library(readxl)

nhis24_df_fit <- 
  read_excel("data/adults_2024.xlsx") |>
  select(-REGION) |>
  
  # Sex
  mutate(
    SEX_A = case_when(SEX_A == 1 ~ 1, SEX_A == 2 ~ 0)
  ) |>
  
  # Diabetes
  mutate(
    DIBEV_A = case_when(
      DIBEV_A == 1 ~ 1,
      DIBEV_A == 2 ~ 0,
      TRUE ~ NA_real_
    )
  ) |>
  filter(!is.na(DIBEV_A)) |>
  
  # BMI
  mutate(
    BMICATD_A = case_when(
      BMICATD_A == 1 ~ 1,
      BMICATD_A == 2 ~ 0,
      BMICATD_A == 3 ~ 2,
      BMICATD_A %in% c(4, 5, 6) ~ 3,
      TRUE ~ NA_real_
    )
  ) |>
  filter(!is.na(BMICATD_A)) |>
  
  # Difficulty walking
  mutate(
    DIFF_A = case_when(
      DIFF_A == 1 ~ 0,
      DIFF_A == 2 ~ 1,
      DIFF_A == 3 ~ 2,
      DIFF_A == 4 ~ 3,
      TRUE ~ NA_real_
    )
  ) |>
  filter(!is.na(DIFF_A)) |>
  
  # Education
  mutate(
    EDUCP_A = case_when(
      EDUCP_A %in% 0:4 ~ 0,
      EDUCP_A %in% 5:7 ~ 1,
      EDUCP_A %in% 8:10 ~ 2,
      TRUE ~ NA_real_
    )
  ) |>
  filter(!is.na(EDUCP_A)) |>
  
  # Life satisfaction
  mutate(
    LSATIS4_A = case_when(
      LSATIS4_A %in% c(1, 2) ~ 0,
      LSATIS4_A %in% c(3, 4) ~ 1,
      TRUE ~ NA_real_
    )
  ) |>
  filter(!is.na(LSATIS4_A)) |>
  
  # Health status
  mutate(
    PHSTAT_A = case_when(
      PHSTAT_A %in% c(1, 2, 3) ~ 0,
      PHSTAT_A %in% c(4, 5) ~ 1,
      TRUE ~ NA_real_
    )
  ) |>
  filter(!is.na(PHSTAT_A)) |>
  
  # Cholesterol
  mutate(
    CHLEV_A = case_when(
      CHLEV_A == 2 ~ 0,
      CHLEV_A == 1 ~ 1,
      TRUE ~ NA_real_
    )
  ) |>
  filter(!is.na(CHLEV_A)) |>
  
  # Hypertension
  mutate(
    HYPEV_A = case_when(HYPEV_A == 1 ~ 1, HYPEV_A == 2 ~ 0, TRUE ~ NA_real_)
  ) |>
  filter(!is.na(HYPEV_A)) |>
  
  # Liver
  mutate(
    LIVEREV_A = case_when(LIVEREV_A == 1 ~ 1, LIVEREV_A == 2 ~ 0, TRUE ~ NA_real_)
  ) |>
  filter(!is.na(LIVEREV_A)) |>
  
  # Stroke
  mutate(
    STREV_A = case_when(STREV_A == 1 ~ 1, STREV_A == 2 ~ 0, TRUE ~ NA_real_)
  ) |>
  filter(!is.na(STREV_A)) |>
  
  # Heart diseases
  mutate(
    MIEV_A  = case_when(MIEV_A  == 1 ~ 1, MIEV_A  == 2 ~ 0, TRUE ~ NA_real_),
    ANGEV_A = case_when(ANGEV_A == 1 ~ 1, ANGEV_A == 2 ~ 0, TRUE ~ NA_real_),
    CHDEV_A = case_when(CHDEV_A == 1 ~ 1, CHDEV_A == 2 ~ 0, TRUE ~ NA_real_)
  ) |>
  filter(!is.na(MIEV_A), !is.na(ANGEV_A), !is.na(CHDEV_A)) |>
  
  # Recode mental health & smoke AFTER filtering 7/8/9
  filter(
    LONELY_A != 7 & LONELY_A != 8 & LONELY_A != 9,
    SMKEV_A  != 7 & SMKEV_A  != 8 & SMKEV_A  != 9,
    DEPEV_A  != 7 & DEPEV_A  != 8 & DEPEV_A  != 9,
    ANXEV_A  != 7 & ANXEV_A  != 8 & ANXEV_A  != 9
  ) |>
  
  mutate(
    LONELY_A = case_when(
      LONELY_A == 5 ~ 0,
      LONELY_A == 4 ~ 1,
      LONELY_A == 3 ~ 2,
      LONELY_A == 2 ~ 3,
      LONELY_A == 1 ~ 4
    ),
    SMKEV_A = case_when(SMKEV_A == 1 ~ 1, SMKEV_A == 2 ~ 0),
    DEPEV_A = case_when(DEPEV_A == 1 ~ 1, DEPEV_A == 2 ~ 0),
    ANXEV_A = case_when(ANXEV_A == 1 ~ 1, ANXEV_A == 2 ~ 0)
  ) |>
  
  select(
    SEX_A, AGEP_A, DIBEV_A, BMICATD_A, WEIGHTLBTC_A, HEIGHTTC_A,
    DIFF_A, EDUCP_A, LSATIS4_A, PHSTAT_A,
    CHLEV_A, HYPEV_A, LIVEREV_A, STREV_A,
    MIEV_A, ANGEV_A, CHDEV_A,
    DEPEV_A, ANXEV_A, SMKEV_A, LONELY_A
  )
