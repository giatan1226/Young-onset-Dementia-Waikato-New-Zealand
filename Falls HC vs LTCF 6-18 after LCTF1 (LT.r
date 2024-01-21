## Falls HC vs LTCF 6-18 after LCTF1 (LTCF2)

library("dplyr")
library("tidyverse")
library("readxl")
library("stringr")
library(fishmethods)
library("lubridate")
library(rcompanion)
library(contingencytables)

#Load Data
df_1 <- read_excel("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/Data.xlsx",
     sheet = "Data")
df_nhi <- read_excel("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/Data.xlsx",
     sheet = "NHI")
df_nhi <- filter(df_nhi, df_nhi$Patient_no != "12" & df_nhi$Patient_no != "27" & df_nhi$Patient_no != "34"& df_nhi$Patient_no != "41")

df_1 <- filter(df_1, df_1$Patient_no != "12" & df_1$Patient_no != "27" & df_1$Patient_no != "34"& df_1$Patient_no != "41")
list <- c("iJ1", "iJ12", "iJ1g", "iJ1h", "iJ1i")
df2 <- df_1 %>% 
    select(c(Assessment_Type, Assessment_Date, Patient_no, any_of(list)))
df2_nhi <- left_join(df2, df_nhi, by = "Patient_no") 
df2 <- df2_nhi %>%
        mutate(Age_Assessment =
        trunc(ymd(DOB) %--% ymd(Assessment_Date)/years(1)))

df2 <- df2 %>%
  mutate(
    iJ1h_1 = case_when(
      iJ1 == 1 ~ 1,
      iJ1 == 2 ~ 0,
      iJ1 == 3 ~ 0,
      iJ1 == 0 ~ 0,
      iJ1h == 1 ~ 1,
      iJ1h == 2 ~ 1,
      TRUE ~ 0,
    ),
    iJ1g_1 = case_when(
      iJ1 == 1 ~ 0,
      iJ1 == 2 ~ 1,
      iJ1 == 3 ~ 2,
      TRUE ~ 0
    )
  )


df2 <- df2 %>%
  mutate(
    iJ1g = ifelse(is.na(iJ1g), iJ1g_1, iJ1g), 
    iJ1h = iJ1h_1, 
    iJ1i = ifelse(is.na(iJ1i), 0, iJ1i)
  )

df2_mcnemar <- df2 %>%  
  select(-c(iJ1, iJ1g_1, iJ1h_1, iJ12)) %>%
  mutate(
    iJ1g = case_when(
        iJ1g == 0 ~ "0 - No Falls",
        iJ1g == 1 ~ "1 - One Fall",
        iJ1g == 2 ~ "2 - 2 or more Falls"
        ),
    iJ1h = case_when(
        iJ1h == 0 ~ "0 - No Falls",
        iJ1h == 1 ~ "1 - One or more Falls",
        iJ1h == 2 ~ "2 - 2 or more Falls"
        ),
    iJ1i = case_when(
        iJ1i == 0 ~ "0 - No Falls",
        iJ1i == 1 ~ "1 - One Fall",
        iJ1i == 2 ~ "2 - 2 or more Falls"
        )
    ) 


df2 <- df2_mcnemar %>% 
rename(
    'Falls - In last 30 days' = iJ1g, 
    'Falls - 31-90 days ago' = iJ1h, 
    'Falls - 91-180 days ago' = iJ1i
  )


# Retrieving cohort with HC assessment
df_hc <- df2 %>%
    filter(Assessment_Type == "HC") %>%
    arrange(Patient_no, desc(Assessment_Date)) %>%
    distinct(Patient_no, .keep_all = TRUE) 
df_hc_1 <- df_hc %>% 
    select(Patient_no,Assessment_Date) %>%
    rename(HC_Date = Assessment_Date)

# Retrieving cohorts with LTCF assessments
df_ltcf <- df2 %>%
    filter(Assessment_Type == "LTCF") %>%
    arrange(Patient_no, Assessment_Date) 

# Retrieving first LTCF assessments
df_ltcf1 <- df_ltcf %>% 
    arrange(Patient_no,Assessment_Date) %>%
    distinct(Patient_no, .keep_all = TRUE) %>%
    mutate(Assessment_Type = "LTCF1")

df_ltcf1_1 <- df_ltcf1 %>%    
    select(Patient_no,Assessment_Date) %>%
    rename(Date1 = Assessment_Date)

df_ltcf2 <- anti_join(df_ltcf, df_ltcf1_1, join_by("Assessment_Date" == "Date1")) %>%
    arrange(Patient_no, Assessment_Date)

# Join and cross-check time between LTCF1 and LTCF2
df_assessments1v2 <- left_join(df_ltcf1_1, df_ltcf2, by = "Patient_no") %>%
    mutate(time_diff = as.numeric(difftime(Assessment_Date, Date1, units = "days"))) %>%
    relocate(Assessment_Date, .after = Patient_no) %>%
    relocate(Date1, .before = Assessment_Date) %>%
    relocate(time_diff, .after = Assessment_Date) %>%
    filter(Assessment_Type !="NA")

# Selecting LTCF1 and LTCF2 assessments between 6-18 months
df_assessments1v6_18 <- df_assessments1v2 %>%
    filter(time_diff >= 180 & time_diff <= 540) %>%
    arrange(Patient_no, Assessment_Date) %>%
    distinct(Patient_no, .keep_all = TRUE)

# Cross-check time between HC and LTCF 6-18/12 after LTCF1 (LTCF2)
df_assessments_final <- df_assessments1v6_18 %>%
  inner_join(df_hc_1 %>% select(Patient_no, HC_Date), by = "Patient_no") %>%
  filter(Date1 >= HC_Date) %>%
    mutate(time_diff = as.numeric(difftime(Assessment_Date, HC_Date, units = "days"))) %>%
    relocate(Assessment_Date, .after = Patient_no) %>%
    relocate(HC_Date, .before = Assessment_Date) %>%
    relocate(time_diff, .after = Assessment_Date) %>%
    filter(Assessment_Type !="NA")

mean(df_assessments_final$time_diff, na.rm = TRUE)

# Merge HC and LTCF2
df_hcvltcf6_18 <- bind_rows(df_hc, df_assessments_final) %>% 
    filter(Patient_no %in% df_assessments_final$Patient_no) %>%
    select(-c(Date1, time_diff, HC_Date)) %>%
    arrange(Patient_no, Assessment_Date)

  
df_falls <- df_hcvltcf6_18 %>% 
  select(Patient_no, Assessment_Type, Assessment_Date,`Falls - In last 30 days`,`Falls - 31-90 days ago`)

df_fallscount_30 <- df_falls %>%
    select(Patient_no, Assessment_Type, `Falls - In last 30 days`) %>%
    group_by(`Falls - In last 30 days`, Assessment_Type) %>%
    summarise(count = n()) %>%
    spread(key = Assessment_Type, value = count, fill = 0) 
  
df_falls_30 <- df_falls %>%
    select(Patient_no, Assessment_Type, `Falls - In last 30 days`) %>%
    pivot_wider(names_from = Assessment_Type, values_from = `Falls - In last 30 days`) %>%
    add_count(HC, LTCF) %>%
    select(c(-Patient_no)) %>%
    unique() %>%
    arrange(HC)

df_all_falls <- df2_mcnemar %>%
    select(iJ1g) %>%
    unique()

falls_HC <- df_all_falls %>%
    rename(HC = iJ1g)

falls_ltcf <- df_all_falls %>%
    rename(LTCF = iJ1g)

df_falls_hcltcf <- merge(falls_HC, falls_ltcf)

df_falls_30 <- full_join(df_falls_30, df_falls_hcltcf) %>%
    replace_na(list(n = 0)) %>%
    arrange(HC)

table_falls_30 <- df_falls_30 %>%
    spread(key = LTCF, value = n, fill = 0)

McNemarBowker_test_paired_cxc(matrix(
          c(table_falls_30[[table_falls_30$HC[1]]], table_falls_30[[table_falls_30$HC[2]]], table_falls_30[[table_falls_30$HC[3]]]),
          nrow = 3))

df_fallscount_31 <- df_falls %>%
    select(Patient_no, Assessment_Type, `Falls - 31-90 days ago`) %>%
    group_by(`Falls - 31-90 days ago`, Assessment_Type) %>%
    summarise(count = n()) %>%
    spread(key = Assessment_Type, value = count, fill = 0) 

df_falls_31 <- df_falls %>%
    select(Patient_no, Assessment_Type, `Falls - 31-90 days ago`) %>%
    pivot_wider(names_from = Assessment_Type, values_from = `Falls - 31-90 days ago`) %>%
    add_count(HC, LTCF) %>%
    select(c(-Patient_no)) %>%
    unique() %>%
    arrange(HC)

df_all_falls <- df2_mcnemar %>%
    select(iJ1h) %>%
    unique()

falls_HC <- df_all_falls %>%
    rename(HC = iJ1h)

falls_ltcf <- df_all_falls %>%
    rename(LTCF = iJ1h)

df_falls_hcltcf <- merge(falls_HC, falls_ltcf)

df_falls_31 <- full_join(df_falls_31, df_falls_hcltcf) %>%
    replace_na(list(n = 0)) 

table_falls_31 <- df_falls_31 %>%
    spread(key = LTCF, value = n, fill = 0)

mcnemar.test(matrix(
    c(table_falls_31[[table_falls_31$HC[1]]], 
    table_falls_31[[table_falls_31$HC[2]]]),
    nrow = 2), correct = FALSE
    )
