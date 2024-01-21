##Pharma

library("dplyr")
library("tidyverse")
library("readxl")
library(openxlsx)
library("lubridate")
library("stringr")

Pharm1 <- read.csv("/Volumes/resmed202100111-YOD-Waikato/student/share/MOH-DataServices_phh1180.csv")
Pharmsub <- read.csv("/Volumes/resmed202100111-YOD-Waikato/student/share/MOH-DataServices_Dim_form_pack_subsidy.csv")
nhi <- read_excel("/Volumes/resmed202100111-YOD-Waikato/Master datasets - do not edit/NHI for linking_Jun23.xlsx")
df_nhi <- read_excel("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/Data.xlsx",
     sheet = "NHI")
antip_id <- read_excel("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/Pharma Chemical Code.xlsx",
     sheet = "Antipsychotics")
antid_id <- read_excel("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/Pharma Chemical Code.xlsx",
     sheet = "Anti-dementia")

df_pharm <- left_join(Pharm1,nhi, join_by(PRIM_HCU == NHI))
df_pharm_nhi <- left_join(df_pharm, df_nhi, join_by('Patient #' == Patient_no))
df_pharm_nhi <- df_pharm_nhi %>%
        rename(Patient_no = `Patient #`) %>%
        filter(Patient_no != '12', Patient_no != '27', Patient_no != '34', Patient_no != '41') %>%
        select(DIM_FORM_PACK_SUBSIDY_KEY,DATE_DISPENSED,DOSE,FREQUENCY,DAILY_DOSE,DAYS_SUPPLY,DISPENSINGS_PRESCRIBED,REPEAT_SEQUENCE_NUMBER,QUANTITY_DISPENSED,Patient_no,Age_dx,Date_dx,Gender,Ethnicity,Diagnosis,DOB.y,DOB.x)
df_pharm_nhi$DOB.y <- ymd(df_pharm_nhi$DOB.y)
df_pharm_nhi$DOB.x <- dmy(df_pharm_nhi$DOB.x)
df_pharm_nhi <- df_pharm_nhi %>%
        mutate(results = ifelse(DOB.x == DOB.y, 1, 2))
unique(df_pharm_nhi$results)

df_pharm_sub <- left_join(df_pharm_nhi, Pharmsub, join_by(DIM_FORM_PACK_SUBSIDY_KEY == DIM_FORM_PACK_SUBSIDY_KEY))
df_pharm_sub <- filter(df_pharm_sub, DOSE != 0)
df_pharm_sub <- df_pharm_sub %>%
        select(Patient_no,DOB.x,Gender,Ethnicity,Diagnosis,Date_dx,Age_dx,DATE_DISPENSED,DOSE,FREQUENCY,DAILY_DOSE,DAYS_SUPPLY,DISPENSINGS_PRESCRIBED,REPEAT_SEQUENCE_NUMBER,QUANTITY_DISPENSED,CHEMICAL_ID,CHEMICAL_NAME,FORMULATION_NAME)
df_pharm_antip <- left_join(df_pharm_sub, antip_id, join_by(CHEMICAL_ID == "Chemical code")) %>%
        filter(!is.na(Description) & !is.na(DAYS_SUPPLY) & DAYS_SUPPLY != 0) 
df_pharm_antid <- left_join(df_pharm_sub, antid_id, join_by(CHEMICAL_ID == "Chemical code")) %>%
        filter(!is.na(Description) & !is.na(DAYS_SUPPLY) & DAYS_SUPPLY != 0) 
df_pharm_antid$DATE_DISPENSED <- dmy(df_pharm_antid$DATE_DISPENSED)
df_pharm_antip$DATE_DISPENSED <- dmy(df_pharm_antip$DATE_DISPENSED)
df_pharm_antip <- df_pharm_antip %>%
        filter(Date_dx < DATE_DISPENSED)
df_pharm_antid_after <- df_pharm_antid %>%
        filter(Date_dx < DATE_DISPENSED)
df_pharm_antid_summary <- df_pharm_antid %>%
        group_by(Patient_no) %>%
        summarise(AllDatesUnique = n() == n_distinct(DATE_DISPENSED))
df_pharm_antip_summary <- df_pharm_antip %>%
        group_by(Patient_no) %>%
        summarise(AllDatesUnique = n() == n_distinct(DATE_DISPENSED))

df_chl <- read.csv("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/antip_df_chl.csv")
df_chl <- df_chl %>%
        select(c(-DAILY_DOSE))
df_pharm_antip_chl <- df_pharm_antip %>%
        left_join(df_chl, join_by(Description == Description,FORMULATION_NAME == FORMULATION_NAME))

# Identify and extract records with non-unique DATE_DISPENSING for each Patient_no
df_non_unique_dates <- df_pharm_antip %>%
  group_by(Patient_no) %>%
  filter(n() != n_distinct(DATE_DISPENSED)) %>%
  ungroup()


# Number of dispensings

antid_before_dx_summ <- df_pharm_antid %>%
        filter(Date_dx > DATE_DISPENSED & DAYS_SUPPLY != 0) %>%
        group_by(Patient_no,Description) %>%
        dplyr::summarise(antid_before_dx = n()) %>%
        filter(antid_before_dx > 0)
write.csv(antid_before_dx_summ, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/antid_before_dx_summ.csv", row.names = FALSE)
antid_after_dx_summ <- df_pharm_antid %>%
        filter(Date_dx < DATE_DISPENSED & DAYS_SUPPLY != 0) %>%
        group_by(Patient_no,Description) %>%
        summarise(antid_after_dx = n()) %>%
        filter(antid_after_dx > 0)
write.csv(antid_after_dx_summ, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/antid_after_dx_summ.csv", row.names = FALSE)
antip_after_dx_summ <- df_pharm_antip %>%
        filter(Date_dx < DATE_DISPENSED & DAYS_SUPPLY != 0) %>%
        group_by(Patient_no,Description) %>%
        dplyr::summarise(antip_after_dx = n(), .groups = 'drop') %>%
        filter(antip_after_dx > 0)
length(unique(antip_after_dx_summ$Patient_no))
write.csv(antip_after_dx_summ, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/antip_after_dx_summ.csv", row.names = FALSE)

unique(df_pharm_antid$CHEMICAL_NAME)
antip_after_dx <- df_pharm_antip %>%
        filter(Date_dx < DATE_DISPENSED) 


# Duration of dispensings

duration_antid <- df_pharm_antid %>%
        filter(Date_dx < DATE_DISPENSED) %>%
        group_by(Patient_no) %>%
        summarise(duration_antid = sum(DAYS_SUPPLY))

duration_antip <- df_pharm_antip %>%
        filter(Date_dx < DATE_DISPENSED) %>%
        group_by(Patient_no) %>%
        dplyr::summarise(duration_antip = sum(DAYS_SUPPLY))
duration_antip_each <- df_pharm_antip %>%
        filter(Date_dx < DATE_DISPENSED) %>%
        group_by(Patient_no,Description) %>%
        dplyr::summarise(duration_antip_each = sum(DAYS_SUPPLY))

dplyr::summarise(duration_antip, 
        range(duration_antip, na.rm = TRUE),
        mean(duration_antip, na.rm = TRUE),
        sd(duration_antip, na.rm = TRUE))

write.csv(duration_antid, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/duration_antid.csv", row.names = FALSE)
write.csv(duration_antip, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/duration_antip.csv", row.names = FALSE)
write.csv(duration_antip_each, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/duration_antip_each.csv", row.names = FALSE)


df_pharm_antip <- df_pharm_antip %>%
        mutate(timeframe = ymd(DATE_DISPENSED) + days(DAYS_SUPPLY)) %>%
        relocate(timeframe, .after = DATE_DISPENSED)

df_pharm_antip_58 <- df_pharm_antip %>%
        filter(Patient_no == 58)
write.csv(df_pharm_antip_58, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/df_pharm_antip_58.csv", row.names = FALSE)

## Dosage
df_pharm_antid <- df_pharm_antid %>%
        mutate(total_daily_dosage = as.numeric(str_extract(FORMULATION_NAME, "\\d+")) * DAILY_DOSE)
df_pharm_antid_after <- df_pharm_antid_after %>%
        mutate(total_daily_dosage = as.numeric(str_extract(FORMULATION_NAME, "\\d+")) * DAILY_DOSE)

df_pharm_antip <- df_pharm_antip %>%
        mutate(total_daily_dosage = as.numeric(str_extract(FORMULATION_NAME, "\\d+")) * DAILY_DOSE)
df_pharm_antip_chl <- df_pharm_antip_chl %>%
        mutate(formulation = str_extract(Chlorpromazine, "\\d+"))

df_pharm_antip_chl <- df_pharm_antip_chl %>%
        mutate(total_daily_dosage = as.numeric(formulation) * DAILY_DOSE)

antip_df <- df_pharm_antip %>%
        select(Description,FORMULATION_NAME,DAILY_DOSE) %>%
        group_by(Description) %>%
        unique() %>%
        arrange(Description)

write.csv(antip_df, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/antip_df.csv", row.names = FALSE)

antip_dosage <- df_pharm_antip %>%
        group_by(Description) %>%
        dplyr::summarise(
                max(total_daily_dosage, na.rm = TRUE),
                min(total_daily_dosage, na.rm = TRUE),
                mean(total_daily_dosage, na.rm = TRUE),
                sd(total_daily_dosage, na.rm = TRUE))
antid_dosage <- dplyr::summarise(df_pharm_antid, 
        max(total_daily_dosage, na.rm = TRUE),
        min(total_daily_dosage, na.rm = TRUE),
        mean(total_daily_dosage, na.rm = TRUE),
        sd(total_daily_dosage, na.rm = TRUE))

write.csv(antid_dosage, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/antid_dosage.csv", row.names = FALSE)

## Type of medication vs diagnosis
antip_dx <- df_pharm_antip %>%
        group_by(Description,Diagnosis) %>%
        summarise(antip_dx = n()) %>%
        filter(antip_dx > 0)
antid_dx <- df_pharm_antid %>%
        group_by(Description,Diagnosis) %>%
        summarise(antid_dx = n()) %>%
        filter(antid_dx > 0)
antid_FTD <- df_pharm_antid %>%
        filter(Diagnosis == "Frontotemporal") %>%
        group_by(Description) 

write.csv(antip_dx, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/antip_dx.csv", row.names = FALSE)
write.csv(antid_dx, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/antid_dx.csv", row.names = FALSE)
write.csv(antid_FTD, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/antid_FTD.csv", row.names = FALSE)

## Number of antipsychotics & combinations
result <- antip_after_dx_summ %>%
   group_by(Patient_no) %>%
   dplyr::summarise(Description = paste(Description, collapse = " & "))

result <- result %>%
        group_by(Description) %>%
        dplyr::summarise(antip_after_dx = n())

write.csv(result, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/antip_after_dx_agg.csv", row.names = FALSE)

ggplot(result, aes(x = Description, y = antip_after_dx)) +
        geom_col() +
        labs(x = "Antipsychotic medication(s)", y = "Number of patients") +
        theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
        theme_bw()

ggsave("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/plot_antip.png", plot = last_plot(), width = 10, height = 6, units = "in", dpi = 300)

## Individual plots for trend of medication

ggplot(data = df_pharm_antid_after, aes(x = DATE_DISPENSED, y = total_daily_dosage, color = Description, group = Patient_no)) +  
    geom_point() +
        labs(x = "Date dispensed", y = "Total daily dosage (mg)") +
    facet_wrap(facets = vars(Patient_no), nrow = 8) +
    geom_line()

ggsave("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/antid_plot_after.png", plot = last_plot())

ggplot(data = df_pharm_antip_chl, aes(x = DATE_DISPENSED, y = total_daily_dosage, color = Description, group = Description)) +  
#     geom_line() +
    geom_point(size = 1) +
        labs(x = "Date dispensed", y = "Chlorpromazine equivalence daily dosage (mg)") +
    facet_wrap(facets = vars(Patient_no), nrow = 9) +
    scale_color_manual(values = c("red", "black", "#6acb5d", "orange", "#056dff"))

ggsave("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/antip_plot_chl.png", plot = last_plot(), width = 15, height = 10)

ggplot(data = df_pharm_antip, aes(x = DATE_DISPENSED, y = total_daily_dosage, color = Description, group = Description)) +  
#     geom_line() +
    geom_point(size = 1) +
        labs(x = "Date dispensed", y = "Total daily dosage (mg)") +
    facet_wrap(facets = vars(Patient_no), nrow = 9) +
    scale_color_manual(values = c("red", "black", "#6acb5d", "orange", "#056dff"))

ggsave("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Pharma/antip_plot.png", plot = last_plot(), width = 15, height = 10)
