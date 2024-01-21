## McNemar HC vs LTCF 6-18 after LCTF1 (LTCF2)

library("dplyr")
library("tidyverse")
library("readxl")
library("stringr")
library(fishmethods)
library("lubridate")
library(rcompanion)
library(contingencytables)

# Load data
df_1 <- read_excel("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/Data.xlsx",
     sheet = "Data")
df_nhi <- read_excel("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/Data.xlsx",
     sheet = "NHI")
df_nhi <- filter(df_nhi, df_nhi$Patient_no != "12" & df_nhi$Patient_no != "27" & df_nhi$Patient_no != "34"& df_nhi$Patient_no != "41")
df_1 <- filter(df_1, df_1$Patient_no != "12" & df_1$Patient_no != "27" & df_1$Patient_no != "34"& df_1$Patient_no != "41")
list <- as.data.frame(unique(read_excel("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/Data.xlsx",sheet = "iCode")$iCode))
names(list)[1] <- "iCode"
remove <- unique(read_excel("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/Data.xlsx",sheet = "remove"))
list_ltcf <- list %>% anti_join(remove, by = "iCode")
df2 <- df_1 %>% 
    select(c(Assessment_Type, Assessment_Date, Patient_no, any_of(list_ltcf$iCode)))

answers_data <- read_excel("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/Data.xlsx", sheet = "Answers")
questions <- read_excel("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/Data.xlsx", sheet = "Questions")

answers <- answers_data %>% 
    left_join(questions, join_by(iCode)) %>%
    select(c(-Acceptable_values, -Description)) %>%
    relocate(Question, .after = iCode)

answers_ltcf <- answers %>% 
    filter(iCode %in% list_ltcf$iCode) %>%
    select(-iCode)

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
df_assessments1v6_12 <- df_assessments1v2 %>%
    filter(time_diff >= 180 & time_diff <= 365) %>%
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
df_hcvltcf6_12 <- bind_rows(df_hc, df_assessments_final) %>% 
    filter(Patient_no %in% df_assessments_final$Patient_no) %>%
    select(-c(Date1, time_diff, HC_Date)) %>%
    arrange(Patient_no, Assessment_Date)    

# Pivot data from wide to long    
df_long <- pivot_longer(df_hcvltcf6_18, c(-Assessment_Type, -Assessment_Date, -Patient_no), names_to = "iCode", values_to = "Answer_Code") %>% 
    arrange(iCode, Patient_no)

# Merge answers with long data
df_ans <- df_long %>%
    merge(answers, by = c("iCode", "Answer_Code")) %>%
    arrange(iCode, Patient_no)

# Data Recoding
answers_filtered <- answers %>% filter(iCode %in% list$iCode) %>%
    select(-iCode)

answers_wide <- answers_filtered %>%
    pivot_wider(names_from = Question, values_from = c(Answer)) 

# Recoding answers from interRAI recoding.r

# Retrieve answers recoded 
answers_long <- answers_wide %>% 
    pivot_longer(cols = c(-Answer_Code), names_to= "Question", values_to = "Answer") %>% 
    arrange(Question) %>%
    filter(!is.na(Answer)) 

# Retrieve all answers for each question
answer_full <- answers_long %>% 
    filter(Question %in% answers_ltcf$Question) %>%
    select(Question, Answer) %>% 
    distinct(Question, Answer)

# Merge recoded answers with long data
df_ans_final <- df_ans %>%
    merge(answers_long, c("Question", "Answer_Code")) %>%
    select(Question, Assessment_Type, Answer.y, Patient_no) %>%
    rename(Answer = Answer.y) %>%
    arrange(Question,Patient_no)

df_ansltcf <- answer_full %>%
    left_join(df_ans_final, by = c("Question", "Answer")) 

df_anscount <- df_ansltcf %>%
    group_by(Question, Answer, Assessment_Type) %>%
    summarise(count = n()) %>%
    spread(key = Assessment_Type, value = count, fill = 0) %>%
    select(c(Question, Answer, HC, LTCF))

write.xlsx(df_anscount, "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/McNemar/df_anscount.xlsx")

# Reshape data to wide format 
df_wide <- df_ans_final %>%
    filter(Question == count_summary$Question) %>%
    pivot_wider(names_from = Assessment_Type, values_from = Answer) %>%
    add_count(Question, HC, LTCF) %>%
    select(c(-Patient_no)) %>%
    unique() %>%
    arrange(Question) 
table_data <- df_wide %>%
    spread(key = LTCF, value = n, fill = 0)

# For-loop to create contingency tables for all questions
table_data_all <- list()  # To store the results
for (question in count_summary$Question){
    table_data <- df_ans_final %>%
        filter(Question == question) %>%
        pivot_wider(names_from = Assessment_Type, values_from = Answer) %>%
        count(HC, LTCF) 
    table_data_all[[question]] <- table_data
} 
combined_data <- bind_rows(table_data_all, .id = "Question")

all_hc <- answer_full %>%
    rename(HC = Answer)

all_ltcf <- answer_full %>%
    rename(LTCF = Answer)

df_all_hcltcf <- merge(all_hc, all_ltcf, by = "Question")

df_all_full <- merge(df_all_hcltcf, combined_data, by = c("Question", "HC", "LTCF"), all = TRUE) %>%
    replace_na(list(n = 0)) %>%
    filter(!is.na(Question) & !is.na(HC) & !is.na(LTCF))

# McNemar's test
# Count the number of rows for each 'Questions'
count_summary <- df_anscount %>%
    group_by(Question) %>%
    summarise(row_count = n())

# nrow=2
count_list_2 <- count_summary %>%
    filter(row_count == "2") 

# Matrix nrow=2
perform_mcnemar <- function(data, question, HC1, HC2) {
      table_data <- data %>%
            filter(Question == question) %>%
            spread(key = LTCF, value = n, fill = 0)
      mcnemar.test(matrix(
              c(table_data[[table_data$HC[1]]], table_data[[table_data$HC[2]]]),
              nrow = 2), correct = FALSE
              )
}

# Loop through health condition pairs and perform McNemar's test
results <- list()  # To store the results
for (question in count_list_2$Question) {
     health_conditions <- count_list_2[[question]]
     result <- perform_mcnemar(df_all_full, question, health_conditions[1], health_conditions[2])
     results[[question]] <- result#[3]
} 

# Extract relevant information from results and assemble into a data frame
result_data <- data.frame(
    Question = character(0),
    ChiSquareStat = numeric(0),
    pValue = numeric(0)
)

for (question in names(results)) {
  result <- results[[question]]
  result_data <- rbind(result_data, data.frame(
    Question = question,
    ChiSquareStat = result$statistic,
    pValue = result$p.value
  ))
}

# Save results data frame to a CSV file
output_filename <- "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/McNemar/pV_2.csv"
write.csv(result_data, file = output_filename, row.names = FALSE)

# Matrix nrow=3
count_list_3 <- count_summary %>%
    filter(row_count == "3")

perform_mcnemar_bowker  <- function(data, question, hc1, hc2, hc3) {
     table_data <- data %>%
            filter(Question == question) %>%
            spread(key = LTCF, value = n, fill = 0)
     McNemarBowker_test_paired_cxc(matrix(
          c(table_data[[table_data$HC[1]]], table_data[[table_data$HC[2]]], table_data[[table_data$HC[3]]]),
          nrow = 3))
}

results_3 <- list()  # To store the results
for (question in count_list_3$Question) {
     health_conditions <- count_list_3[[question]]
     result <- perform_mcnemar_bowker(df_all_full, question, health_conditions[1], health_conditions[2], health_conditions[3])
     results_3[[question]] <- result#[3]
}

# Extract relevant information from results and assemble into a data frame
result_data_3 <- data.frame(
     Question = character(0),
     pValue = numeric(0)
)

for (question in names(results_3)) {
     result <- results_3[[question]]
     result_data_3 <- rbind(result_data_3, data.frame(
          Question = question,
          pValue = result$P
     ))
}

# Save results data frame to a CSV file
output_filename <- "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/McNemar/pVB_3.csv"
write.csv(result_data_3, file = output_filename, row.names = FALSE)

# Matrix nrow=4
count_list_4 <- count_summary %>%
    filter(row_count == "4")

perform_mcnemar_bowker  <- function(data, question, hc1, hc2, hc3, hc4) {
     table_data <- data %>%
            filter(Question == question) %>%
            spread(key = LTCF, value = n, fill = 0)
     McNemarBowker_test_paired_cxc(matrix(
          c(table_data[[table_data$HC[1]]], table_data[[table_data$HC[2]]], table_data[[table_data$HC[3]]], table_data[[table_data$HC[4]]]),
          nrow = 4))
}

results_4 <- list()  # To store the results
for (question in count_list_4$Question) {
     health_conditions <- count_list_4[[question]]
     result <- perform_mcnemar_bowker(df_all_full, question, health_conditions[1], health_conditions[2], health_conditions[3], health_conditions[4])
     results_4[[question]] <- result
}

# Extract relevant information from results and assemble into a data frame
result_data_4 <- data.frame(
     Question = character(0),
     pValue = numeric(0)
)

for (question in names(results_4)) {
     result <- results_4[[question]]
     result_data_4 <- rbind(result_data_4, data.frame(
          Question = question,
          pValue = result$P
     ))
}

# Save results data frame to a CSV file
output_filename <- "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/McNemar/pVB_4.csv"
write.csv(result_data_4, file = output_filename, row.names = FALSE)