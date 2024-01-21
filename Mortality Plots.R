###Mortality

library("dplyr")
library("tidyverse")
library("readxl")
library(openxlsx)
library("survival")
library("survminer")
library("ranger")
library("ggfortify")
library("lubridate")
library("ggsurvfit")
library("gtsummary")
library("tidycmprsk")
library("condSURV")
library("tidyverse")
library("tidytidbits")
library("survivalAnalysis")
library(flextable)
library(readr)


##Data Extraction
NHI_for_linking_Jun23 <- read_excel("/Volumes/research/resmed202100111-YOD-Waikato/Vault/Hui Jia Masters/NHI for linking_Jun23.xlsx")
MOH_DataServices_mos4344 <- read_csv("/Volumes/research/resmed202100111-YOD-Waikato/Vault/Hui Jia Masters/MOH-DataServices_mos4344.csv")
MOH_DataServices_mis5154 <- read_csv("/Volumes/research/resmed202100111-YOD-Waikato/Vault/Hui Jia Masters/MOH-DataServices_mis5154.csv")
df_ddx <- read_excel("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/Data.xlsx", sheet = "Ddx")
df_nhi <- read_excel("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/Data.xlsx",
     sheet = "NHI")
df_nhi <- df_nhi %>% 
    select(c(-`Patient_no...1`)) %>%
    rename(Patient_no = Patient_no...2)

MOHMIS <- MOH_DataServices_mis5154 %>%
  select(PRIM_HCU, GEND, nhi_dob, nhi_dod)
MOHMOS <- MOH_DataServices_mos4344 %>%
  select(PRIM_HCU, DOB, DOD)
MOH <- full_join(MOHMIS, MOHMOS, by = "PRIM_HCU", keep = FALSE)
compared_data <- MOH %>%
  mutate(DOB_match = nhi_dob == DOB,
         DOD_match = nhi_dod == DOD)
DE_MOH <- left_join(NHI_for_linking_Jun23, y = MOHMIS, join_by(NHI == "PRIM_HCU")) %>%
  rename("Patient_no" = `Patient #`, "DOB" = "nhi_dob", "DOD" = "nhi_dod") %>%
  select(-NHI) %>%
  mutate(status = ifelse(is.na(DOD), 0, 1))

DE_MOH$DOB <- dmy(DE_MOH$DOB)
DE_MOH$DOD <- dmy(DE_MOH$DOD)
DE_MOH_ddx <- left_join(DE_MOH, df_ddx, by = "Patient_no")

write.xlsx(DE_MOH_ddx, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/DE_MOH_ddx.xlsx")

DE_MOH_ddx <- read.xlsx("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/DE_MOH_ddx.xlsx")
DE_MOH_ddx <- filter(DE_MOH_ddx, DE_MOH_ddx$Patient_no != "12" & DE_MOH_ddx$Patient_no != "27" & DE_MOH_ddx$Patient_no != "34"& DE_MOH_ddx$Patient_no != "41")

# Assume DE_MOH_ddx is your data frame and DOB is the column with Excel numeric dates
DE_MOH_ddx$DOB <- as.Date(DE_MOH_ddx$DOB, origin = "1899-12-30")

DE_MOH_ddx$DOD <- as.Date(DE_MOH_ddx$DOD, origin = "1899-12-30")
DE_MOH_ddx$Date_dx <- as.Date(DE_MOH_ddx$Date_dx, origin = "1899-12-30")
DE_MOH_ddx <- DE_MOH_ddx %>%
  mutate(
    age_dx = as.duration(DOB %--% Date_dx) / dyears(1)
    )
DE_MOH_ddx <- DE_MOH_ddx %>%
  mutate(
    time_days = ifelse(
      !is.na(DOD),
      as.duration(Date_dx %--% DOD) / ddays(1),
      as.duration(Date_dx %--% as.Date("2021-12-31")) / ddays(1)
    )
  )

DE_MOH_ddx <- DE_MOH_ddx %>%
  mutate(
    Age_grp = ifelse(age_dx < "60", 1, 2)
    )

DE_MOH_ddx <- DE_MOH_ddx %>%
    mutate(
      status = ifelse(!is.na(DOD) & DOD < as.Date("2021-12-31"), 1, 0)
    )

DE_MOH_ddx_nhi <- left_join(DE_MOH_ddx, df_nhi, by = "Patient_no")

DE_MOH_ddx_nhi <- DE_MOH_ddx_nhi %>%
  mutate(
      Ethnic = ifelse(Ethnicity == "Maori", 1, 2),
      Dx_code = ifelse(grepl("alz|Alz", Diagnosis, ignore.case = TRUE), 0, 1)
      ) 

DE_MOH_ddx_nhi_E <- DE_MOH_ddx_nhi %>%
    filter(Ethnicity != "Response Unidentifiable", Ethnicity != "Not Stated") 

DE_MOH_ddx_nhi_antid <- DE_MOH_ddx_nhi %>%
    left_join(duration_antid, by = "Patient_no") %>%
    replace_na(list(duration_antid = 0))
  
DE_MOH_ddx_nhi_pharma <- DE_MOH_ddx_nhi_antid %>%
    left_join(duration_antip, by = "Patient_no") %>%
    replace_na(list(duration_antip = 0))
  
DE_MOH_ddx_nhi_pharma <- DE_MOH_ddx_nhi_pharma %>%
    mutate(pstatus = ifelse(duration_antip > 0, 1, 0)) %>%
    mutate(dstatus = ifelse(duration_antid > 0, 1, 0))

fitmodel <- survfit(Surv(time_days, status) ~ 1, data = DE_MOH_ddx)
survival_plot <- ggsurvplot(fitmodel,
                            data = DE_MOH_ddx,
                            risk.table = "abs_pct",
                            surv.median.line = "hv",
                            cumevents = TRUE,
                            tables.height = 0.1,
                            tables.theme = theme_cleantable(),
                            ggtheme = theme_light(),
                            title = "Kaplan-Meier Mortality Estimate",
                            xscale = "d_y",
                            break.time.by = 730,
                            xlab = "Time (years)",
                            ylab = "Survival probability",
                            palette = "#2E9FDF"
)
ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Mortality/KM plots/General.png",
  plot = survival_plot,
  width = 16, height = 9, dpi = 300) # nolint

(survfitsummary <- gtsummary::tbl_survfit(survfit(Surv(time_days, status) ~ 1, data = DE_MOH_ddx), # nolint
                                          probs = 0.5,
                                          label_header = "**Median Survival**")) %>%  # nolint
  as_flex_table()




###KMPlot with GGplot2 
# add method to grid.draw to save risk tables with plots
  grid.draw.ggsurvplot <- function(x) {
    survminer:::print.ggsurvplot(x, newpage = FALSE)
  }
#Gender
survfitmodel_G <- survfit(Surv(time_days, status) ~ Gender, data = DE_MOH_ddx_nhi)
cox_gender <- ggsurvplot(survfitmodel_G,
           data = DE_MOH_ddx_nhi,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = "abs_pct",
           surv.median.line = "hv",
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           ggtheme = theme_light(),
           title = "Kaplan-Meier Mortality Estimate - Gender",
           xscale = "d_y",
           break.time.by = 730,
           xlab = "Time (years)",
           ylab = "Survival probability",
           ncensor.plot = TRUE,      # plot the number of censored subjects at time t
           ncensor.plot.height = 0.1,
           legend.labs =
            c("Male", "Female"), 
           palette = 
            c("#E7B800", "#2E9FDF") # custom color palettes.
)

ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Mortality/KM plots/Gender.png", 
  plot = cox_gender, width = 16, height = 9, dpi = 300)
survfit(Surv(time_days, status) ~ GEND, data = DE_MOH_ddx)

#Age
survfitmodel_A <- survfit(Surv(time_days, status) ~ Age_grp, data = DE_MOH_ddx)
cox_age <- ggsurvplot(survfitmodel_A,
           data = DE_MOH_ddx_nhi,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = "abs_pct",
           surv.median.line = "hv",
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           ggtheme = theme_light(),
           title = "Kaplan-Meier Mortality Estimate - Age",
           xscale = "d_y",
           break.time.by = 730,
           xlab = "Time (years)",
           ylab = "Survival probability",
           ncensor.plot = TRUE,      # plot the number of censored subjects at time t
           ncensor.plot.height = 0.1,
           legend.labs =
            c("Age < 60", "Age ≥ 60"), 
           palette = 
            c("#E7B800", "#2E9FDF") # custom color palettes.
)
ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Mortality/KM plots/Age.png", 
  plot = cox_age, width = 16, height = 9, dpi = 300)
survfit(Surv(time_days, status) ~ Age_grp, data = DE_MOH_ddx)

#Ethnicity
survfitmodel_E <- survfit(Surv(time_days, status) ~ Ethnic, data = DE_MOH_ddx_nhi_E)
cox_E <- ggsurvplot(survfitmodel_E,
           data = DE_MOH_ddx_nhi,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = "abs_pct",
           surv.median.line = "hv",
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           ggtheme = theme_light(),
           title = "Kaplan-Meier Mortality Estimate - Ethnicity",
           xscale = "d_y",
           break.time.by = 730,
           xlab = "Time (years)",
           ylab = "Survival probability",
           ncensor.plot = TRUE,      # plot the number of censored subjects at time t
           ncensor.plot.height = 0.1,
           legend.labs =
            c("Māori", "non-Māori"), 
           palette = 
            c("#E7B800", "#2E9FDF") # custom color palettes.
)
  ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Mortality/KM plots/Ethnicity.png", 
  plot = cox_E, width = 16, height = 9, dpi = 300)
survfit(Surv(time_days, status) ~ Ethnic, data = DE_MOH_ddx_nhi_E)

#AD
survfitmodel_AD <- survfit(Surv(time_days, status) ~ Dx_code, data = DE_MOH_ddx_nhi)
cox_AD <- ggsurvplot(
           survfitmodel_AD, 
           data = DE_MOH_ddx_nhi,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = "abs_pct",
           surv.median.line = "hv",
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           ggtheme = theme_light(),
           title = "Kaplan-Meier Mortality Estimate - AD",
           xscale = "d_y",
           break.time.by = 730,
           xlab = "Time (years)",
           ylab = "Survival probability",
           ncensor.plot = TRUE,      # plot the number of censored subjects at time t
           ncensor.plot.height = 0.1,
           legend.labs =
            c("AD", "non-AD"), 
           palette = 
            c("#E7B800", "#2E9FDF") # custom color palettes.
)
ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Mortality/KM plots/AD.png", 
  plot = cox_AD, width = 16, height = 9, dpi = 300)
survfit(Surv(time_days, status) ~ Dx_code, data = DE_MOH_ddx_nhi)

#Anti-dementia
survfitmodel_Antid <- survfit(Surv(time_days, status) ~ dstatus, data = DE_MOH_ddx_nhi_pharma)
cox_Antid <- ggsurvplot(
           survfitmodel_Antid, 
           data = DE_MOH_ddx_nhi_pharma,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = "abs_pct",
           surv.median.line = "hv",
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           ggtheme = theme_light(),
          #  title = "Kaplan-Meier Mortality Estimate - Anti-dementia",
           xscale = "d_y",
           break.time.by = 730,
           xlab = "Time (years)",
           ylab = "Survival probability",
           ncensor.plot = TRUE,      # plot the number of censored subjects at time t
           ncensor.plot.height = 0.1,
           legend.labs =
            c("Anti-dementia use", "Not using anti-dementia"), 
           palette = 
            c("#E7B800", "#2E9FDF") # custom color palettes.
)
ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Mortality/KM plots/Anti-dementia.png", 
  plot = cox_Antid, width = 16, height = 9, dpi = 300)

#Anti-psychotics
survfitmodel_Antip <- survfit(Surv(time_days, status) ~ pstatus, data = DE_MOH_ddx_nhi_pharma)
cox_Antip <- ggsurvplot(
           survfitmodel_Antip, 
           data = DE_MOH_ddx_nhi_pharma,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = "abs_pct",
           surv.median.line = "hv",
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           ggtheme = theme_light(),
          #  title = "Kaplan-Meier Mortality Estimate - Anti-dementia",
           xscale = "d_y",
           break.time.by = 730,
           xlab = "Time (years)",
           ylab = "Survival probability",
           ncensor.plot = TRUE,      # plot the number of censored subjects at time t
           ncensor.plot.height = 0.1,
           legend.labs =
            c("Anti-psychotics use", "Not using anti-psychotics"), 
           palette = 
            c("#E7B800", "#2E9FDF") # custom color palettes.
)
ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Mortality/KM plots/Anti-psychotics.png", 
  plot = cox_Antip, width = 16, height = 9, dpi = 300)



##Cox Regression Hazard Ratio
covariates_M <- c("Gender", "Age_grp", "Dx_code")
univ_formulas_M <- sapply(covariates_M,
                        function(x) as.formula(paste('Surv(time_days, status)~', x)))   
variable_M <- "Ethnic"
univ_formula_M_E <- sapply(variable_M,
                        function(x) as.formula(paste('Surv(time_days, status)~', x)))                     
univ_models_M <- lapply( univ_formulas_M, function(x){coxph(x, data = DE_MOH_ddx_nhi)})
univ_model_M_E <- lapply( univ_formula_M_E, function(x){coxph(x, data = DE_MOH_ddx_nhi_E)})
variables_p <- c("pstatus", "dstatus")
univ_formulas_p <- sapply(variables_p,
                        function(x) as.formula(paste('Surv(time_days, status)~', x)))
univ_models_p <- lapply( univ_formulas_p, function(x){coxph(x, data = DE_MOH_ddx_nhi_pharma)})

# Extract data 
univ_results_p <- lapply(univ_models_p,
                       function(x){ 
                          x <- summary(x)
                          p.value<-signif(x$wald["pvalue"], digits=2)
                          wald.test<-signif(x$wald["test"], digits=2)
                          beta<-signif(x$coef[1], digits=2);#coeficient beta
                          HR <-signif(x$coef[2], digits=2);#exp(beta)
                          HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                          HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                          HR <- paste0(HR, " (", 
                                       HR.confint.lower, "-", HR.confint.upper, ")")
                          res<-c(beta, HR, wald.test, p.value)
                          names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                        "p.value")
                          return(res)
                          #return(exp(cbind(coef(x),confint(x))))
                         })
res <- t(as.data.frame(univ_results_p, check.names = FALSE))
as.data.frame(res)
write.csv(res, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Mortality/Coxsumm_Pharma.csv")


##Cox Model Assumptions
CoxM<- coxph(Surv(time_days, status) ~ Gender + Age_grp + Dx_code, data = DE_MOH_ddx_nhi)
test.ph_M <- cox.zph(CoxM)
ggcoxzph(test.ph_M)
grid.draw.ggcoxzph <- function(x) {
    survminer:::print.ggcoxzph(x, newpage = FALSE)
  }

ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Mortality/Coxassump.png", 
  plot = ggcoxzph(test.ph_M), width = 16, height = 9, dpi = 300)

CoxM_E <- coxph(Surv(time_days, status) ~ Ethnic, data=DE_MOH_ddx_nhi_E)
test.ph_M_E <- cox.zph(CoxM_E)
ggcoxzph(test.ph_M_E)
ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Mortality/Coxassump_E.png", 
  plot = ggcoxzph(test.ph_M_E), width = 16, height = 4, dpi = 300)

ph_results <- data.frame(
  test.ph_M$table[,1])
ph_results_E <- data.frame(
  test.ph_M_E$table[,1])
write.csv(ph_results, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Mortality/Coxassump.csv")
write.csv(ph_results_E, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Mortality/Coxassump_E.csv")

length(unique(DE_MOH_ddx_nhi_E$Patient_no))

CoxM_pharma <- coxph(Surv(time_days, status) ~ dstatus + pstatus, data = DE_MOH_ddx_nhi_pharma)
test.ph_M_pharma <- cox.zph(CoxM_pharma)
ggcoxzph(test.ph_M_pharma)
ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Mortality/Coxassump_pharma.png", 
  plot = ggcoxzph(test.ph_M_pharma), width = 16, height = 8, dpi = 300)
ph_results_pharma <- data.frame(
  test.ph_M_pharma)
write.csv(ph_results_pharma, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/Mortality/Coxassump_pharma.csv")
test.ph_M_pharma
