##Time to LTC admission

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


#Data Extraction
df_1 <- read_excel("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/Data.xlsx",
     sheet = "Data")
#df_ddx <- read_excel("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/Data.xlsx", sheet = "Ddx")
df_nhi <- read_excel("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/Data.xlsx",
     sheet = "NHI")
#df_ddx <- filter(df_ddx, df_ddx$Patient_no != "12" & df_ddx$Patient_no != "34"& df_ddx$Patient_no != "41")
df_nhi <- filter(df_nhi, df_nhi$Patient_no != "12" & df_nhi$Patient_no != "27" & df_nhi$Patient_no != "34" & df_nhi$Patient_no != "41")
df_1 <- filter(df_1, df_1$Patient_no != "12" & df_1$Patient_no != "27" & df_1$Patient_no != "34"& df_1$Patient_no != "41")
list <- unique(read_excel("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Data/Data.xlsx",sheet = "iCode")$iCode)
df2 <- df_1 %>% 
    select(c(Assessment_Type, Assessment_Date, Patient_no, any_of(list)))


#Extract Patients' single assessment
df_hc <- df2 %>%
  filter(Assessment_Type == "HC") %>%
  arrange(Patient_no, desc(Assessment_Date)) %>%
  distinct(Patient_no, .keep_all = TRUE) 
df_ltcf <- df2 %>% 
  filter(Assessment_Type == "LTCF") %>%
  arrange(Patient_no, Assessment_Date) %>%
  distinct(Patient_no, .keep_all = TRUE)
df_ca <- df2 %>%
     filter(Assessment_Type == "CA") %>%
     arrange(Patient_no, desc(Assessment_Date)) %>%
     distinct(Patient_no, .keep_all = TRUE)
df_3 <- bind_rows(df_hc, df_ltcf, df_ca) %>%
    arrange(Patient_no, desc(Assessment_Type)) %>%
    distinct(Patient_no, .keep_all = TRUE) %>%
    arrange(Patient_no, Assessment_Date)

#Combine data with NHI
df_3_ddx <- left_join(df_nhi, df_3, by = "Patient_no") %>% 
  mutate(Assessment_Date = ymd(Assessment_Date)) %>% 
  select("Assessment_Type", Assessment_Date, Date_dx, Patient_no, Age_dx, Ethnicity, "Gender", Diagnosis) %>%
  relocate(Patient_no, .before = Assessment_Date)

library(data.table)
setDT(df_3_ddx)[is.na(Assessment_Type),Assessment_Date := ymd("2021-12-30")]
setDT(df_3_ddx)[is.na(Assessment_Type),Assessment_Type := "None"]

df_3_ddx_nhi <- df_3_ddx %>% 
    mutate(Assessment_Date = ymd(Assessment_Date),
      Date_dx = ymd(Date_dx),
      status = ifelse(Assessment_Type == "LTCF", 1, 0),
      Ethnic = ifelse(Ethnicity == "Maori", 1, 2),
      Age_grp = ifelse(Age_dx < "60", 1, 2), 
      Dx_code = ifelse(grepl("alz|Alz", Diagnosis, ignore.case = TRUE), 1, 2)
      ) 
df_3_ddx_nhi <- df_3_ddx_nhi %>%
  mutate(
    time_days = ifelse(
      Assessment_Type == "LTCF",
      as.duration(Date_dx %--% Assessment_Date) / ddays(1),
      as.duration(Date_dx %--% as.Date("2021-12-31")) / ddays(1)
    )
  )

df_3_ddx_nhi <- df_3_ddx_nhi %>%
    filter(time_days > 0 ) %>%
    relocate(c(time_days, status), .before = Date_dx) %>% 
    relocate(Ethnic, .after = Ethnicity)

df3 <- df_3_ddx_nhi %>% 
    filter(Ethnicity != "Response Unidentifiable", Ethnicity != "Not Stated") 


#Cox Regression Model
cox_LTCF_G <- coxph(Surv(time_days, status) ~ Gender, data = df_3_ddx_nhi) #%>%
  #tbl_regression(exp = TRUE)
summary_surv_LTCF <- capture.output(summary(cox_LTCF_G))
surv_LTCF_fit <- survfit(cox_LTCF_G)
autoplot(surv_LTCF_fit)
(survfitsummary_cox <- gtsummary::tbl_survfit(survfit(cox_LTCF_G), # nolint
                                         probs = 0.5, 
                                         label_header = "**Median Survival**")) %>%  # nolint
    as_flex_table()
write.table(summary_surv_LTCF, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/surv_LTCF_summary.csv", quote = FALSE)

gender_df <- with(df_3_ddx_nhi,
                  data.frame(Gender = c("M","F")))

fit <- survfit(cox_LTCF_G, newdata = gender_df)
ggsurvplot(fit, df_3_ddx_nhi,
           conf.int = FALSE, 
           legend.labs = c("Gender = M", "Gender = F"),
           risk.table = TRUE,
surv.median.line = "hv",
cumevents = TRUE,
tables.height = 0.1,
tables.theme = theme_cleantable(),
ggtheme = theme_bw(),
title = "Cox LTCF Admission Estimate",
xscale = "d_y",
xlab = "Time (years)",
ylab = "Survival to LTCF admission"
)$plot + 
  theme(plot.background = element_rect(fill = "#EBEBEB"),
  panel.background = element_rect(fill = "#EBEBEB"),
  legend.box.background = element_rect(fill = "#EBEBEB")) +
  theme(text=element_text(size=24)) #change font size of all text
ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/LTCF Survival/Kaplan_Meier_LTCF_Y2.png",
  plot = survival_plot,
  width = 16, height = 9, dpi = 300) # nolint)

##To apply the univariate coxph function to multiple covariates at once
covariates <- c("Gender", "Age_grp", "Dx_code")
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time_days, status)~', x)))

variable <- "Ethnic"
univ_formula_E <- sapply(variable,
                        function(x) as.formula(paste('Surv(time_days, status)~', x)))
                        
univ_models <- lapply( univ_formulas, function(x){coxph(x, data = df_3_ddx_nhi)})
univ_model_E <- lapply( univ_formula_E, function(x){coxph(x, data = df3)})
# Extract data 
univ_results <- lapply(univ_model_E,
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
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)
write.csv(res, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/LTCF Survival/Coxsumm_E.csv")


##Cox Model Assumptions
CoxLTCF<- coxph(Surv(time_days, status) ~ Gender + Age_grp + Dx_code, data = df_3_ddx_nhi)
test.ph_LTCF <- cox.zph(CoxLTCF)
coxassump_plot <- ggcoxzph(test.ph_LTCF)
grid.draw.ggcoxzph <- function(x) {
    survminer:::print.ggcoxzph(x, newpage = FALSE)
  }

ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/LTCF Survival/Coxassump.png", 
  plot = coxassump_plot, width = 16, height = 9, dpi = 300)

CoxLTCF_E <- coxph(Surv(time_days, status) ~ Ethnic, data=df3)
test.ph_LTCF_E <- cox.zph(CoxLTCF_E)
ggcoxzph(test.ph_LTCF_E)
ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/LTCF Survival/Coxassump_E.png", 
  plot = ggcoxzph(test.ph_LTCF_E), width = 16, height = 4, dpi = 300)

ph_results <- data.frame(
  test.ph_LTCF$table[,1])
ph_results_E <- data.frame(
  test.ph_LTCF_E$table[,1])
write.csv(ph_results, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/LTCF Survival/Coxassump.csv")
write.csv(ph_results_E, file = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/LTCF Survival/Coxassump_E.csv")

##Kaplan-Meier Plots
# add method to grid.draw to save risk tables with plots
  grid.draw.ggsurvplot <- function(x) {
    survminer:::print.ggsurvplot(x, newpage = FALSE)
  }

#General
survfitmodel <- survfit(Surv(time_days, status) ~ 1, data = df_3_ddx_nhi)

KM_plot <- ggsurvplot(
  survfitmodel,           # survfit object with calculated statistics.
  conf.int = TRUE,         # show confidence intervals for point estimates for survival curves
  title = "Kaplan-Meier LTCF Admission Estimate",
  xscale = "d_y",
  xlab = "Time in years",   # customize X axis label.
  ylab = "Survival to LTCF admission",
  break.time.by = 730,     # break X axis in time intervals by .
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.height = 0.1, # Useful to change when you have multiple groups
  cumevents = TRUE,
  tables.height = 0.1,
  tables.theme = theme_cleantable(),
#  ncensor.plot = TRUE,      # plot the number of censored subjects at time t
#  ncensor.plot.height = 0.2,
  surv.median.line = "hv",  # add the median survival pointer.
  palette = "#2E9FDF"
)

ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/LTCF Survival/KM_Plots/General.png", 
  plot = KM_plot, width = 16, height = 9, dpi = 300)

#Gender
survfitmodel_G <- survfit(Surv(time_days, status) ~ Gender, data = df_3_ddx_nhi)

KM_plot_G <- ggsurvplot(
    survfitmodel_G,           # survfit object with calculated statistics.
    pval = TRUE,             # show p-value of log-rank test.
    conf.int = TRUE,         # show confidence intervals for point estimates for survival curves
    title = "Kaplan-Meier LTCF Admission Estimate - Gender",
    xscale = "d_y",
    xlab = "Time in years",   # customize X axis label.
    ylab = "Survival to LTCF admission",
    break.time.by = 730,     # break X axis in time intervals by .
    ggtheme = theme_light(), # customize plot and risk table with a theme.
    risk.table = "abs_pct",  # absolute number and percentage at risk.
    risk.table.height = 0.2, # Useful to change when you have multiple groups
    ncensor.plot = TRUE,      # plot the number of censored subjects at time t
    ncensor.plot.height = 0.15,
    surv.median.line = "hv",  # add the median survival pointer.
    palette = 
     c("#E7B800", "#2E9FDF") # custom color palettes.
      )

  # Save the plot using ggsave
 ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/LTCF Survival/KM_Plots/Gender.png", 
  plot = KM_plot_G, width = 16, height = 9, dpi = 300)

 #Age
survfitmodel_A <- survfit(Surv(time_days, status) ~ Age_grp, data = df_3_ddx_nhi)

KM_plot_A <- ggsurvplot(
  survfitmodel_A,           # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for point estimates for survival curves
  title = "Kaplan-Meier LTCF Admission Estimate - Age",
  xscale = "d_y",
  xlab = "Time in years",   # customize X axis label.
  ylab = "Survival to LTCF admission",
  break.time.by = 730,     # break X axis in time intervals by .
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.height = 0.2, # Useful to change when you have multiple groups
  ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  ncensor.plot.height = 0.15,
  surv.median.line = "hv",  # add the median survival pointer.
  legend.labs = c("Age < 60", "Age ≥ 60"),
  palette = 
    c("#E7B800", "#2E9FDF") # custom color palettes.
)

ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/LTCF Survival/KM_Plots/Age.png", 
  plot = KM_plot_A, width = 16, height = 9, dpi = 300)

#AD
survfitmodel_AD <- survfit(Surv(time_days, status) ~ Dx_code, data = df_3_ddx_nhi)

KM_plot_AD <- ggsurvplot(
  survfitmodel_AD,           # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for point estimates for survival curves
  title = "Kaplan-Meier LTCF Admission Estimate - AD",
  xscale = "d_y",
  xlab = "Time in years",   # customize X axis label.
  ylab = "Survival to LTCF admission",
  break.time.by = 730,     # break X axis in time intervals by .
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.height = 0.2, # Useful to change when you have multiple groups
  ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  ncensor.plot.height = 0.15,
  surv.median.line = "hv",  # add the median survival pointer.
  legend.labs = c("AD", "Non-AD"),
  palette = 
    c("#E7B800", "#2E9FDF") # custom color palettes.
)

ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/LTCF Survival/KM_Plots/AD.png", 
  plot = KM_plot_AD, width = 16, height = 9, dpi = 300)

#Ethnicity
survfitmodel_E <- survfit(Surv(time_days, status) ~ Ethnic, data = df3)

KM_plot_E <- ggsurvplot(
  survfitmodel_E,           # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for point estimates for survival curves
  title = "Kaplan-Meier LTCF Admission Estimate - Ethnicity",
  xscale = "d_y",
  xlab = "Time in years",   # customize X axis label.
  ylab = "Survival to LTCF admission",
  break.time.by = 730,     # break X axis in time intervals by .
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.height = 0.2, # Useful to change when you have multiple groups
  ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  ncensor.plot.height = 0.15,
  surv.median.line = "hv",  # add the median survival pointer.
  legend.labs = c("Māori", "Non-Māori"),
  palette = 
    c("#E7B800", "#2E9FDF") # custom color palettes.
)

ggsave(
  filename = "~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/YOD/Results/LTCF Survival/KM_Plots/Ethnic.png", 
  plot = KM_plot_E, width = 16, height = 9, dpi = 300)
