# Clean data according to the requirement:
library(readxl)
library(tidyverse)
library(ggplot2)

survey_result <- read_xlsx(path = "stats consult data analysis capstone sheet.xlsx")

survey_count <- survey_result[-1,]

survey_clean <- survey_count %>% select(c(4,5,7,14,15,17:ncol(survey_count)))

survey_clean_finished <- survey_clean %>% filter(Finished == "True" & Q1 == "Yes")

# add level to questions with response about agree level:

aa <- function(x){
  factor(x,levels = c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"))
}

survey_clean_finished[23:75] <- lapply(survey_clean_finished[23:75], FUN = aa) 

bb <- function(x){
  as.numeric(x)
}

survey_clean_finished[23:75] <- lapply(survey_clean_finished[23:75], FUN = bb) 

#survey_clean_finished[23:75] <- as.numeric(survey_clean_finished[23:75])
#a <- lapply(survey_clean_finished[23:75], as.numeric)

# add level to Q3:

survey_clean_finished$Q3 <- factor(survey_clean_finished$Q3,levels = c("Less than 1 year","1-4 years","5-9 years","10-14 years","More than 14 years")) %>% as.numeric()

# add level to Q4:

survey_clean_finished$Q4 <- factor(survey_clean_finished$Q4,levels = c("0-20%","21-40%","41-60%","61-80%","81-100%")) %>% as.numeric()

# add level to Q9:

survey_clean_finished$Q9 <- factor(survey_clean_finished$Q9,levels = c("No", "Yes")) %>% as.numeric()

# add level to Q7:

survey_clean_finished$Q7 <- factor(survey_clean_finished$Q7,levels = c("No", "Yes")) %>% as.numeric()

# Replace NA with 0 for Q10:

survey_clean_finished$Q10[is.na(survey_clean_finished$Q10)] <- 0

# add level to Q10:

survey_clean_finished$Q10 <- factor(survey_clean_finished$Q10,levels = c("0", "0-3", "4-6", "7-9","+10")) %>% as.numeric()
                                            
# Do GCs with many years of preconception experience tend to agree with Q103_1 - Q103_7 more often than GCs with less years of preconception experience? 

q10_q103 <- survey_clean_finished %>% select(c(1, 4, 5, 17) | starts_with("Q103"))

# Do the GCs that have encountered a patient that requested affected embryo transfer tend to agree with Q103_1 - Q103_7 more often than GCs who have not encountered a request?

q7_q103 <- survey_clean_finished %>% select(c(1, 4, 5, 21) | starts_with("Q103"))

# Do the GCs that have many years of preconception experience (Q10) tend to have encountered a patient that requested embryo transfer (Q7) more than GCs that have less years of preconception experience?

q7_q10 <- survey_clean_finished %>% select(c(1, 4, 5, 21, 17))

# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with Q103_1 - Q103_7 more often than GCs with less years of patient-facing genetic counseling experience? 

q3_q103 <- survey_clean_finished %>% select(c(1, 4, 5, 12) | starts_with("Q103"))

# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with the embryo transfer justification of “quality of life” across conditions (Q24, Q31, 32, 108, 113, 51, 110) more often than GCs with less years of patient-facing genetic counseling experience? 

q3_qol <- survey_clean_finished %>% select(c(1, 4, 5, 12, 32, 37, 41, 46, 52, 57, 62))

# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with the embryo transfer justification of “family history” across conditions (Q26, 30, 37, 40, 46, 52, 58) more often than GCs with less years of patient-facing genetic counseling experience? 

q3_fh <- survey_clean_finished %>% select(c(1, 4, 5, 12, 34, 38, 43, 48, 54, 58, 63))

# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with the embryo transfer justification of “Variability” across conditions (Q23, 27 33, 38, 45, 56) more often than GCs with less years of patient-facing genetic counseling experience? 

q3_var <- survey_clean_finished %>% select(c(1, 4, 5, 12, 31, 36, 40, 45, 51, 61))

# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with the embryo transfer justification of “reproductive autonomy” across conditions (Q29, 35, 36, 41, 47, 53, 59) more often than GCs with less years of patient-facing genetic counseling experience?

q3_repauto <- survey_clean_finished %>% select(c(1, 4, 5, 12, 35, 39, 44, 50, 56, 60, 64))

# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with the embryo transfer justification of “Resources available” across conditions (Q25, 107, 42, 109, 54) more often than GCs with less years of patient-facing genetic counseling experience? 

q3_resava <- survey_clean_finished %>% select(c(1, 4, 5, 12, 33, 42, 47, 53, 59))

q102 <- survey_clean_finished %>% select(c(1, 4, 5) | starts_with("Q102"))

renal_alport <- q102 %>% select(c(1, 2, 3, 9, 10))

BRCA1_cancer <- q102 %>% select(c(1, 2, 3, 5, 6)) 
  

  
  
  
  