library(readxl)
library(tidyverse)
library(ggplot2)

##------  load the data
# survey_result <- read_xlsx(path = "stats consult data analysis capstone sheet.xlsx")
# 
# survey_count <- survey_result[-1,]
# 
# survey_clean <- survey_count %>% select(c(4,5,7,14,15,17:ncol(survey_count)))
# survey_clean_finished <- survey_clean %>% filter(Finished == "True" & Q1 == "Yes")
# 
# survey_clean_finished$Q9 <- ifelse(survey_clean_finished$Q9 == "Yes", 1,0)

##---------------- bootstrap

# bootstrap <- function (x, g, B = 100) {
#   n <- nrow(x)
#   theta.star <- numeric(B)
#   for (i in 1:B) { # for each bootstrap sample
#     x.star <- x[sample.int(n, replace = TRUE), ]
#     theta.star[i] <- g(x.star)
#   }
#   theta.star
# }

bootstrap_corr <- function(x, B = 100){
  n <- nrow(x)
  obs_n <- numeric(B)
  for (i in 1:B){
    sample.star <- x[sample.int(n, replace = TRUE),]
  }
  corr_value <- cor(sample.star[,1],sample.star[,2])
  corr_value
}

##---------- demo
# x = data.frame(a = seq(1:100), b = sample(x = 1:100))
# a <- replicate(n = 100,expr = bootstrap_corr(x,80))
# mean(a)

##------------ calculate the correlation coefficients among questions

q7_q103$Q7 <- ifelse(q7_q103$Q7 == "Yes", 2,1)
q7_q103.4_corr <- replicate(n = 100,expr = bootstrap_corr(q7_q103[,c("Q7","Q103_4")],1000))
mean(q7_q103.4_corr)



# Do GCs with many years of preconception experience tend to agree with Q103_1 - Q103_7 more often than GCs with less years of preconception experience? 

q10_q103 <- survey_clean_finished %>% select(c(1, 4, 5, 17) | starts_with("Q103"))

for (i in 1:7){
  name1 <- paste0("Q103","_","1")
  name2 <- paste0("q7_",name1,"_corr")
  q7_q103.4_corr <- replicate(n = 100,expr = bootstrap_corr(q7_q103[,c("Q7",name1)],1000))
}

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


