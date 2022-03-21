source("clean_and_wrangle_data.R")
library("ggplot2")
library("scales")
library("dplyr")

# Do all the respondents tend to agree with these questions/justifications?
# 1. Q13

q13 <- survey_clean_finished %>% select(c(1, 4, 5, 23))


# 2. Q103_1 - Q103_7


q103 <- survey_clean_finished %>% select(c(1, 4, 5, 24:30))



# 3. Morally uneasy table: Q102_1-11

q102 <- survey_clean_finished %>% select(c(1, 4, 5) | starts_with("Q102"))


# Descriptive analysis organized by conditions:
# Trisomy 21: Q23 Q24 Q25 Q26 Q29

t_21 <- survey_clean_finished %>% select(c(1, 4, 5, 31:35))

# X-linked Alport female: Q27 Q31 Q30 Q35

x_alport_female <- survey_clean_finished %>% select(c(1, 4, 5, 36:39))


# X-linked Alport male: Q33 Q32 Q107 Q37 Q36

x_alport_male <- survey_clean_finished %>% select(c(1, 4, 5, 40:44))
  

# BRCA1: Q38 Q108 Q42 Q40 Q43

brca1 <- survey_clean_finished %>% select(c(1, 4, 5, 45:49))


# FAP: Q45 Q113 Q109 Q46 Q49 Q47

fap <- survey_clean_finished %>% select(c(1, 4, 5, 51:56))


# GJB2: Q51 Q52 Q54 Q53


glb2 <- survey_clean_finished %>% select(c(1, 4, 5, 57:60))


# Huntington’s: Q56 Q110 Q58 Q59

ht <- survey_clean_finished %>% select(c(1, 4, 5, 61:64))


# Descriptive analysis organized by justifications:
# Quality of life: (Q24, Q31, 32, 108, 113, 51, 110)

qol <- survey_clean_finished %>% select(c(1, 4, 5, 32, 37, 41, 46, 52, 57, 62))


# Family history: (Q26, 30, 37, 40, 46, 52, 58)

fh <- survey_clean_finished %>% select(c(1, 4, 5, 34, 38, 43, 48, 54, 58, 63))


# Variability: (Q23, 27 33, 38, 45, 56)

var <- survey_clean_finished %>% select(c(1, 4, 5, 31, 36, 40, 45, 51, 61)) 
  
  
# Reproductive autonomy: (Q23, 27 33, 38, 45, 56)

repauto <- survey_clean_finished %>% select(c(1, 4, 5, 35, 39, 44, 50, 56, 60, 64))

# Resources available: (Q25, 107, 42, 109, 54)

resava <- survey_clean_finished %>% select(c(1, 4, 5, 33, 42, 47, 53, 59))


# Do GCs who said YES to Q7 tend to agree with Q13, Q103_1 - Q103_7 more often than GCs who answered NO?

q7_q13_q103 <- survey_clean_finished %>% select(c(1, 4, 5, 21, 23) | starts_with("Q103"))

# Do GCs with fewer years of patient-facing genetic counseling experience (Q3)  tend to agree with Q13 (every embryo is a human life worthy of birth) more often than GCs with many years of patient facing experience?

q3_q13 <- survey_clean_finished %>% select(c(1, 4, 5,12, 23))


# Do GCs with fewer years of patient-facing genetic counseling experience (Q3) feel more morally uneasy with transferring affected embryos (Q102_1 - 102_11) more often than GCs with many years of patient facing experience?

q3_q102 <- survey_clean_finished %>% select(c(1, 4, 5,12) | starts_with("Q102"))


#Do all GC respondents tend to agree more with the justifications of the “welfare of the child” group or with the “Autonomy” group?

welfare <- survey_clean_finished %>% select(c(1, 4, 5, 32, 37, 41, 46, 52, 57, 62, 34, 38, 43, 48, 54, 58, 63, 31, 36, 40, 45, 51, 61, 49, 55, 33, 42, 47, 53, 59))

autonomy <- survey_clean_finished %>% select(c(1, 4, 5, 34, 38, 43, 48, 54, 58, 63, 35, 39, 44, 50, 56, 60, 64))

# Do  GCs with fewer years of patient-facing genetic counseling experience (Q3) tend to agree more with the justifications of the “welfare of the child” group or with the “Autonomy” group than GCs with many years of patient-facing genetic counseling experience?

q3_welfare <- survey_clean_finished %>% select(c(1, 4, 5, 12, 32, 37, 41, 46, 52, 57, 62, 34, 38, 43, 48, 54, 58, 63, 31, 36, 40, 45, 51, 61, 49, 55, 33, 42, 47, 53, 59))
q3_autonomy <- survey_clean_finished %>% select(c(1, 4, 5, 12, 34, 38, 43, 48, 54, 58, 63, 35, 39, 44, 50, 56, 60, 64))
