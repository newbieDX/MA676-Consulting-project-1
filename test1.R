library(readxl)
library(tidyverse)
library(ggplot2)

survey_result <- read_xlsx(path = "stats consult data analysis capstone sheet.xlsx")

survey_count <- survey_result[-1,]

survey_clean <- survey_count %>% select(c(4,5,7,14,15,17:ncol(survey_count)))
survey_clean_finished <- survey_clean %>% filter(Finished == "True" & Q1 == "Yes")

# select the questions we care about
q102 <- survey_clean_finished %>% select(starts_with("Q102"))

q103 <- survey_clean_finished %>% select(starts_with("Q103"))



survey_result_q102 <- survey_result[1,] %>%
  select(starts_with("Q102")) %>% 
  pivot_longer(names_to = "question_num", cols = starts_with("Q102"))

# get the short term of each question
survey_result_q102$value <- gsub(".*: - ", "", survey_result_q102$value)


# visualization
ggplot(data = q102, mapping = aes(x=as.factor()))




