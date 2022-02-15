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
x = data.frame(a = seq(1:100), b = sample(x = 1:100))
a <- replicate(n = 100,expr = bootstrap_corr(x,80))
mean(a)

##------------
q7_q103$Q7 <- ifelse(q7_q103$Q7 == 1, 1,2)

a <- replicate(n = 100,expr = bootstrap_corr(q7_q103[,c("Q7","Q103_4")],1000))
mean(a)
