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
set.seed(2022)


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

# q7_q103$Q7 <- ifelse(q7_q103$Q7 == "Yes", 2,1)
# ab <- data.frame()
# q7_q103.4_corr <- replicate(n = 100,expr = bootstrap_corr(q7_q103[,c("Q7","Q103_4")],1000))
# q7_q103.4_corr <- as.data.frame(q7_q103.4_corr)
# ab <- rbind(ab,q7_q103.4_corr)



# Do GCs with many years of preconception experience tend to agree with Q103_1 - Q103_7 more often than GCs with less years of preconception experience? 

## Calculate the correlation coefficient among Q10 and Q103

q10_q103.1_corr <- replicate(n = 100,expr = bootstrap_corr(q10_q103[,c("Q10","Q103_1")],1000))
q10_q103.1_corr <- as.data.frame(q10_q103.1_corr)

q10_q103.2_corr <- replicate(n = 100,expr = bootstrap_corr(q10_q103[,c("Q10","Q103_2")],1000))
q10_q103.2_corr <- as.data.frame(q10_q103.2_corr)

q10_q103.3_corr <- replicate(n = 100,expr = bootstrap_corr(q10_q103[,c("Q10","Q103_3")],1000))
q10_q103.3_corr <- as.data.frame(q10_q103.3_corr)

q10_q103.4_corr <- replicate(n = 100,expr = bootstrap_corr(q10_q103[,c("Q10","Q103_4")],1000))
q10_q103.4_corr <- as.data.frame(q10_q103.4_corr)

q10_q103.5_corr <- replicate(n = 100,expr = bootstrap_corr(q10_q103[,c("Q10","Q103_5")],1000))
q10_q103.5_corr <- as.data.frame(q10_q103.5_corr)

q10_q103.6_corr <- replicate(n = 100,expr = bootstrap_corr(q10_q103[,c("Q10","Q103_6")],1000))
q10_q103.6_corr <- as.data.frame(q10_q103.6_corr)

q10_q103.7_corr <- replicate(n = 100,expr = bootstrap_corr(q10_q103[,c("Q10","Q103_7")],1000))
q10_q103.7_corr <- as.data.frame(q10_q103.7_corr)

### join them together
q10_q103_corr <- cbind(q10_q103.1_corr,q10_q103.2_corr,q10_q103.3_corr,
                       q10_q103.4_corr,q10_q103.5_corr,q10_q103.6_corr,q10_q103.7_corr)



# q10_q103_bootstrap <- data.frame(seq(1:100))
# 
# for (i in 1:7){
#   name1 <- paste0("Q103","_",i)
#   name2 <- paste0("q10_",name1,"_corr")
#   x <- replicate(n = 100,expr = bootstrap_corr(q10_q103[,c("Q10",name1)],1000))
#   assign(name2,x)
#   name2 <- as.data.frame(name2)
#   q10_q103_bootstrap <- cbind(q10_q103_bootstrap,x)  # the problem should be cbind, but I cannot find a better function for this
# }

# Do the GCs that have encountered a patient that requested affected embryo transfer tend to agree with Q103_1 - Q103_7 more often than GCs who have not encountered a request?

q7_q103.1_corr <- replicate(n = 100,expr = bootstrap_corr(q7_q103[,c("Q7","Q103_1")],1000))
q7_q103.1_corr <- as.data.frame(q7_q103.1_corr)

q7_q103.2_corr <- replicate(n = 100,expr = bootstrap_corr(q7_q103[,c("Q7","Q103_2")],1000))
q7_q103.2_corr <- as.data.frame(q7_q103.2_corr)

q7_q103.3_corr <- replicate(n = 100,expr = bootstrap_corr(q7_q103[,c("Q7","Q103_3")],1000))
q7_q103.3_corr <- as.data.frame(q7_q103.3_corr)

q7_q103.4_corr <- replicate(n = 100,expr = bootstrap_corr(q7_q103[,c("Q7","Q103_4")],1000))
q7_q103.4_corr <- as.data.frame(q7_q103.4_corr)

q7_q103.5_corr <- replicate(n = 100,expr = bootstrap_corr(q7_q103[,c("Q7","Q103_5")],1000))
q7_q103.5_corr <- as.data.frame(q7_q103.5_corr)

q7_q103.6_corr <- replicate(n = 100,expr = bootstrap_corr(q7_q103[,c("Q7","Q103_6")],1000))
q7_q103.6_corr <- as.data.frame(q7_q103.6_corr)

q7_q103.7_corr <- replicate(n = 100,expr = bootstrap_corr(q7_q103[,c("Q7","Q103_7")],1000))
q7_q103.7_corr <- as.data.frame(q7_q103.7_corr)

q7_q103_corr <- cbind(q7_q103.1_corr,q7_q103.2_corr,q7_q103.3_corr,
                      q7_q103.4_corr,q7_q103.5_corr,q7_q103.6_corr,q7_q103.7_corr)
q7_q103_corr_mean <- as.data.frame(lapply(q7_q103_corr, FUN = mean))

# Do the GCs that have many years of preconception experience (Q10) tend to have encountered a patient that requested embryo transfer (Q7) more than GCs that have less years of preconception experience?

q7_q10_corr <- replicate(n = 100,expr = bootstrap_corr(q7_q10[,c("Q7","Q10")],1000))
q7_q10_corr <- as.data.frame(q7_q10_corr)

# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with Q103_1 - Q103_7 more often than GCs with less years of patient-facing genetic counseling experience?.

q3_q103 <- survey_clean_finished %>% select(c(1, 4, 5, 12) | starts_with("Q103"))

q3_q103.1_corr <- replicate(n = 100,expr = bootstrap_corr(q3_q103[,c("Q3","Q103_1")],1000))
q3_q103.1_corr <- as.data.frame(q3_q103.1_corr)

q3_q103.2_corr <- replicate(n = 100,expr = bootstrap_corr(q3_q103[,c("Q3","Q103_2")],1000))
q3_q103.2_corr <- as.data.frame(q3_q103.2_corr)

q3_q103.3_corr <- replicate(n = 100,expr = bootstrap_corr(q3_q103[,c("Q3","Q103_3")],1000))
q3_q103.3_corr <- as.data.frame(q3_q103.3_corr)

q3_q103.4_corr <- replicate(n = 100,expr = bootstrap_corr(q3_q103[,c("Q3","Q103_4")],1000))
q3_q103.4_corr <- as.data.frame(q3_q103.4_corr)

q3_q103.5_corr <- replicate(n = 100,expr = bootstrap_corr(q3_q103[,c("Q3","Q103_5")],1000))
q3_q103.5_corr <- as.data.frame(q3_q103.5_corr)

q3_q103.6_corr <- replicate(n = 100,expr = bootstrap_corr(q3_q103[,c("Q3","Q103_6")],1000))
q3_q103.6_corr <- as.data.frame(q3_q103.6_corr)

q3_q103.7_corr <- replicate(n = 100,expr = bootstrap_corr(q3_q103[,c("Q3","Q103_7")],1000))
q3_q103.7_corr <- as.data.frame(q3_q103.7_corr)

q3_q103_corr <- cbind(q3_q103.1_corr,q3_q103.2_corr,q3_q103.3_corr,
                      q3_q103.4_corr,q3_q103.5_corr,q3_q103.6_corr,q3_q103.7_corr)
q3_q103_corr_mean <- as.data.frame(lapply(q3_q103_corr, FUN = mean))


# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with the embryo transfer justification of “quality of life” across conditions (Q24, Q31, 32, 108, 113, 51, 110) more often than GCs with less years of patient-facing genetic counseling experience?

# q3_qol <- survey_clean_finished %>% select(c(1, 4, 5, 12, 32, 37, 41, 46, 52, 57, 62))
###Q24, Q31, 32, 108, 113, 51, 110
q3_q24_corr <- replicate(n = 100,expr = bootstrap_corr(q3_qol[,c("Q3","Q24")],1000))
q3_q24_corr <- as.data.frame(q3_q24_corr)

q3_q31_corr <- replicate(n = 100,expr = bootstrap_corr(q3_qol[,c("Q3","Q31")],1000))
q3_q31_corr <- as.data.frame(q3_q31_corr)

q3_q32_corr <- replicate(n = 100,expr = bootstrap_corr(q3_qol[,c("Q3","Q32")],1000))
q3_q32_corr <- as.data.frame(q3_q32_corr)

q3_q108_corr <- replicate(n = 100,expr = bootstrap_corr(q3_qol[,c("Q3","Q108")],1000))
q3_q108_corr <- as.data.frame(q3_q108_corr)

q3_q113_corr <- replicate(n = 100,expr = bootstrap_corr(q3_qol[,c("Q3","Q113")],1000))
q3_q113_corr <- as.data.frame(q3_q113_corr)

q3_q51_corr <- replicate(n = 100,expr = bootstrap_corr(q3_qol[,c("Q3","Q51")],1000))
q3_q51_corr <- as.data.frame(q3_q51_corr)

q3_q110_corr <- replicate(n = 100,expr = bootstrap_corr(q3_qol[,c("Q3","Q110")],1000))
q3_q110_corr <- as.data.frame(q3_q110_corr)

q3_qol_corr <- cbind(q3_q24_corr,q3_q31_corr,q3_q32_corr,q3_q108_corr,q3_q113_corr,q3_q51_corr,q3_q110_corr)

# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with the embryo transfer justification of “family history” across conditions (Q26, 30, 37, 40, 46, 52, 58) more often than GCs with less years of patient-facing genetic counseling experience?

# q3_fh <- survey_clean_finished %>% select(c(1, 4, 5, 12, 34, 38, 43, 48, 54, 58, 63))
#Q26, 30, 37, 40, 46, 52, 58
q3_q26_corr <- replicate(n = 100,expr = bootstrap_corr(q3_fh[,c("Q3","Q26")],1000))
q3_q26_corr <- as.data.frame(q3_q26_corr)

q3_q30_corr <- replicate(n = 100,expr = bootstrap_corr(q3_fh[,c("Q3","Q30")],1000))
q3_q30_corr <- as.data.frame(q3_q30_corr)

q3_q37_corr <- replicate(n = 100,expr = bootstrap_corr(q3_fh[,c("Q3","Q37")],1000))
q3_q37_corr <- as.data.frame(q3_q37_corr)

q3_q40_corr <- replicate(n = 100,expr = bootstrap_corr(q3_fh[,c("Q3","Q40")],1000))
q3_q40_corr <- as.data.frame(q3_q40_corr)

q3_q46_corr <- replicate(n = 100,expr = bootstrap_corr(q3_fh[,c("Q3","Q46")],1000))
q3_q46_corr <- as.data.frame(q3_q46_corr)

q3_q52_corr <- replicate(n = 100,expr = bootstrap_corr(q3_fh[,c("Q3","Q52")],1000))
q3_q52_corr <- as.data.frame(q3_q52_corr)

q3_q58_corr <- replicate(n = 100,expr = bootstrap_corr(q3_fh[,c("Q3","Q58")],1000))
q3_q58_corr <- as.data.frame(q3_q58_corr)

q3_fh_corr <- cbind(q3_q26_corr,q3_q30_corr,q3_q37_corr,q3_q40_corr,q3_q46_corr,q3_q52_corr,q3_q58_corr)

# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with the embryo transfer justification of “Variability” across conditions (Q23, 27 33, 38, 45, 56) more often than GCs with less years of patient-facing genetic counseling experience?

# Q23, 27 33, 38, 45, 56

q3_q23_corr <- replicate(n = 100,expr = bootstrap_corr(q3_var[,c("Q3","Q23")],1000))
q3_q23_corr <- as.data.frame(q3_q23_corr)

q3_q27_corr <- replicate(n = 100,expr = bootstrap_corr(q3_var[,c("Q3","Q27")],1000))
q3_q27_corr <- as.data.frame(q3_q27_corr)

q3_q33_corr <- replicate(n = 100,expr = bootstrap_corr(q3_var[,c("Q3","Q33")],1000))
q3_q33_corr <- as.data.frame(q3_q33_corr)

q3_q38_corr <- replicate(n = 100,expr = bootstrap_corr(q3_var[,c("Q3","Q38")],1000))
q3_q38_corr <- as.data.frame(q3_q38_corr)

q3_q45_corr <- replicate(n = 100,expr = bootstrap_corr(q3_var[,c("Q3","Q45")],1000))
q3_q45_corr <- as.data.frame(q3_q45_corr)

q3_q56_corr <- replicate(n = 100,expr = bootstrap_corr(q3_var[,c("Q3","Q56")],1000))
q3_q56_corr <- as.data.frame(q3_q56_corr)

q3_var_corr <- cbind(q3_q23_corr,q3_q27_corr,q3_q33_corr,q3_q38_corr,q3_q45_corr,q3_q56_corr)

# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with the embryo transfer justification of “reproductive autonomy” across conditions (Q29, 35, 36, 41, 47, 53, 59) more often than GCs with less years of patient-facing genetic counseling experience?

#Q29, 35, 36, 41, 47, 53, 59
# q3_repauto <- survey_clean_finished %>% select(c(1, 4, 5, 12, 35, 39, 44, 50, 56, 60, 64))

q3_q29_corr <- replicate(n = 100,expr = bootstrap_corr(q3_repauto[,c("Q3","Q29")],1000))
q3_q29_corr <- as.data.frame(q3_q29_corr)

q3_q35_corr <- replicate(n = 100,expr = bootstrap_corr(q3_repauto[,c("Q3","Q35")],1000))
q3_q35_corr <- as.data.frame(q3_q35_corr)

q3_q41_corr <- replicate(n = 100,expr = bootstrap_corr(q3_repauto[,c("Q3","Q41")],1000))
q3_q41_corr <- as.data.frame(q3_q41_corr)

q3_q47_corr <- replicate(n = 100,expr = bootstrap_corr(q3_repauto[,c("Q3","Q47")],1000))
q3_q47_corr <- as.data.frame(q3_q47_corr)

q3_q53_corr <- replicate(n = 100,expr = bootstrap_corr(q3_repauto[,c("Q3","Q53")],1000))
q3_q53_corr <- as.data.frame(q3_q53_corr)

q3_q59_corr <- replicate(n = 100,expr = bootstrap_corr(q3_repauto[,c("Q3","Q59")],1000))
q3_q59_corr <- as.data.frame(q3_q59_corr)

q3_repauto_corr <- cbind(q3_q29_corr,q3_q35_corr,q3_q41_corr,q3_q47_corr,q3_q53_corr,q3_q59_corr)

# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with the embryo transfer justification of “Resources available” across conditions (Q25, 107, 42, 109, 54) more often than GCs with less years of patient-facing genetic counseling experience? 


q3_resava <- survey_clean_finished %>% select(c(1, 4, 5, 12, 33, 42, 47, 53, 59))
#Q25, 107, 42, 109, 54

q3_q25_corr <- replicate(n = 100,expr = bootstrap_corr(q3_resava[,c("Q3","Q25")],1000))
q3_q25_corr <- as.data.frame(q3_q25_corr)

q3_q107_corr <- replicate(n = 100,expr = bootstrap_corr(q3_resava[,c("Q3","Q107")],1000))
q3_q107_corr <- as.data.frame(q3_q107_corr)

q3_q42_corr <- replicate(n = 100,expr = bootstrap_corr(q3_resava[,c("Q3","Q42")],1000))
q3_q42_corr <- as.data.frame(q3_q42_corr)

q3_q109_corr <- replicate(n = 100,expr = bootstrap_corr(q3_resava[,c("Q3","Q109")],1000))
q3_q109_corr <- as.data.frame(q3_q109_corr)

q3_q54_corr <- replicate(n = 100,expr = bootstrap_corr(q3_resava[,c("Q3","Q54")],1000))
q3_q54_corr <- as.data.frame(q3_q54_corr)

q3_resava_corr <- cbind(q3_q25_corr, q3_q107_corr, q3_q42_corr,
                        q3_q109_corr, q3_q54_corr)
# join the corr all together with 100 bootstrap corr coefficients for each and the last row is the mean of every first 100 value
corr_matrix <- cbind(q10_q103_corr, q7_q103_corr, q3_q103_corr, q3_qol_corr, q3_fh_corr, q3_var_corr,q3_repauto_corr)
corr_matrix_mean <- rbind(corr_matrix, as.data.frame(lapply(corr_matrix, FUN = mean)))
write.csv(corr_matrix_mean,file = "corr_matrix.csv")



# make some visualization of the correlation coefficients
## q10_q103_corr, q7_q103_corr, q3_q103_corr, q3_qol_corr, q3_fh_corr, q3_var_corr,q3_repauto_corr
boxplot_corr <- function(df_corr){
  df_corr_longer <- df_corr %>%
    pivot_longer(cols = c(1:ncol(df_corr)),
                 names_to = "question_num",
                 values_to = "corr_value")
  
  ggplot(data = df_corr_longer,mapping = aes(x=question_num,y=corr_value)) +
    geom_boxplot() +
    geom_hline(aes(yintercept=0), colour="#990000", linetype="solid")+
    geom_hline(aes(yintercept=0.05), colour="#990000", linetype="dashed")+
    geom_hline(aes(yintercept=-0.05), colour="#990000", linetype="dashed") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

boxplot_corr_1 <- function(df_corr){
  df_corr_longer <- df_corr %>%
    pivot_longer(cols = c(1:ncol(df_corr)),
                 names_to = "question_num",
                 values_to = "corr_value")
  
  ggplot(data = df_corr_longer,mapping = aes(x=question_num,y=corr_value)) +
    geom_boxplot()
}
##----------------boxplot
boxplot_corr(q10_q103_corr)

boxplot_corr(q7_q103_corr)

boxplot_corr_1(q7_q10_corr)

boxplot_corr(q3_q103_corr)

boxplot_corr(q3_qol_corr)

boxplot_corr(q3_fh_corr)

boxplot_corr(q3_var_corr)

boxplot_corr(q3_repauto_corr)

boxplot_corr(q3_resava_corr)
# lapply(q10_q103_corr,FUN = fivenum)


# boxplot(q10_q103_corr, main = "Correlation")
# abline(h=0,col = "red")
# abline(h=0.05,col = "red",lty = 3)
# abline(h=-0.05,col = "red",lty = 3)

##---------------read the large bootstrap dataset and draw the 95% interval

### get the 0.5, 0.975, 0.025 quantile of one list of data
interval_95 <- function(df1){
  q1 <- quantile(df1,c(0.5,0.975,0.025))
  
}

###test
# df1 <- read.csv("bootstrap_result/q7_q10_corr.csv", header = T)
# df1 <- select(df1,c(-1))
# 
# df1_data <- df1 %>% lapply(FUN = interval_95) %>% as.data.frame()
# 
# df1_data$quantile <- rownames(df1_data)
# df_corr_longer <- df1_data %>%
#   pivot_longer(cols = c(1:(ncol(df1_data)-1)),
#                names_to = "question_num",
#                values_to = "corr_value")
# 
# df_corr_wider <- df_corr_longer %>% 
#   pivot_wider(names_from = "quantile",values_from = "corr_value")
# 
# colnames(df_corr_wider) <- c("question_num","median","up","low")
# 
# 
# 
# 
# 
# ggplot(df_corr_wider, aes(x = question_num, y = median)) + geom_point() + 
#   geom_errorbar(aes(ymin = low, ymax = up))


interval_95_plot <- function(qnum = "q7_q10", baseline = T){
  file_path <- paste0("bootstrap_result","/",qnum,"_","corr.csv")
  df1 <- read.csv(file = file_path, header = T)
  
  df1 <- select(df1,c(-1))
  
  df1_data <- df1 %>% lapply(FUN = interval_95) %>% as.data.frame()
  
  df1_data$quantile <- rownames(df1_data)
  df_corr_longer <- df1_data %>%
    pivot_longer(cols = c(1:(ncol(df1_data)-1)),
                 names_to = "question_num",
                 values_to = "corr_value")
  
  df_corr_wider <- df_corr_longer %>% 
    pivot_wider(names_from = "quantile",values_from = "corr_value")
  
  colnames(df_corr_wider) <- c("question_num","median","up","low")
  
  
  
  
  if (baseline == T){
    ggplot(df_corr_wider, aes(x = question_num, y = median)) + geom_point() + 
      geom_errorbar(aes(ymin = low, ymax = up)) +
      geom_hline(aes(yintercept=0), colour="#990000", linetype="solid")+
      geom_hline(aes(yintercept=0.05), colour="#990000", linetype="dashed")+
      geom_hline(aes(yintercept=-0.05), colour="#990000", linetype="dashed") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  else{
    ggplot(df_corr_wider, aes(x = question_num, y = median)) + geom_point() + 
      geom_errorbar(aes(ymin = low, ymax = up))
  }
  
}

interval_95_plot("q10_q103")

interval_95_plot("q7_q103")

interval_95_plot("q7_q10",baseline = F)

interval_95_plot("q3_q103")

interval_95_plot("q3_qol")

interval_95_plot("q3_fh")

interval_95_plot("q3_var")

interval_95_plot("q3_repauto")

interval_95_plot("q3_resava")
