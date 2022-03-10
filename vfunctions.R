source("clean_and_wrangle_data.R")
library("ggplot2")

# Do GCs with many years of preconception experience tend to agree with Q103_1 - Q103_7 more often than GCs with less years of preconception experience? 

q10_q103_visual <- q10_q103 %>% select(c(1,4:11))
q10_q103_table <- q10_q103_visual %>%
  pivot_longer(-c(IPAddress,Q10), names_to = "question_num", values_to = "Response")

q10_q103_visual_f <- function(qnum){
  df <- q10_q103_table %>% filter(question_num == qnum)
  
  ggplot(df, aes(x = Response, fill = Q10))+
    geom_bar(stat = "count", position = "stack")+
    facet_grid(~Q10)
}


# Relatively high corr qnum: q10 with q103_2, q103_3, q103_4


# q10_q103_visual_f("Q103_4")

# ggplot(data = q10_q103_table, aes(x = Response, 
#                           y = prop.table(stat(count)), 
#                           fill = question_num,
#                           label = scales::percent(prop.table(stat(count))))) +
#   geom_bar(position = "stack") + 
#   geom_text(stat = "count",
#             position = "stack",
#             vjust = -0.5, 
#             size = 3) + 
#   scale_y_continuous(labels = scales::percent) + 
#   labs(x = 'Response', y = 'pct', fill = 'question_num')+
#   facet_grid(~Q10)

# Do the GCs that have encountered a patient that requested affected embryo transfer tend to agree with Q103_1 - Q103_7 more often than GCs who have not encountered a request?

q7_q103_visual <- q7_q103 %>% select(c(1,4:11))
q7_q103_table <- q7_q103_visual %>%
  pivot_longer(-c(IPAddress,Q7), names_to = "question_num", values_to = "Response")

q7_q103_visual_f <- function(qnum){
  df <- q7_q103_table %>% filter(question_num == qnum)
  
  ggplot(df, aes(x = Response, fill = Q7))+
    geom_bar(stat = "count", position = "stack")+
    facet_grid(~Q7)
}

# ggplot(q7_q103_table, aes(x = Response, fill = question_num))+
#   geom_bar(stat = "count", position = "stack")+
#   facet_grid(~Q7)

# Relatively high corr qnum: q7 with q103_6

# q7_q103_visual_f("Q103_6")


# Do the GCs that have many years of preconception experience (Q10) tend to have encountered a patient that requested embryo transfer (Q7) more than GCs that have less years of preconception experience?

q7_q10_visual <- q7_q10 %>% select(c(1,4,5))
q7_q10_table <- q7_q10_visual %>%
  pivot_longer(-c(IPAddress,Q10), names_to = "question_num", values_to = "Response")

q7_q10_visual_plot <- ggplot(q7_q10_table, aes(x = Response, fill = Response))+
  geom_bar(stat = "count", position = "stack")+
  facet_grid(~Q10)

# Positive corrlation between Q7 and Q10

# q7_q10_visual_plot

# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with Q103_1 - Q103_7 more often than GCs with less years of patient-facing genetic counseling experience? 

q3_q103_visual <- q3_q103 %>% select(c(1,4:11))
q3_q103_table <- q3_q103_visual %>%
  pivot_longer(-c(IPAddress,Q3), names_to = "question_num", values_to = "Response")

q3_q103_visual_f <- function(qnum){
  df <- q3_q103_table %>% filter(question_num == qnum)
  
  ggplot(df, aes(x = Response, fill = Q3))+
    geom_bar(stat = "count", position = "stack")+
    facet_grid(~Q3)
}

# ggplot(q3_q103_table, aes(x = Response, fill = question_num))+
#   geom_bar(stat = "count", position = "stack")+
#   facet_grid(~Q3)


# Relatively high negative corr qnum: q3 with q103_3 
# Relatively high positive corr qnum: q3 with q103_5
# q3_q103_visual_f("Q103_3")

# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with the embryo transfer justification of “quality of life” across conditions (Q24, Q31, 32, 108, 113, 51, 110) more often than GCs with less years of patient-facing genetic counseling experience? 


q3_qol_visual <- q3_qol %>% select(c(1,4:11))
q3_qol_table <- q3_qol_visual %>%
  pivot_longer(-c(IPAddress,Q3), names_to = "question_num", values_to = "Response")

q3_qol_visual_f <- function(qnum){
  df <- q3_qol_table %>% filter(question_num == qnum)
  
  ggplot(df, aes(x = Response, fill = Q3))+
    geom_bar(stat = "count", position = "stack")+
    facet_grid(~Q3)
}


# ggplot(q3_qol_table, aes(x = Response, fill = question_num))+
#   geom_bar(stat = "count", position = "stack")+
#   facet_grid(~Q3)

# q3_qol_visual_f("Q108")

# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with the embryo transfer justification of “family history” across conditions (Q26, 30, 37, 40, 46, 52, 58) more often than GCs with less years of patient-facing genetic counseling experience? 

q3_fh_visual <- q3_fh %>% select(c(1,4:11))
q3_fh_table <- q3_fh_visual %>%
  pivot_longer(-c(IPAddress,Q3), names_to = "question_num", values_to = "Response")

q3_fh_visual_f <- function(qnum){
  df <- q3_fh_table %>% filter(question_num == qnum)
  
  ggplot(df, aes(x = Response, fill = Q3))+
    geom_bar(stat = "count", position = "stack")+
    facet_grid(~Q3)
}

# ggplot(q3_fh_table, aes(x = Response, fill = question_num))+
#   geom_bar(stat = "count", position = "stack")+
#   facet_grid(~Q3)
# 
# 
# q3_fh_visual_f("Q37")



# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with the embryo transfer justification of “Variability” across conditions (Q23, 27 33, 38, 45, 56) more often than GCs with less years of patient-facing genetic counseling experience? 

q3_var_visual <- q3_var %>% select(c(1,4:10))
q3_var_table <- q3_var_visual %>%
  pivot_longer(-c(IPAddress,Q3), names_to = "question_num", values_to = "Response")

q3_var_visual_f <- function(qnum){
  df <- q3_var_table %>% filter(question_num == qnum)
  
  ggplot(df, aes(x = Response, fill = Q3))+
    geom_bar(stat = "count", position = "stack")+
    facet_grid(~Q3)
}



# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with the embryo transfer justification of “reproductive autonomy” across conditions (Q29, 35, 36, 41, 47, 53, 59) more often than GCs with less years of patient-facing genetic counseling experience?

q3_repauto_visual <- q3_repauto %>% select(c(1,4:11))
q3_repauto_table <- q3_repauto_visual %>%
  pivot_longer(-c(IPAddress,Q3), names_to = "question_num", values_to = "Response")

q3_repauto_visual_f <- function(qnum){
  df <- q3_repauto_table %>% filter(question_num == qnum)
  
  ggplot(df, aes(x = Response, fill = Q3))+
    geom_bar(stat = "count", position = "stack")+
    facet_grid(~Q3)
}



# ggplot(q3_repauto_table, aes(x = Response, fill = question_num))+
#   geom_bar(stat = "count", position = "stack")+
#   facet_grid(~Q3)
# 
# q3_repauto_visual_f("Q47")

# Do GCs with many years of patient-facing genetic counseling experience (Q3) tend to agree with the embryo transfer justification of “Resources available” across conditions (Q25, 107, 42, 109, 54) more often than GCs with less years of patient-facing genetic counseling experience? 

q3_resava_visual <- q3_resava %>% select(c(1,4:9))
q3_resava_table <- q3_resava_visual %>%
  pivot_longer(-c(IPAddress,Q3), names_to = "question_num", values_to = "Response")

q3_resava_visual_f <- function(qnum){
  df <- q3_resava_table %>% filter(question_num == qnum)
  
  ggplot(df, aes(x = Response, fill = Q3))+
    geom_bar(stat = "count", position = "stack")+
    facet_grid(~Q3)
}


# ggplot(q3_resava_table, aes(x = Response, fill = question_num))+
#   geom_bar(stat = "count", position = "stack")+
#   facet_grid(~Q3)
# 
# 
# q3_resava_visual_f("Q54")

# Do the GCs tend to feel more morally uneasy with transferring affected embryos for “BRCA1 Cancer group” more often than the group “Renal Alport”?

BRCA1_cancer_visual <- BRCA1_cancer %>% select(c(1,4,5))
BRCA1_cancer_table <- BRCA1_cancer_visual %>%
  pivot_longer(-c(IPAddress), names_to = "question_num_1", values_to = "Response")

renal_alport_visual<- renal_alport %>% select(c(1,4,5))
renal_alport_table <- renal_alport_visual %>%
  pivot_longer(-c(IPAddress), names_to = "question_num_2", values_to = "Response")

ggplot(renal_alport_table, aes(x = Response, fill = question_num_2))+
  geom_bar(stat = "count", position = "dodge")

ggplot(BRCA1_cancer_table, aes(x = Response, fill = question_num_1))+
  geom_bar(stat = "count", position = "dodge")
