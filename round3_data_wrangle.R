source("clean_and_wrangle_data.R")
library("ggplot2")
library("scales")
library("dplyr")

# make a function to create figures of barplot:

survey_barplot <- function(df_question, plot_type = "dodge", size = 5){
  
  if (plot_type == "dodge"){
    if (ncol(df_question %>% select(starts_with("Var"))) == 1){
      ggplot(df_question, aes(x = Var1, y = proportion, fill = Var1)) + 
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = percent(proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = size)
    }else{
      ggplot(df_question, aes(x = Var1, y = proportion, fill = Var2)) + 
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = percent(proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = size)
    }
    
  }
  else if(plot_type == "stack"){
    ggplot(df_question, aes(x = Var1, y = proportion, fill = Var2))+
      geom_bar(stat = "identity", position = "stack")+
      geom_text(aes(label = percent(proportion,0.01)), position = position_stack(), vjust = 1, size = size)
    }
  
}
# demo
df_question <- survey_clean_finished %>% select(Q13)


# make a function to wrangle the data

wrangle_survey <- function(df_1, question_series = "", name = ""){
  # df_2 <- df_1 %>% select(question_series)
  df_2 <- df_1 %>% 
    pivot_longer(everything(), names_to = "question_num", values_to = "Response")
  df_3 <- df_2 %>% 
    as.data.frame(table(df_2$question_num,df_2$Response)) 
  df_3 <- df_3 %>% mutate(proportion = df_3["Freq"]/sum(df_3["Freq"]))
  
  return(df_3)
}

# Do all the respondents tend to agree with these questions/justifications?
# 1. Q13
q13 <- survey_clean_finished %>% select(c(1, 4, 5, 23))
q13_agree <- data.frame(table(q13$Q13)) %>% mutate(proportion = Freq/sum(Freq))

survey_barplot(q13_agree)

# 2. Q103_1 - Q103_7
q103 <- survey_clean_finished %>% select(c(24:30))

q103 <- q103  %>%
  pivot_longer(everything(), names_to = "question_num", values_to = "Response")

q103_visual <- as.data.frame(table(q103$question_num,q103$Response)) %>% mutate(proportion = Freq/99)


survey_barplot(q103_visual, plot_type = "dodge", size = 2)
survey_barplot(q103_visual, plot_type = "stack", size = 3)


# 3. Morally uneasy table: Q102_1-11
q102 <- survey_clean_finished %>% select(c(1) | starts_with("Q102"))


q102 <- q102  %>%
  pivot_longer(-c(IPAddress), names_to = "question_num", values_to = "Response")

q102_visual <- as.data.frame(table(q102$question_num,q102$Response)) %>% mutate(proportion = Freq/99)

survey_barplot(q102_visual, plot_type = "dodge", size = 2)
survey_barplot(q102_visual, plot_type = "stack", size = 3)

# ggplot(q102_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "dodge")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size =2)
# 
# 
# ggplot(q102_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "stack")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_stack(), vjust = 1,size = 3)

# Descriptive analysis organized by conditions:
# Trisomy 21: Q23 Q24 Q25 Q26 Q29

t_21 <- survey_clean_finished %>% select(c(1, 31:35))

t_21 <- t_21  %>%
  pivot_longer(-c(IPAddress), names_to = "question_num", values_to = "Response")

t_21_visual <- as.data.frame(table(t_21$question_num,t_21$Response)) %>% mutate(proportion = Freq/99)

survey_barplot(t_21_visual, plot_type = "dodge", size = 2)
survey_barplot(t_21_visual, plot_type = "stack", size = 3)

# ggplot(t_21_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "dodge")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = 2)
# 
# ggplot(t_21_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "stack")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_stack(0.5), vjust = 1.5, size = 3)

# X-linked Alport female: Q27 Q31 Q30 Q35

x_alport_female <- survey_clean_finished %>% select(c(1, 36:39))

x_alport_female <- x_alport_female  %>%
  pivot_longer(-c(IPAddress), names_to = "question_num", values_to = "Response")

x_alport_female_visual <- as.data.frame(table(x_alport_female$question_num,x_alport_female$Response)) %>% mutate(proportion = Freq/99)


survey_barplot(t_21_visual, plot_type = "dodge", size = 2)
survey_barplot(t_21_visual, plot_type = "stack", size = 3)

# ggplot(x_alport_female_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "dodge")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = 3)
# 
# ggplot(x_alport_female_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "stack")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_stack(0.5), vjust = 1.5, size = 3)




# X-linked Alport male: Q33 Q32 Q107 Q37 Q36

x_alport_male <- survey_clean_finished %>% select(c(1, 40:44))

x_alport_male <- x_alport_male  %>%
  pivot_longer(-c(IPAddress), names_to = "question_num", values_to = "Response")

x_alport_male_visual <- as.data.frame(table(x_alport_male$question_num,x_alport_male$Response)) %>% mutate(proportion = Freq/99)


survey_barplot(x_alport_male_visual, plot_type = "dodge", size = 2)
survey_barplot(x_alport_male_visual, plot_type = "stack", size = 3)

# ggplot(x_alport_male_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "dodge")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5,size =3)
# 
# ggplot(x_alport_male_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "stack")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_stack(0.5), vjust = 1.5, size = 3)
#   

# BRCA1: Q38 Q108 Q42 Q40 Q43

brca1 <- survey_clean_finished %>% select(c(1, 45:49))

brca1 <- brca1  %>%
  pivot_longer(-c(IPAddress), names_to = "question_num", values_to = "Response")

brca1_visual <- as.data.frame(table(brca1$question_num,brca1$Response)) %>% mutate(proportion = Freq/99)

survey_barplot(brca1_visual, plot_type = "dodge", size = 2)
survey_barplot(brca1_visual, plot_type = "stack", size = 3)

# ggplot(brca1_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "dodge")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = 3)
# 
# ggplot(brca1_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "stack")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_stack(0.5), vjust = 1.5, size =3)


# FAP: Q45 Q113 Q109 Q46 Q49 Q47

fap <- survey_clean_finished %>% select(c(1, 51:56))

fap <- fap  %>%
  pivot_longer(-c(IPAddress), names_to = "question_num", values_to = "Response")

fap_visual <- as.data.frame(table(fap$question_num,fap$Response)) %>% mutate(proportion = Freq/99)

survey_barplot(fap_visual, plot_type = "dodge", size = 2)
survey_barplot(fap_visual, plot_type = "stack", size = 3)

# ggplot(fap_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "dodge")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = 3)
# 
# ggplot(fap_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "stack")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_stack(0.5), vjust = 1.5,size =3)


# GJB2: Q51 Q52 Q54 Q53


gjb2 <- survey_clean_finished %>% select(c(1, 57:60))

gjb2 <- gjb2  %>%
  pivot_longer(-c(IPAddress), names_to = "question_num", values_to = "Response")

gjb2_visual <- as.data.frame(table(gjb2$question_num,gjb2$Response)) %>% mutate(proportion = Freq/99)

survey_barplot(gjb2_visual, plot_type = "dodge", size = 2)
survey_barplot(gjb2_visual, plot_type = "stack", size = 3)

# ggplot(gjb2_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "dodge")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = 3)
# 
# ggplot(gjb2_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "stack")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_stack(0.5), vjust = 1.5, size = 3)


# Huntington’s: Q56 Q110 Q58 Q59

ht <- survey_clean_finished %>% select(c(1, 61:64))

ht <- ht  %>%
  pivot_longer(-c(IPAddress), names_to = "question_num", values_to = "Response")

ht_visual <- as.data.frame(table(ht$question_num,ht$Response)) %>% mutate(proportion = Freq/99)


survey_barplot(ht_visual, plot_type = "dodge", size = 2)
survey_barplot(ht_visual, plot_type = "stack", size = 3)
# ggplot(ht_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "dodge")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = 3)
# 
# ggplot(ht_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "stack")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_stack(), vjust = 1.5, size = 3)


# Descriptive analysis organized by justifications:
# Quality of life: (Q24, Q31, 32, 108, 113, 51, 110)

qol <- survey_clean_finished %>% select(c(1, 32, 37, 41, 46, 52, 57, 62))

qol <- qol  %>%
  pivot_longer(-c(IPAddress), names_to = "question_num", values_to = "Response")

qol_visual <- as.data.frame(table(qol$question_num,qol$Response)) %>% mutate(proportion = Freq/99)


survey_barplot(qol_visual, plot_type = "dodge", size = 2)
survey_barplot(qol_visual, plot_type = "stack", size = 3)

# ggplot(qol_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "dodge")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = 3)
# 
# ggplot(qol_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "stack")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_stack(0.5), vjust = 1.5,size =3)



# Family history: (Q26, 30, 37, 40, 46, 52, 58)

fh <- survey_clean_finished %>% select(c(1, 34, 38, 43, 48, 54, 58, 63))

fh <- fh  %>%
  pivot_longer(-c(IPAddress), names_to = "question_num", values_to = "Response")

fh_visual <- as.data.frame(table(fh$question_num,fh$Response)) %>% mutate(proportion = Freq/99)

survey_barplot(fh_visual, plot_type = "dodge", size = 2)
survey_barplot(fh_visual, plot_type = "stack", size = 3)
# ggplot(fh_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "dodge")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = 3)
# 
# ggplot(fh_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "stack")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_stack(0.5), vjust = 1.5, size = 3)


# Variability: (Q23, 27 33, 38, 45, 56)

var <- survey_clean_finished %>% select(c(1, 31, 36, 40, 45, 51, 61)) 
  
var <- var  %>%
  pivot_longer(-c(IPAddress), names_to = "question_num", values_to = "Response")

var_visual <- as.data.frame(table(var$question_num,var$Response)) %>% mutate(proportion = Freq/99)

survey_barplot(var_visual, plot_type = "dodge", size = 2)
survey_barplot(var_visual, plot_type = "stack", size = 3)
# ggplot(var_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "dodge")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = 3)
# 
# ggplot(var_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "stack")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_stack(0.5), vjust = 1.5, size = 3)
  
# Reproductive autonomy: (Q23, 27 33, 38, 45, 56)

repauto <- survey_clean_finished %>% select(c(1, 35, 39, 44, 50, 56, 60, 64))

repauto <- repauto  %>%
  pivot_longer(-c(IPAddress), names_to = "question_num", values_to = "Response")

repauto_visual <- as.data.frame(table(repauto$question_num,repauto$Response)) %>% mutate(proportion = Freq/99)

survey_barplot(repauto_visual, plot_type = "dodge", size = 2)
survey_barplot(repauto_visual, plot_type = "stack", size = 3)
# ggplot(repauto_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "dodge")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5,size =3)
# 
# ggplot(repauto_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "stack")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_stack(0.5), vjust = 1.5, size = 3)



# Resources available: (Q25, 107, 42, 109, 54)

resava <- survey_clean_finished %>% select(c(1, 33, 42, 47, 53, 59))

resava <- resava  %>%
  pivot_longer(-c(IPAddress), names_to = "question_num", values_to = "Response")

resava_visual <- as.data.frame(table(resava$question_num,resava$Response)) %>% mutate(proportion = Freq/99)

survey_barplot(resava_visual, plot_type = "dodge", size = 2)
survey_barplot(resava_visual, plot_type = "stack", size = 3)
# ggplot(resava_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "dodge")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = 3)
# 
# ggplot(resava_visual, aes(x = Var1, y = proportion, fill = Var2))+
#   geom_bar(stat = "identity", position = "stack")+
#   geom_text(aes(label = percent(proportion,0.01)), position = position_stack(0.5), vjust = 1.5, size = 3)


# Do GCs who said YES to Q7 tend to agree with Q13, Q103_1 - Q103_7 more often than GCs who answered NO?

q7_q13_q103 <- survey_clean_finished %>% select(c(1, 4, 5, 21, 23) | starts_with("Q103"))

q7_q13_q103_visual <- q7_q13_q103 %>% select(c(1,4:12))
q7_q13_q103_table <- q7_q13_q103_visual %>%
  pivot_longer(-c(IPAddress,Q7), names_to = "question_num", values_to = "Response")

q7_q13_q103_yes <- q7_q13_q103_table %>% filter(Q7 == "Yes")

q7_q13_q103_yes_agree_prop <- as.data.frame(table(q7_q13_q103_yes$question_num, q7_q13_q103_yes$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  # mutate("1" = 0) %>%
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/22) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q7 = "Yes")

q7_q13_q103_no <- q7_q13_q103_table %>% filter(Q7 == "No")

q7_q13_q103_no_agree_prop <- as.data.frame(table(q7_q13_q103_no$question_num, q7_q13_q103_no$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/77) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q7 = "No")

q7_q13_q103_agree_prop <- rbind(q7_q13_q103_yes_agree_prop, q7_q13_q103_no_agree_prop)

ggplot(q7_q13_q103_agree_prop, aes(x = question_num, y = agree_proportion, fill = Q7))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = percent(agree_proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5)

ggplot(q7_q13_q103_agree_prop, aes(x = question_num, y = agree_proportion, fill = Q7))+
  geom_bar(stat = "identity", position = "stack")+
  geom_text(aes(label = percent(agree_proportion,0.01)), position = position_stack(), vjust = 1.5)


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
