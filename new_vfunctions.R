source("clean_and_wrangle_data.R")
library("ggplot2")
library("scales")

# Do GCs who answered YES to Q7 tend to agree with the embryo transfer justification of “quality of life” across conditions (Q24, Q31, Q32, Q108, Q113, Q51, Q110) more often than GCs who answered NO to Q7? 

q7_qol_visual <- q7_qol %>% select(c(1,4:11))
q7_qol_table <- q7_qol_visual %>%
  pivot_longer(-c(IPAddress,Q7), names_to = "question_num", values_to = "Response")

q7_qol_visual_f <- function(qnum){
  df <- q7_qol_table %>% filter(question_num == qnum)
  
  ggplot(df, aes(x = Response, fill = Q7))+
    geom_bar(stat = "count", position = "stack")+
    facet_grid(~Q7)
}

# q7_qol_visual_f("Q108")

q7_qol_general <- ggplot(q7_qol_table, aes(x = Response, fill = question_num))+
  geom_bar(stat = "count", position = "dodge")+
  facet_grid(~Q7)

# who answer yes to q7 and quality of life
q7_qol_yes_prop_visual_f <- function(cleaned_table){
  yes <- cleaned_table %>% filter(Q7 == "Yes")

  yes_prop <- as.data.frame(table(yes$question_num, yes$Response)) %>% 
    pivot_wider(names_from = Var2, values_from = Freq)%>% 
    mutate("1" = 0) %>% 
    pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
    mutate(proportion = Freq/22)
  
  ggplot(yes_prop, aes(x = Response, y = proportion, fill = Response))+
    geom_bar(stat = "identity", position = "dodge")+
    geom_text(aes(label = percent(proportion,0.01)), vjust = 0)+
    facet_grid(~Var1)
}

# q7_qol_yes_prop_visual_f(q7_qol_table)

# who answer no to q7 and quality of life
q7_qol_no_prop_visual_f <- function(cleaned_table){
  no <- cleaned_table %>% filter(Q7 == "No")
  
  no_prop <- as.data.frame(table(no$question_num, no$Response)) %>% 
    pivot_wider(names_from = Var2, values_from = Freq)%>% 
    pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>% 
    mutate(proportion = Freq/77)

  ggplot(no_prop, aes(x = Response, y = proportion, fill = Response))+
    geom_bar(stat = "identity", position = "dodge")+
    geom_text(aes(label = percent(proportion,0.01)), vjust = 0)+
    facet_grid(~Var1)
}

# q7_qol_no_prop_visual_f(q7_qol_table)

# Do GCs who answered YES to Q7 tend to agree with the embryo transfer justification of “family history” across conditions (Q26, Q30, Q37, Q40, Q46, Q52, Q58) more often than Do GCs who answered NO to Q7 ?

q7_fh_visual <- q7_fh %>% select(c(1,4:11))
q7_fh_table <- q7_fh_visual %>%
  pivot_longer(-c(IPAddress,Q7), names_to = "question_num", values_to = "Response")

q7_fh_visual_f <- function(qnum){
  df <- q7_fh_table %>% filter(question_num == qnum)
  
  ggplot(df, aes(x = Response, fill = Q7))+
    geom_bar(stat = "count", position = "stack")+
    facet_grid(~Q7)
}

q7_fh_general <- ggplot(q7_fh_table, aes(x = Response, fill = question_num))+
  geom_bar(stat = "count", position = "dodge")+
  facet_grid(~Q7)


# q7_fh_visual_f("Q37")
  

# who answer yes to q7 and family history
q7_fh_yes_prop_visual_f <- function(cleaned_table){
  yes <- cleaned_table %>% filter(Q7 == "Yes")
  
  yes_prop <- as.data.frame(table(yes$question_num, yes$Response)) %>% 
    pivot_wider(names_from = Var2, values_from = Freq)%>% 
    mutate("1" = 0) %>% 
    pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
    mutate(proportion = Freq/22)
  
  ggplot(yes_prop, aes(x = Response, y = proportion, fill = Response))+
    geom_bar(stat = "identity", position = "dodge")+
    geom_text(aes(label = percent(proportion,0.01)), vjust = 0)+
    facet_grid(~Var1)
}

# q7_fh_yes_prop_visual_f(q7_fh_table)

# who answer no to q7 and family history
q7_fh_no_prop_visual_f <- function(cleaned_table){
  no <- cleaned_table %>% filter(Q7 == "No")
  
  no_prop <- as.data.frame(table(no$question_num, no$Response)) %>% 
    pivot_wider(names_from = Var2, values_from = Freq)%>% 
    pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>% 
    mutate(proportion = Freq/77)
  
  ggplot(no_prop, aes(x = Response, y = proportion, fill = Response))+
    geom_bar(stat = "identity", position = "dodge")+
    geom_text(aes(label = percent(proportion,0.01)), vjust = 0)+
    facet_grid(~Var1)
}

# q7_fh_no_prop_visual_f(q7_fh_table)


# Do GCs who answered YES to Q7 tend to agree with the embryo transfer justification of “Variability” across conditions (Q23, Q27, Q33, Q38, Q45, Q56) more often than GCs who answered NO to Q7?

q7_var_visual <- q7_var %>% select(c(1,4:10))
q7_var_table <- q7_var_visual %>%
  pivot_longer(-c(IPAddress,Q7), names_to = "question_num", values_to = "Response")

q7_var_visual_f <- function(qnum){
  df <- q7_var_table %>% filter(question_num == qnum)
  
  ggplot(df, aes(x = Response, fill = Q7))+
    geom_bar(stat = "count", position = "stack")+
    facet_grid(~Q7)
}

# q7_var_visual_f("Q23")

q7_var_general <- ggplot(q7_var_table, aes(x = Response, fill = question_num))+
  geom_bar(stat = "count", position = "dodge")+
  facet_grid(~Q7)


# who answer yes to q7 and variability
q7_var_yes_prop_visual_f <- function(cleaned_table){
  yes <- cleaned_table %>% filter(Q7 == "Yes")
  
  yes_prop <- as.data.frame(table(yes$question_num, yes$Response)) %>% 
    pivot_wider(names_from = Var2, values_from = Freq)%>% 
    # mutate("1" = 0) %>% 
    pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
    mutate(proportion = Freq/22)
  
  ggplot(yes_prop, aes(x = Response, y = proportion, fill = Response))+
    geom_bar(stat = "identity", position = "dodge")+
    geom_text(aes(label = percent(proportion,0.01)), vjust = 0)+
    facet_grid(~Var1)
}

# q7_var_yes_prop_visual_f(q7_var_table)

# who answer no to q7 and variability
q7_var_no_prop_visual_f <- function(cleaned_table){
  no <- cleaned_table %>% filter(Q7 == "No")
  
  no_prop <- as.data.frame(table(no$question_num, no$Response)) %>% 
    pivot_wider(names_from = Var2, values_from = Freq)%>% 
    pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>% 
    mutate(proportion = Freq/77)
  
  ggplot(no_prop, aes(x = Response, y = proportion, fill = Response))+
    geom_bar(stat = "identity", position = "dodge")+
    geom_text(aes(label = percent(proportion,0.01)), vjust = 0)+
    facet_grid(~Var1)
}

# q7_var_no_prop_visual_f(q7_var_table)

  
# Do GCs who answered YES to Q7  tend to agree with the embryo transfer justification of “reproductive autonomy” across conditions (Q29, Q35, Q36, Q41, Q47, Q53, Q59) more often than GCs who answered NO to Q7 ?

q7_repauto_visual <- q7_repauto %>% select(c(1,4:11))
q7_repauto_table <- q7_repauto_visual %>%
  pivot_longer(-c(IPAddress,Q7), names_to = "question_num", values_to = "Response")

q7_repauto_visual_f <- function(qnum){
  df <- q7_repauto_table %>% filter(question_num == qnum)
  
  ggplot(df, aes(x = Response, fill = Q7))+
    geom_bar(stat = "count", position = "stack")+
    facet_grid(~Q7)
}


q7_repauto_general <- ggplot(q7_repauto_table, aes(x = Response, fill = question_num))+
  geom_bar(stat = "count", position = "dodge")+
  facet_grid(~Q7)

# q7_repauto_visual_f("Q47")


# who answer yes to q7 and reproductive autonomy
q7_repauto_yes_prop_visual_f <- function(cleaned_table){
  yes <- cleaned_table %>% filter(Q7 == "Yes")
  
  yes_prop <- as.data.frame(table(yes$question_num, yes$Response)) %>% 
    pivot_wider(names_from = Var2, values_from = Freq)%>% 
    # mutate("1" = 0) %>% 
    pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
    mutate(proportion = Freq/22)
  
  ggplot(yes_prop, aes(x = Response, y = proportion, fill = Response))+
    geom_bar(stat = "identity", position = "dodge")+
    geom_text(aes(label = percent(proportion,0.01)), vjust = 0)+
    facet_grid(~Var1)
}

# q7_repauto_yes_prop_visual_f(q7_repauto_table)

# who answer no to q7 and reproductive autonomy
q7_repauto_no_prop_visual_f <- function(cleaned_table){
  no <- cleaned_table %>% filter(Q7 == "No")
  
  no_prop <- as.data.frame(table(no$question_num, no$Response)) %>% 
    pivot_wider(names_from = Var2, values_from = Freq)%>% 
    pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>% 
    mutate(proportion = Freq/77)
  
  ggplot(no_prop, aes(x = Response, y = proportion, fill = Response))+
    geom_bar(stat = "identity", position = "dodge")+
    geom_text(aes(label = percent(proportion,0.01)), vjust = 0)+
    facet_grid(~Var1)
}

# q7_repauto_no_prop_visual_f(q7_repauto_table)
  
# Do GCs who answered YES to Q7  tend to agree with the embryo transfer justification of “Resources available” across conditions (Q25, Q107, Q42, Q109, Q54) more often than GCs who answered NO to Q7?

q7_resava_visual <- q7_resava %>% select(c(1,4:9))
q7_resava_table <- q7_resava_visual %>%
  pivot_longer(-c(IPAddress,Q7), names_to = "question_num", values_to = "Response")

q7_resava_visual_f <- function(qnum){
  df <- q7_resava_table %>% filter(question_num == qnum)
  
  ggplot(df, aes(x = Response, fill = Q7))+
    geom_bar(stat = "count", position = "stack")+
    facet_grid(~Q7)
}


q7_resava_general <- ggplot(q7_resava_table, aes(x = Response, fill = question_num))+
  geom_bar(stat = "count", position = "dodge")+
  facet_grid(~Q7)


# q7_resava_visual_f("Q54")


# who answer yes to q7 and Resources available
q7_resava_yes_prop_visual_f <- function(cleaned_table){
  yes <- cleaned_table %>% filter(Q7 == "Yes")
  
  yes_prop <- as.data.frame(table(yes$question_num, yes$Response)) %>% 
    pivot_wider(names_from = Var2, values_from = Freq)%>% 
    mutate("1" = 0) %>%
    pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
    mutate(proportion = Freq/22)
  
  ggplot(yes_prop, aes(x = Response, y = proportion, fill = Response))+
    geom_bar(stat = "identity", position = "dodge")+
    geom_text(aes(label = percent(proportion,0.01)), vjust = 0)+
    facet_grid(~Var1)
}

q7_resava_yes_prop_visual_f(q7_resava_table)

# who answer no to q7 and Resources available
q7_resava_no_prop_visual_f <- function(cleaned_table){
  no <- cleaned_table %>% filter(Q7 == "No")
  
  no_prop <- as.data.frame(table(no$question_num, no$Response)) %>% 
    pivot_wider(names_from = Var2, values_from = Freq)%>% 
    mutate("1" = 0) %>%
    pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>% 
    mutate(proportion = Freq/77)
  
  ggplot(no_prop, aes(x = Response, y = proportion, fill = Response))+
    geom_bar(stat = "identity", position = "dodge")+
    geom_text(aes(label = percent(proportion,0.01)), vjust = 0)+
    facet_grid(~Var1)
}

q7_resava_no_prop_visual_f(q7_resava_table)

  
# Do GCs who answered YES to Q7 tend to feel more morally uneasy with transferring affected embryos (Q102_1 - 102_11) more often than GCs who answered NO to Q7

q7_q102_visual <- q7_q102 %>% select(c(1,4:11))
q7_q102_table <- q7_q102_visual %>%
  pivot_longer(-c(IPAddress,Q7), names_to = "question_num", values_to = "Response")

q7_q102_visual_f <- function(qnum){
  df <- q7_q102_table %>% filter(question_num == qnum)
  
  ggplot(df, aes(x = Response, fill = Q7))+
    geom_bar(stat = "count", position = "stack")+
    facet_grid(~Q7)
}

# q7_q102_visual_f("Q102_1")

q7_q102_general <- ggplot(q7_q102_table, aes(x = Response, fill = question_num))+
  geom_bar(stat = "count", position = "dodge")+
  facet_grid(~Q7)
