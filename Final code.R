library(tidyverse)
library(dplyr)
library(purrr)
library(broom)
#install.packages
ess <- read.csv('/Users/nicole.yu/Desktop/Sandbox/ESS11_alldata.csv')
ess <- ess %>%
  mutate(
    eqwrkbg = ifelse( eqwrkbg %in% c(7, 8), NA, eqwrkbg),
    eqmgmbg = ifelse( eqmgmbg %in% c(7, 8), NA, eqmgmbg),
    eqpaybg = ifelse( eqpaybg %in% c(7, 8), NA, eqpaybg),
    eqpolbg = ifelse( eqpolbg %in% c(7, 8), NA, eqpolbg),
    eqparep = ifelse( eqparep %in% c(7, 8), NA, 6-eqparep),
    eqparlv = ifelse( eqparlv %in% c(7, 8), NA, 6-eqparlv),
    freinsw = ifelse( freinsw %in% c(7, 8), NA, 6-freinsw),
    fineqpy = ifelse( fineqpy  %in% c(7, 8), NA, 6-fineqpy)
  )
#clean the 8 variable that can measure the Gender Attitude
ess <- ess %>%
  mutate(
    Gender_Attitude = rowSums(select(., eqwrkbg, eqmgmbg, eqpaybg, eqpolbg, eqparep, eqparlv, freinsw, fineqpy), na.rm = TRUE)
  )
#get Gender Attitude by adding 8 varaibles up
vars <- c("eqwrkbg", "eqmgmbg", "eqpaybg", "eqpolbg", 
          "eqparep", "eqparlv", "freinsw", "fineqpy")
df <- ess %>%
  select(all_of(vars)) %>%
  drop_na() 
cor(df, method = "spearman")  
str(ess[vars])
df <- ess %>% select(all_of(vars))%>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
cor_matrix <- cor(df, method = "spearman")
print(cor_matrix)
#get the spearsman correlation matrix between 8 varaibles(table 1)



ess <- ess %>%
  mutate(
    ipsucesa = ifelse(ipsucesa%in% c(66,77, 88,99), NA, 7-ipsucesa),
    ipshabta = ifelse(ipshabta%in% c(66,77, 88,99), NA, 7-ipshabta)
  )
#clean the variables for Achievement_Motivation
ess <- ess %>%
  mutate(
    Achievement_Motivation = rowSums(select(., ipsucesa, ipshabta), na.rm = TRUE)
  )
#get the variable Achievement_Motivation


ess <- ess %>%
  mutate(
    wkdcorga = ifelse(wkdcorga%in% c(66,77, 88, 99), NA, wkdcorga),
    iorgact = ifelse(iorgact%in% c(66,77, 88,99), NA, iorgact)
  )
#clean the variables for Career_Achievement
ess <- ess %>%
  mutate(
    Career_Achievement = rowSums(select(., wkdcorga, iorgact), na.rm = TRUE)
  ) 
#get the variable Career_Achievement


library(purrr)
ess_female <- ess %>% filter(gndr == 2)
spearman_by_country <- function(data, var1, var2) {
  data %>%
    select(cntry, x = {{var1}}, y = {{var2}}) %>%  # 重命名变量为 x 和 y
    drop_na() %>%
    group_by(cntry) %>%
    group_modify(~ {
      test <- cor.test(.x$x, .x$y, method = "spearman", exact = FALSE)
      tibble(rho = test$estimate, p = test$p.value)
    }) %>%
    ungroup()
}

r1 <- spearman_by_country(ess_female, Gender_Attitude, Achievement_Motivation) %>%
  rename('Gender_Attitude & Achievement_Motivation' = rho, 'p_1' = p)
r2 <- spearman_by_country(ess_female, Gender_Attitude, Career_Achievement) %>%
  rename('Gender_Attitude & Career_Achievement'  = rho, 'p_2' = p)
r3 <- spearman_by_country(ess_female, Achievement_Motivation, Career_Achievement) %>%
  rename('Achievement_Motivation & Career_Achievement' = rho, 'p_3' = p)
result <- r1 %>%
  left_join(r2, by = "cntry") %>%
  left_join(r3, by = "cntry")
view(result)
#get table 2 for female

ess_male <- ess %>% filter(gndr == 1)
spearman_by_country <- function(data, var1, var2) {
  data %>%
    select(cntry, x = {{var1}}, y = {{var2}}) %>% 
    drop_na() %>%
    group_by(cntry) %>%
    group_modify(~ {
      test <- cor.test(.x$x, .x$y, method = "spearman", exact = FALSE)
      tibble(rho = test$estimate, p = test$p.value)
    }) %>%
    ungroup()
}

r1 <- spearman_by_country(ess_male, Gender_Attitude, Achievement_Motivation) %>%
  rename('Gender_Attitude & Achievement_Motivation' = rho, 'p_1' = p)

r2 <- spearman_by_country(ess_male, Gender_Attitude, Career_Achievement) %>%
  rename('Gender_Attitude & Career_Achievement'  = rho, 'p_2' = p)

r3 <- spearman_by_country(ess_male, Achievement_Motivation, Career_Achievement) %>%
  rename('Achievement_Motivation & Career_Achievement' = rho, 'p_3' = p)

result_male <- r1 %>%
  left_join(r2, by = "cntry") %>%
  left_join(r3, by = "cntry")

view(result_male)
#get table 3 for male


install.packages("lme4") 
library(lme4)
library(tidyverse)
ess_female$cntry <- as.factor(ess_female$cntry)
ess_female <- ess_female %>%
  mutate(edulevel = case_when(
    edulvlb < 100 ~ 1,  
    edulvlb < 200 ~ 2,   
    edulvlb < 300 ~ 3,
    edulvlb < 400 ~ 4,
    edulvlb < 500 ~ 5,
    edulvlb < 600 ~ 6,
    edulvlb < 700 ~ 7,
    edulvlb <= 900 ~ 8,
  ))
# convert educational level

model1 <- lmer(Achievement_Motivation ~ Gender_Attitude * Laborforce_female + agea + I(agea^2) + edulvlb + (1 | cntry), data = ess_female)
stargazer(model1, type = "latex", title = "Multilevel Regression Results", digits = 3, 
          dep.var.labels = "Achievement_Motivation", 
          covariate.labels = c("Gender Attitudes* Laborforce_female ", "Age", "Age Squared", "Education Level"),
          no.space = TRUE, single.row = TRUE)
stargazer(model1, type = "text")
library(interactions)
interact_plot(model1, pred = Gender_Attitude, modx = Laborforce_female,
              plot.points = TRUE, interval = TRUE,
              x.label = "Gender Attitude (Egalitarianism)",
              modx.label = "Standardized Female Labor Force Participation Rate",
              y.label = "Predicted Achievement Motivation",
              main.title = "Interaction between Gender Attitude and Female Labor Force Participation")
#get the 2 Achievement_Motivation&Laborforce_female tables, one is multilevel analysis, one is interact plot(table 4 and 5)


model2 <- lmer(Career_Achievement ~ Gender_Attitude * Laborforce_female + agea + I(agea^2) + edulvlb + (1 | cntry), data = ess_female)
library(stargazer)  
stargazer(model2, type = "latex", title = "Multilevel Regression Results", digits = 3, 
          dep.var.labels = "Career_Achievement", 
          covariate.labels = c("Gender Attitudes*Laborforce_female", "Age", "Age Squared", "Education Level"),
          no.space = TRUE, single.row = TRUE)
stargazer(model2, type = "text")
interact_plot(model2, pred = Gender_Attitude, modx = Laborforce_female,
              plot.points = TRUE, interval = TRUE,
              x.label = "Gender Attitude (Egalitarianism)",
              modx.label = "Standardized Female Labor Force Participation Rate",
              y.label = "Predicted Career Achievement",
              main.title = "Interaction between Gender Attitude and Female Labor Force Participation")
#get the 2 Career Achievement&Laborforce_female tables, one is multilevel analysis, one is interact plot(table 6 and 7)

model3 <- lmer(Achievement_Motivation ~ Gender_Attitude * Unemployment_female+ agea + I(agea^2) + edulvlb + (1 | cntry), data = ess_female)
stargazer(model3, type = "latex", title = "Multilevel Regression Results", digits = 3, 
          dep.var.labels = "Achievement_Motivation", 
          covariate.labels = c("Gender Attitudes*Unemployment_female", "Age", "Age Squared", "Education Level"),
          no.space = TRUE, single.row = TRUE)
interact_plot(model3, pred = Gender_Attitude, modx = Unemployment_female,
              plot.points = TRUE, interval = TRUE,
              x.label = "Gender Attitude (Egalitarianism)",
              modx.label = "Unemployment_female Rate",
              y.label = "Predicted Achievement Motivation",
              main.title = "Interaction between Gender Attitude and Unemployment_female Rate")
#get the 2 Achievement_Motivation&Unemployment_femal tables, one is multilevel analysis, one is interact plot(table 8 and 9)

model4 <- lmer(Career_Achievement ~ Gender_Attitude * Unemployment_female + agea + I(agea^2) + edulvlb + (1 | cntry), data = ess_female)
library(stargazer)  
stargazer(model4, type = "latex", title = "Multilevel Regression Results", digits = 3, 
          dep.var.labels = "Career_Achievement", 
          covariate.labels = c("Gender Attitudes* Career_Achievement ", "Age", "Age Squared", "Education Level"),
          no.space = TRUE, single.row = TRUE)
stargazer(model4, type = "text")
interact_plot(model4, pred = Gender_Attitude, modx = Unemployment_female,
              plot.points = TRUE, interval = TRUE,
              x.label = "Gender Attitude (Egalitarianism)",
              modx.label = "Standardized Unemployment female rate",
              y.label = "Predicted Career Achievement",
              main.title = "Interaction between Gender Attitude and Unemployment female rate")
#get the 2 Career_Achievement&Unemployment_femal tables, one is multilevel analysis, one is interact plot(table 10 and 11)

