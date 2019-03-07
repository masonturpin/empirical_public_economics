library(foreign)
library(tidyverse)

data_oh <- read.dta("data/DID_OH_ver12.dta")
#lpm
model_plan <- lm(hlthpln1 ~ treat + post + treat_post + male + veteran3 + children +
              married + divorced + widowed + separated + never_married +
              log_income + edu_hs_grad + edu_college_attd + 
              edu_college_grad, data_oh)
summary(model_plan)

model_cost <- lm(medcost ~ treat + post + treat_post + male + veteran3 + children +
              married + divorced + widowed + separated + never_married +
              log_income + edu_hs_grad + edu_college_attd + 
              edu_college_grad, data_oh)
summary(model_cost)

data_oh$plan_fit <- model_plan$fitted.values
data_oh$cost_fit <- model_cost$fitted.values

#logistic
logi_plan <- glm(hlthpln1 ~ treat + post + treat_post + male + veteran3 + children +
                 married + divorced + widowed + separated + never_married +
                 log_income + edu_hs_grad + edu_college_attd + 
                 edu_college_grad, data = data_oh, family = "binomial")
summary(logi_plan)

logi_cost <- glm(medcost ~ treat + post + treat_post + male + veteran3 + children +
                                married + divorced + widowed + separated + never_married +
                                log_income + edu_hs_grad + edu_college_attd + 
                                edu_college_grad, data = data_oh, family = "binomial")
summary(logi_cost)

data_oh$logi_plan_fit <- logi_plan$fitted.values
data_oh$logi_cost_fit <- logi_cost$fitted.values

library(ggthemes)
library(scales)
data_oh$treat <- factor(data_oh$treat, levels = c(0, 1), labels = c("Texas", "Ohio"))
data_oh$year <- factor(data_oh$year)

ggplot(data = data_oh %>% 
         group_by(treat, year) %>% 
         summarise(hlthpln1_prop = sum(hlthpln1)/length(hlthpln1))) +
  geom_line(aes(x = year, y = hlthpln1_prop, group = treat, color = treat)) +
  geom_point(aes(x = year, y = hlthpln1_prop, group = treat, color = treat)) +
  geom_vline(xintercept = 3.5) +
  labs(x = "Year", y = "Population Proportion with Health Insurance", color = "State", 
       title = "Effect of Medicaid Expansion on Healthcare Trends", subtitle = "Raw Values",
       caption = "The ACA went into full effect on January 1st, 2014. 
       Ohio chose to expand Medicaid while Texas did not.") +
  theme_classic() + scale_color_pander()

ggplot(data = data_oh %>% 
         group_by(treat, year) %>% 
         summarise(medcost_prop = sum(medcost)/length(medcost))) +
  geom_line(aes(x = year, y = medcost_prop, group = treat, color = treat)) +
  geom_point(aes(x = year, y = medcost_prop, group = treat, color = treat)) +
  geom_vline(xintercept = 3.5) +
  labs(x = "Year", y = "Population Proportion Skipping Care due to Cost", color = "State", 
       title = "Effect of Medicaid Expansion on Healthcare Trends", subtitle = "Raw Values",
       caption = "The ACA went into full effect on January 1st, 2014. 
       Ohio chose to expand Medicaid while Texas did not.") +
  theme_classic() + scale_color_pander()

ggplot(data = data_oh %>% 
         group_by(treat, year) %>% 
         summarise(hlthpln1_prop = sum(plan_fit)/length(plan_fit))) +
  geom_line(aes(x = year, y = hlthpln1_prop, group = treat, color = treat)) +
  geom_point(aes(x = year, y = hlthpln1_prop, group = treat, color = treat)) +
  geom_vline(xintercept = 3.5) +
  labs(x = "Year", y = "Population Proportion with Health Insurance", color = "State", 
       title = "Effect of Medicaid Expansion on Healthcare Trends", subtitle = "Fitted Values from Linear Probability Model",
       caption = "The ACA went into full effect on January 1st, 2014. 
       Ohio chose to expand Medicaid while Texas did not.") +
  theme_classic() + scale_color_pander()

ggplot(data = data_oh %>% 
         group_by(treat, year) %>% 
         summarise(medcost_prop = sum(cost_fit)/length(cost_fit))) +
  geom_line(aes(x = year, y = medcost_prop, group = treat, color = treat)) +
  geom_point(aes(x = year, y = medcost_prop, group = treat, color = treat)) +
  geom_vline(xintercept = 3.5) +
  labs(x = "Year", y = "Population Proportion Skipping Care due to Cost", color = "State", 
       title = "Effect of Medicaid Expansion on Healthcare Trends", subtitle = "Fitted Values from Linear Probability Model",
       caption = "The ACA went into full effect on January 1st, 2014.
       Ohio chose to expand Medicaid while Texas did not.") +
  theme_classic() + scale_color_pander()

ggplot(data = data_oh %>% 
         group_by(treat, year) %>% 
         summarise(hlthpln1_prop = sum(logi_plan_fit)/length(logi_plan_fit))) +
  geom_line(aes(x = year, y = hlthpln1_prop, group = treat, color = treat)) +
  geom_point(aes(x = year, y = hlthpln1_prop, group = treat, color = treat)) +
  geom_vline(xintercept = 3.5) +
  labs(x = "Year", y = "Population Proportion with Health Insurance", color = "State", 
       title = "Effect of Medicaid Expansion on Healthcare Trends", subtitle = "Fitted Values from Logistic Model",
       caption = "The ACA went into full effect on January 1st, 2014. 
       Ohio chose to expand Medicaid while Texas did not.") +
  theme_classic() + scale_color_pander()

ggplot(data = data_oh %>% 
         group_by(treat, year) %>% 
         summarise(medcost_prop = sum(logi_cost_fit)/length(logi_cost_fit))) +
  geom_line(aes(x = year, y = medcost_prop, group = treat, color = treat)) +
  geom_point(aes(x = year, y = medcost_prop, group = treat, color = treat)) +
  geom_vline(xintercept = 3.5) +
  labs(x = "Year", y = "Population Proportion Skipping Care due to Cost", color = "State", 
       title = "Effect of Medicaid Expansion on Healthcare Trends", subtitle = "Fitted Values from Logistic Model",
       caption = "The ACA went into full effect on January 1st, 2014.
       Ohio chose to expand Medicaid while Texas did not.") +
  theme_classic() + scale_color_pander()
