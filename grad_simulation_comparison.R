library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(rstatix)
library(corrplot)
fiu_curric <- read.csv("C:/Users/cpanc/OneDrive/Documents/IDC6940/compare_fiu_curric.csv")
names(fiu_curric)

# assumptions 
# a) normality - shapiro wilk test
fiu_curric %>% filter(Institution=='FIU_OLD') %>% pull(Grad_Rate) %>% 
  shapiro.test()
fiu_curric %>% filter(Institution=='FIU_NEW') %>% pull(Grad_Rate) %>% 
  shapiro.test()

# equal variance
box_m(fiu_curric[, c("Grad_Rate", "Std_Max_Attempt", "Time_To_Degree")], 
           group = fiu_curric$Institution)

# independence - all samples are def independent they are simulated

# new vs old fiu stat tests 
oldfiu <- fiu_curric %>% filter(Institution=='FIU_OLD')
newfiu <- fiu_curric %>% filter(Institution=='FIU_NEW')
t.test(oldfiu$Grad_Rate, newfiu$Grad_Rate, var.equal = TRUE) # p = 0.1211
t.test(oldfiu$Std_Max_Attempt, newfiu$Std_Max_Attempt, var.equal = TRUE) # p = 0.4654
t.test(oldfiu$Time_To_Degree, newfiu$Time_To_Degree, var.equal = TRUE) # *** p < 2e-16; curric complexity influence time on degree

mean(oldfiu$Time_To_Degree)

# all unis

all_curric <- read.csv("C:/Users/cpanc/OneDrive/Documents/IDC6940/all_uni_sim_results.csv")
names(all_curric)

all_curric <- all_curric %>% filter(Institution != 'FIU_NEW')

# all but one normal 
# per statology MANOVA is robust against departures from multivariate 
# normality so small to moderate departures typically don’t cause any problems

for (uni in unique(all_curric$Institution))
{
  result <- all_curric %>% filter(Institution==uni) %>% pull(Grad_Rate) %>% 
    shapiro.test()
  print(result)
}

# passes equal variance
box_m(all_curric[, c("Grad_Rate", "Std_Max_Attempt", "Time_To_Degree")], 
      group = all_curric$Institution)

# no multivariate outlier
ctr <- all_curric %>% select("Grad_Rate", "Std_Max_Attempt", "Time_To_Degree") %>%
  colMeans()

covar <- all_curric %>% select("Grad_Rate", "Std_Max_Attempt", "Time_To_Degree") %>%
  cov()

all_curric$mahal <- all_curric %>% select("Grad_Rate", "Std_Max_Attempt", "Time_To_Degree") %>%
  mahalanobis(center = ctr, cov = covar)

all_curric$p <- pchisq(all_curric$mahal, df=2, lower.tail=FALSE)

# remove outliers
all_curric <- all_curric %>% filter(p > 0.001) %>% select(-p, -mahal)
names(all_curric)

model <- manova(cbind(Grad_Rate, Std_Max_Attempt,Time_To_Degree) ~ Institution, data = all_curric)
summary(model)
summary.aov(model)

all_curric$Grad_Rate <- all_curric$Grad_Rate * 100

all_curric$Institution <- gsub("FIU_OLD","FIU",all_curric$Institution)

labels <- all_curric %>%
  group_by(Institution) %>%
  summarize(mean_grad = mean(Grad_Rate))

ggplot(all_curric, aes(x = Institution, y = Grad_Rate)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  geom_text(data = labels,
            aes(x = Institution, y = mean_grad, label = round(mean_grad, 2)),
            vjust = -1.75) +
  labs(x = 'Universities',
       y = 'Graduation Rate',
       title = 'Simulated Graduation Rate by University')
