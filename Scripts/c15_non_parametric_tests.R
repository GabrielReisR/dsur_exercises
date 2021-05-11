# Chapter 15 - Non-parametric tests ====

# Initializing ====
library(car)
library(clinfun) #' `jonckheere.test()`
library(dplyr)
library(ggplot2)
library(magrittr)
library(pastecs)
library(pgirmess) #' `kruskalmc()`, `friedmanmc`
library(tidyr)

# Comparing two independent groups with Wilcoxon rank-sum test ====
df <- read.delim(".\\Data Files\\Drug.dat")
df$drug %<>% as.factor

# Checking for normality and homoscedasticity ====
by(df %>% select(ends_with('BDI')), df$drug,
   stat.desc, norm = T, basic = F)

#' In the alcohol condition, wedsBDI violates normality
#' In the esctasy condition, sundayBDI violates normality

leveneTest(df$sundayBDI, df$drug, data = df)
leveneTest(df$wedsBDI, df$drug, data = df)

#' Neither of the groups violate homoscedasticity

# Conducting Wilcoxon rank-sum test ====
#' In order to use a normal approximation, we set `exact = F` and `correct = F`
sunday_model <- wilcox.test(sundayBDI ~ drug, data = df,
                            exact = F, correct = F)

wedsneday_model <- wilcox.test(wedsBDI ~ drug, data = df,
                            exact = F, correct = F)

#' Type of drug affected depression symptoms only on wednesday model, 
#' with W = 4 and p < .001.

# Calculating effect size ====
#' Small function to calculate the effect size based on [R = z / sqrt(N)]
rFromWilcox <- function(wilcoxModel, N) {
  z <- qnorm(wilcoxModel$p.value / 2)
  r <- z / sqrt(N)
  cat(wilcoxModel$data.name, "Effect Size, r = ", r)
}

rFromWilcox(sunday_model, 20)
rFromWilcox(wedsneday_model, 20)

# Writing the results ====
#' "Depression levels in ecstasy users (Mdn = 17.50) did not differ 
#' significantly from alcohol users (Mdn = 16.00) the day after the drugs were 
#' taken, W = 35.5, p = 0.286, r = −.25. However, by Wednesday, ecstasy users 
#' (Mdn = 33.50) were significantly more depressed than alcohol users 
#' (Mdn = 7.50), W = 4, p < .001, r = −.78."

# Comparing two related conditions with Wilcoxon signed-rank test ====
df <- read.delim(".\\Data Files\\Drug.dat")
df$drug %<>% as.factor

# Checking for normality distribution of the difference in scores ====
df$bdi_change <- df$wedsBDI - df$sundayBDI

by(df$bdi_change, df$drug,
   stat.desc, norm = T, basic = F)

#' Alcohol change doesn't follow a normal distribution

# Creating separate datasets ====
ecstasy_df <- df %>% filter(drug == 'Ecstasy')
alcohol_df <- df %>% filter(drug == 'Alcohol')

# Conducting Wilcoxon signed-rank test ====
alcohol_model <- wilcox.test(alcohol_df$wedsBDI, alcohol_df$sundayBDI,
                             exact = F, correct = F, paired = T)
ecstasy_model <- wilcox.test(ecstasy_df$wedsBDI, ecstasy_df$sundayBDI,
                             exact = F, correct = F, paired = T)

# Calculating effect size ====
#' `N` is the number of observations, not the number of people
rFromWilcox(alcohol_model, 20)
rFromWilcox(ecstasy_model, 20)

# Writing results ====
#' "For ecstasy users, depression levels were significantly higher on Wednesday 
#' (Mdn = 33.50) than on Sunday (Mdn = 17.50), p = .047, r = −.56. However, for 
#' alcohol users the opposite was true: depression levels were significantly 
#' lower on Wednesday (Mdn = 7.50) than on Sunday (Mdn = 16.0), p = .012, 
#' r = −.45."

# Comparing several independent groups using the Kruskal-Wallis test ====
df <- read.delim(".\\Data Files\\Soya.dat")

df$Soya <- factor(df$Soya,
                  levels = c(1, 2, 3, 4),
                  labels = c("No Soya",
                             "1 Soya Meal",
                             "4 Soya Meals",
                             "7 Soya Meals"))

df$Soya <- relevel(df$Soya, "No Soya")

# Checking for normality and homoscedasticity ====
by(df$Sperm, df$Soya,
   stat.desc, norm = T, basic = F)

#' With the exception of *No Soya*, all other groups violate normality

leveneTest(df$Sperm, df$Soya, data = df)

#' Homoscedasticity is violated between groups

# Conducting Kruskal-Wallis test ====
sperm_model <- kruskal.test(Sperm ~ Soya, data = df)

#' There is a significant difference between groups
#' Let's obtain the mean rank between groups
#' We could run `df$Ranks <- rank(df$Sperm)` to obtain ranks

by(df$Ranks, df$Soya, mean)

# Visualizing differences through a boxplot ====
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = Soya, y = Sperm)) +
  
  # Creating boxplot
  geom_boxplot() +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Soya Condition') +
  ylab('Sperm Count') +
  
  # Alterando limites
  ylim(c(0,25)) +
  
  # Theme
  theme_minimal()

# Post hoc tests for Kruskal-Wallis ====
kruskalmc(Sperm ~ Soya, data = df)

#' A significant difference between groups was not attested comparing all groups
#' against each other.
#' However, we can focus the comparisons on a baseline group, adding the
#' parameter `cont = 'two-tailed`. This fixes the first level of the factor
#' to be compared against the others. This way, we're comparing 1/4/7 Soya Meals
#' against No Soya Meals. This lowers our expected critical difference,
#' allowing us to observe differences when those truly occur.

kruskalmc(Sperm ~ Soya, data = df,
          cont = 'two-tailed')

# Testing for trends with the Jonckheere-Terpstra test ====
jonckheere.test(df$Sperm, as.numeric(df$Soya))

# Writing results ====
#' Sperm counts were significantly affected by eating soya meals, H(3) = 8.66, 
#' p = .034.
#' 
#' Sperm counts were significantly affected by eating soya meals, H(3) = 8.66, 
#' p = .034. Focused comparisons of the mean ranks between groups showed that 
#' sperm counts were not significantly different when one soya meal 
#' (difference = 2.2 ) or four soya meals (difference = 2.2) were eaten per 
#' week compared to none. However, when seven soya meals were eaten per week 
#' sperm counts were significantly lower than when no soya was eaten 
#' (difference = 19). In all cases, the critical difference (α = .05 corrected 
#' for the number of tests) was 15.64. We can conclude that if soya is eaten 
#' every day it significantly reduces sperm counts compared to eating none; 
#' however, eating soya less frequently than every day has no significant 
#' effect on sperm counts (‘phew!’ says the vegetarian man!).
#' 
#' Jonckheere’s test revealed a significant trend in the data: as more soya was 
#' eaten, the median sperm count decreased, J = 912, p = .013.

# Differences between several related groups: Friedman’s ANOVA ====
df <- read.delim(".\\Data Files\\Diet.dat")

# Checking for normality and descriptive statistics ====
stat.desc(df, basic = F, norm = T)

# Adjusting dataset ====
#' Creating *id*
df$id <- c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3), rep(5, 3), 
           rep(6, 3), rep(7, 3), rep(8, 3), rep(9, 3), rep(10, 3))

#' Pivoting to longer format
df %<>%
  pivot_longer(
    cols = Start:Month2,
    names_to = "group",
    values_to = "weight"
  )

df$group <- factor(df$group,
                   levels = c('Start', 'Month1', 'Month2'))
View(df)

# Conducting Friedman’s ANOVA ====
friedman.test(weight ~ group | id, data = df)

# Post hoc tests for Friedman’s ANOVA ====
friedmanmc(df$weight, df$group, df$id)

# Calculating effect sizes ====
#' We can use `rFromWilcox()` to get specific comparisons' effect sizes

# Writing results ====
#' "The weight of participants did not significantly change over the two months 
#' of the diet, χ2(2) = 0.20, p > .05."
#' 
#' "The weight of participants did not significantly change over the two months 
#' of the diet, χ2(2) = 0.20, p > .05. Post hoc tests were used with Bonferroni 
#' correction applied. It appeared that weight didn’t significantly change from 
#' the start of the diet to one month, (difference = 1), from the start of the 
#' diet to two months, (difference = 2), or from one month to two months, 
#' (difference = 1). In all cases, the critical difference (α = .05 corrected 
#' for the number of tests) was 10.71. We can conclude that the Andikins diet, 
#' like its creator, is a complete failure."


