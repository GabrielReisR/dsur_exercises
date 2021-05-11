# Chapter 12  - Factorial ANOVA ====

# Initializing ====
library(car)
library(compute.es)
library(dplyr)
library(ggplot2)
library(magrittr)
library(multcomp)
library(pastecs)
library(WRS2)

df <- read.csv('.\\Data Files\\goggles.csv')
df$alcohol <- factor(df$alcohol,
                     levels = c('None', '2 Pints', '4 Pints'))
df$gender %<>% as.factor

# Creating interation graph (a line graph with error bars by factor) ====
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = alcohol, y = attractiveness, color = gender)) +
  
  # Criando o intervalo de confiança
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               #colour = 'steelblue',
               geom = 'errorbar',
               width = .1) +
  
  # Criando o ponto de média
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               #colour = 'steelblue',
               geom = 'point') +
  
  # Criando a linha que conecta
  stat_summary(fun = mean,
               geom = 'line',
               aes(x = as.numeric(alcohol), y = attractiveness)) +
  
  # Mudando cores dos gêneros
  scale_color_brewer(name = 'Gender',
                     type = 'qual',
                     palette = 'Dark2') +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Alcohol Comsumption') +
  ylab('Mean Attractiveness (%)') +
  
  # Especificando limites do eixo y
  ylim(c(0,100)) +

  # Colocando tema
  theme_minimal()

# Creating boxplots by factor ====
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = alcohol, y = attractiveness)) +
  
  # Creating boxplots
  geom_boxplot() +
  
  # Facetting
  facet_wrap(~ gender) +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Alcohol Comsumption') +
  ylab('Mean Attractiveness (%)') +
  
  # Theme
  theme_minimal()

# Getting descriptive statistics ====
by(df$attractiveness, list(df$alcohol, df$gender), stat.desc)

# Testing for homogeneity of variances ====
#' We have to specify different conditions using the`interaction()` term 
leveneTest(df$attractiveness,
           interaction(df$alcohol, df$gender),
           center = median)

# Setting up contrasts ====
# Comparing gender
contrasts(df$gender) <- c(-1, 1)

# Comparing 'No alcohol' VS 2 'Alcohol', then '2 Pints' VS '4 Pints'
contrasts(df$alcohol) <- cbind(c(-2, 1, 1), c(0, -1, 1))

contrasts(df$gender)
contrasts(df$alcohol)

# Fitting the model ====
model <- aov(attractiveness ~ alcohol * gender, data = df)
summary(model)

Anova(model, type = 'III')

#' It's important to note that alcohol does not affect women regarding
#' attractiveness of date. However, the significant result in this variable
#' *may falsely lead* to the conclusion that alcohol tends to diminish date
#' attractiveness. That's a good example of the importance of interpreting only
#' the interaction effect when other independent variables are also significant.

# Looking at ANOVA's plots ====
plot(model)

# Looking at the alcohol effect ====
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = alcohol, y = attractiveness)) +
  
  # Creating columns
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               #colour = 'steelblue',
               geom = 'bar',
               color = 'black',
               fill = 'white') +
  
  # Creating errorbar
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               #colour = 'steelblue',
               geom = 'errorbar',
               width = .1) +
  
  # Creating point
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               #colour = 'steelblue',
               geom = 'point') +
  # Facetting
  #facet_wrap(~ gender) +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Alcohol Comsumption') +
  ylab('Mean Attractiveness of Date (%)') +
  
  # Alterando limites
  ylim(c(0,100)) +
  
  # Theme
  theme_minimal()

# Looking at the gender effect ====
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = gender, y = attractiveness)) +
  
  # Creating columns
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               #colour = 'steelblue',
               geom = 'bar',
               color = 'black',
               fill = 'white') +
  
  # Creating errorbar
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               #colour = 'steelblue',
               geom = 'errorbar',
               width = .1) +
  
  # Creating point
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               #colour = 'steelblue',
               geom = 'point') +
  # Facetting
  #facet_wrap(~ gender) +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Gender') +
  ylab('Mean Attractiveness of Date (%)') +
  
  # Alterando limites
  ylim(c(0,100)) +
  
  # Theme
  theme_minimal()

# Interpreting contrasts ====
summary.lm(model)
# Doing post hoc tests ====
#' *It doesn't make sense to do these post hocs* because alcohol is *NOT* a good
#' predictor of mate attractiveness. We know that because of the already 
#' attested interaction between alcohol and gender.

#' *Bonferroni*
pairwise.t.test(df$attractiveness, df$alcohol,
                p.adjust.method = "bonferroni")

#' *Tukey*
model <- aov(attractiveness ~ alcohol, data = df)

tukey_output <- glht(model, linfct = mcp(alcohol = "Tukey"))
summary(tukey_output)
confint(tukey_output)
# Fitting a robust factorial ANOVA ====
#' For this, we'll use the *WRS2* package
#' Factorial ANOVA with trimmed means
robust_anova_two_way <- t2way(attractiveness ~ alcohol * gender, data = df)

#' Alcohol
robust_anova_two_way$Qa
robust_anova_two_way$A.p.value

#' Gender
robust_anova_two_way$Qb
robust_anova_two_way$B.p.value

#' Alcohol * Gender
robust_anova_two_way$Qab
robust_anova_two_way$AB.p.value

#' *Writing results*:
#' "We could conclude that there was no significant main effect of gender,
#' Q = 1.67, p = .209, but there was a significant main effect of alcohol,
#' Q = 48.28, p = .001, and a significant gender × alcohol interaction,
#' Q = 26.26, p = .001."

# Post hoc factorial ANOVA with trimmed means ====
#' Based on trimmed means
mcp2atm(attractiveness ~ alcohol * gender, data = df)

#' *CAUTION*: see next post hoc section on how to interpret these results

# Factorial ANOVA with trimmed means and bootstrapping ====
#' For this, we'll use the *WRS2* package
robust_anova_two_way_boot <- pbad2way(attractiveness ~ alcohol * gender,
                                      data = df)

#' Aqui, não há um valor de resultado, apenas a significância do teste

#' Alcohol
robust_anova_two_way_boot$A.p.value

#' Gender
robust_anova_two_way_boot$B.p.value

#' Alcohol * Gender
robust_anova_two_way_boot$AB.p.value

#' *Writing results*:
#' "There was no significant main effect of gender, p = .171, but there was a 
#' significant main effect of alcohol, p < .001, and a significant 
#' gender × alcohol interaction, p < .001."

# Post hoc factorial ANOVA based on M-estimator ====
#' Based on M-estimator
mcp2a(attractiveness ~ alcohol * gender, data = df)

#' Let's write only the M-estimator results
#' The interpretation is very different from before, where we compared different
#' conditions. Now, each row represents a condition being compared. *Beware* on
#' how each factor is levelled in the data.
levels(df$alcohol)
levels(df$gender)

#' *alcohol1*: explores difference between 'No Alcohol' VS '2 Pints'
#' *alcohol2*: explores difference between 'No Alcohol' VS '4 Pints'
#' *alcohol3*: explores difference between '2 Pints' VS '4 Pints'
#' *gender1*: explores difference between 'Female' VS 'Male'

#' *alcohol1:gender1*: is there a difference between 'No Alcohol' VS '2 Pints'
#' when considering gender?
#' *alcohol2:gender1*: is there a difference between 'No Alcohol' VS '4 Pints'
#' when considering gender?
#' *alcohol3:gender1*: is there a difference between '2 Pints' VS '4 Pints'
#' when considering gender?

#' Writing results:
#' "For the main effect of alcohol we find a significant difference in
#' attractiveness scores for 4 pints compared to both no alcohol, ψ∧ = 35.80,
#' p < .001, and 2 pints, ψ∧ = 40.80, p < .001, but not between 2 pints and no
#' alcohol, ψ∧ = −5, p = .383. Similarly, for the interaction term, males and
#' females were comparable in terms of the difference in attractiveness ratings
#' between 4 pints compared to both no alcohol, ψ∧ = −32.23, p < .001, and 
#' 2 pints, ψ∧ = −27.23, p < .01, but not between 2 pints and no alcohol, 
#' ψ∧ = −5, p = .318."

# Calculating the model effect size====
omega_factorial <- function(n,
                            a, b,
                            SSa, SSb, SSab, SSr)
{
  MSa <- SSa / (a - 1)
  MSb <- SSb / (b - 1)
  MSab <- SSab / ((a - 1) * (b - 1))
  MSr <- SSr / (a * b * (n - 1))
  varA <- ((a - 1) * (MSa - MSr)) / (n * a * b)
  varB <- ((b - 1) * (MSb - MSr)) / (n * a * b)
  varAB <- ((a - 1) * (b - 1) * (MSab - MSr)) / (n * a * b)
  varTotal <- varA + varB + varAB + MSr
  print(paste("Omega-Squared A: ", varA / varTotal))
  print(paste("Omega-Squared B: ", varB / varTotal))
  print(paste("Omega-Squared AB: ", varAB / varTotal))
}

#' Fitting the model
model <- aov(attractiveness ~ alcohol * gender, data = df)
summary(model)

#' Getting omega
n_per_condition <- 8 # how many participants per condition
n_levels_a <- 2 # gender has two levels, 'Female' and 'Male'
n_levels_b <- 3 # alcohol has three levels, 'None', '2 Pints' and '4 Pints'
SSa <- 169
SSb <- 3332
SSab <- 1978
SSr <- 3487

model_omega <- omega_factorial(n_per_condition,
                               n_levels_a, n_levels_b,
                               SSa, SSb, SSab, SSr)

#' Writing results:
#' "For the main effect of gender we get ωgender² = 0.009; for the main effect
#' of alcohol we get ωalcohol² = 0.350; and for the interaction 
#' ωgender × alcohol²= 0.200."

# Calculating the effect sizes for specific differences ====
#' We can get effect sizes for specific differences in some conditions
#' To do that, we again need the data's descriptive statistics
by(df$attractiveness, list(df$alcohol, df$gender), stat.desc)

#' The `compute.es::mes()` function takes the form of:
#' `mes(MEAN_group1, MEAN_group2, SD_group1, SD_group2, N_group1, N_group2)`

#' No Alcohol: Male VS Female
no_alcohol_gender <- mes(66.875, 60.625, 10.3293963, 4.95515604, 8, 8)
no_alcohol_gender$d
no_alcohol_gender$r

#' 2 Pints: Male VS Female
two_pints_gender <- mes(66.875, 62.5, 12.5178444, 6.5465367, 8, 8)
two_pints_gender$d
two_pints_gender$r

#' 4 Pints: Male VS Female
four_pints_gender <- mes(35.625, 57.5, 10.8356225, 7.0710678, 8, 8)
four_pints_gender$d
four_pints_gender$r

#' Writing results:
#' "The difference in attractiveness scores between males and females who drank 
#' no alcohol is a medium effect (the means are under a standard deviation 
#' different), d = 0.77, r = .36; the difference between males and females who 
#' drank 2 pints is a fairly small effect (there is less than half a standard 
#' deviation difference between the group means), d = 0.44, r = .21; finally, 
#' the difference between males and females who drank 4 pints is a very large 
#' effect (the means are more than 2 standard deviation apart), d = −2.39, 
#' r = −.77."

# Reporting factorial ANOVA ====
#' *Reporting alcohol effect*
#' "There was a significant main effect of the amount of alcohol consumed at the
#' nightclub, on the attractiveness of the mate they selected, F(2, 42) = 20.07,
#' p < .001, ω2 = .35. The Bonferroni post hoc tests revealed that the 
#' attractiveness of selected."

#' *Reporting an effect*
#' "There was a non-significant main effect of gender on the attractiveness of
#' selected mates, F(1, 42) = 2.03, p = .161, ω2 = .009."

#' *Reporting interaction effect*
#' "There was a significant interaction effect between the amount of alcohol 
#' consumed and the gender of the person selecting a mate, on the attractiveness
#' of the partner selected, F(2, 42) = 11.91, p < .001, ω2= .20. This indicates
#' that male and female genders were affected differently by alcohol. 
#' Specifically, the attractiveness of partners was similar in males (M = 66.88,
#' SD = 10.33) and females (M = 60.63, SD = 4.96) after no alcohol, d = 0.77;
#' the attractiveness of partners was also similar in males (M = 66.88,
#' SD = 12.52) and females (M = 62.50, SD = 6.55) after 2 pints, d = 0.44;
#' however, attractiveness of partners selected by males (M = 35.63, SD = 10.84)
#' was significantly lower than those selected by females (M = 57.50, SD = 7.07)
#' after 4 pints, d = −2.39."
