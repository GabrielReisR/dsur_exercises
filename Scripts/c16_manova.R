# Chapter 16 - Multivariate Analysis of Variance (MANOVA) ====

# Initializing ====
library(car)
library(dplyr)
library(ggplot2)
library(magrittr)
library(MASS) #' `lda()`
library(mvnormtest)
library(mvoutlier)
library(pastecs)
library(tidyr)
library(WRS2)

df <- read.delim(".\\Data Files\\OCD.dat")
df

df$Group <- factor(df$Group,
                   levels = c('CBT', 'BT', 'No Treatment Control'))

levels(df$Group)[3] <- 'NT'

# Visualizing the data 1 - Correlation between DVs by group ====
df %>% 
  ggplot(aes(Actions, Thoughts)) +
  
  geom_point() +
  
  geom_smooth(method = 'lm') +
  
  facet_wrap(~ Group) +
  
  theme_minimal()

# Visualizing the data 2 - Count of DVs by group ====
df %>% 
  pivot_longer(
    cols = Actions:Thoughts,
    names_to = "Outcome",
    values_to = "Frequency"
  ) %>% 
  
  ggplot(aes(x = Group, y = Frequency, group = Outcome, color = Outcome)) +
  
  # Creating columns
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               #colour = 'steelblue',
               geom = 'bar',
               #color = 'black',
               fill = 'white',
               position = 'dodge') +
  
  # Creating errorbar
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               #colour = 'steelblue',
               geom = 'errorbar',
               width = .1,
               position = position_dodge(width = 0.9)) +
  
  # Creating point
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               #colour = 'steelblue',
               geom = 'point',
               position = position_dodge(width = 0.9)) +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Treatment Group') +
  ylab('Number of Thoughts/Actions') +
  
  # Alterando limites
  ylim(c(0, 20)) +

  theme_minimal()



# Visualizing the data 3 - Boxplot of DVs by group ====
df %>% 
  pivot_longer(
    cols = Actions:Thoughts,
    names_to = "Outcome",
    values_to = "Frequency"
  ) %>% 
  
  ggplot(aes(x = Group, y = Frequency, color = Outcome)) +
  
  # Creating columns
  geom_boxplot() +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Treatment Group') +
  ylab('Number of Thoughts/Actions') +
  
  # Alterando limites
  ylim(c(0, 20)) +
  
  theme_minimal()

# Getting descriptive statistics ====
by(df$Actions, df$Group,
   stat.desc, norm = T, basic = F)

by(df$Thoughts, df$Group,
   stat.desc, norm = T, basic = F)

# Getting the covariance and assessing covariance ratio ====
by(df[, 2:3], df$Group, cov)

# Assessing multivariate normality with `mshapiro.test` ====
#' *First*, we need to divide our groups
cbt <- df %>% filter(Group == 'CBT') %>% dplyr::select(- Group)
bt <- df %>% filter(Group == 'BT') %>% dplyr::select(- Group)
nt <- df %>% filter(Group == 'NT') %>% dplyr::select(- Group)

#' *Second*, we transpose the matrix
cbt <- t(cbt)
bt <- t(bt)
nt <- t(nt)

#' *Third*, we assess normality
mshapiro.test(cbt)
mshapiro.test(bt)
mshapiro.test(nt)

# Assessing normality with plots using `aq.plot` ====
aq.plot(df[, 2:3])

#' Case 26 might be an outlier. We could consider deleting it.
#' Let's see what happens if we do this:
df_temp <- df[-26,]

nt <- df_temp %>% filter(Group == 'NT') %>% dplyr::select(- Group)
nt <- t(nt)
mshapiro.test(nt)

#' Removing the outlier causes the "NT" group to have a normal distribution

# Setting up contrasts ====
CBT_vs_NT <- c(1, 0, 0)
BT_vs_NT <- c(0, 1, 0)

contrasts(df$Group) <- cbind(CBT_vs_NT, BT_vs_NT)

# Conducting the MANOVA ====
#' To conduct the MANOVA, we need to create a matrix with the DVs
outcome <- cbind(df$Actions, df$Thoughts)

# Running the initial model
ocd_model <- manova(outcome ~ Group, data = df)

# Checking the results of the model ====
summary(ocd_model, intercept = TRUE) # test = "Pillai"
summary(ocd_model, intercept = TRUE, test = "Wilks")
summary(ocd_model, intercept = TRUE, test = "Hotelling")
summary(ocd_model, intercept = TRUE, test = "Roy")

# Checking the results of DVs ====
summary.aov(ocd_model)

#' Response 1: Actions
#' Response 2: Thoughts

#' Both results indicate that treatment resulted in no difference either in
#' Actions or Thoughts. This is conterintuitive since our MANOVA was 
#' significant! This happens because MANOVA takes into account the relationship
#' between the two DVs, characterizing the independent variable by the DVs
#' behavior.

# Contrasts for specific differences ====
#' To generate the specific differences we have to perform One-Way ANOVAs
action_model <- lm(Actions ~ Group, data = df)
thoughts_model <- lm(Thoughts ~ Group, data = df)

summary.lm(action_model)
summary.lm(thoughts_model)

#' Behavior therapy seems to be significantly different than 'No therapy' in
#' the frequency of OCD behaviors.

# Robust MANOVA ====
#' `mulrank()`: This performs a MANOVA on the ranked data using Munzel and 
#' Brunner’s method (Munzel & Brunner, 2000)
#' 
#' `cmanova()`: This performs Choi and Marden’s (1997) robust test based on the 
#' ranked data. It is an extension of the Kruskal–Wallis test.

#' I couldn't find recent packages to do a Robust MANOVA. Unfortunately, the
#' *WRS* package was updated to the *WRS2* package, and this new version doesn't
#' come with a robust MANOVA procedure.

# Writing results ====
#' There was a significant effect of therapy on the number of obsessive thoughts
#' and behaviours, F(4, 54) = 2.56, p < .05.
#' 
#' Using Pillai’s trace, there was a significant effect of therapy on the number
#' of obsessive thoughts and behaviours, V = 0.32, F(4, 54) = 2.56, p < .05.
#' 
#' Using Wilks’s lambda statistic, there was a significant effect of therapy on 
#' the number of obsessive thoughts and behaviours, Λ = 0.70, F(4, 52) = 2.56, 
#' p < .05. 
#' 
#' Using Hotelling’s trace statistic, there was not a significant effect of 
#' therapy on the number of obsessive thoughts and behaviours, T = 0.41, 
#' F(4, 50) = 2.55, p > .05. 
#' 
#' Using Roy’s largest root, there was a significant effect of therapy on the 
#' number of obsessive thoughts and behaviours, Θ = 0.35, F(2, 27) = 4.52, 
#' p < .05. 
#' 
#' *Reporting the follow-up*
#' Using Pillai’s trace, there was a significant effect of therapy on the number
#' of obsessive thoughts and behaviours, V = 0.32, F(4, 54) = 2.56, p < .05. 
#' However, separate univariate ANOVAs on the outcome variables revealed 
#' non-significant treatment effects on obsessive thoughts, F(2, 27) = 2.15, 
#' p > .05, and behaviours, F(2, 27) = 2.77, p > .05.

# Conducting a discriminant analysis/discriminant function analysis ====
ocd_dfa <- lda(Group ~ Actions + Thoughts,
               data = df)

ocd_dfa

# Looking at discriminant scores ====
predict(ocd_dfa)

# Plotting group membership and scores ====
plot(ocd_dfa)

# Writing results for the discriminant analysis ====
#' The MANOVA was followed up with discriminant analysis, which revealed two 
#' discriminant functions. The first explained 82.2% of the variance, whereas 
#' the second explained only 17.8%. The coefficients of the discriminant 
#' functions revealed that function 1 differentiated obsessive behaviours 
#' (b = 0.603) and thoughts (b = -0.335). The second variate produced similar 
#' coefficients for actions (-0.425) and thoughts (-0.339). The discriminant 
#' function plot showed that the first function discriminated the BT group from 
#' the CBT group, and the second function differentiated the notreatment group 
#' from the two interventions.

