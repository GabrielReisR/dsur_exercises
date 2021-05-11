# Chapter 14 - Mixed designs ====

# Initializing ====
library(compute.es)
library(dplyr)
library(ez)
library(ggplot2)
library(magrittr)
library(multcomp)
library(nlme)
library(pastecs)
library(tidyr)
library(WRS2)

df <- read.delim(".\\Data Files\\LooksOrPersonality.dat")

# Arranging dataset ====
#' *participant* as factor
df$participant %<>% as.factor

#' *gender* as factor
df$gender %<>% as.factor

#' Transforming into long format
df %<>%
  pivot_longer(
    cols = att_high:ug_none,
    names_to = 'groups',
    values_to = 'date_rating'
  )

#' Order of columns
df %<>% dplyr::select(participant, gender, groups, date_rating)

#' Creating new factor *personality*
df %<>%
  mutate(personality = case_when(
    (groups == 'att_high' |
       groups == 'av_high' | groups == 'ug_high') ~ 'Charismatic',
    (groups == 'att_some' |
       groups == 'av_some' | groups == 'ug_some') ~ 'Average',
    (groups == 'att_none' |
       groups == 'av_none' | groups == 'ug_none') ~ 'Dullard'
  ))

df$personality <- factor(df$personality,
                   levels = c("Charismatic",
                              "Average",
                              "Dullard"),
                   labels = c("Charismatic",
                              "Average",
                              "Dullard"))

#' Creating new factor *looks*
df %<>%
  mutate(looks = case_when(
    (groups == 'att_high' |
       groups == 'att_some' | groups == 'att_none') ~ 'Attractive',
    (groups == 'av_high' |
       groups == 'av_some' | groups == 'av_none') ~ 'Average',
    (groups == 'ug_high' |
       groups == 'ug_some' | groups == 'ug_none') ~ 'Ugly'
  ))

df$looks <- factor(df$looks,
                         levels = c("Attractive",
                                    "Average",
                                    "Ugly"),
                         labels = c("Attractive",
                                    "Average",
                                    "Ugly"))

# Creating boxplots for each condition ====
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = looks, y = date_rating, color = personality)) +
  
  # Creating boxplot
  geom_boxplot() +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Attractiveness') +
  ylab('Mean Rating of Date') +
  
  # Alterando limites
  ylim(c(0,100)) +
  
  # Mudando cores dos gêneros
  scale_color_brewer(name = 'Charisma',
                     type = 'qual',
                     palette = 'Dark2') +
  
  # Facetando
  facet_wrap(~ gender) +
  
  # Theme
  theme_minimal()

# Getting descriptive statistics ====
by(df$date_rating, 
   list(df$looks, df$personality, df$gender),
   stat.desc, basic = FALSE)

# Setting up contrasts for the ezANOVA analysis ====
#' Only orthogonal contrasts! They must be set just so we can conduct the
#' analysis, although they're not much important here.
#' Creating *personality* contrasts
some_vs_none <- c(1, 1, -2)
high_vs_ave <- c(-1, 1, 0)

contrasts(df$personality) <- cbind(some_vs_none, high_vs_ave)

#' Creating *looks* contrasts
attractive_vs_ugly <- c(1, 1, -2)
attractive_vs_average <- c(-1, 1, 0)

contrasts(df$looks) <- cbind(attractive_vs_ugly, attractive_vs_average)

#' Checking contrasts
contrasts(df$personality)
contrasts(df$looks)

# Conducting mixed ANOVA with ez::ezANOVA() ====
#' *IMPORTANT*
#' If you plan to look at Type III sums of squares then you must set an 
#' orthogonal contrast for all predictors before constructing the model
model <- ezANOVA(data = df,
                 dv = .(date_rating), # dependent variable
                 wid = .(participant), # variable with participants
                 within = .(looks, personality), # the independent variable
                 between = .(gender), # between variable
                 detailed = T, # asking for a detailed output
                 type = 3)

model

# Graph 1 - Gender differences ====
#' To better grasp what it all means, the best approach is to just graph
#' the interactions and the effects
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = gender, y = date_rating)) +
  
  # Creating boxplot
  geom_boxplot() +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Gender') +
  ylab('Mean Rating of Date') +
  
  # Alterando limites
  ylim(c(0,100)) +
  
  # Theme
  theme_minimal()

#' Appears to have no difference. There isn't a significant effect of gender
#' on the ratings, F(1, 18) = .004, p = .945.

# Graph 2 - Looks differences ====
#' To better grasp what it all means, the best approach is to just graph
#' the interactions and the effects
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = looks, y = date_rating)) +
  
  # Creating boxplot
  geom_boxplot() +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Attractiveness') +
  ylab('Mean Rating of Date') +
  
  # Alterando limites
  ylim(c(0,100)) +
  
  # Theme
  theme_minimal()

#' Appears to have differences. There is a significant effect of looks
#' on the ratings, F(2, 36) = 423.73, p < .001. The group means seem to differ.

# Calculating specific effect sizes for looks ====
#' Which of the groups differ?
#' We can interpret the output much the same as the traditional ANOVA way

pairwise.t.test(df$date_rating, df$looks,
                p.adjust.method = 'bonferroni')

#' It seems that all three groups are substantially different from one another.
#' To get specific differences, we can run the compute.es::mes() function
#' The `compute.es::mes()` function takes the form of:
#' `mes(MEAN_group1, MEAN_group2, SD_group1, SD_group2, N_group1, N_group2)`

#' Getting descriptives
by(df$date_rating, df$looks, stat.desc, basic = F)

#' Ugly VS Average
ugly_vs_average <- mes(55.8166667, 67.7833333, 15.1618295, 16.8925735, 60, 60)
ugly_vs_average$d
ugly_vs_average$r

#' Ugly VS Attractive
ugly_vs_attractive <- mes(55.8166667, 82.10, 15.1618295, 14.7483984, 60, 60)
ugly_vs_attractive$d
ugly_vs_attractive$r

#' Average VS Attractive
average_vs_attractive <- mes(67.7833333, 82.10, 16.8925735, 14.7483984, 60, 60)
average_vs_attractive$d
average_vs_attractive$r

# Graph 3 - Personality differences ====
#' To better grasp what it all means, the best approach is to just graph
#' the interactions and the effects
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = personality, y = date_rating)) +
  
  # Creating boxplot
  geom_boxplot() +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Charisma') +
  ylab('Mean Rating of Date') +
  
  # Alterando limites
  ylim(c(0,100)) +
  
  # Theme
  theme_minimal()

#' Appears to have differences. There is a significant effect of personality
#' on the ratings, F(2, 36) = 328.25, p < .001. The group means seem to differ.

# Setting up contrasts for multilevel modelling ====
#' "The key is that the baseline category is coded as 0 for all contrasts 
#' (that’s how R knows it is the baseline). So, we give our baseline group 
#' (average attractiveness) a value of 0 in both contrasts. Then for one of 
#' the contrasts we assign a 1 to attractive and in the other we assign a 1 to 
#' ugly."
#' Defining *looks* contrasts
attract_vs_ave <- c(1, 0, 0)
ugly_vs_ave <- c(0, 0, 1)

contrasts(df$looks) <- cbind(attract_vs_ave, ugly_vs_ave)

#' Defining *looks* contrasts
high_vs_ave <- c(1, 0, 0)
dull_vs_ave <- c(0, 0, 1)

contrasts(df$personality) <- cbind(high_vs_ave, dull_vs_ave)

#' Checking contrasts
contrasts(df$looks) #' *average* (baseline) receives `0` on all conditions
contrasts(df$personality) #' *average* (baseline) receives `0` on all conditions

# Conduction mixed ANOVA as a GLM with nlme::lme() ====
#' What if there were no independent variables?
baseline <- lme(
  date_rating ~ 1, # formula without independent variables
  random = ~1|participant/looks/personality, # scores are dependent on
  data = df,
  method = 'ML')

#' If we want to see the overall effect of each predictor then we need to add 
#' them one at a time.

#' What's our model score with the independent variable 1?
looks_m <- update(baseline, .~. + looks)

#' What's our model score with the independent variable 1 & 2?
personality_m <- update(looks_m, .~. + personality)

#' What's our model score with the independent variable 1 & 2 & 3?
gender_m <- update(personality_m, .~. + gender)

#' What's our model score with the independent variable 1 & 2 & 3 & 1:3?
looks_gender <- update(gender_m, .~. + looks:gender)

#' What's our model score with the independent variable 1 & 2 & 3 & 1:3 & 2:3?
personality_gender <- update(looks_gender, .~. + personality:gender)

#' What's our model score with 1 & 2 & 3 & 1:3 & 2:3 & 1:2?
looks_personality <- update(personality_gender, .~. + looks:personality)

#' What's our model score with 1 & 2 & 3 & 1:3 & 2:3 & 1:2?
speed_dating_model <- update(looks_personality, .~. + looks:personality:gender)

#' Is *model* significantly better than *baseline* and so on?
anova_output <- anova(
  baseline,
  looks_m,
  personality_m,
  gender_m,
  looks_gender,
  personality_gender,
  looks_personality,
  speed_dating_model
)

#' Writing results (remember that df = df2 - df1):
#' "By adding looks as a predictor we increase the degrees of freedom by 2 (the 
#' two contrasts that we used to code this variable) and significantly improve 
#' the model. In other words, the attractiveness of the date had a significant 
#' effect on ratings, χ2(2) = 68.30, p < .0001. Next, we see the effect of 
#' adding the main effect of personality into the model (compared to the 
#' previous model that contained only the effect of looks). Again the degrees 
#' of freedom are increased by 2 (the two contrasts used to code this variable) 
#' and the fit of the model is significantly improved; the personality of the 
#' date had a significant effect on attitudes, χ2(2) = 138.76, p < .0001. The 
#' next model tells us whether adding gender improved the fit of the model; it 
#' did not, indicating that gender did not have a significant overall effect on 
#' ratings χ2(1) = 0.002, p = .966. This effect adds only 1 degree of freedom 
#' because it was coded with a single contrast.
#' 
#' The next model (looks_gender) shows that the looks × gender interaction is 
#' also significant, χ2(2) = 39.54, p < .0001. This interaction adds 2 degrees 
#' of freedom (because looks is coded with two contrasts and gender only one, 
#' so we get 2 × 1 = 2 df). This significant interaction means that although 
#' the ratings were affected by whether the date was attractive, average or 
#' ugly, the way in which ratings were affected by attractiveness was different 
#' in male and female raters.
#' 
#' The next model (personality_gender) shows that the personality × gender 
#' interaction is also significant, χ2(2) = 57.96, p < .0001, indicating that 
#' this effect of charisma differed in male and female raters. This interaction 
#' adds 2 degrees of freedom (because personality is coded with two contrasts 
#' and gender only one, so we get 2 × 1 = 2 df).
#' 
#' The next model (looks_personality) tells us that there is a significant 
#' interaction between looks and personality, χ2(4) = 77.14, p < .0001. This 
#' interaction adds 4 degrees of freedom (because it is made up of two variables 
#' each coded with two contrasts, so we get 2 × 2 = 4 df). This interaction term 
#' means that if we ignore the gender of the rater, the profile of ratings 
#' across different levels of attractiveness was different for highly 
#' charismatic dates, charismatic dates and dullards. (It is equally true to 
#' say this the opposite way around: the profile of ratings across different 
#' levels of charisma was different for attractive, average and ugly dates.)
#' 
#' The final model (speedDateModel) shows that the looks × personality × gender 
#' interaction is also significant, χ2(4) = 79.59, p < .0001, meaning that the 
#' looks × personality interaction was significantly different in male and 
#' female participants. This interaction adds 4 degrees of freedom (because 
#' personality is coded with two contrasts, looks is also coded with two 
#' contrasts, and gender with only one, so we get 2 × 2 × 1 = 4 df)."

# Looking at the results ====
#' *IMPORTANT*
#' "Because there are significant interactions involving the variables, we 
#' shouldn’t really interpret the main effects (because the higher-order 
#' interactions supersede it)."

final_model <- summary(speed_dating_model)
model_coefs <- final_model$tTable

# Graph 1 - The main effect of gender ====
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = gender, y = date_rating)) +
  
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
  
  # Mudando nome de todas as partes do gráfico
  xlab('Gender') +
  ylab('Mean Rating of Date') +
  
  # Alterando limites
  ylim(c(0,100)) +
  
  # Theme
  theme_minimal()

anova_output

#' "Gender did not have a significant overall effect on ratings of the dates, 
#' χ2(1) = 0.002, p = .966"

# Graph 2 - The main effect of looks ====
#' *IMPORTANT*
#' "Because there are significant interactions involving the variables, we 
#' shouldn’t really interpret the main effects (because the higher-order 
#' interactions supersede it)."

df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = looks, y = date_rating)) +
  
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
  
  # Mudando nome de todas as partes do gráfico
  xlab('Attractiveness') +
  ylab('Mean Rating of Date') +
  
  # Alterando limites
  ylim(c(0,100)) +
  
  # Theme
  theme_minimal()

anova_output

#' "We can report that the attractiveness of the date had a significant effect 
#' on ratings, χ2(2) = 68.30, p < .0001."

by(df$date_rating, df$looks, stat.desc, basic = F)

#' Looking at the specific differences in the model's output
model_coefs

#' "The first contrast that we set (AttractivevsAv) shows that attractive dates 
#' were rated significantly higher than average dates, b = 18.2, t(36) = 7.58, 
#' p < .001. The second contrast (UglyvsAv) shows that average dates were rated
#' significantly higher than ugly ones, −17.7, t(36) = −7.37, p < .001."

# Graph 3 - The main effect of personality ====
#' *IMPORTANT*
#' "Because there are significant interactions involving the variables, we 
#' shouldn’t really interpret the main effects (because the higher-order 
#' interactions supersede it)."
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = personality, y = date_rating)) +
  
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
  
  # Mudando nome de todas as partes do gráfico
  xlab('Charisma') +
  ylab('Mean Rating of Date') +
  
  # Alterando limites
  ylim(c(0,100)) +
  
  # Theme
  theme_minimal()

anova_output

#' "We can report that there was a significant main effect of charisma, 
#' χ2(2) = 138.76, p < .0001"

by(df$date_rating, df$personality, stat.desc, basic = F)

#' Looking at the specific differences in the model's output
model_coefs

#' "The first contrast that we set (HighvsAv) shows that highly charismatic 
#' dates were rated significantly higher than dates with average charisma, 
#' b = 19.5, t(108) = 8.12, p < .001. The second contrast (DullvsAv) shows that 
#' dates with average charisma were rated significantly higher than dullards, 
#' b = −21.9, t(108) = −9.12, p < .001"



# Graph 4 - The interaction gender:looks ====
#' *IMPORTANT*
#' "We wouldn’t normally interpret this interaction because the significant 
#' higher-order three-way interaction supersedes it."
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = looks, y = date_rating, color = gender)) +
  
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
               aes(x = as.numeric(looks), y = date_rating)) +
  
  # Mudando cores dos gêneros
  scale_color_brewer(name = 'Gender',
                     type = 'qual',
                     palette = 'Dark2') +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Attractiveness') +
  ylab('Mean Rating of Date') +
  
  # Alterando limites
  ylim(c(0,100)) +
  
  # Theme
  theme_minimal()

anova_output

#' "We can report that there was a significant interaction between the 
#' attractiveness of the date and the gender of the participant, χ2(2) = 39.54,
#' p < .0001"

by(df$date_rating, 
   list(df$looks, df$gender),
   stat.desc, basic = F)

#' Looking at the specific differences in the model's output
model_coefs

#' The interaction doesn't seem to be significant, although we can notice a 
#' sharper decline in men's interest going from attractive to average and then
#' to ugly.
#' 
#' "The first contrast for the looks × gender interaction term 
#' (AttractivevsAv:gender) compares male and female ratings of attractive 
#' relative to average-looking dates. This contrast is not significant, 
#' b = −1.5, t(36) = −0.44, p = .661. This result tells us that the increased 
#' interest in attractive dates compared to average-looking dates found for men 
#' is not significantly more than for women."
#' 
#' "The second contrast (UglyvsAv:gender) compares male and female ratings of 
#' ugly relative to average-looking dates. This contrast is not significant, 
#' b = −5.8, t(36) = −1.71, p = .096, which suggests that the decreased interest 
#' in ugly dates compared to average-looking dates found for male raters is not 
#' significantly different than for female raters."
#' 
#' "We can conclude that the preferences for average-looking dates, compared to 
#' ugly dates, are similar for males and females."

# Graph 5 - The interaction gender:personality ====
#' *IMPORTANT*
#' "We wouldn’t normally interpret this interaction because the significant 
#' higher-order three-way interaction supersedes it."
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = personality, y = date_rating, color = gender)) +
  
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
               aes(x = as.numeric(personality), y = date_rating)) +
  
  # Mudando cores dos gêneros
  scale_color_brewer(name = 'Gender',
                     type = 'qual',
                     palette = 'Dark2') +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Charisma') +
  ylab('Mean Rating of Date') +
  
  # Alterando limites
  ylim(c(0,100)) +
  
  # Theme
  theme_minimal()

anova_output

#' "We can report that there was a significant interaction between the 
#' attractiveness of the date and the gender of the participant, χ2(2) = 57.96,
#' p < .0001. This effect suggests that the profile of ratings across dates of 
#' different levels of charisma was different for men and women."

by(df$date_rating, 
   list(df$personality, df$gender),
   stat.desc, basic = F)

#' Looking at the specific differences in the model's output
model_coefs

#' The interaction doesn't seem to be significant, although we can notice a 
#' sharper decline in men's interest going from attractive to average and then
#' to ugly.

#' "The first contrast for this interaction term (HighvsAv:gender) looks at high 
#' charisma compared to average charisma, comparing male and female scores. This 
#' contrast is significant, b = −8.5, t(108) = −2.50, p = .014. This result 
#' tells us that the increased interest in highly charismatic dates compared to 
#' averagely charismatic dates found for women is significantly more than for 
#' men. We can conclude that the preferences for very charismatic dates, 
#' compared to averagely charismatic dates, are significantly greater for 
#' females than males."
#' 
#' "The second contrast for this interaction term (DullvsAv:gender) looks at 
#' differences in male and female ratings of dullards compared to dates with 
#' average charisma. This contrast is not significant, b = −2.1, t(108) = −0.62,
#' p = .538. This result tells us that the decreased  interest in dull dates 
#' compared to averagely charismatic dates found for women is not significantly 
#' more than for men. We can conclude that the preferences for dates with some 
#' charisma over dullards are similar for females than males."

# Graph 6 - The interaction looks:personality ====
#' *IMPORTANT*
#' "We wouldn’t normally interpret this interaction because the significant 
#' higher-order three-way interaction supersedes it."
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = looks, y = date_rating, color = personality)) +
  
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
               aes(x = as.numeric(looks), y = date_rating)) +
  
  # Mudando cores dos gêneros
  scale_color_brewer(name = 'Charisma',
                     type = 'qual',
                     palette = 'Dark2') +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Attractiveness') +
  ylab('Mean Rating of Date') +
  
  # Alterando limites
  ylim(c(0,100)) +
  
  # Theme
  theme_minimal()

anova_output

#' "We can report that there was a significant interaction between the 
#' attractiveness of the date and the charisma of the date, χ2(4) = 77.14, 
#' p < .0001. This effect tells us that the profile of ratings across dates of 
#' different levels of charisma was different for attractive, average and 
#' ugly dates."

by(df$date_rating, 
   list(df$looks, df$personality),
   stat.desc, basic = F)

#' Looking at the specific differences in the model's output
model_coefs

#' "We can report that there was a significant interaction between the 
#' attractiveness of the date and the charisma of the date, χ2(4) = 77.14, 
#' p < .0001. This effect tells us that the profile of ratings across dates of 
#' different levels of charisma was different for attractive, average and ugly 
#' dates."
#' 
#' "This is like asking: is the difference between high charisma and average 
#' charisma the same for ugly people and average-looking people? I have again 
#' extracted the relevant bit of the interaction graph (Figure 14.10). You can 
#' see that the interest (as indicated by high ratings) decreases from 
#' average-looking dates to ugly ones in both high- and some-charisma dates; 
#' however, this fall is slightly greater in the average-charisma dates (the 
#' light blue line is slightly steeper). The contrast is significant, b = 16.0, 
#' t(108) = 4.71, p < .001, and tells us that as dates become less attractive 
#' there is a greater decline in interest when charisma is low compared to when 
#' charisma is high."
#' 
#' "This is like asking: is the difference between no charisma and average 
#' charisma the same for attractive people and average-looking people? Again, 
#' the best way to understand what this contrast is testing is to extract the 
#' relevant bit of the interaction graph (see Figure 14.11). If you look at this 
#' you can see that the interest (as indicated by high ratings) in attractive 
#' dates was higher when they had some charisma than when they were a dullard. 
#' The same is also true for average-looking dates. In fact the two lines are 
#' fairly parallel. The contrast, however, is significant, b = −13.4, 
#' t(108) = −3.95, p < .001, and tells us that as dates become less attractive 
#' the decline in interest is different depending on whether charisma is 
#' average or low."
#' 
#' "This is like asking: is the difference between no charisma and some charisma 
#' the same for ugly people and average-looking people? Figure 14.12 shows the 
#' relevant bits of the interaction graph; you can see that the interest (as 
#' indicated by high ratings) in average-looking dates was higher when they had 
#' some charisma than when they were a dullard, but for ugly dates the ratings 
#' were roughly the same regardless of the level of charisma. This contrast is 
#' highly significant, b = 16.8, t(108) = 4.95, p < .001, and tells us that as 
#' dates become less attractive the decline in interest in dates with a bit of 
#' charisma is significantly greater than for dullards."

#






# Graph 7 - Higher-order effet: The interaction looks:personality:gender ====
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = looks, y = date_rating, color = personality)) +
  
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
               aes(x = as.numeric(looks), y = date_rating)) +
  
  # Mudando cores dos gêneros
  scale_color_brewer(name = 'Charisma',
                     type = 'qual',
                     palette = 'Dark2') +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Attractiveness') +
  ylab('Mean Rating of Date') +
  
  # Alterando limites
  ylim(c(0,100)) +
  
  # Facetando
  facet_wrap(~ gender) +
  
  # Theme
  theme_minimal()

anova_output

#' "The three-way interaction tells us whether the looks × personality 
#' interaction described above is the same for men and women (i.e., whether the
#' combined effect of attractiveness of the date and their level of charisma is 
#' the same for male participants as for female subjects). Output 14.3 tells us 
#' that there is a significant three-way looks × personality × gender 
#' interaction, χ2(4) = 79.59, p < .0001. This is the highest-order effect that 
#' is significant, and consequently, we would ordinarily focus on interpreting 
#' this effect and not all the lower-order ones"

#' Looking at the specific differences in the model's output
model_coefs

# Interaction 1 - looks:personality:gender ====
#' attractive vs. average,
#' high charisma vs. some charisma, 
#' male vs. female

model_coefs

# Creating graph by excluding non-present variables
df %>% 
  
  filter(looks != "Ugly" & personality != 'Dullard') %>% 
  
  # Criando primeira camada
  ggplot(aes(x = looks, y = date_rating, color = personality)) +
  
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
               aes(x = as.numeric(looks), y = date_rating)) +
  
  # Mudando cores dos gêneros
  scale_color_brewer(name = 'Charisma',
                     type = 'qual',
                     palette = 'Dark2') +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Attractiveness') +
  ylab('Mean Rating of Date') +
  
  # Alterando limites
  ylim(c(0,100)) +
  
  # Facetando
  facet_wrap(~ gender) +
  
  # Theme
  theme_minimal()

#' "The first contrast for this interaction term compares ratings for attractive 
#' dates to average-looking dates, when high charisma is compared to average 
#' charisma in males compared to females, b = 5.8, t(108) = 1.21, p = .230. The 
#' interaction graph in Figure 14.14 shows that interest (as indicated by high 
#' ratings) in attractive dates was the same regardless of whether they had high 
#' or average charisma. However, for average-looking dates, there was more 
#' interest when that person had high charisma rather than some charisma. Most 
#' important, this pattern of results is the same in males and females, and this 
#' is reflected in the nonsignificance of this contrast."

# Interaction 2 - looks:personality:gender ====
#' ugly vs. average,
#' high charisma vs. some charisma,
#' males vs. females

model_coefs

# Creating graph by excluding non-present variables
df_n <- df %>% 
  filter(looks != 'Attractive' & personality != 'Dullard') 

df_n$looks <- factor(df_n$looks,
                     levels = c("Average", "Ugly"))

df_n %>% 
  
  # Criando primeira camada
  ggplot(aes(x = looks, y = date_rating, color = personality)) +
  
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
               aes(x = as.numeric(looks), y = date_rating)) +
  
  # Mudando cores dos gêneros
  scale_color_brewer(name = 'Charisma',
                     type = 'qual',
                     palette = 'Dark2') +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Attractiveness') +
  ylab('Mean Rating of Date') +
  
  # Alterando limites
  ylim(c(0,100)) +
  
  # Facetando
  facet_wrap(~ gender) +
  
  # Theme
  theme_minimal()

# Results
model_coefs

#' "The second contrast for this interaction term compares interest in ugly 
#' compared to average-looking dates, when high charisma is compared to average 
#' charisma, in men compared to women. The interaction graph in Figure 14.15 
#' shows that the patterns are different for men and women. This is reflected 
#' by the fact that the contrast is significant, b = −18.5, t(108) = −3.85, 
#' p < .001. To unpick this we need to look at the graph. First, let’s look at 
#' the men. For men, as attractiveness goes down, so does interest when the date 
#' has high charisma and when they have average charisma. In fact the lines are 
#' parallel. So, regardless of charisma, there is a similar reduction in 
#' interest as attractiveness declines. For women the picture is quite 
#' different. When charisma is high, there is no decline in interest as 
#' attractiveness falls (the black line is flat); however, when charisma is 
#' average, the attractiveness of the date does matter and interest is lower in 
#' an ugly date than in an average-looking date. Another way to look at it is 
#' that for dates with average charisma, the reduction in interest as 
#' attractiveness goes down is about the same in men and women (the light blue 
#' lines have the same slope). However, for dates who have high charisma, the 
#' decrease in interest if these dates are ugly rather than average looking is 
#' much more dramatic in men than women (the black line is much steeper for men 
#' than it is for women). This is what the significant contrast tells us."










































# Interaction 3 - looks:personality:gender ====
#' attractive vs. average,
#' dullard vs. some charisma,
#' male vs. female

model_coefs

# Creating graph by excluding non-present variables
df %>% 
  filter(looks != 'Ugly' & personality != 'Charismatic') %>% 

  # Criando primeira camada
  ggplot(aes(x = looks, y = date_rating, color = personality)) +
  
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
               aes(x = as.numeric(looks), y = date_rating)) +
  
  # Mudando cores dos gêneros
  scale_color_brewer(name = 'Charisma',
                     type = 'qual',
                     palette = 'Dark2') +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Attractiveness') +
  ylab('Mean Rating of Date') +
  
  # Alterando limites
  ylim(c(0,100)) +
  
  # Facetando
  facet_wrap(~ gender) +
  
  # Theme
  theme_minimal()

# Results
model_coefs

#' "The third contrast for this interaction term compares interest in attractive 
#' compared to average-looking dates, when dullards are compared to average 
#' charisma, in men compared to women. The interaction graph in Figure 14.16 
#' shows that the patterns are different for men and women. This is reflected by 
#' the fact that the contrast is significant, b = 36.2, t(108) = 7.54, p < .001. 
#' To unpick this effect we need to look at the graph. First, if we look at 
#' average-looking dates, for both men and women more interest is expressed when 
#' the date has average charisma than when they are a dullard (and the distance 
#' between the lines is about the same). So the difference doesn’t appear to be 
#' here. If we now look at attractive dates, we see that men are equally 
#' interested in their dates regardless of their charisma, but women are much 
#' less interested in an attractive person if they are a dullard. Put another 
#' way, for attractive dates, the distance between the lines is much smaller for 
#' men than it is for women. Another way to look at it is that for dates with 
#' average charisma, the reduction in interest as attractiveness goes down is 
#' about the same in men and women (the black lines have the same slope). 
#' However, for dates who are dullards, the decrease in interest if these dates 
#' are average-looking rather than attractive is much more dramatic in men than 
#' women (the light blue line is much steeper for men than it is for women)."



























# Interaction 4 - looks:personality:gender ====
#' ugly vs. average,
#' dullard vs. some charisma,
#' male vs. female

model_coefs

# Creating graph by excluding non-present variables
df_n <- df %>% 
  filter(looks != 'Attractive' & personality != 'Charismatic') 

df_n$looks <- factor(df_n$looks,
                     levels = c("Average", "Ugly"))

df_n %>% 
  
  # Criando primeira camada
  ggplot(aes(x = looks, y = date_rating, color = personality)) +
  
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
               aes(x = as.numeric(looks), y = date_rating)) +
  
  # Mudando cores dos gêneros
  scale_color_brewer(name = 'Charisma',
                     type = 'qual',
                     palette = 'Dark2') +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Attractiveness') +
  ylab('Mean Rating of Date') +
  
  # Alterando limites
  ylim(c(0,100)) +
  
  # Facetando
  facet_wrap(~ gender) +
  
  # Theme
  theme_minimal()

# Results
model_coefs

#' "The final contrast for this interaction term compares interest in ugly 
#' compared to averagelooking dates, when comparing dullards to average 
#' charisma, in men compared to women. The interaction graph in Figure 14.17 
#' shows that interest (as indicated by high ratings) in ugly dates was the same 
#' regardless of whether they had average charisma or were a dullard. However, 
#' for average-looking dates, there was more interest when that person had some 
#' charisma rather than if they were a dullard. Most important, this pattern of 
#' results is similar in males and females, and this is reflected in the 
#' non-significance of this contrast, b = 4.7 , t(108) = 0.98, p = .330."

# Calculating rcontrast effect sizes for different comparisons on lme() ====
#' *R = SQRT [ t² / (t² + df)]*
rcontrast <- function(t, df) {
  
  r <- sqrt(t^2 / (t ^ 2 + df))
  
  print(paste("r = ", r))
  
}

#' Getting comparisons
model_coefs

#' *Interaction 1*
#' attractive vs. average,
#' high charisma vs. some charisma, 
#' male vs. female

rcontrast(1.2080154, 108)

#' *Interaction 2*
#' ugly vs. average,
#' high charisma vs. some charisma,
#' males vs. females

rcontrast(-3.8531525, 108)

#' *Interaction 3*
#' attractive vs. average,
#' dullard vs. some charisma,
#' male vs. female

rcontrast(7.5396823, 108)

#' *Interaction 4*
#' ugly vs. average,
#' dullard vs. some charisma,
#' male vs. female

rcontrast(0.9789090, 108)

# Reporting the results of mixed ANOVA via ez::ezANOVA() ====
# ez::ezANOVA() results
model

#' *Reporting general results*
#' All effects are reported as significant at p < .05. There were significant 
#' main effects of the attractiveness of the date, F(2, 36) = 423.73, and the 
#' amount of charisma the date possessed, F(2, 36) = 328.25 on interest 
#' expressed by the participant. However, the ratings from male and female 
#' participants were, in general, the same, F(1, 18) < 1, r = .02.

#' *Reporting lower-order interactions*
#' "There were significant interaction effects of the attractiveness of the date 
#' and gender of the participant, F(2, 36) = 80.43, the level of charisma of the 
#' date and the gender of the participant, F(2, 36) = 62.45, and the level of 
#' charisma of the date and the attractiveness of the date, F(4, 72) = 36.63."

#' *Reporting higher-order interactions*
#' "Most important, the looks × personality × gender interaction was 
#' significant, F(4, 72) = 24.12. This indicates that the looks × personality 
#' interaction described previously was different in male and female 
#' participants."

# Reporting the results of mixed ANOVA via multilevel modelling ====
# nlme::lme() results
final_model
model_coefs

#' *Reporting general results*
#' "There were significant main effects of the attractiveness of the date, 
#' χ2(2) = 68.30, p < .0001, and the amount of charisma the date possessed, 
#' χ2(2) = 138.76, p < .0001, on interest expressed by the participant. However,
#' the ratings from male and female participants were, in general, the same, 
#' χ2(1) = 0.002, p = .966."

#' *Reporting lower-order interactions*
#' "There were significant interaction effects of the attractiveness of the date
#' and the gender of the participant, χ2(2) = 39.54, p < .0001, the level of 
#' charisma of the date and the gender of the participant, χ2(2) = 57.96, 
#' p < .0001, and the level of charisma of the date and the attractiveness of 
#' the date, χ2(4) = 77.14, p < .0001."

#' *Reporting higher-order interactions*
#' "Most important, the looks × personality × gender interaction was 
#' significant, χ2(4) = 79.59, p < .0001. This indicates that the 
#' looks × personality interaction described previously was different in male 
#' and female participants. Contrasts were used to break down this interaction; 
#' these contrasts compared male and females scores at each level of charisma 
#' compared to the middle category of ‘average charisma’ across each level of 
#' attractiveness compared to the category of average attractiveness.
#' 
#' The first contrast revealed a non-significant difference between male and 
#' female responses when comparing attractive dates to average-looking dates 
#' when the date had high charisma compared to some charisma, b = 5.8, 
#' t(108) = 1.21, p = .230, r = .12, and tells us that for both males and 
#' females, as dates become less attractive there is a greater decline in 
#' interest when charisma is average compared to when it is high.
#' 
#' The second contrast looked for differences between males and females when 
#' comparing ugly dates to average-looking dates when the date had high charisma
#' compared to average charisma. This contrast was significant, b = −18.5, 
#' t(108) = −3.85, p < .001, r = .35, and tells us that for dates with average 
#' charisma, the reduction in interest as attractiveness goes down is about the 
#' same in men and women, but for dates who have high charisma, the decrease in 
#' interest if these dates are ugly rather than average-looking is much more 
#' dramatic in men than women. 
#' 
#' The third contrast investigated differences between males and females when 
#' comparing attractive dates to average-looking dates when the date was a 
#' dullard compared to when they had average charisma. This contrast was 
#' significant, b = 6.2, t(108) = 7.54, p < .001, r = .59, and tells us that 
#' for dates with average charisma, the reduction in interest as attractiveness 
#' goes down is about the same in men and women, but for dates who are dullards, 
#' the decrease in interest if these dates are average-looking rather than
#' attractive is much more dramatic in men than women. 
#' 
#' The final contrast looked for differences between men and women when 
#' comparing ugly dates to average-looking dates when the date was a dullard 
#' compared to when they had average charisma. This contrast was not 
#' significant, b = 4.7 , t(108) = 0.98, p = .330, r = .09, and tells us that 
#' for both men and women, as dates become less attractive the decline in 
#' interest in dates with average charisma is greater than for dullards."

# 



















