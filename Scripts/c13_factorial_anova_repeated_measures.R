# Chapter 13 - Factorial Repeated-measures Design ====

# Initializing ====
library(ez)
library(dplyr)
library(ggplot2)
library(magrittr)
library(multcomp)
library(nlme)
library(pastecs)
library(tidyr)
library(WRS2)

df <- read.delim(".\\Data Files\\Attitude.dat")

# Arranging dataset ====
#' Order of columns
df %<>% dplyr::select(participant, everything())

#' Participant as factor
df$participant %<>% as.factor

#' Transforming into long format
df %<>%
  pivot_longer(
    cols = beerpos:waterneu,
    names_to = 'groups',
    values_to = 'attitude'
  )

#' Creating new factor *drink*
df %<>%
  mutate(drink = case_when(
    (groups == 'beerpos' |
       groups == 'beerneg' | groups == 'beerneut') ~ 'Beer',
    (groups == 'winepos' |
       groups == 'wineneg' | groups == 'wineneut') ~ 'Wine',
    (groups == 'waterpos' |
       groups == 'waterneg' | groups == 'waterneu') ~ 'Water'
  ))

df$drink <- factor(df$drink,
                   levels = c("Beer",
                               "Wine",
                               "Water"),
                   labels = c("Beer",
                              "Wine",
                              "Water"))

#' Creating new factor *imagery*
df %<>%
  mutate(imagery = case_when(
    (groups == 'beerpos' |
       groups == 'winepos' | groups == 'waterpos') ~ 'Positive',
    (groups == 'beerneg' |
       groups == 'wineneg' | groups == 'waterneg') ~ 'Negative',
    (groups == 'beerneut'|
       groups == 'wineneut' | groups == 'waterneu') ~ 'Neutral'
  ))

df$imagery <- factor(df$imagery,
                     levels = c("Positive",
                                "Negative",
                                "Neutral"),
                     labels = c("Positive",
                                "Negative",
                                "Neutral"))

r

# Getting descriptive statistics ====
by(df$attitude, list(df$drink, df$imagery), stat.desc)

# Setting up contrasts ====
#' Alcohol (Beer & Wine) X Non-Alcohol (Water), Alcohol (Beer) X Alcohol (Wine) 
alcohol_vs_non_alcohol <- c(1, 1, -2)
beer_vs_wine <- c(-1, 1, 0)
contrasts(df$drink) <- cbind(alcohol_vs_non_alcohol,
                             beer_vs_wine)

#' Negative Img. X Positive & Neutral Img., Positive Img. X Neutral Img.
negative_vs_positive_and_neutral <- c(1, -2, 1)
positive_vs_neutral <- c(-1, 0, 1)
contrasts(df$imagery) <- cbind(negative_vs_positive_and_neutral,
                               positive_vs_neutral)

#' Checking out contrasts
contrasts(df$drink)
contrasts(df$imagery)

# Conduction factorial repeated-measures ANOVA with ez::ezANOVA() ====
model <- ezANOVA(data = df,
                 dv = .(attitude), # dependent variable
                 wid = .(participant), # variable with participants
                 within = .(drink, imagery), # the independent variable
                 #between = .(variable), # between variable, in this case EMPTY
                 detailed = T, # asking for a detailed output
                 type = 3) 

model

# Looking at the effect of drink ====
by(df$attitude, df$drink, stat.desc, basic = F)

df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = drink, y = attitude)) +
  
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
  xlab('Type of Drink') +
  ylab('Mean Preference Score') +
  
  # Alterando limites
  ylim(c(-100,100)) +
  
  # Theme
  theme_minimal()

# Looking at the effect of imagery ====
by(df$attitude, df$imagery, stat.desc, basic = F)

df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = imagery, y = attitude)) +
  
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
  xlab('Type of Imager') +
  ylab('Mean Preference Score') +
  
  # Alterando limites
  ylim(c(-100,100)) +
  
  # Theme
  theme_minimal()

# Looking at the interaction effect ====
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = drink, y = attitude, color = imagery)) +
  
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
               aes(x = as.numeric(drink), y = attitude)) +
  
  # Mudando cores dos gêneros
  scale_color_brewer(name = 'Type of Imagery',
                     type = 'qual',
                     palette = 'Dark2') +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Type of Drink') +
  ylab('Mean Preference Score') +
  
  # Alterando limites
  ylim(c(-45,45)) +
  
  # Colocando tema
  theme_minimal()

# Post hoc tests using Bonferroni ====
pairwise.t.test(df$attitude, df$groups, 
                paired = TRUE,
                p.adjust.method = "bonferroni")

# Conduction factorial repeated-measures as a GLM with nlme::lme() ====
#' What if there were no independent variables?
baseline <- lme(attitude ~ 1, # formula without independent variable
                random = ~1|participant/drink/imagery, # scores are dependent on
                data = df,
                method = 'ML')

#' If we want to see the overall effect of each predictor then we need to add 
#' them one at a time.

#' What's our model score with the independent variable 1?
model_1 <- update(baseline, .~. + drink)

#' What's our model score with the independent variable 1 & 2?
model_2 <- update(model_1, .~. + imagery)

#' What's our model score with the independent variable 1 & 2 & 1:2?
model_3 <- update(model_2, .~. + drink:imagery)

#' Is *model* significantly better than *baseline* and so on?
anova(baseline, model_1, model_2, model_3)

#' Writing results (remember that df = df2 - df1):
#' "The interaction term is made up of four contrasts (the number of contrasts 
#' for each variable in the interaction multiplied) and significantly improves
#' the model fit; therefore, attitudes were significantly affected by the 
#' combined effect of the type of drink and type of imagery, 
#' χ²(4) = 42.0, p < .001."

#' Looking at *model_3*'s results again
summary(model_3)

#' Writing results:
#' "First, we get the two contrasts for drink, which show a significant effect
#' on attitudes when comparing alcoholic drinks to water, b = 2.19, 
#' t(38) = 3.18, p = .003, but not when comparing beer with wine b = -1.75,
#' t(38) = -1.47, p = .150. Next, we get the two contrasts for imagery, which 
#' show a significant effect on attitudes when comparing negative imagery to 
#' other types, b = 6.74, t(114) = 17.26, p < .001, and when comparing positive 
#' to neutral imagery, b = -6.63, t(114) = -9.81, p < .001. The next four 
#' effects are the contrasts for the interaction term and we’ll look at these 
#' in turn."

# Understanding interaction terms: part 1 ====
#' Look at *drinkalcohol_vs_non_alcohol*
#' Look at *imagerynegative_vs_positive_and_neutral*
#' Look at *drinkalcohol_vs_non_alcohol:imagerynegative_vs_positive_and_neutral*
summary(model_3)

#' Question: 'Negative imagery is associated with lower attitude scores. Is the 
#' negative imagery VS positive&neutral imagery effect the same in alcoholic 
#' drinks VS non-alcoholic drinks? The answer is yes, it's the same; b = 0.19, 
#' t(114) = 0.69, p = .492.

#' "The first interaction term looks at the effect of alcoholic drinks (i.e., 
#' wine and beer combined) relative to water when comparing negative imagery 
#' with other types of imagery (i.e., positive and neutral combined). This 
#' contrast is non-significant. This result tells us that the decreased liking 
#' found when negative imagery is used (compared to other forms) is the same for
#' both alcoholic drinks and water. The top left panel of Figure 13.10 shows the
#' means being compared. The gap between the lines, which represents the effect 
#' of negative imagery compared to other forms, is roughly the same for 
#' alcoholic drinks and water. This finding indicates that the effect of 
#' negative imagery (compared to other forms) in lowering attitudes is 
#' comparable in alcoholic and non-alcoholic drinks, 
#' b = 0.19, t(114) = 0.69, p = .492."

# See graph in page 597

# Understanding interaction terms: part 2 ====
#' Look at *drinkbeer_vs_wine*
#' Look at *imagerynegative_vs_positive_and_neutral*
#' Look at *drinkbeer_vs_wine:imagerynegative_vs_positive_and_neutral*
summary(model_3)

#' Question: 'Negative imagery is associated with lower attitude scores. Is this
#' effect the same when comparing beer and wine? No, it's not the same. This
#' effect is significantly smaller in beer; b = 3.24, t(114) = 6.77, p < .001.'

#' "The second interaction term looks at whether the effect of negative imagery 
#' compared to other types of imagery (i.e., positive and neutral combined) is 
#' comparable in beer and wine. This contrast is significant. This result tells 
#' us that the decreased liking found when negative imagery is used (compared to
#' other forms) is different in beer and wine. The top right panel of Figure 
#' 13.10 shows the means being compared. The gap between the lines, which 
#' represents the effect of negative imagery compared to other forms, is much 
#' bigger for wine than it is for beer. This finding suggests that the effect 
#' of negative imagery (compared to other forms) in lowering attitudes to beer
#' was significantly smaller than for wine, b = 3.24, t(114) = 6.77, p < .001."

# See graph in page 597

# Understanding interaction terms: part 3 ====
#' Look at *drinkalcohol_vs_non_alcohol*
#' Look at *imagerypositive_vs_neutral*
#' Look at *drinkalcohol_vs_non_alcohol:imagerypositive_vs_neutral*
summary(model_3)

#' Question: 'Positive imagery is associated with higher attitude scores
#' when compared to neutral imagery. Is this effect the same when comparing 
#' alcoholic VS non-alcoholic drinks? Yes, this effect is the same, 
#' b = 0.45, t(114) = 0.93, p = .353.'

#' "The third interaction term looks at whether the effect of positive imagery 
#' (compared to neutral) is comparable in alcoholic drinks (i.e., wine and beer 
#' combined) relative to water. This contrast is non-significant. This result 
#' tells us that the increased liking found when positive imagery is used 
#' (compared to neutral) is similar for both alcoholic drinks and water. The 
#' bottom left panel of Figure 13.10 shows the means being compared. The 
#' distance between the lines, which represents the effect of positive imagery 
#' compared to neutral, is roughly the same for beer as it is for wine. This 
#' finding suggests that positive imagery has a similar effect in increasing 
#' attitudes (compared to neutral imagery) in both alcoholic and non-alcoholic 
#' drinks, b = 0.45, t(114) = 0.93, p = .353."

# See graph in page 597

# Understanding interaction terms: part 4 ====
#' Look at *drinkbeer_vs_wine*
#' Look at *imagerypositive_vs_neutral*
#' Look at *drinkbeer_vs_wine:imagerypositive_vs_neutral*
summary(model_3)

#' Question: 'Positive imagery is associated with higher attitude scores
#' when compared to neutral imagery. Is this effect the same when comparing 
#' beer VS wine? Yes, this effect is the same, 
#' b = -0.66, t(114) = -0.80, p = .426.'

#' "The final interaction term looks at whether the effect of positive imagery 
#' compared to neutral is comparable in beer and wine. This contrast is not 
#' significant. This result tells us that the increased liking found when 
#' positive imagery is used (compared to neutral) is comparable in beer and 
#' wine. The bottom right panel of Figure 13.10 shows the means being compared. 
#' Note that the distance between the lines (i.e., the effect of positive 
#' imagery compared to neutral) is roughly the same for beer as it is for wine. 
#' In summary, the effect of positive imagery (compared to neutral) in 
#' increasing attitudes to beer was not significantly different to that for 
#' wine, b = -0.66, t(114) = -0.80, p = .426."

# See graph in page 597

# Calculating generalised eta-squared effect sizes with ez::ezANOVA() ====
#' The model shows the generalised eta-squared effect sizes in the column *ges*
model

#' In this case the values were .575 for the main effect of imagery, .116 for 
#' the main effect of drink and .141 for the interaction term.

# Calculating rcontrast effect sizes for different comparisons on lme() ====
#' *R = SQRT [ t² / (t² + df)]*
rcontrast <- function(t, df) {
  
  r <- sqrt(t^2 / (t ^ 2 + df))
  
  print(paste("r = ", r))
  
}

#' Getting comparisons
summary(model_3)

#' *Conditions contrasts*
#' Alcohol VS Water
rcontrast(3.182748, 38)

#' Beer VS Wine
rcontrast(-1.469116, 38)

#' Negative Imagery VS Positive&Neutral Imagery
rcontrast(17.255002, 114)

#' Positive Imagery VS Neutral Imagery
rcontrast(-9.806136, 114)

#' *Interaction contrasts*
#' Alcohol VS Water WITH Negative VS Positive&Neutral Imagery
rcontrast(0.689017, 114)

#' Beer VS Wine WITH Negative VS Positive&Neutral Imagery
rcontrast(6.768475, 114)

#' Alcohol VS Water WITH Positive Imagery VS Neutral Imagery
rcontrast(0.932081, 114)

#' Beer VS Wine WITH Positive Imagery VS Neutral Imagery
rcontrast(-0.799662, 114)

# Reporting factorial repeated-measures designs results ====
#' *Reporting the violation of sphericity assumption from the Mauchly's test*:
#' "Mauchly’s test indicated that the assumption of sphericity had been violated 
#' for the main effects of drink, W = 0.267, p < .001, ε = .58, and imagery, 
#' W = 0.662, p < .05, ε = .75. Therefore degrees of freedom were corrected 
#' using Greenhouse–Geisser estimates of sphericity."

#' *Classic one-way ANOVA report with sphericity and HF correction*:
#' "All effects are reported as significant at p < .05. There was a significant 
#' main effect of the type of drink on ratings of the drink, 
#' F(1.15, 21.93) = 5.11."
#' 
#' "There was also a significant main effect of the type of imagery on ratings 
#' of the drinks, F(1.50, 28.40) = 122.57."

#' *Reporting interaction from ez::ezANOVA()*:
#' "There was a significant interaction effect between the type of drink and the
#' type of imagery used, F(4, 76) = 17.16. This indicates that imagery had 
#' different effects on people’s ratings, depending on which type of drink was 
#' used. Bonferroni post hoc tests revealed that for beer there were significant
#' differences between positive imagery and both negative (p = .002) and neutral
#' (p = .020), but not between negative and neutral (p = 1.00); for wine, 
#' there were significant differences between positive imagery and both 
#' negative (p < .001) and neutral (p < .001), and between negative and neutral 
#' (p < .001); and for water, there were significant differences between 
#' positive imagery and both negative (p < .001) and neutral (p < .001), and 
#' between negative and neutral (p < .001). These findings suggest that beer is 
#' unusual in that negative imagery does appear to reduce attitudes compared to 
#' neutral imagery."

#' *Reporting multilevel modelling*:
#' "The type of drink had a significant effect on attitudes, χ2(2) = 9.1, 
#' p = .010, as did the type of imagery used in the advert, χ2(2) = 151.9, 
#' p < .001. Most important, the drink × imagery interaction was significant, 
#' χ2(4) = 42.0, p < .001. Contrasts revealed that (1) the effect of negative 
#' imagery (compared to other forms) in lowering attitudes is comparable in 
#' alcoholic and non-alcoholic drinks, b = 0.19, t(114) = 0.69, p = .492; (2) 
#' the effect of negative imagery (compared to other forms) in lowering 
#' attitudes to beer was significantly smaller than for wine, b = 3.24, 
#' t(114) = 6.77, p < .001; (3) positive imagery has a similar effect in 
#' increasing attitudes (compared to neutral imagery) in both alcoholic and 
#' non-alcoholic drinks, b = 0.45, t(114) = 0.93, p = .353; and (4) the effect 
#' of positive imagery (compared to neutral) in increasing attitudes to beer 
#' was not significantly different from that for wine, b = -0.66, 
#' t(114) = -0.80, p = .426."