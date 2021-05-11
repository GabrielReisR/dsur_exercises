# Chapter 13 - ANOVA in Repeated-measures Design ====

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

df <- read.delim(".\\Data Files\\Bushtucker.dat")

df %<>%
  pivot_longer(
    cols = stick_insect:witchetty_grub,
    names_to = 'animal',
    values_to = 'retch'
  )

df$animal %<>% recode("stick_insect" = "Stick Insect")
df$animal %<>% recode("kangaroo_testicle" = "Kangaroo Testicle")
df$animal %<>% recode("fish_eye" = "Fish Eye")
df$animal %<>% recode("witchetty_grub" = "Witchetty Grub")

df$animal <- factor(df$animal,
                    levels = c("Stick Insect",
                               "Kangaroo Testicle",
                               "Fish Eye",
                               "Witchetty Grub"),
                    labels = c("Stick Insect",
                               "Kangaroo Testicle",
                               "Fish Eye",
                               "Witchetty Grub"))

df$participant %<>% as.factor()

# Creating a bar graph with error bars ====
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = animal, y = retch)) +
  
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
  xlab('Type of Animal Eaten') +
  ylab('Mean Time to Retch (Seconds)') +
  
  # Alterando limites
  ylim(c(0,13)) +
  
  # Theme
  theme_minimal()

# Creating boxplots for time to retch in each animal eaten ====
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = animal, y = retch)) +
  
  # Creating boxplot
  geom_boxplot() +

  # Mudando nome de todas as partes do gráfico
  xlab('Type of Animal Eaten') +
  ylab('Mean Time to Retch (Seconds)') +
  
  # Alterando limites
  ylim(c(0,13)) +
  
  # Theme
  theme_minimal()

# Getting descriptive statistics for retch in each animal eaten ====
by(df$retch, df$animal, stat.desc)

# Creating contrasts ====
parts_vs_whole <- c(1, -1, -1, 1)
testicle_vs_eye <- c(0, -1, 1, 0)
stick_vs_grub <- c(-1, 0, 0, 1)
contrasts(df$animal) <- cbind(parts_vs_whole,
                              testicle_vs_eye,
                              stick_vs_grub)

contrasts(df$animal)

# Conducting ANOVA with ez::ezANOVA() ====
model <- ezANOVA(data = df,
                 dv = .(retch), # dependent variable
                 wid = .(participant), # variable with participants
                 within = .(animal), # the independent variable
                 #between = .(variable), # between variable, in this case EMPTY
                 detailed = T, # asking for a detailed output
                 type = 3) 

model

# Post hocs using pairwise t-tests ====
pairwise.t.test(df$retch, df$animal,
                paired = TRUE,
                p.adjust.method = "bonferroni")

# Conducting ANOVA with nlme::lme() - multilevel modelling ====
#' To conduct this, first we need to stablish our baseline model
#' What if there were no independent variables?
baseline <- lme(retch ~ 1, # formula without independent variable
             random = ~1|participant/animal, # scores are dependent on particip.
             data = df,
             method = 'ML')

#' What's our model score with the independent variables?
model <- lme(retch ~ animal, # formula with independent variable
             random = ~1|participant/animal, # scores are dependent on particip.
             data = df,
             method = 'ML')

#' Is *model* significantly better than *baseline*?
anova(baseline, model)

#' Explanation:
#' The AIC and BIC tell us about the fit of the model (smaller values mean a
#' better fit). The fact that these values are smaller in the final model than
#' the baseline tells us that the fit of the model has got better. The
#' likelihood ratio (L.Ratio in the output) tells us whether this improvement in
#' fit is significant, and because the p-value of .0054 is less than .05 it is.
#' Therefore, Animal is a significant predictor of Retch. We can conclude, then,
#' that the type of animal consumed had a significant effect on the time taken
#' to retch, χ²(3) = 12.69, p = .005.

# Further exploring the lme() model ====
summary(model)

#' Writing:
#' "First, when we compare whole animals (stick insect and witchetty grub
#' combined) to animal parts (testicle and eye) retching times were
#' significantly different, b = 1.38, t(21) = 3.15, p = .005. From the
#' descriptive statistics (Output 13.1) it looks as though people retched more
#' quickly after eating parts of animals ((4.25 + 4.125)/2 = 4.188) than whole
#' animals ((8.125 + 5.75)/2 = 6.938). The second contrast tells us that there
#' was no significant difference in the time to retch after eating a kangaroo
#' testicle and a fish eye, b = -0.063, t(21) = -0.101, p = .920. The final 
#' contrast tells us that there was a trend for retching times to be shorter
#' after eating a witchetty grub (M = 5.75) than a stick insect (M = 8.125),
#' b = -1.188, t(21) = -1.924, p = .068."

# Post hocs using Tukey ====
tukey_output <- glht(model, linfct = mcp(animal = "Tukey"))
summary(tukey_output)
confint(tukey_output)

# Robust one-way repeated-measures ANOVA with trimmed means ====
rmanova(y = df$retch, # dependent variable, outcome
        groups = df$animal, # independent variable
        blocks = df$participant) # repeated measures variable

# Post hoc tests
rmmcp(y = df$retch, # dependent variable, outcome
      groups = df$animal, # independent variable
      blocks = df$participant) # repeated measures variable

#' Reporting post hocs based on trimmed means:
#' "We could report that there was no significant difference between the time to
#' retch after eating a stick insect compared to a kangaroo testicle, 
#' ψ∧ = 3.67 (-0.48, 7.82), p > .05, fish eye ψ∧ = 4.00 (-0.36, 8.36),
#' p > .05, or witchetty grub ψ∧ = 2.00 (-8.10, 12.10), p > .05; or a 
#' kangaroo testicle compared to a fish eye ψ∧ = 0 (-5.39, 5.39), p > .05,
#' or witchetty grub ψ∧ = -1.83 (-9.23, 5.57), p > .05; or a fish eye
#' compared to a witchetty grub ψ∧ = -2.00 (-12.55, 8.55), p > .05. Note
#' that in each case I have reported ψ∧ and its confidence interval."

# Robust one-way repeated-measures ANOVA with trimmed means & bootstrapping ====
rmanovab(y = df$retch, # dependent variable, outcome
         groups = df$animal, # independent variable
         blocks = df$participant) # repeated measures variable)

# Post hoc tests
pairdepb(y = df$retch, # dependent variable, outcome
         groups = df$animal, # independent variable
         blocks = df$participant) # repeated measures variable))

#' Reporting post hocs based on trimmed means & bootstrapping:
#' "We could again report that (note that the values and confidence intervals 
#' for ψ∧ have changed): there was no significant difference between the
#' time to retch after eating a stick insect compared to a kangaroo testicle, 
#' ψ∧ = 3.83 (-0.70, 8.37), p > .05, fish eye ψ∧ = 4.00 (-1.15, 9.15),
#' p > .05, or witchetty grub ψ∧ = 2.00 (-7.78, 11.78), p > .05; or a 
#' kangaroo testicle compared to a fish eye ψ∧ = 0.17 (-7.27, 7.61), 
#' p > .05, or witchetty grub ψ∧ = -1.83 (-9.76, 6.09), p > .05; or a fish
#' eye compared to a witchetty grub ψ∧ = -2.00 (-12.90, 8.90), p > .05."

# Calculating generalized omega square effect size ====
#' We can get generalized ω² with ez::ezANOVA()'s output
model <- ezANOVA(data = df,
                 dv = .(retch), # dependent variable
                 wid = .(participant), # variable with participants
                 within = .(animal), # the independent variable
                 #between = .(variable), # between variable, in this case EMPTY
                 detailed = T, # asking for a detailed output
                 type = 3) 
model

#' ges for the variable *animal* is 0.3274249. The effect of animal in retch,
#' therefore, is of ω² = 0.33.

# Calculating rcontrast effect sizes for different comparisons on lme() ====
#' *R = SQRT [ t² / (t² + df)]*
rcontrast <- function(t, df) {
  
  r <- sqrt(t^2 / (t ^ 2 + df))
  
  print(paste("r = ", r))
  
}

#' Let's use the lme results
model <- lme(retch ~ animal, # formula with independent variable
             random = ~1|participant/animal, # scores are dependent on particip.
             data = df,
             method = 'ML')
summary(model)

# Running comparisons
rcontrast(3.149752, 21)
rcontrast(-0.101237, 21)
rcontrast(-1.923500, 21)

#' Writing results:
#' "The difference between body parts and whole animals was a large effect 
#' (r = .57), between the stick insect and witchetty grub a medium effect 
#' (r = .39), but between the testicle and eyeball a very small effect
#' (r = .02)."

# Reporting one-way repeated-measures designs ====
#' *Classic one-way ANOVA report with sphericity and GG correction*:
#' "Mauchly’s test indicated that the assumption of sphericity had been violated,
#' χ2(5) = 11.41, p < .05, therefore Greenhouse–Geisser corrected tests are
#' reported (ε = .53). The results show that the time to retch was not
#' significantly affected by the type of animal eaten, F(1.60, 11.19) = 3.79,
#' p > .05, η2 = . 327.

#' *Classic one-way ANOVA report with sphericity and HF correction*:
#' "Mauchly’s test indicated that the assumption of sphericity had been violated,
#' χ2(5) = 11.41, p < .05, therefore degrees of freedom were corrected using
#' Huynh–Feldt estimates of sphericity (ε = .67). The results show that the time
#' to retch was significantly affected by the type of animal eaten, 
#' F(2, 13.98) = 3.79, p < .05, η2 = . 327."

#' *Reporting multilevel modelling*:
#' "The type of animal consumed had a significant effect on the time taken to 
#' retch, χ2(3) = 12.69, p = .005. Orthogonal contrasts revealed that retching
#' times were significantly quicker for animal parts (testicle and eye) compared
#' to whole animals (stick insect and witchetty grub), b = 1.38, t(21) = 3.15,
#' p = .005; there was no significant difference in the time to retch after
#' eating a kangaroo testicle and a fish eye, b = -0.063, t(21) = -0.101,
#' p = .920, or between eating a witchetty grub or a stick insect, b = -1.188,
#' t(21) = -1.924, p = .068."
