# Chapter 16 - Multivariate Analysis of Variance (MANOVA) ====

# Initializing ====
library(dplyr)
library(ggplot2)
library(gmodels)
library(magrittr)
library(MASS) # log-linear analysis

df <- read.delim(".\\Data Files\\cats.dat")
df

# Running a chi-square distribution test ====
#' First, we insert the predictor
#' After that, the outcome
ct <- CrossTable(df$Training, df$Dance,
                 prop.r = T, prop.c = T,
                 prop.t = F, prop.chisq = F,
                 chisq = T, fisher = T,
                 expected = F,
                 sresid = T, # to calculate residual
                 format = 'SPSS')

#' *Chi-square test*
#' The highly significant result indicates that there is an association between
#' the type of training and whether the cat danced or not. What we mean by an
#' association is that the pattern of responses (i.e., the proportion of cats 
#' that danced to the proportion that did not) in the two training conditions 
#' is significantly different. This significant finding reflects the fact that 
#' when food is used as a reward, about 74% of cats learn to dance and 26% do 
#' not, whereas when affection is used, the opposite is true (about 70% refuse 
#' to dance and 30% do dance). Therefore, we can conclude that the type of 
#' training used significantly influences the cats: they will dance for food 
#' but not for love!
#' 
#' *Residuals*
#' When food was used as a reward the standardized residual was significant for
#' both those that danced (z = 3.57) and those that didn’t dance (z = -2.79) 
#' because both values are bigger than 1.96 (when you ignore the minus sign). 
#' The plus or minus sign tells us something about the direction of the effect, 
#' as do the counts and expected counts within the cells. We can interpret these 
#' standardized residuals as follows: when food was used as a reward 
#' significantly more cats than expected danced, and significantly fewer cats 
#' than expected did not dance. When affection was used as a reward the 
#' standardized residual was not significant for both those that danced 
#' (z = -1.73) and those that didn’t dance (z = 1.35) because they are both 
#' smaller than 1.96 (when you ignore the minus sign). This tells us that when 
#' affection was used as a reward as many cats as expected danced and did not 
#' dance. In a nutshell, the cells for when food was used as a reward both 
#' significantly contribute to the overall chi-square statistic. Put another 
#' way, the association between the type of reward and dancing is mainly driven 
#' by when food is a reward.
#' 
#' *Odds ratio*
#' What this tells us is that if a cat was trained with food the odds of their 
#' dancing were 6.65 times higher than if they had been trained with affection.
#' 
#' There was a significant association between the type of training and whether
#' or not cats would dance χ²(1) = 25.36, p < .001. This seems to represent the
#' fact that, based on the odds ratio, the odds of cats dancing were 6.58 
#' (2.84, 16.43) times higher if they were trained with food than if trained
#' with affection.


