# Chapter 19 - Multilevel Linear Models ====

# Initializing ====
library(dplyr)
library(ggplot2)
library(nlme)
library(tidyr)

df <- read.delim(".\\Data Files\\Cosmetic Surgery.dat")
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

# Loglinear analysis ====
# Apparently we can't directly insert a three-factor table into CrossTable
# We'll have to divide our dataset in two

df <- read.delim(".\\Data Files\\CatsandDogs.dat")
df_cats <- df %>% filter(Animal == 'Cat')
df_dogs <- df %>% filter(Animal == 'Dog')

# Running two chi-square tests ====
#' *Cats*
CrossTable(df_cats$Training, df_cats$Dance,
           sresid = TRUE, format = "SPSS",
           prop.t = FALSE, prop.c = FALSE, prop.chisq = FALSE,
           fisher = T)

#' *Dogs*
CrossTable(df_dogs$Training, df_dogs$Dance,
           sresid = TRUE, format = "SPSS",
           prop.t = FALSE, prop.c = FALSE, prop.chisq = FALSE,
           fisher = T)

# Running loglinear analysis as chi-square ====
#' First, we have to create a table. We'll need this for modelling.
cats_table <- xtabs(~ Training + Dance, df_cats)

cats_m1 <- loglm(~ Training + Dance + Training:Dance,
                 data = cats_table, # our newly created table
                 fit = T)


cats_m2 <- loglm(~ Training + Dance, # removing highest order interaction
                 data = cats_table, # our newly created table
                 fit = T)

# Visualizing mosaics and interpreting them ====
mosaicplot(cats_m1$fit, shade = TRUE, main = "Cats: Saturated Model")

#' This notion of residuals is going back to the chi-square formula.
#' Cells with residuals above |1.96| (or 2) indicate that they're influential
#' to the result of the chi-square. Put simply, they are our main results. Their 
#' deviance from expected values can be either high (> 2) or too low (< 2).
#' When it's high (blue box, food as reward & could dance), this shows us that
#' we have more cases there than was expected.
#' When it's low (red box, food as reward & could not dance), this shows us that
#' we have less cases there than was expected.
#' 
#' In this sense, food as reward seems more effective when training cats to 
#' dance in comparison with affection as reward.

# Now let's see our mosaic without the interaction term.
mosaicplot(cats_m2$fit, shade = TRUE, main = "Cats: Expected Values")

# Seems that the interaction term is important to differentiate the groups.

# Interpreting our "loglinear-chi-square" models ====
summary(cats_m1)

#' Chi-square equals 0 because the expected value are the same as our data.
#' I mean, that's why its called a saturated model.

summary(cats_m2)

#' This model does the following: it tries to estimate the frequencies only
#' considering `Training + Dance` as predictors. This is then compared to our
#' data and what actually happened. The high deviation characterized by high
#' values of Likelihood Ratio and Chi-squares indicate that this model is very
#' far from our data - it's significantly different from what happened.

# Running loglinear analysis ====
#' Again, we start by creating contingency tables
cat_dog_table <- xtabs(~ Animal + Training + Dance, data = df)

# Running saturated model
cat_dog_m1 <- loglm(~ Animal*Training*Dance,
                    data = cat_dog_table,
                    fit = T)

# Running second model, without highest order interaction
cat_dog_m2 <- loglm(~ Animal*Training + Animal*Dance + Training*Dance,
                    data = cat_dog_table,
                    fit = T)

summary(cat_dog_m2)

#' It's significantly different from what was expected. So we reject this model,
#' stop removing predictors and accept the highest order interaction as
#' significant.

anova(cat_dog_m1, cat_dog_m2)

# Visualizing the relationships with a mosaic plot ====
mosaicplot(cat_dog_m1$fit, shade = TRUE, main = "Cats & Dogs")

#' This is now a very complicated plot to interpret. Let's guide ourselves via
#' the significant residuals.
#' 
#' First, lots of cats that received affection as reward could not dance.
#' (blue top-left box). In fact, very very few cats that received affection as
#' reward could dance (red top-right box). Very few cats that received food as
#' reward could not dance (red bottom-left box), indicating that, for cats, food
#' as reward is more efficient than affection when teaching them to dance.
#' 
#' Now for dogs. Very few dogs that received affection as reward could not
#' dance (red top-left box). At the same time, a lot of dogs that received food
#' as reward could then dance (blue bottom-right). This indicates that, no
#' matter the type of training, dogs tend to dance when rewarded.

# Effect sizes ====
#' We just do the odds ratio on both groups. This is done in section
#' *Running loglinear analysis as chi-square*.
#' 
#' This can be the follow-up to the loglinear analysis. We could report both
#' groups' chi-square tests.
#' 
#' We could also do the odds ratios by hand. Odds ratio is a great way to 
#' compare both groups in relation to training.

# Reporting loglinear analysis ====
#' "The three-way loglinear analysis produced a final model that retained all 
#' effects. The likelihood ratio of this model was χ2 (0) = 0, p = 1. This 
#' indicated that the highest order interaction (the Animal × Training × Dance 
#' interaction) was significant, χ2 (1) = 20.31, p < .001. To break down this 
#' effect, separate chi-square tests on the Training and Dance variables were 
#' performed separately for dogs and cats. For cats, there was a significant 
#' association between the type of training and whether or not cats would dance, 
#' χ2 (1) = 25.36, p < .001; this was true in dogs also, χ2 (1) = 3.93, p < .05. 
#' Odds ratios indicated that the odds of dancing were 6.58 higher after food 
#' than affection in cats, but only 0.35 in dogs (i.e., in dogs, the odds of 
#' dancing were 2.90 times lower if trained with food compared to affection). 
#' Therefore, the analysis seems to reveal a fundamental difference between dogs 
#' and cats: cats are more likely to dance for food rather than affection, 
#' whereas dogs are more likely to dance for affection than food."