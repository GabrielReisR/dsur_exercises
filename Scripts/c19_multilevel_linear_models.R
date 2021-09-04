# Chapter 19 - Multilevel Linear Models ====

# Initializing ====
library(dplyr)
library(ggplot2)
library(nlme)
library(tidyr)

df <- read.delim(".\\Data Files\\Cosmetic Surgery.dat")

# Graphing the relationship between baseline and post-surgery ====
df %>% 
  
  ggplot(aes(Base_QoL, Post_QoL)) +
  
  geom_point(aes(color = Surgery_Text)) +
  
  geom_smooth(aes(color = Surgery_Text), method = 'lm', se = F) +
  
  labs(title = 'Quality of Life Pre-Post Surgery at 10 Clinics',
       x = 'Quality of Life (Baseline)',
       y = 'Quality of Life (After Surgery)') +
  
  scale_color_discrete(name = 'Surgery') +
  
  facet_wrap(~ Clinic, ncol = 5) +
  
  theme_minimal()

# Assessing the need for a multilevel linear model ====
#' For this, we'll create two models. The first one will only estimate the
#' intercept. The second one will estimate the intercept given the second level
#' variable.

interceptOnly <- gls(Post_QoL ~ 1,
                     data = df, method = "ML")

randomInterceptOnly <- lme(Post_QoL ~ 1,
                           data = df, method = "ML",
                           random = ~ 1 | Clinic)

# Comparing models:
anova(interceptOnly, randomInterceptOnly)

#' We can report that the intercept vary significantly between clinics, 
#' χ²(1) = 107.65, p < .0001. We'll then stick to the multilevel modelling.

# Adding other variables to the intercept models ====
#' Adding *Surgery*
randomInterceptSurgery <- lme(Post_QoL ~ Surgery,
                              data = df, method = "ML",
                              random = ~ 1 | Clinic)

#' Adding *Base_QoL*
randomInterceptSurgeryQoL <- lme(Post_QoL ~ Surgery + Base_QoL,
                                 data = df, method = "ML",
                                 random = ~ 1 | Clinic)

# Comparing intercept models ====
anova(randomInterceptOnly,
      randomInterceptSurgery,
      randomInterceptSurgeryQoL)

# Assessing final intercept model ====
#' Given the improved fit of the randomInterceptSurgeryQoL, let's stick to this
#' model. To access its contents, we can run `summary(object)`

summary(randomInterceptSurgeryQoL)

#' "The regression parameter for the effect of Surgery is -0.31, which is not 
#' significant, t(264) = -0.37, p > .05. However, baseline quality of life has a
#' regression parameter of 0.48, which is highly significant t(264) = 9.07, 
#' p < .001. The standard deviation of the intercepts is 3.04 (we can square 
#' this value to get the variance of intercepts across clinics, which in this 
#' case is 9.24)."


# Assessing the need for random slopes ====
#' Until now we only worked with random intercepts. Let's check if we reduce
#' model error when taking into account the randomness of the surgery effect
#' variable on Post_QoL given the clinic.

addRandomSlope <- lme(Post_QoL ~ Surgery + Base_QoL,
                      data = df, method = 'ML',
                      random = ~ Surgery | Clinic)

anova(randomInterceptSurgeryQoL,
      addRandomSlope)

# The model significantly improved when adding a random slope.

#' "By allowing the effect of Surgery to vary across clinics we have reduced the
#' BIC from 1865.59 to 1837.97, and the -2LL has changed significantly,
#' χ² (2) = 38.87, p < .0001. In short, adding random slopes to the model has 
#' significantly improved its fit, which means there is significant variability 
#' in the effect of surgery across clinics. Across this model and the previous 
#' one, we can conclude from the -2LL as we built up the models that the 
#' intercepts, χ² (1) = 64.65, p < .0001, and slopes, χ² (2) = 38.87, p < .0001,
#' for the relationship between surgery and quality of life (when controlling 
#' for baseline quality of life) vary significantly across the different 
#' clinics."

# Adding other variables to the model ====
# Adding Reason
addReason <- lme(Post_QoL ~ Surgery + Base_QoL + Reason,
                  data = df, method = 'ML',
                  random = ~ Surgery | Clinic)

# Adding Reason + Interaction term
finalModel <- lme(Post_QoL ~ Surgery + Base_QoL + Reason + Surgery:Reason,
                  data = df, method = 'ML',
                  random = ~ Surgery | Clinic)

# Comparing models
anova(addRandomSlope,
      addReason,
      finalModel)

# Final model wins!

# Summary of our final model ====
summary(finalModel)

#' Quality of life before surgery significantly predicted quality of life after
#' surgery, t(262) = 5.75, p < .001, surgery still did not significantly predict
#' quality of life, t(262) = -1.46, p = .15, but the reason for surgery, 
#' t(262) = -3.08, p < .01, and the interaction of the reason for surgery and
#' surgery, t(262) = 2.48, p < .05, both did significantly predict quality of
#' life. The table of estimates also gives us the regression coefficients.

# Getting 95% confidence intervals
intervals(finalModel, 0.95)

#' These confidence intervals are very useful for establishing whether the 
#' variance of the intercepts and slopes is significant. For example, we can see
#' that the standard deviation for intercepts was 5.48 with a 95% confidence 
#' interval from 3.31 to 9.07, for the slope of Surgery we get 
#' 5.42 (3.13, 9.37); because both confidence intervals do not cross zero we can
#' see that the variability in both slopes and intercepts was significant,
#' p < .05.

# Running analyses for the interaction term ====
#' Since we can't directly interpret the interaction term, let's break it down.

physicalSubset <- df$Reason == 1

cosmeticSubset <- df$Reason == 0

# Creating physicalModel
physicalModel <- lme(Post_QoL ~ Surgery + Base_QoL,
                     data = df,
                     random = ~ Surgery | Clinic,
                     subset = physicalSubset, method = "ML")

# Creating cosmeticModel
cosmeticModel <- lme(Post_QoL ~ Surgery + Base_QoL,
                     data = df,
                     random = ~ Surgery | Clinic,
                     subset = cosmeticSubset, method = "ML")

#' It shows that for those operated on only to change their appearance, surgery
#' almost significantly predicted quality of life after surgery,
#' b = -4.31, t(87) = -1.89, p = .06. The negative gradient shows that for these
#' people quality of life after surgery was lower compared to the control group.
#' However, for those who had surgery to solve a physical problem surgery did
#' not significantly predict quality of life, b = 1.20, t(166) = 0.57, p = .57.
#' However, the slope was positive, indicating that people who had surgery
#' scored higher on quality of life than those on the waiting list (although not
#' significantly so!).

# Reporting results ====
#' *Final model of cosmetic surgery*
#' "The relationship between surgery and quality of life showed significant 
#' variance in intercepts across participants, SD = 5.48 (95% CI: 3.31, 9.07), 
#' χ2(1) = 107.65, p < .0001. In addition, the slopes varied across 
#' participants, SD = 5.42 (3.13, 9.37), χ2 (2) = 38.87, p < .0001, and the
#' slopes and intercepts were negatively and significantly correlated, 
#' cor = -.95 (-.99, -.60)."

#' *Interpretation of betas in the cosmetic surgery model*
#' "Quality of life before surgery significantly predicted quality of life after
#' surgery, b = 0.31, t(262) = 5.75, p < .001, surgery did not significantly
#' predict quality of life, b = -3.19, t(262) = -1.46, p = .15, but the reason
#' for surgery, b = -3.52, t(262) = -3.08, p < .01, and the interaction of the
#' reason for surgery and surgery, b = 4.22, t(262) = 2.48, p < .05, both did
#' significantly predict quality of life. This interaction was broken down by
#' conducting separate multilevel models on the ‘physical reason’ and
#' ‘attractiveness reason’. The models specified were the same as the main model
#' but excluded the main effect and interaction term involving the reason for
#' surgery. These analyses showed that for those operated on only to change
#' their appearance, surgery almost significantly predicted quality of life
#' after surgery, b = −4.31, t(87) = −1.89, p = .06: quality of life was lower
#' after surgery compared to the control group. However, for those who had
#' surgery to solve a physical problem, surgery did not significantly predict
#' quality of life, b = 1.20, t(166) = 0.57, p = .57. The interaction effect,
#' therefore, reflects the difference in slopes for surgery as a predictor of
#' quality of life in those who had surgery for physical problems (slight
#' positive slope) and those who had surgery purely for vanity (a negative
#' slope)."
