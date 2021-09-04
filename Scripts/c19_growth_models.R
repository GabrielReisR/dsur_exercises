# Chapter 19 - Growth Models ====

# Initializing ====
library(dplyr)
library(ggplot2)
library(magrittr)
library(nlme)
library(tidyr)

df <- read.delim(".\\Data Files\\Honeymoon Period.dat")

# Reestructuring the dataset ====
df %<>%
  pivot_longer(
    cols = Satisfaction_Base:Satisfaction_18_Months,# these columns
    names_to = "Time", # will have their names stored here
    names_prefix = "Satisfaction_", # but only the name after "..."
    values_to = "Life_Satisfaction" # and their values stored here
  )

# Recoding time
df$Time <- recode(df$Time,
                  "Base" = 0,
                  "6_Months" = 1,
                  "12_Months" = 2,
                  "18_Months" = 3)

df

# Fitting different models ====
# Fixed intercepts
intercept <- gls(Life_Satisfaction ~ 1,
                 data = df, method = "ML", na.action = na.exclude)

# Randomizing intercepts
randomIntercept <- lme(Life_Satisfaction ~ 1,
                       data = df, method = "ML", na.action = na.exclude,
                       random = ~ 1 | Person,
                       control = list(opt = "optim")) # use this optimizer: ""

# Adding Time as a fixed effect
timeRI <- update(randomIntercept, .~. + Time) # updating previous model

# Introducing random slopes
timeRS <- update(timeRI, random = ~ Time | Person)

# Modelling covariance structure to be autoregressive
ARModel <- update(timeRS, correlation = corAR1(0, form = ~ Time | Person))

# Comparing models and examining final model ====
anova(intercept,
      randomIntercept,
      timeRI,
      timeRS,
      ARModel)

#' "The resulting output, in Output 19.18, shows that adding a random intercept 
#' significantly improved the fit of the model, χ2(1) = 74.66, p < .0001. 
#' Similarly, adding the fixed effect of time to the model significantly 
#' improved the fit compared to the previous model, χ2 (1) = 121.67, p < .0001.
#' However, adding a random slope for the effect of time across participants 
#' did not significantly improve the model, χ2(2) = 1.10, p = .576. Finally, 
#' adding a first-order autoregressive covariance structure did more or less 
#' significantly improve the model, χ2 (1) = 3.74, p = .053."

summary(ARModel)
intervals(ARModel)

#' "The effect of time, b = -0.87 (-1.03, -0.71), t(322) = -10.97, p < .001, was
#' highly significant, indicating that life satisfaction significantly changed
#' over the 18 month period (see Figure 19.11). In addition, the standard
#' deviation of intercepts was 1.62 (1.31, 2.02), and for the effect of time
#' across people (slopes) was 0.05 (0.00, 41.83)."

# Specifying higher-order polynomials ====
#' We'll simply update our model using the I() function that lets R know that
#' it has to treat the enclosed object as an arithmetic operator rather than
#' part of the model specification.

# Creating quadratic polynomial
timeQuadratic <- update(ARModel, .~. + I(Time^2))

# Creating cubic polynomial
timeCubic <- update(timeQuadratic, .~. + I(Time^3))

# Comparing models
anova(ARModel, timeQuadratic, timeCubic)

#' "It is clear from this that adding the
#' quadratic term to the model significantly improves the fit,
#' χ²(1) = 57.35, p < .0001; however, adding in the cubic trend does not, 
#' χ²(1) = 3.38, p = .066."

# Summarizing
summary(timeCubic)
intervals(timeCubic)

#' "The linear, b = 1.55 (0.61, 2.48), t(320) = 3.24, p < .01, and quadratic,
#' b = -1.33 (-2.15, -0.50), t(320) = -3.15, p < .01, both significantly 
#' described the pattern of the data over time; however, the cubic trend was not
#' significant, b = 0.17 (-0.01, 0.35), t(320) = 1.84, p > .05. This confirms
#' what we already know from comparing the fit of successive models. The trend
#' in the data is best described by a secondorder polynomial, or a quadratic
#' trend. This reflects the initial increase in life satisfaction 6 months after
#' finding a new partner but a subsequent reduction in life satisfaction at 12
#' and 18 months after the start of the relationship."

# Another way of creating polynomials: the poly() function ====
polyModel <- update(ARModel, .~ poly(Time, 3))

#' "The previous predictor of Time that was specified for the ARModel will be
#' replaced by the linear, quadratic andcubic polynomials for Time that are 
#' created by the poly() function."

summary(polyModel)

#' "The advantage of this method of creating polynomials is that the resulting
#' predictors are orthogonal (independent)."



