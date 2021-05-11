# Chapter 11  - ANCOVA - Analysis of Covariance ====

# Initializing ====
library(car)
library(compute.es)
library(dplyr)
library(effects)
library(ggplot2)
library(magrittr)
library(multcomp)
library(pastecs)
library(WRS2)

df <- read.delim('.\\Data Files\\ViagraCovariate.dat')
df$dose <- factor(df$dose,
                  levels = c(1:3),
                  labels = c("Placebo", "Low Dose", "High Dose"))

# Investigating data with boxplots ====
# Participant's libido
participant_boxplot <- df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = dose, y = libido)) +
  
  # Criando boxplot
  geom_boxplot() +
  
  # Colocando labels
  xlab('') + ylab('') + ggtitle('Participant') +
  
  # Estabelecendo limite de y
  ylim(c(0,10)) +
  
  # Colocando tema
  theme_minimal()

# Participant's libido
partner_boxplot <- df %>%
  
  # Criando primeira camada
  ggplot(aes(x = dose, y = partnerLibido)) +
  
  # Criando boxplot
  geom_boxplot() +
  
  # Colocando labels
  xlab('') + ylab('') + ggtitle('Partner') +
  
  # Estabelecendo limite de y
  ylim(c(0, 10)) +
  
  # Colocando tema
  theme_minimal() +
  
  # Retirando numeração no eixo y
  theme(axis.text.y = element_blank())


# Both graphs
#' If we excluded some things, we could arrange them beautifully, but that's
#' not the current aim, so let's move on
cowplot::plot_grid(participant_boxplot, partner_boxplot)

# Let's get the descriptive statistics ====
by(df$libido, df$dose, stat.desc, basic = F, norm = T)
by(df$partnerLibido, df$dose, stat.desc, basic = F, norm = T)

# Performing Levene's test with car::leveneTest ====
leveneTest(df$libido, df$dose, center = 'median')

# Checking independence assumption of the covariate ====
model <- aov(partnerLibido ~ dose, data = df)
summary(model)

#' Since the value of p is above our alpha (0.05), we can conclude that there 
#' isn't significant differences between the means of partner libido by dose of
#' viagra. That is to say that dose of viagra doesn't predict, or isn't 
#' associated, with partner's libido. Our independence assumption seems to be 
#' in order.



# Fitting the ANCOVA model ====
#' First, let's run the first ANOVA on this data without taking into account
#' partner's libido
model <- aov(libido ~ dose, data = df)
summary(model)

#' It *appears* that dose is not a good predictor of libido.

#' Now, we could run a Type I sums of squares ANOVA with:
model <- aov(libido ~ partnerLibido + dose, data = df)
summary(model)

#' Notice that the covariate comes first, then the independent variable is added

#' To get Type III sums of squares, we use `car::Anova(model, type = III)`
#' This is the model we're interested in:
model <- Anova(model, type = 'III')

#' We notice that partner's libido has a significant effect on participant's
#' libido (p = 0,035). Also, when the effect of partner's libido is removed,
#' Viagra dose still could predict the participant's libido (p = 0,027).
#' Contrast that with the first model created and we can see the importance of
#' taking covariates into account.

# Getting the adjusted means of libido by dose without the covariate ====
#' We already saw the means of libido by dose, but let's recap
by(df$libido, df$dose, mean)

#' These means don't tell us much, because the effect of partner's libido isn't
#' reflected in its values. So, we need to correct for the effect of partner's
#' libido in the means of participants' libido by dose.
#' To do that, we use the `effects::effect()` function.
#' This function provides a summary table of means for a specified effect (dose)
#' in a *aov()* or *lm()* model, adjusted for other variables in the model.
adjusted_means_of_libido_by_dose <- effect('dose', model, se = T)
summary(adjusted_means_of_libido_by_dose)

adjusted_means_of_libido_by_dose$se

#' What's interesting to me is that we can use these values to calculate t-tests
# Fitting ANCOVA model with constrasts ====
#' Getting Helmert's contrasts
contrasts(df$dose) <- contr.helmert(3)

#' Doing the contrasts by hand
contrasts(df$dose) <- cbind(c(-2,1,1), c(0,-1,1))

# Running the ANCOVA
model <- aov(libido ~ partnerLibido + dose, data = df)

# Getting the results
summary.lm(model)

#' There is a significant increase going in libido going from placebo to any
#' viagra condition, that difference being an increase in 0.6684 points in
#' libido (p = 0.009). There isn't, however, a significant difference in libido
#' going from low doses of viagra to high doses of viagra, that change being
#' only of 0.219 points (p = 0.593).
#' The value of b for partnerLibido indicates that, with all things equal, 
#' as partner's libido increases by one unit, the participant's libido tends to
#' increase, on average, by 0.416 units (p = 0.348).

# Post hoc tests ====
#' We can only get the post hoc tests using the `multcomp::glht()` function
#' since its the only function to calculate the *adjusted* means.
tukey_output <- glht(model, linfct = mcp(dose = "Tukey"))

summary(tukey_output)

confint(tukey_output)

#' There appears to be significant differences only between the placebo and high
#' dose groups, t = 2.771, p = 0.026. The `confint()` function shows that, for 
#' other conditions, the 95% confidence interval of true difference between
#' means crosses 0, indicating that, possibly, there isn't any change at all
#' across those conditions.



# Plots from the aov() model ====
plot(model)

#' The first one can be used to assess homogeneity of variances
#' Ours show some funnelling (scores are wider on one part), indicating possible
#' heteroscedacity

#' The second plot can be used to assess normality of differences. It appears
#' that some residuals are far from the diagonal line.

# Checking homogeneity of regression slope ====
ggplot(df, aes(x = partnerLibido, y = libido, group = dose, color = dose)) +
  
  # Pontos
  geom_point(alpha = 1, position = 'jitter') +
  
  # Linha
  stat_smooth(method = "lm", se = T, size = 0.5, aes(color = dose)) +
  
  # Ajustando x
  xlab('Partner Libido') +
  xlim(c(0, 10)) +
  
  # Ajustando y
  ylab('Participant Libido') +
  ylim(c(0, 10)) +
  
  # Theme
  theme_classic() +
  
  # Renaming legend
  scale_color_discrete(name = '')

#' The first way is to redo the model with a new interaction term
model <- aov(libido ~ partnerLibido + dose + dose:partnerLibido, data = df)

#' The second is to just specify the interaction
model <- aov(libido ~ partnerLibido*dose, data = df)

#' The third and final method is to update the already existing model
# Initial model:
model <- aov(libido ~ partnerLibido + dose, data = df)

updated_model <- update(model, .~. + partnerLibido:dose)

#' "The `.~.` simply means ‘keep the same outcome variable and predictor as 
#' before' and the `+ partnerLibido: dose` means ‘add the interaction term’."

#' Now, we're interested in the higher-order interaction, so we can use both
#' the Type II and Type III sums of squares. Let's go with the Type III.
Anova(updated_model, type = 'III')

#' We can see that there is a significant interaction between partnerLibido and
#' dose, which means that the homogeneity of regression slopes assumption is
#' violated. This raises concerns about the main analyses.
# Robust ANCOVA ====
df <- read.delim('.\\Data Files\\CloakOfInvisibility.dat')
df$cloak<-factor(df$cloak, 
                 levels = c(1:2),
                 labels = c("No Cloak", "Cloak"))
df

model <- ancova(mischief2 ~ mischief1 + cloak, data = df)
model

model <- ancboot(mischief2 ~ mischief1 + cloak, data = df, n = 2000)
model

#' "In other words, in most cases the groups differ significantly in their mean 
#' level of mischief after the intervention (adjusted for baseline levels of 
#' mischievousness). We didn’t get a significant difference for values of the 
#' covariate around 5 (the middle of the five design points tested), which seems 
#' to suggest that having an invisibility cloak increased mischievousness in 
#' those who were ordinarily not very mischievous (baseline scores around 2 and 
#' 4) or ordinarily highly mischievous (baseline scores around 6 and 7), but not
#' in the ‘averagely mischievous’ person."

# Eta-squared and partial eta-squared effect sizes ====
#' Remember that *η² = SSm/SSt* - percentage of explanation from the total
#' variation provided by the model, simple as that.
#' In ANCOVA, we have to distinguish what is explained by the model by what's
#' explained by the model AND the covariate. So the formula changes:
#' *η²partial = SSm/SSm + SSr*. In this case, we calculate the η²partial for
#' each of the independent variables.
#' `aov()`'s output provides our values
model

#' Calculating partnerLibido η²partial effect size on the model
partner_libido_eta_squared <- 15.076/(15.076 + 79.047)
partner_libido_eta_squared

#' Calculating partnerLibido η²partial effect size on the model
dose_eta_squared <- 25.185/(25.185 + 79.047)
dose_eta_squared

# Calculating effect size d for group differences ====
#' To calculate that we need the `compute.es::mes()` function
#' To use that, we need to have the means and standard deviations for each group
#' And, to have that, we have to calculate both the adjusted means and also
#' the adjusted standard deviations.

#' Calculating *adjusted means*
adjusted_means_of_libido_by_dose <- effect('dose', model, se = T)
adjusted_means_of_libido_by_dose

#' Calculating *adjusted standard deviations*
#' The standard deviation can be calculated multiplying the standard error by
#' the squared root of n.
#' Getting the Ns
by(df$libido, df$dose, psych::describe)

n <- c(9, 8, 13)

adjusted_sd_of_libido_by_dose <- adjusted_means_of_libido_by_dose$se * sqrt(n)
adjusted_sd_of_libido_by_dose

#' The `mes()` function takes the form of:
#' `mes(MEAN_group1, MEAN_group2, SD_group1, SD_group2, N_group1, N_group2)`

# Placebo vs. Low Dose
mes_placebo_low <- mes(2.926370, 4.712050,
                       1.788613, 1.755879,
                       9, 8)
mes_placebo_low$d # mean difference
mes_placebo_low$r # correlation coefficient

# Placebo vs. High Dose
mes_placebo_high <- mes(2.926370, 5.151251,
                        1.788613, 5.151251,
                        9, 13)
mes_placebo_high$d # mean difference
mes_placebo_high$r # correlation coefficient

# Low Dose vs. High Dose
mes_low_high <- mes(4.712050, 5.151251,
                    1.755879, 5.151251,
                    8, 13)
mes_low_high$d # mean difference
mes_low_high$r # correlation coefficient










# Calculating rcontrast effect size ====
# Calculating r after planned contrasts ====
#' Another way to do that is to follow another r formula:
#' *R = SQRT [ t² / (t² + df)]*
rcontrast <- function(t, df) {
  
  r <- sqrt(t^2 / (t ^ 2 + df))
  
  print(paste("r = ", r))
  
}

#' To assess the t and df values, we execute the `aov()` contrast output again
#' Doing the contrasts by hand
contrasts(df$dose) <- cbind(c(-2,1,1), c(0,-1,1))
# Running the ANCOVA
model <- aov(libido ~ partnerLibido + dose, data = df)

# Getting the results
summary.lm(model)

# Creating a t vector
t <- c(2.227,
       2.785,
       0.541)

# Creating df
df <- 26

# Running the effect size
rcontrast(t, df)

# Reporting the ANCOVA ====

#' *General form*
#' "The covariate, partner’s libido, was significantly related to the 
#' participant’s libido, F(1, 26) = 4.96, p < .05, r = .40. There was also a 
#' significant effect of the dose of Viagra on levels of libido after 
#' controlling for the effect of partner’s libido, F(2, 26) = 4.14, p < .05,
#' partial η2 = .24."

#' *Planned contrasts*
#' "Planned contrasts revealed that taking a high or low dose of Viagra
#' significantly increased libido compared to taking a placebo, t(26) = 2.79,
#' p < .01, r = .48; there was no significant difference between the high and
#' low doses of Viagra, t(26) = 0.54, p = .59, r = .11"

#' *Post hocs and effect sizes*
#' "Tukey post hoc tests revealed that the covariate adjusted mean of the 
#' high-dose group was significantly greater than that of the placebo 
#' (difference = 2.22, t = 2.77, p < .05, d = 1.13). However, there was no 
#' significant difference between the low-dose and placebo groups 
#' (difference = 1.79, t = 2.10, p = .11, d = 1.04) and between the low-dose and
#' high-dose groups (difference = 0.44, t = 0.54, p = .85, d = 0.11). Despite 
#' the lack of significance between the low-dose and placebo groups, the effect
#' size was quite large."

# BONUS: Plotting covariate VS outcome ====
ggplot(df, aes(x = partnerLibido, y = libido)) +
  
  # Pontos
  geom_point(alpha = 1, position = 'jitter', color = "#011e5a") +
  
  # Linha
  stat_smooth(method = "lm", se = T, color = "black", size = 0.5) +
  
  # Ajustando x
  xlab('Partner Libido') +
  xlim(c(0, 10)) +
  
  # Ajustando y
  ylab('Participant Libido') +
  ylim(c(0, 10)) +
  
  # theme
  theme_classic()






