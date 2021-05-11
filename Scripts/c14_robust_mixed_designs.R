# Chapter 14 - Robust mixed two-way ANOVA ====

# Initializing ====
library(compute.es)
library(dplyr)
library(ggplot2)
library(magrittr)
library(pastecs)
library(tidyr)
library(WRS2)

df <- read.delim(".\\Data Files\\ProfilePicture.dat")

# Arranging dataset ====
#' *case* as factor
df$case %<>% as.factor

#' *relationship_status* as factor
df$relationship_status %<>% as.factor

#' Transforming into long format
df %<>%
  pivot_longer(
    cols = couple:alone,
    names_to = 'picture',
    values_to = 'requests'
  )

#' Recoding *picture*
df$picture %<>% recode("alone" = "Alone")
df$picture %<>% recode("couple" = "With Man")

#' *picture* as factor
df$picture %<>% as.factor

# Creating boxplots for each condition ====
df %>% 
  
  # Criando primeira camada
  ggplot(aes(x = relationship_status, y = requests, color = picture)) +
  
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
               aes(x = as.numeric(relationship_status), y = requests)) +
  
  # Mudando cores dos gêneros
  scale_color_brewer(name = 'Picture',
                     type = 'qual',
                     palette = 'Dark2') +
  
  # Mudando nome de todas as partes do gráfico
  xlab('Relationship Status') +
  ylab('Number of Friend Requests') +
  
  # Alterando limites
  ylim(c(0,12)) +
  
  # Theme
  theme_minimal()

# Conducting a robust two-way mixed ANOVA with trimmed means ====
model <- bwtrim(requests ~ 
                  relationship_status +
                  picture +
                  relationship_status:picture,
                id = case,
                data = df)

# Results
model$Qa #' main effect of factor A (relationship_status)
model$A.p.value

model$Qb #' main effect of factor B (picture)
model$B.p.value

model$Qab #' main effect of factor A & B (higher-order)
model$AB.p.value

# Calculating effect sizes for factor A using M-estimator and bootstrap ====
a_effect_size <- sppba(
  requests ~
    relationship_status +
    picture +
    relationship_status:picture,
  id = case,
  data = df
)

a_effect_size

# Calculating effect sizes for factor B using M-estimator and bootstrap ====
b_effect_size <- sppbb(
  requests ~
    relationship_status +
    picture +
    relationship_status:picture,
  id = case,
  data = df
)

b_effect_size

# Calculating effect sizes for factor B using M-estimator and bootstrap ====
int_effect_size <- sppbi(
  requests ~
    relationship_status +
    picture +
    relationship_status:picture,
  id = case,
  data = df
)

int_effect_size

# Reporting robust two-way mixed ANOVA results ====
#' *Reporting general results*
#' "Remember that factor A was relationship status and factor B the profile 
#' picture used; therefore, we could conclude that there were significant main 
#' effects of relationship status, Q = 10.79, p = .003, and type of profile 
#' picture, Q = 92.13, p < .001, and a significant 
#' relationship status × type of profile picture interaction, 
#' Q = 8.17, p = .008."
#' 
#' *Reporting specific effects*
#' "There were significant main effects of relationship status, 
#' Ψˆ = -1.46, p = .001, and type of profile picture,Ψˆ = -3.17, p < .001, 
#' and a significant relationship status × type of profile picture interaction, 
#' Ψˆ = 1.44, p = .015."












