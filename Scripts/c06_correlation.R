# Chapter 6 - Correlation ====

# Initializing ====
load_libraries <- function(){
  if (!require("boot"))
    install.packages("boot"); library(boot)
  if (!require("corrplot"))
    install.packages("corrplot"); library(corrplot)
  if (!require("dplyr"))
    install.packages("dplyr"); library(dplyr)
  if (!require("ggplot2"))
    install.packages("ggplot2"); library(ggplot2)
  if(!require("ggm"))
    install.packages("ggm"); library(ggm)
  if(!require("magrittr"))
    install.packages("magrittr"); library(magrittr)
  if(!require("polycor"))
    install.packages("polycor"); library(polycor)
  if(!require("psych"))
    install.packages("psych"); library(psych)
  if(!require("qgraph"))
    install.packages("qgraph"); library(qgraph)
}

load_libraries()

# Simple Pearson correlations ====
# Reading data
df <- read.delim(".\\Data Files\\Exam Anxiety.dat")

# Running correlations
df <- df[,c("Revise", "Exam", "Anxiety")]

cor_matrix <- cor(df)
cor_matrix

# Visualing a scatterplot between two variables ====
ggplot(df, aes(x = Revise, y = Exam)) +
  
  # Pontos
  geom_point(alpha = 1, position = 'jitter', color = "#011e5a") +
  
  # Linha
  stat_smooth(method = "lm", se = F, color = "black", size = 0.5) +
  
  # theme
  theme_classic()

# Discovering p-values from a Pearson correlation ====
#' Pearson: *interval data*
cor.test(df$Revise, df$Anxiety)
cor.test(df$Exam, df$Anxiety)
cor.test(df$Exam, df$Revise)

# Bootstrapping a correlation ====
df <- read.delim(".\\Data Files\\The Biggest Liar.dat")

cor.test(df$Creativity, df$Position, method = "kendall")

# Creating bootstrapping function ====
boot_function <- function(df, i){
    cor(df$Creativity[i],
        df$Position[i],
        use = "complete.obs",
        method = "pearson")
  }

# Running the bootstrap
boot_results <- boot(df, boot_function, 2000)
boot_results
boot.ci(boot_results)

# Partial correlation ====
# Reading data
df <- read.delim(".\\Data Files\\Exam Anxiety.dat")

# Running correlations
df <- df[,c("Revise", "Exam", "Anxiety")]

# Correlation between Exam and Revise without the influence of Anxiety
pc <- ggm::pcor(c('Exam', 'Revise', 'Anxiety'), var(df))

# If we want to know a p-value associated with it...
pcor.test(pc, # partial correlation
          1, # number of control variables
          nrow(df)) # sample size 

# Point-biserial correlation ====
#' Correlate *interval data* with a *dichotomized variable*
# Reading data
df <- read.delim(".\\Data Files\\Exam Anxiety.dat")

# Split "Revise" by its median
df %<>%
  mutate(Studied = case_when(Revise > 15 ~ 1,
                             Revise <= 15 ~ 0))

polycor::polyserial(df$Exam, df$Studied)

# Though cited, splitting by the median is not advised

# Tetrachoric correlation ====
#' Correlate *dichotomous variable* with a *dichotomous variable*
# Preparing a crosstabulation
table_gender_studied <- table(df$Gender, df$Studied)

# Running correlation: Example 1
cor_tetra <- psych::tetrachoric(table_gender_studied)
cor_tetra

# Running correlation: Example 2
polychor(df$Gender, df$Studied)

# Polychoric correlation ====
#' Correlate *polytomous variable* with a *polytomous variable*
#' These data can have 2 levels or more.
df <- psych::bfi

poly_cor <- polychoric(df %>% select(A1:A5))
poly_cor$rho

# Correlating everything at one with qgraph ====
# Reading data
df <- read.delim(".\\Data Files\\Exam Anxiety.dat")

cor_auto(df)

# Visualizing correlations quickly ====
corrplot::corrplot(
  cor_auto(df),
  method = 'color',
  diag = T,
  type = 'lower',
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  tl.cex = .7,
  number.cex = .5
)

