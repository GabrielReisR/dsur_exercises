# Chapter 17 - Exploratory Factor Analysis ====

# Initializing ====
library(corpcor)
library(GPArotation)
library(dplyr)
library(ggplot2)
library(magrittr)
library(psych)
library(tidyr)

df <- read.delim(".\\Data Files\\raq.dat")

# Creating correlation matrix of items ====
cor_matrix <- cor(df)

# Checking Bartlett's assumption ====
#' Bartlett's assumption is that my correlation matrix is an identity matrix.
#' The H1 for this is that my matrix is NOT an identity matrix.
#' So if we get a significant result, I can proceed with the analysis.
cortest.bartlett(df) # significant, yay!

#' But it's best if we give the matrix of correlations...

cortest.bartlett(cor_matrix, n = 2571) # significant, yay!

# Checking Kaiser-Meyer-Olkin results ====
#' This is the sum of correlations of an item with all other items divided by
#' the sum of correlations of an item with all other items PLUS its partial
#' correlations with all other items.
#' 
#' If the partial correlations with all other items == 0, that means we get a
#' KMO result of 1. That means that we have correlations between items only when
#' considering those items to be separate. When we take into account all other
#' items, we get low partial correlations because there's "something else"
#' explaining the correlations between items (that is, the latent variable).
#' 
#' From that, we can conclude that:
#' 0.5 is the minimum acceptable value for KMO
#' 0.7 is a good enough value, since at least 49% of the variation in the
#' correlations can be explained by only the correlations (0.7 ^2 = 49%).
#' 0.8 is also a good number.
#' 0.9 is a great number!

KMO(cor_matrix)

# Overall MSA =  0.93
#' *great* number!

# Accessing the determinant ====
#' This is not particularly done nor reported.
det(cor_matrix)

#' The recommendation is that it should be above 0.00001
#' It is, in this case, 0.0005... so yay us!

# Running a Principal Components Analysis using psych::principal() ====
pc1 <- principal(cor_matrix,
                 nfactors = length(cor_matrix[1,]), # 23
                 rotate = 'none') # not for now, at least

pc1

# Getting scree plot for the first solution ====
plot(pc1$values)

# Running second Principal Components Analysis using psych::principal() ====
pc2 <- principal(cor_matrix,
                 nfactors = 4,
                 rotate = 'none') # not for now, at least

pc2

# Getting the factor model - the reproduced correlations between items ====
factor.model(pc2$loadings)

#' The diagonal of this matrix contains the communalities after extraction 
#' for each variable

# Getting the factor residuals ====
factor.residuals(cor_matrix, pc2$loadings)

#' The diagonal of this matrix is the uniquenesses. This matrix contains the 
#' differences between the observed correlation coefficients and the ones 
#' predicted from the model.

# Creating a function to calculate residuals for the model ====
residual.stats <- function(matrix){
  residuals <- as.matrix(matrix[upper.tri(matrix)])
  large.resid <- abs(residuals) > 0.05
  numberLargeResids <- sum(large.resid)
  propLargeResid <- numberLargeResids/nrow(residuals)
  rmsr <- sqrt(mean(residuals^2)) #' already in psych::principal()'s output
  cat("Root means squared residual = ", rmsr, "\n")
  cat("Number of absolute residuals > 0.05 = ", numberLargeResids, "\n")
  cat("Proportion of absolute residuals > 0.05 = ", propLargeResid, "\n")
  hist(residuals)
}

residuals <- factor.residuals(cor_matrix, pc2$loadings)
residual.stats(residuals)

# Rotation 1: varimax (orthogonal) ====
pc3 <- principal(cor_matrix, nfactors = 4, rotate = "varimax")
pc3

print.psych(pc3, cut = 0.3, sort = TRUE) # printing loadings

# Rotation 2: direct oblimin (oblique) ====
pc4 <- principal(cor_matrix, nfactors = 4,
                 rotate = "oblimin",
                 oblique.scores = T, scores = TRUE)
pc4

print.psych(pc4, cut = 0.3, sort = TRUE) # printing loadings

pc4$Structure # structure matrix, in case anybody's interested

# Getting factor scores ====
#' Apparently, there's some bug in psych() and I can't get the scores.

# Reporting the principal components analysis ====
#' A principal components analysis (PCA) was conducted on the 23 items with 
#' orthogonal rotation (varimax). The Kaiser–Meyer–Olkin measure verified the 
#' sampling adequacy for the analysis KMO = .93 (‘superb’ according to Kaiser, 
#' 1974), and all KMO values for individual items were > .77, which is well 
#' above the acceptable limit of .5. Bartlett’s test of sphericity, 
#' χ² (253) = 19,334, p < .001, indicated that correlations between items were 
#' sufficiently large for PCA. An initial analysis was run to obtain eigenvalues 
#' for each component in the data. Four components had eigenvalues over Kaiser’s 
#' criterion of 1 and in combination explained 50.32% of the variance. The scree 
#' plot was slightly ambiguous and showed inflexions that would justify 
#' retaining both two and four components. Given the large sample size, and the 
#' convergence of the scree plot and Kaiser’s criterion on four components, four 
#' components were retained in the final analysis. Table 17.1 shows the factor 
#' loadings after rotation. The items that cluster on the same components 
#' suggest that component 1 represents a fear of computers, component 2 a fear 
#' of statistics, component 3 a fear of maths and component 4 peer evaluation 
#' concerns.

# Computing alpha ====
#' Alpha shouldn't be computed because of the *tau-equivalence assumption*:
#' https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2792363/
#' https://www.researchgate.net/publication/326136417_Reliability_from_alpha_to_omega_a_tutorial
#' This is a learning exercise, so we'll do it anyways

computer_fear <- df[, c(6, 7, 10, 13, 14, 15, 18)]
statistics_fear <- df[, c(1, 3, 4, 5, 12, 16, 20, 21)]
math_fear <- df[, c(8, 11, 17)]
peer_evaluation <- df[, c(2, 9, 19, 22, 23)]

alpha(computer_fear)
alpha(statistics_fear, keys = c(1, -1, 1, 1, 1, 1, 1, 1))
alpha(math_fear)
alpha(peer_evaluation)

#' The fear of computers, fear of statistics and fear of maths subscales of the 
#' RAQ all had high reliabilities, all Cronbach’s α = .82. However, the fear of 
#' negative peer evaluation subscale had relatively low reliability, 
#' Cronbach’s α = .57








