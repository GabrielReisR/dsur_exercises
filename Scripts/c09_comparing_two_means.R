# Chapter 9 - Comparing Two Means ====

# Initializing ====
library(ggplot2)
library(dplyr)
library(magrittr)
library(pastecs)
library(psych)
library(WRS2)

# Lendo banco
df_long <- read.delim('.\\Data Files\\SpiderLong.dat')
df_wide <- read.delim('.\\Data Files\\SpiderWide.dat')

# Teste-t Independente (ou inter-grupo ou medidas independentes) ====
#' Usando *df_long*
t_test_long <- t.test(Anxiety ~ Group, data = df_long)

#' Usando *df_wide*
t_test_wide <- t.test(df_wide$real, df_wide$picture)

# Interpretando os outputs ====
t_test_output <- t_test_wide
t_test_output

#' Perceba o nome de *Welch* no título - isso significa que a correção de Welch
#' para heteroscedasticidade foi implementada. Isso explica o valor "quebrado" 
#' de graus de liberdade (df = 21.385). Como o valor de p é maior que nosso alfa
#' de 0.05, a hipótese nula não pode ser rejeitada.
#' 
#' Ainda, a subseção *95 percent confidence interval* mostra o intervalo de 
#' confiança de 95% para os possíveis valores de diferença entre a média dos 
#' dois grupos.
#' 
#' Em suma, não há diferença significativa entre os grupos.

# Test-t independente robusto de Wilcox ====
#' Esses testes são feitos utilizando a biblioteca *WRS2*
#' O banco de dados deve estar em formato long (cada coluna representa um grupo)

yuen_output <- yuen(Anxiety ~ Group, data = df_long) # trim = 0.2, ou seja, 20%
yuen_output

yuen_output_10_trim <- yuen(Anxiety ~ Group, 
                            data = df_long, tr = 0.1) # trim = 0.1, ou seja, 10%
yuen_output_10_trim

yuen_bootstrap <- yuenbt(Anxiety ~ Group, data = df_long)
yuen_bootstrap

yuen_bootstrap_2000 <- yuenbt(Anxiety ~ Group, data = df_long, nboot = 2000)
yuen_bootstrap_2000

# Calculando tamanho de efeito (r) para teste-t independente ====
#' r = SQRT[ t² / t² + df ]
#' Indo pelo output...
t <- t_test_output$statistic[[1]]
df <- t_test_output$parameter[[1]]

r <- sqrt(t^2/(t^2 + df))
r

#' O efeito foi de r = 0.34, um efeito considerado médio. Ou seja, mesmo não
#' sendo significativo, o efeito foi relevante. Desta forma, possivelmente, o 
#' valor de p foi influenciado pelo pequeno número amostral.

# Reportando teste-t independente ====

#' Sugestão do Andy Field:
#' "On average, participants experienced greater anxiety from real spiders 
#' (M = 47.00, SE = 3.18), than from pictures of spiders (M = 40.00, SE = 2.68).
#' This difference was not significant t(21.39) = −1.68, p > .05; however, it 
#' did represent a medium-sized effect r = .34"

# Teste-t dependente (ou intragrupo ou de medidas repetidas) ====
#' A fim de avaliar a normalidade, estamos interessados na distribuição normal
#' das diferenças entre condições. Ou seja, na diferença dos escores da condição
#' real para a condição da foto.

# Avaliando normalidade dos resíduos (da diferença entre condições) ====
# Criando nova variável
df_wide$diff <- df_wide$real - df_wide$picture

#' Avaliando normalidade com `pastecs::stat.desc`
stat.desc(df_wide$diff, basic = F, desc = F, norm = T)

# Realizando o teste-t dependente ====
#' Mesmo procedimento de antes, porém agora com o argumento `paired = T`
# Utilizando banco long
dep_t_test_output <- t.test(Anxiety ~ Group, data = df_long, paired = T)
dep_t_test_output

# Utilizando banco wide
dep_t_test_output <- t.test(df_wide$real, df_wide$picture, paired = T)
dep_t_test_output

#' Pode-se concluir que há uma diferença significativa entre a ansiedade dos
#' participantes nas duas condições especificadas.
#' O output possui interpretação semelhante ao já visto no teste-t independente

# Teste-t dependente robusto de Wilcox ====
yuen_d_output <- yuend(df_wide$real, df_wide$picture)
yuen_d_output

#' Utilizando o teste-t dependente robusto de Wilcox, não são encontradas
#' diferenças significativas entre os grupos.

yuen_d_bootstrap_effect_output <- dep.effect(df_wide$real, df_wide$picture,
                                             nboot = 2000)
yuen_d_bootstrap_effect_output

# Calculando tamanho de efeito (r) para teste-t dependente ====
#' r = SQRT[ t² / t² + df ]
#' Indo pelo output...
t <- dep_t_test_output$statistic[[1]]
df <- dep_t_test_output$parameter[[1]]

r <- sqrt(t^2/(t^2 + df))
r

#' O tamanho de efeito é grande (segundo Cohen, > 0.5). Isso significa que há
#' diferenças relevantes entre as duas condições.

# Reportando teste-t dependente ====

#' Sugestão de Andy Field:
#' "On average, participants experienced significantly greater anxiety from real
#' spiders (M = 47.00, SE = 3.18) than from pictures of spiders (M = 40.00,
#' SE = 2.68), t(11) = 2.47, p < .05, r = .60."
