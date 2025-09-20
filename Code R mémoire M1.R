install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)

df <- read_excel("Reponse questionnaire RSTUDIO.xlsx", sheet = "Questionnaire - M1", skip = 1)

view(df)
###### Shapiro-Wilk : H0 = l'echantillon suit une distrib normale (si p-value > 0.05)

### Tests de Shapiro-Wilk pour les principales variables
shapiro.test(df$Q_GA)
shapiro.test(df$Q_GT)
shapiro.test(df$Q_PA)
shapiro.test(df$Q_PT)

### Tests de Shapiro-Wilk pour QQ_GA1 a QQ_GA10
shapiro.test(df$QQ_GA1)
shapiro.test(df$QQ_GA2)
shapiro.test(df$QQ_GA3)
shapiro.test(df$QQ_GA4)
shapiro.test(df$QQ_GA5)
shapiro.test(df$QQ_GA6)
shapiro.test(df$QQ_GA7)
shapiro.test(df$QQ_GA8)
shapiro.test(df$QQ_GA9)
shapiro.test(df$QQ_GA10)

### Tests de Shapiro-Wilk pour QQ_GT1 a QQ_GT10
shapiro.test(df$QQ_GT1)
shapiro.test(df$QQ_GT2)
shapiro.test(df$QQ_GT3)
shapiro.test(df$QQ_GT4)
shapiro.test(df$QQ_GT5)
shapiro.test(df$QQ_GT6)
shapiro.test(df$QQ_GT7)
shapiro.test(df$QQ_GT8)
shapiro.test(df$QQ_GT9)
shapiro.test(df$QQ_GT10)

### Tests de Shapiro-Wilk pour QQ_PA1 a QQ_PA10
shapiro.test(df$QQ_PA1)
shapiro.test(df$QQ_PA2)
shapiro.test(df$QQ_PA3)
shapiro.test(df$QQ_PA4)
shapiro.test(df$QQ_PA5)
shapiro.test(df$QQ_PA6)
shapiro.test(df$QQ_PA7)
shapiro.test(df$QQ_PA8)
shapiro.test(df$QQ_PA9)
shapiro.test(df$QQ_PA10)

### Tests de Shapiro-Wilk pour QQ_PT1 a QQ_PT10
shapiro.test(df$QQ_PT1)
shapiro.test(df$QQ_PT2)
shapiro.test(df$QQ_PT3)
shapiro.test(df$QQ_PT4)
shapiro.test(df$QQ_PT5)
shapiro.test(df$QQ_PT6)
shapiro.test(df$QQ_PT7)
shapiro.test(df$QQ_PT8)
shapiro.test(df$QQ_PT9)
shapiro.test(df$QQ_PT10)

# AUCUNE DISTRIB NORMALE (toutes les p-values > 0.05), DONC MANN-WHITNEY TEST

### Test de Mann_WHitney. H0 : pas de difference signficatives entre la moyenne des 2 variables (si p-value > 0.05)



### Tester l'hypothese H0 de mon etude
wilcox.test(df$Q_GA, df$Q_GT, paired = TRUE, exact = FALSE)

#p-value = 0.4342 > 0.05 donc pas de differences significative entre les moyennes des 2 variables

wilcox.test(df$Q_PA, df$Q_PT, paired = TRUE, exact = FALSE)

#p-value = 0.8581 > 0.05 donc pas de differences significative entre les moyennes des 2 variables



### Tester l'hypothese H1 de mon etude
wilcox.test(df$Q_GA, df$Q_PA, paired = TRUE, exact = FALSE)

#p-value = 0.01154 < 0.05 donc difference significative entre les moyennes

wilcox.test(df$Q_GT, df$Q_PT, paired = TRUE, exact = FALSE)

#p-value = 0.03058 < 0.05 donc difference significative entre les moyennes



#Graphique de regression

series <- c("QQ_GA", "QQ_GT", "QQ_PA", "QQ_PT")

par(mfrow = c(2, 2))  # 2 lignes, 2 colonnes

for (i in 1:4) {
  s <- series[i]
  vars <- paste0(s, 1:10)
  y <- colMeans(df[vars], na.rm = TRUE)
  x <- 1:10
  mod <- lm(y ~ x)
  pas_x <- ifelse(i <= 2, 1, 2)
  pas_y <- ifelse(i <= 2, 1, 2)
  
  plot(x, y, main = paste("Regression of the variable", s),
       xlab = "",
       ylab = "",
       pch = 20, col = "blue",
       xlim = c(1, 10),
       ylim = range(floor(min(y)):ceiling(max(y))),
       xaxt = "n", yaxt = "n")
  axis(1, at = seq(1, 10, by = pas_x))
  axis(2, at = seq(floor(min(y)), ceiling(max(y)), by = pas_y))
  abline(mod, col = "red", lwd = 4)
  
  # Legende en bas a droite
  legend("bottomright",
         legend = paste0("y = ", round(coef(mod)[1], 2), 
                         " + ", round(coef(mod)[2], 2), "x\nR2 = ", 
                         round(summary(mod)$r.squared, 3)),
         bty = "n")
}



############PAS INCLU DANS LE MEMOIRE###################
########################################################
# Supposons que ton fichier est déjà chargé dans df, contenant les 4 colonnes : Q_GA, Q_GT, Q_PA, Q_PT

# Créer un tableau long avec toutes les observations
df_long <- data.frame(
  transfert = c(df$Q_GA, df$Q_GT, df$Q_PA, df$Q_PT),
  resource = rep(c("money", "time", "money", "time"), each = nrow(df)),
  frame = rep(c("gain", "gain", "loss", "loss"), each = nrow(df))
)

# Convertir en facteurs
df_long$resource <- factor(df_long$resource, levels = c("money", "time"))
df_long$frame <- factor(df_long$frame, levels = c("gain", "loss"))

# Modèle avec interaction
model <- lm(transfert ~ resource * frame, data = df_long)

# Résumé du modèle
summary(model)

boxplot(transfert ~ frame * resource, data = df_long,
        col = c("lightblue", "lightgreen"),
        xlab = "Condition (Frame × Resource)",
        ylab = "Amount sent")
