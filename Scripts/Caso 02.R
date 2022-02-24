#####################
# Carga de paquetes #
#####################

library(openxlsx) # Para abrir documentos de Excel
library(tidyverse) # ggplot2, para graficos
library(moments) # Estadísticos varios
library(nortest) # Test de normalidad no parametricos
library(lmtest) # Test de normalidad no parametricos
library(parameters) # Analisis de parametros ANOVA
library(effectsize)
library(lsr) # Efectos
library(agricolae) # Test Multimedias
library(DescTools) # Test Multimedias
library(pwr2) #Potencia
library(car)
library(gridExtra)


##################
# Carga de datos #
##################

data.df <- read.xlsx(xlsxFile ="Data/Caso 02.xlsx",sheet = "Caso 2")
View(data.df)
str(data.df)
data.df <- data.df[order(data.df$Orden),]


########################
# Exploración de datos #
########################

table(data.df$Desayuno)
aggregate(Promedio ~ Desayuno, data = data.df, FUN = mean)
aggregate(Promedio ~ Desayuno, data = data.df, FUN = sd)

ggplot(data = data.df, aes(x = Desayuno, y = Promedio, color = Desayuno)) +
  geom_boxplot() +
  theme_bw()

    # Convirtiendo a factor
data.df$Desayuno <- parse_factor(data.df$Desayuno,
                                 levels = c('Mala calidad',
                                         'Insuficiente calidad',
                                         'Mejorable calidad',
                                         'Buena calidad',
                                         'Completo'))
  
  

#########
# ANOVA #
#########

anova <- aov(Promedio ~ Desayuno, data = data.df)
summary(anova)
model_parameters(anova)

eta_squared(anova, partial = FALSE)
etaSquared(anova)


#############
# Supuestos #
#############

    # 1. Normalidad
    # -------------

# Métodos gráficos
hist(anova$residuals)
hist(anova$residuals, breaks = 10) 

plot(anova, which = 2)
qqnorm(anova$residuals)
qqline(anova$residuals)

# Estadísticos
skewness(anova$residuals)
moments::kurtosis(anova$residuals)
moments::kurtosis(anova$residuals) - 3 # Exceso de curtosis

# Pruebas de hipotesis no paramétricas
ks.test(anova$residuals, pnorm, mean(anova$residuals), sd(anova$residuals), alternative = c("greater"))
shapiro.test(anova$residuals)
ad.test(anova$residuals)

    # 2. Independencia
    # ----------------

plot(anova$residuals)
dwtest(anova)
acf(anova$residuals, ylim=c(-1,1))

    # 3. Homocedasticidad
    # -------------------

plot(anova, which=1)
bartlett.test(anova$residuals, anova$fitted.values) # Datos normales

#p-value=0.03483<0.05
bartlett.test(data.df$Promedio, data.df$Desayuno) 

