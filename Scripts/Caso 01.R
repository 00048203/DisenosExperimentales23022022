#####################
# Carga de paquetes #
#####################

install.packages("openxlsx")
library(openxlsx) # Para abrir documentos de Excel

install.packages("tidyverse")
library(tidyverse) # ggplot2, para graficos

install.packages("moments")
library(moments) # Estadísticos varios

install.packages("nortest")
library(nortest) # Test de normalidad no parametricos

install.packages("lmtest")
library(lmtest) # Test de normalidad no parametricos

install.packages("parameters")
library(parameters) # Analisis de parametros ANOVA

install.packages("effectsize")
install.packages("lsr")
library(effectsize)
library(lsr) # Efectos

install.packages("agricolae")
library(agricolae) # Test Multimedias

install.packages("DescTools")
library(DescTools) # Test Multimedias

install.packages("pwr2")
library(pwr2) #Potencia

library(car)
library(gridExtra)


##################
# Carga de datos #
##################

data.df <- read.xlsx(xlsxFile ="Data/Caso 01.xlsx",sheet = "Caso 1")
data.df

View(data.df)

colnames(data.df)
colnames(data.df) <- c("Ambiente","Colaboradores","Orden")

str(data.df)

data.df <- data.df[order(data.df$Orden),]


########################
# Exploración de datos #
########################

table(data.df$Ambiente)
aggregate(Colaboradores ~ Ambiente, data = data.df, FUN = mean)
aggregate(Colaboradores ~ Ambiente, data = data.df, FUN = sd)

ggplot(data = data.df, aes(x = Ambiente, y = Colaboradores, color = Ambiente)) +
  geom_boxplot() +
  theme_bw()


#########
# ANOVA #
#########

anova <- aov(Colaboradores ~ Ambiente, data = data.df)
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
kurtosis(anova$residuals)
kurtosis(anova$residuals) - 3 # Exceso de curtosis

# Pruebas de hipotesis no paramétricas
ks.test(anova$residuals, pnorm, mean(anova$residuals), sd(anova$residuals), alternative = c("greater"))
shapiro.test(anova$residuals)
ad.test(anova$residuals)

    # 2. Independencia
    # ----------------

plot(anova$residuals)
dwtest(anova)
durbinWatsonTest(anova)

bgtest(anova,order = 2)
bgtest(anova,order = 1)

acf(anova$residuals, ylim=c(-1,1))

    # 3. Homocedasticidad
    # -------------------

plot(anova, which=1)
bartlett.test(anova$residuals, anova$fitted.values) # Datos normales
leveneTest(anova$residuals, factor(data.df$Ambiente), center="median") # Datos no normales

    # 4. Graficos de ANOVA
    # --------------------

par(mfrow=c(2,2))
plot(anova)


###############################
# Comparación de tratamientos #
###############################

par(mfrow=c(1,1))

  # Fisher
  # ------
LSD.test(anova, "Ambiente",console=T) 
plot(LSD.test(anova, "Ambiente",console=T))

  # Tukey
  # -----
TukeyHSD(anova) 
plot(TukeyHSD(anova))

HSD.test(anova, "Ambiente",console=T)
plot(HSD.test(anova, "Ambiente",console=T))

  # Duncan
  # ------
duncan.test(anova, "Ambiente",console=T) 
plot(duncan.test(anova, "Ambiente",console=T))

  # Newman
  # ------
SNK.test(anova, "Ambiente", console = T)
plot(SNK.test(anova, "Ambiente", console = T))

  # Dunnet (un grupo de control)
  # ----------------------------
DunnettTest(x=data.df$Colaboradores, g=factor(data.df$Ambiente))
DunnettTest(x=data.df$Colaboradores, g=factor(data.df$Ambiente), control = "B")

############
# Potencia #
############

n <- seq(2, 10, by=0.5)
pwr.plot(n=n, k=5, f= 1, alpha=0.05)

n <- seq(2, 10, by=0.5)
f <- seq(1, 5)
pwr.plot(n=n, k=5, f=f, alpha=0.05)


######################################
# ¿Qué más nos proporciona el ANOVA? #
######################################

  # Parametros del modelo
  # ---------------------
anova$coefficients
lm.model <- lm(Colaboradores ~ Ambiente, data = data.df) # Internamente es un modelo de regresión
model_parameters(lm.model)

  # Valores estimados
  # -----------------
anova$fitted.values
par(mfrow=c(2,1))
data.df['Estimado'] <- anova$fitted.values

plot1 <- ggplot(data = data.df, aes(x = Ambiente, y = Colaboradores, color = Ambiente)) +
  geom_point() +
  geom_text(label=data.df$Colaboradores,nudge_x = 0.5, nudge_y = 0.5)
plot2 <- ggplot(data = data.df, aes(x = Ambiente, y = Estimado, color = Ambiente)) +
  geom_point() +
  geom_text(label=data.df$Estimado,nudge_x = 0.5, nudge_y = 0.5)
grid.arrange(plot1, plot2, ncol=2)

  


