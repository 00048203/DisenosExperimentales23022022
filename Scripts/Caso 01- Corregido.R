#####################
# Carga de paquetes #
#####################

install.packages("openxlsx")
install.packages("tidyverse")
install.packages("moments")
install.packages("nortest")
install.packages("lmtest")
install.packages("parameters")
install.packages("effectsize")
install.packages("lsr")
install.packages("agricolae")
install.packages("DescTools")
install.packages("pwr2")

###Librerías a Usarse
#Cargar librerías
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

####ctrl + L Limpia la consola

##################
# Carga de datos #
##################

data.df <- read.xlsx(xlsxFile ="Data/Caso 01.xlsx",sheet = "Caso 1")
data.df

#Visualización de los datos
View(data.df)
head(data.df)

#Cambio de nombre las columnas
colnames(data.df)
colnames(data.df) <- c("Ambiente","Colaboradores","Orden")

?str
str(data.df)

#Ordeno los datos
data.df <- data.df[order(data.df$Orden),]

#data.df$Ambiente[1]
#data.df$Ambiente[2]
#data.df$Colaboradores[1]


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
anova
#Tabla Anova
summary(anova)

#Modelos del parámetro de la anova
model_parameters(anova)

#Explica la variación y da un intervalo de confianza.
eta_squared(anova, partial = FALSE)


#varianza parcial explicada por el factor sin intervalo de confianza
etaSquared(anova)

#anova$residuals


#############
# Supuestos #
#############

    # 1. Normalidad
    # -------------

# Métodos gráficos
#Histograma de anova de residuales
hist(anova$residuals)
#Se puede indicar cuántos grupos con el comando break
hist(anova$residuals, breaks = 10) 


#Gráfica de los puntos (Gráfica Q-Q)
plot(anova, which = 2)

#Gráfica de los residuales sin la línea
qqnorm(anova$residuals)

#Gráfica de los residuales con la línea
qqline(anova$residuals)

############################
############################
# Estadísticos

#Coeficiente de asimetría de los residuales de la Anova
#Si esta entre -1 y 1.
skewness(anova$residuals)

#Curtosis
kurtosis(anova$residuals)

#Exceso de curtosis
kurtosis(anova$residuals) - 3 # Exceso de curtosis

#####################################
# PRUEBA DE HIPÓTESIS NO PARAMÉTRICAS
#?ks.test
#Prueba K-S "greater" cola superior.
ks.test(anova$residuals, pnorm, mean(anova$residuals), sd(anova$residuals), alternative = c("greater"))
#p-valor 0.05 < 0.2687


#Prueba Shapiro-wilk
#p-valor 0.05 < 0.1818
#?shapiro.test
shapiro.test(anova$residuals)


#?ad.test
#Prueba ANDERSON-DARLING
#p-valor 0.05 < 0.1699
ad.test(anova$residuals)

 

########################
########################
   # 2. Independencia
    # ----------------

plot(anova$residuals)

#Test Durbin watson (Anova).
dwtest(anova)

#Test Durbin Watson (Autocorrelación)
durbinWatsonTest(anova)

#?bgtest: Breuch-Godfrey Test

bgtest(anova,order = 2)
bgtest(anova,order = 1)

#Auto-and-Cross-Covariance and Correlation Function Estimation
acf(anova$residuals, ylim=c(-1,1))


##############################
    # 3. Homocedasticidad
    # -------------------

plot(anova, which=1)

#Prueba de Barlett p-value=0.9198 > 0.05 se cumple homocedasticidad.
bartlett.test(anova$residuals, anova$fitted.values) # Datos normales

#Si los datos no provienen de una distribución normal se ocupa Test de Levene
leveneTest(anova$residuals, factor(data.df$Ambiente), center="median") # Datos no normales

###########################
    # 4. Graficos de ANOVA
    # --------------------

par(mfrow=c(2,2))
#par(mfrow=c(1,1))
plot(anova)



###############################
# Comparación de tratamientos #
###############################

  par(mfrow=c(1,1))

  # Fisher
  # ------
  #(anova, "ambiente", desplegarlo en consola=true)
  LSD.test(anova, "Ambiente",console=T) 
  
  #Gráfico
  plot(LSD.test(anova, "Ambiente",console=T))

  # Tukey
  # -----

  TukeyHSD(anova) 
  #Gráfico
  plot(TukeyHSD(anova))

#Tukey HSD Test Con grupos  
  HSD.test(anova, "Ambiente",console=T)
#Gráfico
plot(HSD.test(anova, "Ambiente",console=T))


  # Duncan
  # ------

duncan.test(anova, "Ambiente",console=T) 
#Gráfico
plot(duncan.test(anova, "Ambiente",console=T))

  # Newman
  # ------
SNK.test(anova, "Ambiente", console = T)
#Gráfico
plot(SNK.test(anova, "Ambiente", console = T))

  
  # Dunnet (un grupo de control)
  # ----------------------------
DunnettTest(x=data.df$Colaboradores, g=factor(data.df$Ambiente))
DunnettTest(x=data.df$Colaboradores, g=factor(data.df$Ambiente), control = "B")

#Hasta aquí quedó la clase


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

  


