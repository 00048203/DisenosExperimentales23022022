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
# No Paramétrico #
##################

##################
# Carga de datos #
##################

# Comparando todos los tratamientos
data.df <- read.xlsx(xlsxFile ="Data/Caso 02.xlsx",sheet = "Caso 2")
data.df <- data.df[order(data.df$Orden),]

#kruskal-wallis Test
?kruskal.test
kruskal.test(Promedio ~ Desayuno, data = data.df)
#Kruskal-Wallis chi-squared = 26.35, df = 4, p-value = 2.69e-05



# Comparando cada pareja de tratamientos

#pairwise.wilcox.test(data.df$Promedio, data.df$Desayuno, p.adjust.method = "BH")

#Pairwise comparisons using Wilcoxon rank sum exact test 

#data:  data.df$Promedio and data.df$Desayuno 

#Buena calidad Completo Insuficiente calidad Mala calidad
#Completo             0.0096        -        -                    -           
#Insuficiente calidad 0.0062        0.0062   -                    -           
#Mala calidad         0.0054        0.0054   0.1275               -           
#Mejorable calidad    0.0054        0.0054   0.0062               0.0062      

#P value adjustment method: BH 


