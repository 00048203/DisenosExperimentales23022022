############
# Potencia #
############

n <- seq(2, 10, by=0.5)
f <- seq(1, 5)
pwr.plot(n=n, k=5, f=f, alpha=0.05)


#########
# Datos #
#########

  # 1. Lectura de datos
data.df <- read.xlsx(xlsxFile ="Data/Caso 03.xlsx",sheet = "Caso 3")
View(data.df)

  # 2. Muestreo

set.seed(12345)
install.packages('sampling')
library(sampling)

estratos <- strata(data.df, stratanames = c('Altura'), size = c(10,10,10,10), method = "srswor")
estratos

data.df.muestra <- getdata(data.df, estratos)
View(data.df.muestra)
table(data.df.muestra$Altura)

rows <- sample(nrow(data.df.muestra))
rows
data.df.muestra <- data.df.muestra[rows, ]
View(data.df.muestra)


########################
# Exploración de datos #
########################

aggregate(Goles ~ Altura, data = data.df.muestra, FUN = mean)
aggregate(Goles ~ Altura, data = data.df.muestra, FUN = sd)

ggplot(data = data.df.muestra, aes(x = Altura, y = Goles, color = Altura)) +
  geom_boxplot() +
  theme_bw()

# Convirtiendo a factor (los levels se pueden modificar dependiendo de los resultados obtenidos)
data.df.muestra$Altura <- parse_factor(data.df.muestra$Altura,
                                  levels = c('0 a 250 m',
                                            '250 a 500 m',
                                            '1500 a 3500 m',
                                            'Mas de 3500 m'))


#########
# ANOVA #
#########

anova <- aov(Goles ~ Altura, data = data.df.muestra)
summary(anova)
model_parameters(anova)

eta_squared(anova, partial = FALSE)
etaSquared(anova)
