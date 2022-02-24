##################
# No Paramétrico #
##################

  # Comparando todos los tratamientos
data.df <- read.xlsx(xlsxFile ="Data/Caso 02.xlsx",sheet = "Caso 2")
data.df <- data.df[order(data.df$Orden),]

kruskal.test(Promedio ~ Desayuno, data = data.df)

  # Comparando cada pareja de tratamientos

pairwise.wilcox.test(data.df$Promedio, data.df$Desayuno, p.adjust.method = "BH")



