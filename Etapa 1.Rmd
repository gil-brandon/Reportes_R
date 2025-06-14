---
output:
  pdf_document: default
  html_document: default
  word_document: default
---

Gil Brandon García Contreras - A01254164

Fernando Emilio Soto Gallegos - A01641995

Jorge Eduardo Varela Muñiz - A01384569

Maximo Cruz Olvera - a01723622

Etapa 1: Analisis Exploratorio de los datos


1. Las variables y datos

```{r}
M = read.csv("G106-datos05-1.csv") 
str(M)
```
126 datos con 22 variables involucradas

```{r}
table(M$entidad)
```
Tenemos datos tanto de la region de Oceania como la del Caribe


2. Elección de las variables con las que se trabajará

```{r}
names (M) 
summary(M)
```
Elegimos:
entidad, 
electrd_fosiles, electr_nuclear, electr_energ_renov, 
crecimiento PIB,
emisiones_CO2, densidad_pobl, consumo_energ_prim_pc, superficie 

Se eligio la variable entidad como identificador. Las tres energias porque son las primarias ha analizar para e estudio. El crecimiento PIB porque el equipo estima es un factor muy importante a tomar en cuenta. Por ultimo elegimos emisoiones CO2, densidad poblacional, consumo energetico primo y superficie. Esto con la intencion de identidicar si existe una relación entre el tamaño de un pais y su gasto energetico.


3. Creación de una base de datos de trabajo del equipo

```{r}
R1 = c("Australia", "Samoa", "Papua New Guinea")
R2 = c("Cuba", "Jamiaca", "Trinidad and Tobago")
region = ifelse(M$entidad %in% R1, "Oceania", "Caribe")
M1 = cbind(M, region)
library(dplyr) 
M1 = select(M1, "entidad", "region", "energ_renov",  "electrd_fosiles", "electrd_de_energ_renov", "acceso_electrd", "acceso_combust_limpios", "consumo_energ_prim", "PIB_per_cap", "crecimiento_PIB", "emisiones_CO2", "densidad_pobl_Km2", "superficie") 
M1 = M1[-1]
M1 = na.omit(M1) 
str(M1)
write.csv(M1, "datosEq5.csv", row.names = FALSE)
```

4. Análisis Estadístico de los datos comparativo por región

```{r}
R1 = subset(M1, M1$region == "Oceania")[-1]
R2 = subset(M1, M1$region == "Caribe")[-1]
cat("Oceania", "\n")
summary(R1) 
cat("\n", "Desviación estándar", "\n")
apply(R1,2, sd)
cat("\n", "Rango Medio", "\n")
Rm = function(x)((max(x)+min(x))/2)
apply(R1, 2, Rm) 

cat("Caribe", "\n")
summary(R2) 
cat("\n", "Desviación estándar", "\n")
apply(R2,2, sd)
cat("\n", "Rango Medio", "\n")
Rm = function(x)((max(x)+min(x))/2)
apply(R2, 2, Rm) 
```
```{r}
par(mfrow = c(1,2))
hist(R1$electrd_fosiles, col = 4, main = "Región Oceania", xlab = "Electricidad de origen fósil(kWh)", ylab = "Frecuencia")
hist(R2$electrd_fosiles, col = 3, main = "Región Caribe", xlab = "Electricidad de origen fósil(kWh)", ylab = "")

hist(R1$electrd_de_energ_renov, col = 4, main = "Región Oceania", xlab = "Electricidad de energias renovables(kWh)", ylab = "Frecuencia")
hist(R2$electrd_de_energ_renov, col = 3, main = "Región Caribe", xlab = "Electricidad de energias renovables(kWh)", ylab = "")

```
```{r}
boxplot(electrd_fosiles ~ region, data = M, col=3:4, main = "Electricidad de origen fósil")
boxplot(electrd_de_energ_renov ~ region, data = M, col=3:4, main = "Electricidad de energias renovables")
```
```{r}
par(mfrow = c(1,2))
boxplot(R1$electrd_fosiles, col=4, main = "Electricidad de origen fósil, Oceania", horizontal=TRUE)
boxplot(R2$electrd_fosiles, col=3, main = "Electricidad de origen fósil, Caribe", horizontal=TRUE)

boxplot(R1$electrd_de_energ_renov, col=4, main = "Electricidad de energias renovables, Oceania", horizontal=TRUE)
boxplot(R2$electrd_de_energ_renov, col=3, main = "Electricidad de energias renovables, Caribe", horizontal=TRUE)
```

Segun los graficos obtenidos podemos ver como ambas regiones son bastante diferentes debido a su tamaño. Por lo tanto aunque oceania tenga mayor cantidad de energias renovables, tambien cuenta con una gran cantidad de energias de origen fosil.

5. Análisis relacional por región:

```{r}
correl = cor(R1)
round(correl,3)

plot(R1, col = "blue") 
plot(R1$electrd_fosiles, R1$electrd_de_energ_renov, col = "red", pch = 20, main = "Electridad de origen fósil vs de Origen renovable", ylab="Energias renovables", xlab="Energias fosiles") 

correl = cor(R2)
round(correl,3)

plot(R2, col = "blue") 
plot(R2$electrd_fosiles, R2$electrd_de_energ_renov, col = "red", pch = 20, main = "Electridad de origen fósil vs de Origen renovable", ylab="Energias renovables", xlab="Energias fosiles") 

```
