#The intention of this file is to show the R tag due to github lack of recognition of the .Rmd extension

options(scipen=999)
M = read.csv("G106-datos05-1.csv") 
str(M)

table(M$entidad)

names (M) 
summary(M)

R1 = c("Australia", "Samoa", "Papua New Guinea")
R2 = c("Cuba", "Jamiaca", "Trinidad and Tobago")
region = ifelse(M$entidad %in% R1, "Oceania", "Caribe")
M1 = cbind(M, region)
library(dplyr) 
M1 = select(M1, "entidad", "region", "electrd_fosiles", ,"electrd_nuclear", "electrd_de_energ_renov", "crecimiento_PIB", "emisiones_CO2", "densidad_pobl_Km2", "consumo_energ_prim", "superficie") 
M1 = M1[-1]
M1 = na.omit(M1) 
str(M1)
write.csv(M1, "datosEq5.csv", row.names = FALSE)

R1 = subset(M1, M1$region == "Oceania")[-1]
R2 = subset(M1, M1$region == "Caribe")[-1]
cat("Principales Medias Oceania", "\n")
summary(R1) 

cat("\n", "Desviación estándar", "\n")
apply(R1,2, sd)
cat("\n", "Rango Medio", "\n")
Rm = function(x)((max(x)+min(x))/2)
apply(R1, 2, Rm) 

cat("Principales Medias Caribe", "\n")
summary(R2) 

cat("\n", "Desviación estándar", "\n")
apply(R2,2, sd)
cat("\n", "Rango Medio", "\n")
Rm = function(x)((max(x)+min(x))/2)
apply(R2, 2, Rm) 

par(mfrow = c(2,2))
hist(R1$electrd_fosiles, col = 4, main = "Region Oceania", xlab = "Electricidad de origen fosil(kWh)", ylab = "Frecuencia")
hist(R2$electrd_fosiles, col = 3, main = "Region Caribe", xlab = "Electricidad de origen fosil(kWh)", ylab = "")

hist(R1$electrd_de_energ_renov, col = 4, main = "Region Oceania", xlab = "Electricidad de energias renovables(kWh)", ylab = "Frecuencia")
hist(R2$electrd_de_energ_renov, col = 3, main = "Region Caribe", xlab = "Electricidad de energias renovables(kWh)", ylab = "")

par(mfrow = c(1,2))
boxplot(electrd_fosiles ~ region, data = M, col=3:4, main = "Electricidad de origen fosil")
boxplot(electrd_de_energ_renov ~ region, data = M, col=3:4, main = "Electricidad de energias renovables")

par(mfrow = c(2,2))
boxplot(R1$electrd_fosiles, col=4, main = "Electricidad de origen fosil, Oceania", horizontal=TRUE)
boxplot(R2$electrd_fosiles, col=3, main = "Electricidad de origen fosil, Caribe", horizontal=TRUE)

boxplot(R1$electrd_de_energ_renov, col=4, main = "Electricidad de energias renovables, Oceania", horizontal=TRUE)
boxplot(R2$electrd_de_energ_renov, col=3, main = "Electricidad de energias renovables, Caribe", horizontal=TRUE)

correl = cor(R1);
round(correl,3)
correl = cor(R2);
round(correl,3)

plot(R1, col = "blue") 
plot(R2, col = "blue") 
