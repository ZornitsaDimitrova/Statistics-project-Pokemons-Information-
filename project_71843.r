#This file loads pokemons data
#https://www2.cs.arizona.edu/classes/cs120/fall17/ASSIGNMENTS/assg02/EXAMPLES-pokemon.html - source of data

rm(list = ls())
library(fpp2)
library(readxl)
Pokemon_Info<-read.csv("E:/Downloads/PokeInfo-250.csv")
View(Pokemon_Info)

Type1 <- Pokemon_Info[,3]
Type2 <- Pokemon_Info[,4]
Total <- Pokemon_Info[,5]
HP <- Pokemon_Info[,6]
Attack <- Pokemon_Info[,7]
Defense <- Pokemon_Info[,8]
SpeedAttack <- Pokemon_Info[,9]
SpeedDefense <- Pokemon_Info[,10]
Speed <- Pokemon_Info[,11]
Generation <- Pokemon_Info[,12]
Legendary <- Pokemon_Info[,13]

#Summary on the quantitative data
summary(Total)
summary(HP)
summary(Attack)
summary(Defense)
summary(SpeedAttack)
summary(SpeedDefense)
summary(Speed)

#Graphic representations according to one categorical feature
barplot(table(Type1), main = "Pokemons by Type1")
barplot(table(Type2), main = "Pokemons by Type2")
barplot(table(Generation), main = "Pokemons by Generation")
barplot(table(Legendary), main = "Pokemons by Legendary")

#Graphic representations according to one quantitative feature
barplot(table(Total), main = "Pokemons by Total")
barplot(table(HP), main = "Pokemons by HP")
barplot(table(Attack), main = "Pokemons by Attack")
barplot(table(Defense), main = "Pokemons by Defense")
barplot(table(SpeedAttack), main = "Pokemons by Speed Attack")
barplot(table(SpeedDefense), main = "Pokemons by Speed Defense")
barplot(table(Speed), main = "Pokemons by Speed")

#Graphic representation according to one quantitative and one qualitative feature
boxplot(Attack ~ Type1, main = "Attack ~ Type1")
boxplot(Attack ~ Type2, main = "Attack ~ Type2")
boxplot(Speed ~ Generation, main = "Speed ~ Generation")
boxplot(Speed ~ Legendary, main = "Speed ~ Legendary")

#SpeedAttack and SpeedDefense bar:
stripchart(scale(SpeedAttack), cex = 0.3, main = "Pokemons Speed Attack")
stripchart(scale(SpeedDefense), cex = 0.3, main = "Pokemons Speed Defense")

#Empirical functions for SpeedAttack and SpeedDefense distribution:
par(mfrow = c(2,1))
plot(ecdf(SpeedAttack), verticals = FALSE, col = "RED", do.points = FALSE, lwd = 1, xlab = "km/h")
plot(ecdf(SpeedDefense), verticals = FALSE, col = "RED", do.points = FALSE, lwd = 1, xlab = "km/h")

#SpeedAttack and SpeedDefense histograms of Pokemon that have Type2:
type2pokemons = Pokemon_Info[(Type2 != ''),]
hist(scale(type2pokemons$SpAt),
       probability = TRUE,
       main = "Speed Attack of Pokemons with Second Type",
       xlab = "Speed Attack",
       ylab = "Number")
rug(jitter(scale(type2pokemons$SpAtk)))
lines(density(scale(type2pokemons$SpAtk), bw = 1), col = "red")

hist(scale(type2pokemons$SpDef),
       probability = TRUE,
       main = "Speed Defense of Pokemons with Second Type",
       xlab = "Speed Defense",
       ylab = "Number")
rug(jitter(scale(type2pokemons$SpDef)))
lines(density(scale(type2pokemons$SpDef), bw = 1), col = "red")


#SpeedAttack and SpeedDefense histograms:
hist(scale(SpeedAttack), probability = TRUE, right = FALSE, main = "Speed Attack Histogram", xlab = "", ylab = "Number")
rug(jitter(scale(SpeedAttack)))
lines(density(scale(SpeedAttack), bw = 1), col = "red")
hist(scale(SpeedDefense), probability = TRUE, right = FALSE, main = "Speed Defense Histogram", xlab = "", ylab = "Number")
rug(jitter(scale(SpeedDefense)))
lines(density(scale(SpeedDefense), bw = 1), col = "red")
cor(SpeedAttack, SpeedDefense)

#Correlation field and regression line through the resulting field
plot(SpeedAttack, SpeedDefense, col = "red", cex = 0.70)
abline(lm(SpeedDefense~SpeedAttack), col = "red")

result = lm(SpeedDefense~SpeedAttack)
result$coefficients[1]
result$coefficients[2]

#Correlations between different pairs of quantitative traits
cor(Attack, SpeedAttack)
cor(Defense, SpeedDefense)
cor(Defense, Attack)
cor(HP, Total)

#Dotplot for HP
dotchart(HP, cex = .8, main = "Dotplot for HP", xlab = "HP", ylab = "Pokemons")

#Normal Q-Q Plot and Shapiro-Wilk normality test
mean(SpeedAttack)
qqnorm(SpeedAttack)
qqline(SpeedAttack)
shapiro.test(SpeedAttack)

