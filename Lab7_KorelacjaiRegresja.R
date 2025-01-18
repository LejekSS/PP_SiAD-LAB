#######################################Zadanie 1
# Dane
X <- c(14, 23, 9, 17, 10, 22, 5, 12, 6, 16)  # Ilość zużytego surowca (litry)
Y <- c(68, 105, 40, 79, 81, 95, 31, 72, 45, 93)   # Wielkość produkcji (kg)

# (a) Wykres punktowy (scatter plot)
plot(X, Y)

# (b) Kowariancja próbkowa
cov(X, Y)
#Dodatnia wartość kowariancji oznacza, że przy wzroście wartości X wartości Y na ogół także rosną.
#Ujemna wartość kowariancji będzie wskazywała, że przy wzroście X wartości Y na ogół maleją.

#Kowariancja dodatnia, przy wzroscie X wartosci Y rosną

# (c) Współczynnik korelacji
cor(X, Y)
#Korelacja może przyjąć wartości od -1 do +1. 
#Odchylenie skrajnie ujemne oznacza, że im wyższa jest wartość jednej zmiennej, tym niższa dla drugiej
#z kolei skrajnie dodatni wynik oznacza, że obie wartości będą rosły lub malały synchronicznie. 
#0 wskazuje na całkowity brak związku pomiędzy zmiennymi.

#Silna korelacja, obie wartosci rosną synchronicznie

# (d) Prosta regresji: y = a + b * x
model <- lm(Y ~ X)
# y = 22,405 + 3,619 * x
#jeśli współczynnik kierunkowy jest dodatni, to wraz ze wzrostem wartości x (oś pozioma) rosną wartości y (oś pionowa).
#Jeśli współczynnik kierunkowy jest ujemny, to wraz ze wzrostem wartości x maleją wartości y.

#Dodani współczynnik kierunowy, wartości x rosną z wartosciami Y

# (e) Dodanie prostej regresji do wykresu punktowego
abline(model, col = "red")

# (f) Zmiana produkcji o 1 litr surowca
slope <- coef(model)[2]  # Współczynnik kierunkowy prostej regresji

# (g) Wielkość produkcji przy zużyciu 20 litrów surowca
pred_20 <- predict(model, newdata = data.frame(X = 20))

# (h) Wielkość produkcji przy zużyciu 15 litrów surowca
pred_15 <- predict(model, newdata = data.frame(X = 15))

# (i) Dopasowanie regresji do danych
summary(model)
#Multiple R-squared:  0.8016,
#Dopasowanie = 80%

# (j) Test istotoności regresji
summary(model)

alfa = 0.05
# H0 Regresja nie jest istotna 
# H1 Regresja jest istotna
# p-value: 0.0004617

# p < alfa - odrzucamy hipoteze zerową

#######################################Zadanie 2
# Dane
efektywnoscX <- c(18,20,18,17,15,15,14,12,10)
zywotnoscY <- c(2,3,3,4,5,6,7,11,9)

# a) Wykres punktowy
plot(efektywnoscX, zywotnoscY)

# b) Kowariancja
cov(efektywnoscX, zywotnoscY)
#Kowariancja ujemna, przy wzroscie X wartosci Y maleją

# c) Współczynnik korelacji
cor(efektywnoscX, zywotnoscY)
#silna korelacja ujemna, obie wartosci maleją synchronicznie

# d) Regresja liniowa
lm(zywotnoscY ~ efektywnoscX)
#y = 18.8823 - 0.8629 * x

#Dodani współczynnik kierunowy, wartości x rosną z wartosciami Y

# e) Zmiana żywotności przy wzroście efektywności o 1 element
coef(model)[2] * 1

# f) Przewidywanie żywotności przy efektywności 11
predict(model, newdata = data.frame(efektywnosc = 11))

# g) Przewidywanie żywotności przy efektywności 19
predict(model, newdata = data.frame(efektywnosc = 19))

# h) Ocena dopasowania modelu
summary(model)
#Multiple R-squared:  0.827,
#Dopasowanie = 83%

# i) Test istotności regresji (ANOVA)
summary(model)

alfa = 0.01
# H0 Regresja nie jest istotna 
# H1 Regresja jest istotna
# p-value: 0.0006735
# F-statistic: 33.47

# p < alfa - odrzucamy hipoteze zerową

#######################################Zadanie 3
data = read.csv("Reg_arszenik.csv", sep = ";", header = TRUE)

# (a) Diagram
plot(data$pH, data$arszenik)

# (b) Kowariancja próbkowa i wspóczynnik korelacji
cov(data$pH, data$arszenik)
#Kowariancja ujemna, przy wzroscie X wartosci Y maleją
cor(data$pH, data$arszenik)
#Korelacja skrajnie ujemna, obie wartości maleją synchronicznie

# (c) Prosta regresji: y = a + b * x
model = lm(data$arszenik ~ data$pH); model
#y = 190.27 - 18.03 * x

# (d)
coef(model)[2]  # Współczynnik kierunkowy prostej regresji

# (e)
pred_0 = predict(model, newdata = data.frame(X = 7.5))

# (f)
pred_1 = predict(model, newdata = data.frame(X = 7.5))

# (g) Ocena dopasowania modelu
summary(model)
#Multiple R-squared:  0.9034,
#Dopasowanie = 90%

# (h) 

# i) Test istotności regresji (ANOVA)
summary(model)

alfa = 0.01
# H0 Regresja nie jest istotna 
# H1 Regresja jest istotna
# p-value: 1.552e-09
# F-statistic: 149.7

# p < alfa - odrzucamy hipoteze zerową