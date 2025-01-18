#######################################Podstawy działania w R######################################

#######################################Zadanie 1
sin (2 * pi)

cos (3/4)

tan(pi)

log(100)

log(15, exp(1))

log(1/7,7)

exp(1)^3

64^1/3

######################################Zadanie 2
wektor = 1:10
sum(wektor)

######################################Zadanie 3
# Tworzenie wektora x
x = seq(2, 20, by = 2)

# a) Najmniejsza i największa składowa
min(x)
max(x)

# b) Indeksy najmniejszej i największej składowej
which.min(x)
which.max(x)

# c) Składowe większe od 12
x[x > 12]

# d) Indeksy składowych większych od 12
which(x > 12)

# e) Indeks składowej równej 8
which(x == 8)

# f) Odwrócenie kolejności elementów
rev(x)

# g) Liczba składowych
length(x)

# h) Wynik działania x*x i x^2
x * x
x^2

# i) Długość wektora
sqrt(sum(x^2))

######################################Zadanie 4
# Tworzenie wektora a1
a1 = c(3, 1, 6, 2, 7, 4, 4)

# a) Zwiększenie każdej składowej o 3
a1 + 3

# b) Tworzenie a2 bez składowych 4 i 5
a2 = a1[-c(4, 5)]

# c) Tworzenie wektora a3 jako sumy a1 i a2
a3 = a1 + a2[1:length(a1)] # Dostosowanie długości
print(a3)

######################################Zadanie 5
# Tworzenie wektora a4
a4 = c(0, 1, 2, 3, 5, 7, 8)

# a) Tworzenie bazy danych
X = data.frame(Lato = a4, Zima = a1)

# b) Tworzenie wektorów lato i zima
lato = X$Lato
zima = X$Zima

# c) Liczba butów zimowych i letnich
sum(lato)
sum(zima)

# d) Wykres punktowy
plot(lato, zima, main = "Liczba butów letnich i zimowych",
     xlab = "Lato", ylab = "Zima", col = "blue", pch = 16)

######################################Zadanie 6
# Tworzenie bazy danych Y
koszykowka = c(6, 4, 8, 5, 7)
pilka_nozna = c(5, 9, 6, 3, 7)
siatkowka = c(4, 6, 5, 8, 7)
Y = data.frame(Koszykowka = koszykowka, Pilka_Nozna = pilka_nozna, Siatkowka = siatkowka)

# a) Najchętniej uprawiany sport
najpopularniejszy_sport = names(Y)[which.max(colSums(Y))]

# b) Liczba klas z >5 osób w każdej dyscyplinie
sum(Y$Koszykowka > 5)
sum(Y$Pilka_Nozna > 5)
 sum(Y$Siatkowka > 5)

# c) Wykres punktowy
plot(Y$Pilka_Nozna, Y$Siatkowka, main = "Piłka nożna vs Siatkówka",
     xlab = "Piłka nożna", ylab = "Siatkówka", col = "red", pch = 19)

######################################Zadanie 7
# Tworzenie wektorów b1 i b2
b1 = c(4, 7, 9, 2, 1, 5)
b2 = c(7, 1, 1, 6, 3, 6)

# a) Tworzenie bazy danych D
D = data.frame(Dzien = b1, Noc = b2)

# b) Wektory dzien i noc
dzien = D$Dzien
noc = D$Noc

# c) Liczba pracowników, którzy nie wykonali limitu
sum(dzien < 5)
sum(noc < 5)

# d) Zmiana, która złożyła więcej długopisów
suma_dzien = sum(dzien)
suma_noc = sum(noc)
ifelse(suma_dzien > suma_noc, "Dzienna", "Nocna")

# e) Stanowisko z największą liczbą długopisów (zmiana dzienna)
which.max(dzien)

######################################Zadanie 8
# Wykres funkcji f(x) = x^2 + 3x - 5
curve(x^2 + 3 * x - 5, from = -3, to = 4, main = "Wykres funkcji f(x)",
      xlab = "x", ylab = "f(x)", col = "green", lwd = 2)

# Testowanie innej funkcji
curve(sin(x), from = -pi, to = pi, main = "Wykres funkcji sin(x)",
      xlab = "x", ylab = "sin(x)", col = "blue", lwd = 2)


