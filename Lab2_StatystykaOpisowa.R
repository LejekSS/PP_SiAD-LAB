#######################################Statystyka opisowa######################################

#######################################Zadanie 1
loty = read.csv("loty.csv", sep=";",head=TRUE)

# a) Wartości w pliku i typ danych
loty
class(loty)

# b) histogram liczebności lotów
k=sqrt(6*12);
nazwy=names(loty)

przedzialy=seq(220,670,length=10)

kolory = c("blue", "red", "green", "yellow","orange","grey")
par(mfrow=c(2,3))
for (j in 1:ncol(loty)){
  hist(loty[,j],
       main=paste("rok", nazwy[j]),
       xlab = "liczba pasazerow",
       breaks = przedzialy,
       col = kolory[j])
}

# c) Miary statystyczne
summary(loty)

# d) Wykresy pudełkowe
par(mfrow=c(1,1))
boxplot(loty, col=rainbow(6))


#######################################Zadanie 2
oceny = read.csv("oceny.csv", sep=";",head=TRUE)

# a) Wartości w pliku i ich długości
oceny = read.csv("oceny.csv", sep=";",head=TRUE)

# b) Zamiana przecinków na kropki
oceny = read.csv("oceny.csv", sep=";",dec=",",head=TRUE)
class(oceny)
# c) Szeregi rozdzielcze w poszczególnych grupach
table(oceny$grupa.M1)
# d) Diagramy odcinkowe
grupy = names(oceny)
w=c("red","green","blue", "yellow")
library(arm)
par(mfrow=c(2,2))

for(j in 1:ncol(oceny)){
  title=paste0("Histogram liczebności gruopy M", grupy[j])
  discrete.histogram(oceny[,j],freq = TRUE, main=title,xlab="oceny",prob.col = w[j])
}

# e) Dane zz c) na wykresach kołowych
for (j in 1:ncol(oceny)){
  title=paste0("liczebnosci grupy M", grupy[j])
  pie(table(oceny[,j]).main=title,prob.col=w[j])
}
# f) Podstawowe miary statystyczne
summary_stats = function(x) {
  x <- na.omit(x)
  list(
    Średnia = mean(x),
    Mediana = median(x),
    Odchylenie_standardowe = sd(x),
    Minimum = min(x),
    Maksimum = max(x)
  )
}
# g) Wykresy pudełkowe
par(mfrow=c(1,1))
boxplot(oceny)
