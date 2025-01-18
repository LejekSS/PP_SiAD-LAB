setwd("C:/Users/Bartek/Studia/Statystyka/lab6")
#zad1
#ogarniamy standardowo dane
Anova_cisnienie=read.csv2("Anova_cisnienie.csv")
obiekty=rep(names(Anova_cisnienie),
            c(length(na.omit(Anova_cisnienie$Niskie)),
              length(na.omit(Anova_cisnienie$Srednie)),
              length(na.omit(Anova_cisnienie$Silne)),
              length(na.omit(Anova_cisnienie$BardzoSilne))))

wyniki=c(na.omit(Anova_cisnienie$Niskie),
         na.omit(Anova_cisnienie$Srednie),
         na.omit(Anova_cisnienie$Silne),
         na.omit(Anova_cisnienie$BardzoSilne))
data.frame(obiekty,wyniki)
#ładujemy biblioteje jeśli nie mamy
library(nortest)
#testujemy założenie normalności (p-value musi być większe od alfa [czyli poziomu istotności])
lillie.test(wyniki)
#sprawdzamy jednorodność wariancji (p-value musi być większe od alfa [czyli poziomu istotności])
bartlett.test(wyniki~obiekty)
#h0  Średnie produkcji dla różnych poziomów ciśnienia są równe (μ1=μ2=μ3=μ4)
#h1  Średnie produkcji dla różnych poziomów nie różnią się
anova(lm(wyniki~obiekty))
#Jeżeli Pr(>F) <= poziomowi istotności to odrzucamy hipotezę zerową
#Czyli różnice ciśnienia się różnią (mają istotny wpływ na średnią produkcję)
#Jeżeli Pr(>F) >= poziomowi istotności to brak podstaw do odrzucenia hipotezy zerowej
#Czyli różnice w średniej produkcji nie są statystycznie istotne
#brak istonych różnic między porównywaniymi średnimi,
#warunki ciśnieniowe nie mają wpływu..
#Pełna odpowiedź: Na podstawie przeprowadzonej analizy wariancji nie możemy stwierdzić, że ciśnienie 
#ma istotny wpływ na wielkość produkcji na poziomie istotności alfa=0.05 
#zad2
Anova_kopalnie=read.csv2("Anova_kopalnie.csv")
obiekty=rep(names(Anova_kopalnie),
            c(length(na.omit(Anova_kopalnie$K1)),
              length(na.omit(Anova_kopalnie$K2)),
              length(na.omit(Anova_kopalnie$K3)),
              length(na.omit(Anova_kopalnie$K4)),
              length(na.omit(Anova_kopalnie$K5))))

wyniki=c(na.omit(Anova_kopalnie$K1),
         na.omit(Anova_kopalnie$K2),
         na.omit(Anova_kopalnie$K3),
         na.omit(Anova_kopalnie$K4),
         na.omit(Anova_kopalnie$K5))
#testujemy założenie normalności (p-value musi być większe od alfa [czyli poziomu istotności])
lillie.test(wyniki)
#sprawdzamy jednorodność wariancji (p-value musi być większe od alfa [czyli poziomu istotności])
bartlett.test(wyniki~obiekty)
#h0 Średnia zawartość popiołu w ekogroszku jest taka sama dla wszystkich pięciu kopalni (μ1=μ2=μ3=μ4=μ5).
#h1 Przynajmniej jedna średnia zawartość popiołu w ekogroszku różni się między kopalniami.
anova(lm(wyniki~obiekty))
#Jeżeli Pr(>F) >= poziomowi istotności to brak podstaw do odrzucenia hipotezy zerowej
#Jeżeli Pr(>F) <= poziomowi istotności to odrzucamy hipotezę zerową
#brak różnic w zawartości popiołu
#Odp: różnice w średnich zawartościach popiołu między pięcioma kopalniami nie są istotne statystycznie na poziomie istotności 0.01


#zad3
Anova_mikrometr=read.csv2("anova_mikrometr.csv")
obiekty=rep(names(Anova_mikrometr),
            c(length(na.omit(Anova_mikrometr$mikrometrI)),
              length(na.omit(Anova_mikrometr$mikrometrII)),
              length(na.omit(Anova_mikrometr$mikrometrIII))))

wyniki=c(na.omit(Anova_mikrometr$mikrometrI),
         na.omit(Anova_mikrometr$mikrometrII),
         na.omit(Anova_mikrometr$mikrometrIII))
# Hipotezy:
# h0: Wybór mikrometru nie ma wpływu na uzyskane wyniki pomiarów (μ1 = μ2 = μ3).
# h1: Przynajmniej jeden mikrometr daje istotnie różne wyniki pomiarów.

# Przeprowadzenie analizy wariancji (ANOVA)
anova(lm(wyniki ~ obiekty))
#brak podstaw do odrzucenia h0
# Interpretacja:
# Jeżeli Pr(>F) >= poziomowi istotności (0.05), brak podstaw do odrzucenia hipotezy zerowej (h0).
# Jeżeli Pr(>F) <= poziomowi istotności (0.05), odrzucamy hipotezę zerową.
# Wnioski:
# Na poziomie istotności 0.05, brak podstaw do odrzucenia hipotezy zerowej. 
# Wybór mikrometru nie ma wpływu na uzyskane wyniki pomiarów.

#zad4
Anova_sportowcy=read.csv2("anova_sportowcy.csv")
obiekty=rep(names(Anova_sportowcy),
            c(length(na.omit(Anova_sportowcy$Niepalacy)),
              length(na.omit(Anova_sportowcy$Lekkopalacy)),
              length(na.omit(Anova_sportowcy$Sredniopalacy)),
              length(na.omit(Anova_sportowcy$Duzopalacy))))

wyniki=c(na.omit(Anova_sportowcy$Niepalacy),
         na.omit(Anova_sportowcy$Lekkopalacy),
         na.omit(Anova_sportowcy$Sredniopalacy),
         na.omit(Anova_sportowcy$Duzopalacy))
# Testowanie założenia normalności (p-value musi być większe od alfa [0.01])
lillie.test(wyniki)

# Sprawdzanie jednorodności wariancji (p-value musi być większe od alfa [0.01])
bartlett.test(wyniki ~ obiekty)

# Hipotezy:
# h0: Palenie nie ma wpływu na rytm serca sportowców (μ1 = μ2 = μ3 = μ4).
# h1: Przynajmniej jedna grupa różni się od pozostałych w rytmie serca sportowców.

# Przeprowadzenie analizy wariancji (ANOVA)
anova(lm(wyniki~obiekty))
#wartości rytmu serca różnią się pomiędzy grupami na poziomie istotności  0,01
# Interpretacja:
# Jeśli Pr(>F) >= poziomowi istotności (0.01), brak podstaw do odrzucenia hipotezy zerowej.
# Jeśli Pr(>F) <= poziomowi istotności (0.01), odrzucamy hipotezę zerową.
# Na poziomie istotności 0.01, wyniki rytmu serca różnią się między grupami.

# b. Test Tukey'a - porównanie par średnich grup
tukey_result = TukeyHSD(aov(wyniki ~ obiekty))
# Wyświetlenie wyników testu Tukey'a
print(tukey_result)
# Wykres dla wyników testu Tukey'a
plot(tukey_result)
#Wyniki testu Tukey'a wskazują, że istnieją istotne różnice w rytmach serca między grupami 
#"Niepalący" a "Lekkopalący" (p = 0.006) oraz między grupami "Sredniopalacy" a "Lekkopalacy" 
#(p = 0.009), natomiast różnice między innymi parami grup nie są istotne statystycznie (p > 0.05).
#zad5
Anova_chomiki=read.csv2("anova_chomiki.csv")
obiekty=rep(names(Anova_chomiki),
            c(length(na.omit(Anova_chomiki$I)),
              length(na.omit(Anova_chomiki$II)),
              length(na.omit(Anova_chomiki$III)),
              length(na.omit(Anova_chomiki$IV))))

wyniki=c(na.omit(Anova_chomiki$I),
         na.omit(Anova_chomiki$II),
         na.omit(Anova_chomiki$III),
         na.omit(Anova_chomiki$IV))
data.frame(obiekty,wyniki)
# Sprawdzanie jednorodności wariancji (p-value musi być większe od alfa [0.05])
bartlett.test(wyniki ~ obiekty)
# Hipotezy:
# h0: Masa tarczycy nie zależy od poziomu inbredu (μ1 = μ2 = μ3 = μ4).
# h1: Przynajmniej jedna grupa różni się w masie tarczycy.
anova(lm(wyniki~obiekty))
# Interpretacja:
# Jeśli Pr(>F) >= poziomowi istotności (0.05), brak podstaw do odrzucenia hipotezy zerowej.
# Jeśli Pr(>F) <= poziomowi istotności (0.05), odrzucamy hipotezę zerową.
# Na poziomie istotności 0.05, masa tarczycy zależy od poziomu inbredu, ponieważ Pr(>F) = 0.024 < 0.05.
#średnie różnią się w sposób istotny
# b. Test Tukey'a - porównanie par średnich grup
tukey_result = TukeyHSD(aov(wyniki ~ obiekty))

# Wyświetlenie wyników testu Tukey'a
print(tukey_result)

# Wykres dla wyników testu Tukey'a
plot(tukey_result)
#istotna różnica pomiędzy IV i I
#Wyniki testu Tukey'a wskazują, że istnieje istotna różnica w masie tarczycy pomiędzy grupami
#I (osobniki niezinbredowane) i IV (poziom inbredu > 0.21), podczas gdy różnice pomiędzy pozostałymi grupami
#nie są istotne statystycznie.
