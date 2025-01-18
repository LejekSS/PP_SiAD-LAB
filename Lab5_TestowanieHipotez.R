#######################################Zadanie 1
prędkości_wiatru <- c(5.9, 4.4, 5.4, 3.8, 4.0, 4.2, 3.4, 3.6, 4.6, 6.5, 5.6, 4.8)
alfa = 0.05

#H0 <= 4
#H1 > 4

t.test(prędkości_wiatru, mu=4 , alternative="greater")

# t nie znajduje sie w przedziale ufności, p < alfa
# Odrzucamy hipotezę zerową, Darłowo nadaje sie do budowy elektorni

#######################################Zadanie 2

# H0 średnia >= 3,5
# H1 średnia < 3,5

COP <- c(3.5, 3.2, 3.6, 3.0, 3.3, 3.8, 2.5, 3.0, 3.7, 3.9)
alfa = 0.01

t.test(COP, mu=3.5, alternnative="less", conf.level=0.99)

#  p >= alfa
# Brak podstaw do odrzucenia hipotezy zerowej


#######################################Zadanie 3
morze = c(862,870,876,866,871)
odchylenie = 5
srednia = 870
alfa = 0.05

# H0 średnia = 870
# H1 średnia != 870

z.test(morze, sigma.x = odchylenie, alternative="two.sided", mu=srednia)
# p >= alfa
# Brak podstaw do odrzucenia hipotezy zerowej

#######################################Zadanie 4
blaszki <- c(0.048, 0.028, 0.037, 0.033, 0.054, 0.046, 0.041, 0.043, 0.044, 0.05,
             0.047, 0.052, 0.053, 0.048, 0.027, 0.056, 0.058, 0.039, 0.026, 0.034,
             0.043, 0.042, 0.047, 0.022, 0.046, 0.04, 0.036, 0.043, 0.041, 0.044,
             0.043, 0.044, 0.038, 0.046, 0.041, 0.038, 0.047, 0.03, 0.041, 0.049)

#H0 <= 0.04
#H1 > 0.04

t.test(blaszki, mu=0.4,alternative="greater", conf.level=0.98)
# p >= alfa
# Brak podstaw do odrzucenia hipotezy zerowej

#######################################Zadanie 5
mleko <- c(1.5, 1.8, 1.5, 1.7, 1.6, 1.6, 1.8, 1.6, 1.7, 1.6)

# (a)

#H0 srednia = 1,7
#H1 srednia != 1,7

t.test(mleko, mu=1.7, alternative="two.sided")
# p >= alfa
# Brak podstaw do odrzucenia hipotezy zerowej

# (b)
#H0 wariancja >= 0,02
#H1 wariancja < 0,02

sigma.test(mleko, alternative="less", sigmasq=0.02)
# p >= alfa
# Brak podstaw do odrzucenia hipotezy zerowej

#######################################Zadanie 6
jaja <- c(17.93, 18.52, 19.66, 14.30, 17.52, 20.76, 20.26,
                  19.82, 21.40, 16.54, 18.64, 17.62, 20.79, 19.14,
                  16.74, 14.93, 18.56, 15.43, 15.19, 21.05, 20.79)

srednia = 17
odchylenie = 2.5

# (a) 
#H0 srednia = 17
#H1 srednia != 17

alfa = 0.05
# (i)
z.test(jaja,odchylenie, alternative = "two.sided", srednia)
# p < alfa
# Odrzucamy hipotezę zerową

# (ii)
sigma.test(jaja, alternative="two.sided", odchylenie)
# p >= alfa
# Brak podstaw do odrzucenia hipotezy zerowej

# (b)
z.test(jaja, odchylenie , 0.95)

#######################################Zadanie 7
grupa = 2500
wybrani = 1600
alfa = 0.05

#H0 bioracych w glosowaniu = 60%
#H1 biorących w głosowaniu != 60%

binom.test(1600, 2500, 0.95)
# p < alfa
# Odrzucamy hipotezę zerową

#######################################Zadanie 8
#H0 p >= 16
#H1 p < 16

alfa = 0.05
p = 0.02
q = 0.98
n = 1200

binom.test(16,1200,0.02,alternative = "less")
# p >= alfa
# Brak podstaw do odrzucenia hipotezy zerowej

#######################################Zadanie 9
n = 1100
m = 1000

#H0 p <=90
#H1 p > 90

binom.test(1000,1100,0.05, alternative = "greater")
# p >= alfa
# Brak podstaw do odrzucenia hipotezy zerowej
