#######################################Estymacja######################################


# Zadanie 1
# Dane
wagi <- c(0.46, 0.61, 0.52, 0.48, 0.57, 0.54, 0.47, 0.63, 0.51, 0.49, 0.58, 0.55)
n <- length(wagi) # Rozmiar próby


# Oceny punktowe
mean(wagi)            # Średnia próby
var(wagi)           # Wariancja próby
sd(wagi)             # Odchylenie standardowe próby


# c) Przedział ufności dla średniej (95%)
alfa <- 0.05
t_value <- qt(1 - alfa / 2, df = n - 1) # Kwantyl t-Studenta
standard_error <- odch_std / sqrt(n)   # Błąd standardowy średniej

# Granice przedziału
srednia - t_value * standard_error
srednia + t_value * standard_error

#dodać uzasadnienie

# d) Przedział ufności dla średniej (99%)
alfa_99 <- 0.01
t_value_99 <- qt(1 - alfa_99 / 2, df = n - 1)

# Granice dla 99% ufności
srednia - t_value_99 * standard_error
srednia + t_value_99 * standard_error

#Wraz z zwiększaniem ufności przedział się poszerza

# e) Przedział ufności dla wariancji i odchylenia standardowego
alfa <- 0.05
chi2_dolny <- qchisq(1 - alfa / 2, df = n - 1) # Kwantyl chi-kwadrat (górny)
chi2_gorny <- qchisq(alfa / 2, df = n - 1)     # Kwantyl chi-kwadrat (dolny)

# Granice przedziału ufności dla wariancji
dolna_granica_wariancja <- (n - 1) * wariancja / chi2_dolny
gorna_granica_wariancja <- (n - 1) * wariancja / chi2_gorny

# Granice przedziału ufności dla odchylenia standardowego
dolna_granica_odch_std <- sqrt(dolna_granica_wariancja)
gorna_granica_odch_std <- sqrt(gorna_granica_wariancja)

#######################################Zadanie 2
# Dane dla nikotyny
nicotine = c(1.87, 2.28, 1.77, 2.13, 1.43, 1.64, 2.38, 1.39, 1.94, 2.68, 1.95, 0.86, 1.98, 1.69, 1.15)
mean_nicotine = mean(nicotine)
sigma = 0.7
n_nicotine = length(nicotine)
z_value = qnorm(1 - 0.05 / 2)

# 90% przedział ufności dla średniej
c(mean_nicotine - z_value * sigma / sqrt(n_nicotine), mean_nicotine + z_value * sigma / sqrt(n_nicotine))

#######################################Zadanie 3
# Dane
zawartosc_bialka = c(4.28, 3.3, 4.22, 2.77, 2.75, 2.93, 3.86, 3.05, 4.12, 2.88, 3.94, 4.99, 2.08, 4.35, 2.7, 4.09, 2.81, 2.82)

# (a) Estymatory punktowe średniej i wariancji populacji
mean(zawartosc_bialka)
var(zawartosc_bialka)

# (b) 90% przedział ufności dla średniej
alfa <- 0.10
n = length(zawartosc_bialka)
t_value = qt(1 - alfa / 2, df = n - 1) # Kwantyl t-Studenta
standard_error = sd(zawartosc_bialka) / sqrt(n) # Błąd standardowy

#Dolna i górna granica średniej zawortości białka
srednia - t_value * standard_error
srednia + t_value * standard_error

# (c) 90% przedział ufności dla wariancji
chi2_dolny <- qchisq(1 - alfa / 2, df = n - 1) # Kwantyl chi-kwadrat (górny)
chi2_gorny <- qchisq(alfa / 2, df = n - 1)     # Kwantyl chi-kwadrat (dolny)

#Dolna i górna granica wariancji zawartości białka
(n - 1) * wariancja / chi2_dolny
(n - 1) * wariancja / chi2_gorny

#######################################Zadanie 4
# Dane
srednia_zuzycie <- 102       
wariancja_zuzycie <- 81      
n <- 365                     
alfa <- 0.02   

# (a) Przedział ufności dla średniego dziennego zużycia wody
standard_error <- sqrt(wariancja_zuzycie) / sqrt(n)
z_value <- qnorm(1 - alfa / 2) 

srednia_zuzycie - z_value * standard_error
srednia_zuzycie + z_value * standard_error

# (b) Czy średnie dzienne zużycie wody wyniesie co najmniej 122 hl?
prog_zuzycia <- 122
pnorm(prog_zuzycia, mean = srednia_zuzycie, sd = sqrt(wariancja_zuzycie) / sqrt(n), lower.tail = FALSE)
#Prawdopodobnienstwo = 0, czyli sytuacja nie jest prawdopodobna 

#######################################Zadanie 5
# Dane dla filiżanek kawy
cups = c(0, 1, 2, 3, 4)
counts = c(14, 28, 36, 28, 14)
n_total = sum(counts)
prob_up_to_2 = sum(counts[1:3]) / n_total
prob_up_to_3 = sum(counts[1:4]) / n_total

#  (a) Przedział ufności dla frakcji (95%)
z_value_cups = qnorm(1 - alpha / 2)
#dla 2 filizanek
c(prob_up_to_2 - z_value_cups * sqrt(prob_up_to_2 * (1 - prob_up_to_2) / n_total),
                     prob_up_to_2 + z_value_cups * sqrt(prob_up_to_2 * (1 - prob_up_to_2) / n_total))
#dla 3 filizanek
c(prob_up_to_3 - z_value_cups * sqrt(prob_up_to_3 * (1 - prob_up_to_3) / n_total),
                     prob_up_to_3 + z_value_cups * sqrt(prob_up_to_3 * (1 - prob_up_to_3) / n_total))

# (b) Średnia liczba filiżanek kawy
mean_cups = sum(cups * counts) / n_total
var_cups = sum((cups - mean_cups)^2 * counts) / (n_total - 1)
sd_cups = sqrt(var_cups)
c(mean_cups - z_value_cups * sd_cups / sqrt(n_total),
                  mean_cups + z_value_cups * sd_cups / sqrt(n_total))

#######################################Zadanie 6
# Dane o opóźnieniach pociągów
n_trains = 1000
n_late = 160
prob_late = n_late / n_trains

#Przedział prawdopodobieństwa opóźnienia z ufnościa 90%
c(prob_late - z_value * sqrt(prob_late * (1 - prob_late) / n_trains),
             prob_late + z_value * sqrt(prob_late * (1 - prob_late) / n_trains))

#######################################Zadanie 7
# Dane
n <- 100      
k <- 4        
p_hat <- k / n    
alfa <- 0.05

# Błąd standardowy
standard_error <- sqrt(p_hat * (1 - p_hat) / n)

# Kwantyl rozkładu normalnego
z_value <- qnorm(1 - alfa / 2)

# Granice przedziału ufności
p_hat - z_value * standard_error
p_hat + z_value * standard_error


#######################################Zadanie 8
# Dane dla magistrów zarządzania
# Dane
n <- 1000  
k <- 122       
p_hat <- k / n    
alfa <- 0.10    

# Błąd standardowy
standard_error <- sqrt(p_hat * (1 - p_hat) / n)

# Kwantyl rozkładu normalnego
z_value <- qnorm(1 - alfa / 2)

# Granice przedziału ufności
p_hat - z_value * standard_error
p_hat + z_value * standard_error


