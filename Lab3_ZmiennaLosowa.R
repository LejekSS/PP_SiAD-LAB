#######################################Zmienna losowa######################################

#######################################Zadanie 1
p = 0.3
n = 5
S = 0:n
prob = dbinom(S, size = n, prob = p)
print(data.frame(S, prob))
barplot(prob, names.arg = S, main = "Rozkład zmiennej losowej S", col = "lightblue")

#######################################Zadanie 2
p_bulb = 0.9
n_bulbs = 8
B = 0:n_bulbs

# (a)
P_B_8 = dbinom(8, size = n_bulbs, prob = p_bulb)
print(P_B_8)

# (b)
P_B_7 = dbinom(7, size = n_bulbs, prob = p_bulb)
print(P_B_7)

# (c)
P_B_gt_5 = sum(dbinom(6:8, size = n_bulbs, prob = p_bulb))
print(P_B_gt_5)

# (d)
E_B = n_bulbs * p_bulb
print(E_B)

# (e)
SD_B = sqrt(n_bulbs * p_bulb * (1 - p_bulb))
print(SD_B)

#######################################Zadanie 3
lambda = 0.01
curve(dexp(x, rate = lambda), from = 0, to = 500, col = "blue", main = "Funkcja gęstości rozkładu wykładniczego")

# (a)
P_200_days = pexp(200, rate = lambda, lower.tail = FALSE)
print(P_200_days)

# (b)
P_100_days = pexp(100, rate = lambda)
print(P_100_days)

#######################################Zadanie 4
mean_eq = 2.4
lambda_eq = 1 / mean_eq

# (a)
P_gt_3 = pexp(3, rate = lambda_eq, lower.tail = FALSE)
print(P_gt_3)

# (b)
P_between_2_and_3 = pexp(3, rate = lambda_eq) - pexp(2, rate = lambda_eq)
print(P_between_2_and_3)

#######################################Zadanie 5
mean_resistance = 0.13
sd_resistance = 0.005
curve(dnorm(x, mean = mean_resistance, sd = sd_resistance), from = 0.12, to = 0.14, col = "green", main = "Funkcja gęstości rezystancji")
P_resistance = pnorm(0.14, mean = mean_resistance, sd = sd_resistance) - pnorm(0.12, mean = mean_resistance, sd = sd_resistance)
print(P_resistance)

#######################################Zadanie 6
mean_drying = 2
sd_drying = 15 / 60
curve(dnorm(x, mean = mean_drying, sd = sd_drying), from = 1.8, to = 2.3, col = "red", main = "Funkcja gęstości czasu schnięcia farby")
P_drying = pnorm(2.25, mean = mean_drying, sd = sd_drying) - pnorm(1.85, mean = mean_drying, sd = sd_drying)
print(P_drying)

#######################################Zadanie 7
p_scholarship = 0.25
n_students = 100

# Dokładne
P_X_le_15 = pbinom(15, size = n_students, prob = p_scholarship)
print(P_X_le_15)

# Przybliżone (rozkład normalny)
mean_X = n_students * p_scholarship
sd_X = sqrt(n_students * p_scholarship * (1 - p_scholarship))
P_X_le_15_norm = pnorm(15.5, mean = mean_X, sd = sd_X) # Kontynuacja
print(P_X_le_15_norm)

#######################################Zadanie 8
mean_resistors = 200
sd_resistors = 10
n_resistors = 25

# (a)
mean_sample = mean_resistors
sd_sample = sd_resistors / sqrt(n_resistors)
P_mean_between = pnorm(202, mean = mean_sample, sd = sd_sample) - pnorm(199, mean = mean_sample, sd = sd_sample)
print(P_mean_between)

# (b)
P_total_resistance = pnorm(5100, mean = n_resistors * mean_resistors, sd = sqrt(n_resistors) * sd_resistors)
print(P_total_resistance)

#######################################Zadanie 9
mean_cholesterol = 202
sd_cholesterol = 14
n_employees = 64

mean_sample_chol = mean_cholesterol
sd_sample_chol = sd_cholesterol / sqrt(n_employees)
P_cholesterol = pnorm(206, mean = mean_sample_chol, sd = sd_sample_chol) - pnorm(198, mean = mean_sample_chol, sd = sd_sample_chol)
print(P_cholesterol)

#######################################Zadanie 10
mean_thread = 0.5
sd_thread = 0.2
n_threads = 100

mean_rope = n_threads * mean_thread
sd_rope = sqrt(n_threads) * sd_thread
P_rope = pnorm(47, mean = mean_rope, sd = sd_rope)
print(P_rope)
