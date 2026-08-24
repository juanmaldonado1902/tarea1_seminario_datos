#  Solucionario -- Tarea "Sueño y desempeño académico"
#  Realizada por : Juan Pablo Maldonado Rojas 

d <- read.csv("sueno_gpa.csv")

#1a) Histograma avg_sleep_hours
x <- d$avg_sleep_hours

hist(x, breaks = 30, col = "lightsteelblue",
     main = "Horas promedio de sueño (n = 634)",
     xlab = "avg_sleep_hours", ylab = "Frecuencia")
abline(v = mean(x), lty = 2, lwd = 2)

c(media = mean(x), sd = sd(x), varianza = var(x))
quantile(x, c(.10, .25, .50, .75, .90))

# Posibles valores extremos: Ocupo el intercuartilico para detectar posibles extremos
ric    <- IQR(x)
cercas <- quantile(x, c(.25, .75)) + c(-1.5, 1.5) * ric
sort(x[x < cercas[1] | x > cercas[2]])   # atipicos
sum(x < cercas[1] | x > cercas[2])       # total

#1b) Tabla descriptiva de las seis variables 
vars <- c("avg_sleep_hours", "term_gpa", "prior_gpa",
          "gpa_change", "bedtime_variability", "daytime_sleep_minutes")

tabla_1b <- t(sapply(d[vars], function(v)
  c(n = length(v), media = mean(v), sd = sd(v),
    mediana = median(v), min = min(v), max = max(v))))
round(tabla_1b, 3)

# 2c) Tabla por grupo y recuperación de la media global (Ley de las medias iteradas)
d$grupo <- paste(d$university, d$semester, sep = " | ")
tabla_2c <- aggregate(avg_sleep_hours ~ grupo, data = d,
                      FUN = function(v) c(n = length(v), media = mean(v)))
tabla_2c <- do.call(data.frame, tabla_2c)
names(tabla_2c) <- c("grupo", "n", "media")
tabla_2c

# media global SOLO con la tabla: promedio ponderado por n_g/N
media_ponderada <- sum(tabla_2c$n / sum(tabla_2c$n) * tabla_2c$media)
c(ponderada = media_ponderada, muestral = mean(x),
  diferencia = media_ponderada - mean(x))

# 3a) Media conforme se acumulan observaciones (orden aleatorio con semilla)
set.seed(2026)                      # semilla 
xs <- sample(x)                     # reordenamiento aleatorio 
n_tot   <- length(xs)
med_ac  <- cumsum(xs) / seq_len(n_tot)
resalta <- c(1, 2, 5, 10, 25, 50, 100, 150, 300, 500, 634)
plot(med_ac, type = "l", xlab = "n", ylab = "media con n observaciones",
     main = "Trayectoria de la media")
abline(h = mean(x), lty = 2, col = "firebrick")
points(resalta, med_ac[resalta], pch = 16)
round(med_ac[resalta], 4)

# 3b) Varianza muestral con las primeras n observaciones
var_ac <- sapply(2:n_tot, function(k) var(xs[1:k]))
plot(2:n_tot, var_ac, type = "l", xlab = "n", ylab = "varianza con n observaciones",
     main = "Trayectoria de la varianza muestral")
abline(h = var(x), lty = 2, col = "firebrick")
round(var_ac[resalta[-1] - 1], 4)

# 3c) Estimador de la varianza de la media: S2_n / n
varmed_ac <- var_ac / (2:n_tot)
plot(2:n_tot, varmed_ac, type = "l", xlab = "n", ylab = "S2_n / n",
     main = "Trayectoria de la varianza de la media")
abline(h = 0, lty = 2, col = "firebrick")
round(varmed_ac[resalta[-1] - 1], 5)

# 4a) 4,000 submuestras con reemplazo de tamaños 50, 100 y 500
set.seed(2027)                      # semilla 
tams   <- c(50, 100, 500)
medias <- sapply(tams, function(k) replicate(4000, mean(sample(x, k, replace = TRUE))))
colnames(medias) <- paste0("n", tams)
lim <- range(medias)                
par(mfrow = c(1, 3))
for (j in 1:3) hist(medias[, j], breaks = 40, xlim = lim, col = "lightsteelblue",
                    main = paste0("Medias, n = ", tams[j]), xlab = "media muestral")
par(mfrow = c(1, 1))
rbind(sd_simulada = apply(medias, 2, sd),
      sd_analitica = sd(x) / sqrt(tams))

# 4c) Tres versiones del IC al 92% para las submuestras de tamaño 100
z92  <- qnorm(0.96)                 # 92% deja 4% en cada cola
m100 <- medias[, "n100"]
ic1 <- mean(x) + c(-1, 1) * z92 * sd(x) / sqrt(100)   # varianza analítica
ic2 <- mean(x) + c(-1, 1) * z92 * sd(m100)            # varianza bootstrap + normalidad
ic3 <- quantile(m100, c(0.04, 0.96))                  # distribución empírica
round(rbind(analitico = ic1, boot_normal = ic2, percentiles = ic3), 4)

# 4d) Prueba H0: mu = 6.7 vs H1: mu != 6.7 con la varianza bootstrap
se_boot <- apply(medias, 2, sd)
z_est   <- (mean(x) - 6.7) / se_boot
valor_p <- 2 * pnorm(-abs(z_est))
round(rbind(se_boot, z = z_est, valor_p), 4)

# 5a) Variable binaria de dormir menos de 8 horas
d$menos8 <- as.integer(d$avg_sleep_hours < 8)
p_hat <- mean(d$menos8)
c(promedio = p_hat, porcentaje = 100 * p_hat, conteo = sum(d$menos8))

# 5b) Varianza muestral de la dummy contra p(1-p)
c(var_muestral = var(d$menos8),
  p_por_1menosp = p_hat * (1 - p_hat),
  razon = var(d$menos8) / (p_hat * (1 - p_hat)))

# 6a) Diagrama de dispersión
plot(d$avg_sleep_hours, d$term_gpa, pch = 16, cex = 0.5, col = "gray30",
     xlab = "avg_sleep_hours", ylab = "term_gpa",
     main = "Sueno promedio y GPA del semestre")

# 6b) Covarianza  (unidades: horas x puntos de GPA)
cov(d$avg_sleep_hours, d$term_gpa)

# 6c) Coeficiente de correlación  (sin unidades)
cor(d$avg_sleep_hours, d$term_gpa)

# 6d) Bootstrap de la correlación: remuestrear estudiantes completos (filas)
set.seed(2026)                      # semilla documentada
tams <- c(50, 100, 500)
correl <- sapply(tams, function(k)
  replicate(4000, { i <- sample(nrow(d), k, replace = TRUE)
  cor(d$avg_sleep_hours[i], d$term_gpa[i]) }))
colnames(correl) <- paste0("n", tams)
lim <- range(correl)
par(mfrow = c(1, 3))
for (j in 1:3) hist(correl[, j], breaks = 40, xlim = lim,
                    col = "lightsteelblue",
                    main = paste0("Correlaciones, n = ", tams[j]),
                    xlab = "correlacion")
par(mfrow = c(1, 1))

# 6e) IC del 87% para la correlación, sin suponer normalidad (percentiles)
#     el 87% deja 6.5% en cada cola
apply(correl, 2, quantile, probs = c(0.065, 0.935))

# 6f) Puntos extra
par(mfrow = c(1, 3))
for (j in 1:3) { qqnorm(correl[, j], main = paste0("QQ-plot, n = ", tams[j]),
                        pch = 16, cex = 0.4); qqline(correl[, j]) }
par(mfrow = c(1, 1))
apply(correl, 2, function(v) shapiro.test(v)$p.value)