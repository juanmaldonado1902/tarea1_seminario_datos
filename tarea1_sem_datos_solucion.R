# Seminario de Analisis de Datos
# Soluciones -- Tarea 1
# Realizada por : Juan Pablo Maldonado Rojas 
# Revisada y editada por: Arturo Aguilar

# Librerias
paquetes <- c("dplyr", "ggplot2", "tidyr", "purrr", "stargazer", "tseries", "moments")
faltantes <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltantes) > 0) install.packages(faltantes, repos = "https://cloud.r-project.org")
invisible(lapply(paquetes, library, character.only = TRUE))

# Formato predeterminado para graficas
theme_set(theme_minimal(base_size = 12))
theme_update(
  plot.title       = element_text(face = "bold", size = 13),
  plot.title.position = "plot",
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(linewidth = 0.3, color = "grey85"),
  axis.title       = element_text(color = "grey30")
)
update_geom_defaults("bar",  list(fill = "lightsteelblue"))
update_geom_defaults("line", list(linewidth = 0.6))
update_geom_defaults("point", list(color = "grey30"))

# Cargar base de datos
datos <- read.csv("sueno_gpa.csv")

# ====/// 1a) Histograma avg_sleep_hours \\\=====
ggplot(datos, aes(x = avg_sleep_hours)) + 
  geom_histogram(bins = 30) +
  geom_vline(aes(xintercept = mean(avg_sleep_hours)), 
             linetype = 2, linewidth = 0.8) +
  labs(title = "Horas promedio de sueño", 
       x = "avg_sleep_hours", 
       y = "Frecuencia")

# Creando el archivo con la grafica
ggsave("Grafica_1a.png",  width = 5.54, height = 4.95)

# Percentiles
datos %>% summarise(media = mean(avg_sleep_hours), 
                    sd = sd(avg_sleep_hours)) 
quantile(datos$avg_sleep_hours, c(.10, .25, .50, .75, .90))

# Datos atípicos
ric    <- IQR(datos$avg_sleep_hours)
extremos <- quantile(datos$avg_sleep_hours, c(.25, .75)) + c(-1.5, 1.5) * ric
atipicos <- datos %>% select(avg_sleep_hours) %>%
  filter(avg_sleep_hours < extremos[1] | avg_sleep_hours > extremos[2]) %>%
  arrange(avg_sleep_hours)

# ====/// 1b) Tabla descriptiva de las seis variables \\\=====
vars <- c("avg_sleep_hours", "term_gpa", "prior_gpa",
          "gpa_change", "bedtime_variability", "daytime_sleep_minutes")
datos %>% select(all_of(vars)) %>%
  as.data.frame() %>%
  stargazer(summary = TRUE, median = TRUE, digits = 3, type = "latex",
            title = "Estadistica descriptiva",  
            label = "tab:estadistica_1b", header = FALSE, font.size = "small",  
            table.placement = "H", out = "tabla_1b.tex")

# ====/// 2c) Tabla por grupo y semestre \\\=====

tabla_2c <- datos %>% group_by(university, semester) %>%
  summarise(n = n(), media = mean(avg_sleep_hours), .groups = "drop") %>%
  arrange(university, semester)

# Creando la tabla en LaTex
stargazer(tabla_2c, summary = FALSE, rownames = FALSE, 
          type = "latex", title = "Medias por universidad y semestre",  
          label = "tab:grupos_2c", header = FALSE, font.size = "small",  
          table.placement = "H", out = "tabla_2c.tex")

# Comprobacion que la media es igual
(w_mean <- weighted.mean(tabla_2c$media, tabla_2c$n))
(simple_mean <- mean(datos$avg_sleep_hours))


# ====/// 3a) Media conforme se acumulan observaciones \\\=====

set.seed(2026)                      # semilla 

n_tot   <- nrow(datos)
resalta <- c(1, 2, 5, 10, 25, 50, 100, 150, 300, 500, n_tot)


datos_ord <- datos %>% slice_sample(prop = 1) %>%  
  mutate(n = row_number(),  
         media_ac = cummean(avg_sleep_hours)) %>%
  select(n, media_ac,avg_sleep_hours)

# Subconjunto para los puntos resaltados
puntos <- datos_ord %>% filter(n %in% resalta)

# Gráfica
ggplot(datos_ord, aes(x = n, y = media_ac)) +
  geom_hline(yintercept = simple_mean,
             linetype = 2, color = "firebrick") +
  geom_line() +
  geom_point(data = puntos, size = 2) +
  labs(title = "Grafica de la media", 
       x = "Observaciones incluidas (n)", 
       y = "Media con n observaciones") 

ggsave("Grafica_3a_medias.png",  width = 5.54, height = 4.95)


# ====/// 3b) Varianza muestral con las primeras n observaciones \\\=====

var_simple <- var(datos$avg_sleep_hours)

datos_ord <- datos_ord %>%  
  mutate(dif_2 = (avg_sleep_hours-media_ac)^2,
    sum_dif2 = cumsum(dif_2),
    var_ac   = sum_dif2 / (n - 1)) 

puntos_var <- datos_ord %>% filter(n %in% resalta)

# Gráfica
ggplot(datos_ord %>% filter(n>=2), aes(x = n, y = var_ac)) +
  geom_hline(yintercept = var_simple,
             linetype = 2, color = "firebrick") +
  geom_line() +
  geom_point(data = puntos_var %>% filter(n>=2), size = 2) +
  labs(title = "Grafica de la varianza muestral",
       x = "Observaciones incluidas (n)", 
       y = "Varianza con n observaciones") 
  
ggsave("Grafica_3b_varianzas.png",  width = 5.54, height = 4.95)


# ====/// 3c) Estimador de la varianza de la media \\\=====

datos_ord <- datos_ord %>%
  mutate(varmed_ac = var_ac/n)

puntos_var_xbar <- datos_ord %>% filter(n %in% resalta)

# Gráfica
ggplot(datos_ord %>% filter(n>=2), aes(x = n, y = varmed_ac)) +
  geom_hline(yintercept = var_simple/n_tot,
             linetype = 2, color = "firebrick") +
  geom_line() +
  geom_point(data = puntos_var_xbar %>% filter(n>=2), size = 2) +
  labs(title = "Grafica de la varianza de la media",
       x = "Observaciones incluidas (n)", 
       y = "Varianza con n observaciones") 

ggsave("Grafica_3c_varmed.png",  width = 5.54, height = 4.95)


# ====/// 4a) Bootstrap con reemplazo de tamaños 50, 100 y 500 \\\=====

set.seed(2027)             # semilla 
L   <- c(50, 100, 500)     # tamaño de submuestras
M <- 4000                  # número de repeticiones

medias_boot <- map_dfr(L, \(k) tibble(tam = k,
                                      media = replicate(M, mean(sample(datos$avg_sleep_hours, k, replace = TRUE)))))

# Agregar una etiqueta para la grafica
medias_boot <- medias_boot %>%
  mutate(tam_f = factor(tam, levels = L, labels = paste0("L = ", L)))

# Gráfica
ggplot(medias_boot, aes(x = media)) +
  geom_histogram(bins = 40) +
  facet_wrap(~ tam_f) +
  labs(title = "Simulaciones de la media muestral por tamaño de submuestra",  
       x = "Media muestral",
       y = "Frecuencia")

ggsave("fig_4a_boot.png",width = 5.54, height = 4.95)

# ====/// 4c) Tres versiones del IC al 92% \\\=====

z96  <- qnorm(0.96)                 # 92% deja 4% en cada cola

ic1 <- simple_mean + c(-1, 1) * z92 * sqrt(var_simple/n_tot)    # varianza analítica

X_boot <- mean(medias_boot$media[medias_boot$tam==100])
S2_boot <- var(medias_boot$media[medias_boot$tam==100])
ic2 <- X_boot + c(-1, 1) * sqrt(S2_boot * (100/n_tot))            # varianza bootstrap + normalidad

ic3 <- quantile(medias_boot$media[medias_boot$tam==100], 
                c(0.04, 0.96))                                   # distribución empírica

round(rbind(analitico = ic1, boot_normal = ic2, percentiles = ic3), 4)

# ====/// 4d) Prueba H0 con distintos tamaños de submuestra \\\=====

medias_boot_M <- medias_boot %>% group_by(tam) %>%
  summarise(mean_boot = mean(media),
            var_boot = var(media)) %>%
  mutate(Z_boot = (mean_boot-6.7)/sqrt(var_boot * (tam/n_tot)),
         pval = 2 * (1-pnorm(abs(Z_boot)))) 


# ====/// 5a) Variable binaria de dormir menos de 8 horas \\\=====

datos <- datos %>% mutate(menos8 = (datos$avg_sleep_hours < 8))
(p_hat <- mean(datos$menos8))

# ====/// 5b) Varianza muestral de la dummy contra p(1-p) \\\=====

(var_muestral = var(datos$menos8))
(var_analitica =  p_hat * (1 - p_hat))

# ====/// 6a) Diagrama de dispersión \\\=====
 
ggplot(datos, aes(x = avg_sleep_hours, y = term_gpa)) +
  geom_point(alpha = 0.35, size = 1.4) +
  labs(title = "Sueño promedio y GPA",    
       x = "Horas de sueño",
       y     = "GPA")

ggsave("fig_6a.png",width = 5.54, height = 4.95)

# ====/// 6b-c) Covarianza y correlacion \\\=====

(cov(datos$avg_sleep_hours, datos$term_gpa))
(cor(datos$avg_sleep_hours, datos$term_gpa))

# ====/// 6d) Bootstrap de la correlación \\\=====

set.seed(2026)                      # semilla 

correl_boot <- map_dfr(L, \(k) tibble(tam = k,
                                      correl = replicate(M, {sub <- slice_sample(datos, n = k, replace = TRUE)
                                        cor(sub$avg_sleep_hours, sub$term_gpa)})))

# Agregar una etiqueta
correl_boot <- correl_boot %>%
  mutate(tam_f = factor(tam, levels = L, labels = paste0("L = ", L)))

# Gráfica
ggplot(correl_boot, aes(x = correl)) +
  geom_histogram(bins = 40) +
  facet_wrap(~ tam_f) +
  labs(title = "Simulaciones de la correlación por tamaño de submuestra",
       x = "Correlación muestral",
       y = "Frecuencia")

ggsave("fig_6d_boot.png", width = 5.54, height = 4.95)


# ====/// 6e) IC 87% de las correlaciones \\\=====

ic_50 <- quantile(correl_boot$correl[correl_boot$tam==50], 
                c(0.065, 0.935))
ic_100 <- quantile(correl_boot$correl[correl_boot$tam==100], 
                  c(0.065, 0.935))
ic_500 <- quantile(correl_boot$correl[correl_boot$tam==500], 
                  c(0.065, 0.935))
round(rbind(L50 = ic_50, L100 = ic_100, L500 = ic_500), 4)

# ====/// 6f) Test de normalidad \\\=====

# QQ-plots por tamaño de submuestra
ggplot(correl_boot, aes(sample = correl)) +
  stat_qq(size = 0.6, alpha = 0.4) +
  stat_qq_line(color = "firebrick", linetype = 2) +
  facet_wrap(~ tam_f, scales = "free_y") +
  labs(title = "QQ-plots de la correlación muestral por tamaño de submuestra",
       x = "Cuantiles teóricos (normal)",
       y = "Cuantiles muestrales")

ggsave("fig_6f_qq.png", width = 5.54, height = 4.95)
