library(Rfuels)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(vegan)
library(BiodiversityR)
library(hillR)
library(scales)
library(ggrepel)


#################################################################################
#                          CARGA DE COMBUSTIBLES
#################################################################################
example_fuels_data = read.csv(file = 'datos/example_fuels.csv', stringsAsFactors = FALSE)
example_trees_data = read.csv(file = 'datos/example_treelist.csv', stringsAsFactors = FALSE)

# Datos de combustibles
example_fuels_data$duff_depth_cm <- as.numeric(gsub(",", ".", example_fuels_data$duff_depth_cm))
example_trees_data$dbh_cm <- as.numeric(gsub(",", ".", example_trees_data$dbh_cm))

transect_fuel_loads = estimate_fuel_loads(fuels_data = example_fuels_data,
                                          trees_data = example_trees_data,
                                          results_type = 'results_only')

transect_fuel_loads$tipo <- ifelse(
  grepl("UB", transect_fuel_loads$plot_id),
  "no quemado",
  "quemado"
)

transect_fuel_loads$year <- as.numeric(
  substr(transect_fuel_loads$plot_id, 1, 4)
)

##########PRUEBAS DE NORMALIDAD SHAPIRO

transect_fuel_loads %>%
  group_by(tipo) %>%
  summarise(p_shapiro = shapiro.test(fuelload_1000h_Mgha)$p.value)

transect_fuel_loads %>%
  group_by(tipo) %>%
  summarise(p_shapiro = shapiro.test(fuelload_1000s_Mgha)$p.value)

#transect_fuel_loads %>%
 # group_by(tipo) %>%
  #summarise(p_shapiro = shapiro.test(fuelload_1000r_Mgha)$p.value)

transect_fuel_loads %>%
  group_by(tipo) %>%
  summarise(p_shapiro = shapiro.test(fuelload_100h_Mgha)$p.value)

transect_fuel_loads %>%
  group_by(tipo) %>%
  summarise(p_shapiro = shapiro.test(fuelload_10h_Mgha)$p.value)

transect_fuel_loads %>%
  group_by(tipo) %>%
  summarise(p_shapiro = shapiro.test(fuelload_1h_Mgha)$p.value)

transect_fuel_loads %>%
  group_by(tipo) %>%
  summarise(p_shapiro = shapiro.test(fuelload_fwd_Mgha)$p.value)

transect_fuel_loads %>%
  group_by(tipo) %>%
  summarise(p_shapiro = shapiro.test(fuelload_litter_Mgha)$p.value)

transect_fuel_loads %>%
  group_by(tipo) %>%
  summarise(p_shapiro = shapiro.test(fuelload_surface_Mgha)$p.value)


###################PRUEBAS DE DIFERENCIAS
#Paramétricas
t.test(fuelload_10h_Mgha ~ tipo, data = transect_fuel_loads, var.equal = FALSE)
transect_fuel_loads %>%group_by(tipo) %>%summarise(Media = mean(fuelload_10h_Mgha), SE    = sd(fuelload_10h_Mgha) / sqrt(n()))

t.test(fuelload_100h_Mgha ~ tipo, data = transect_fuel_loads, var.equal = FALSE)
transect_fuel_loads %>%group_by(tipo) %>%summarise(Media = mean(fuelload_100h_Mgha), SE    = sd(fuelload_100h_Mgha) / sqrt(n()))

t.test(fuelload_fwd_Mgha ~ tipo, data = transect_fuel_loads, var.equal = FALSE)
transect_fuel_loads %>%group_by(tipo) %>%summarise(Media = mean(fuelload_fwd_Mgha), SE    = sd(fuelload_fwd_Mgha) / sqrt(n()))

#No paramétrica
wilcox.test(fuelload_1h_Mgha ~ tipo, data = transect_fuel_loads)
transect_fuel_loads %>%group_by(tipo) %>%summarise(Mediana = median(fuelload_1h_Mgha),IQR= IQR(fuelload_1h_Mgha))

wilcox.test(fuelload_1000h_Mgha ~ tipo, data = transect_fuel_loads)
transect_fuel_loads %>%group_by(tipo) %>%summarise(Mediana = median(fuelload_1000h_Mgha),IQR= IQR(fuelload_1000h_Mgha))

wilcox.test(fuelload_1000s_Mgha ~ tipo, data = transect_fuel_loads)
transect_fuel_loads %>%group_by(tipo) %>%summarise(Mediana = median(fuelload_1000s_Mgha),IQR= IQR(fuelload_1000s_Mgha))

wilcox.test(fuelload_1000r_Mgha ~ tipo, data = transect_fuel_loads)
transect_fuel_loads %>%group_by(tipo) %>%summarise(Mediana = median(fuelload_1000r_Mgha),IQR= IQR(fuelload_1000r_Mgha))

wilcox.test(fuelload_litter_Mgha ~ tipo, data = transect_fuel_loads)
transect_fuel_loads %>%group_by(tipo) %>%summarise(Mediana = median(fuelload_litter_Mgha),IQR= IQR(fuelload_litter_Mgha))

wilcox.test(fuelload_surface_Mgha ~ tipo, data = transect_fuel_loads)
transect_fuel_loads %>%group_by(tipo) %>%summarise(Mediana = median(fuelload_surface_Mgha),IQR= IQR(fuelload_surface_Mgha))

########################PLOT

# ------------------------------------------------------------
# 1. Seleccionar las variables de cargas de combustible
# ------------------------------------------------------------

fuel_cols <- c(
  "fuelload_litter_Mgha",
  "fuelload_1h_Mgha",
  "fuelload_10h_Mgha",
  "fuelload_100h_Mgha",
  "fuelload_1000s_Mgha",
  "fuelload_1000r_Mgha",
  "fuelload_fwd_Mgha",
  "fuelload_1000h_Mgha",
  "fuelload_surface_Mgha",
  "fuelload_total_Mgha"
)

# ------------------------------------------------------------
# 2. Pasar los datos a formato largo
# ------------------------------------------------------------

datos_plot <- transect_fuel_loads %>%
  select(tipo, all_of(fuel_cols)) %>%
  pivot_longer(
    cols = all_of(fuel_cols),
    names_to = "combustible",
    values_to = "carga"
  )

# ------------------------------------------------------------
# 3. Cambiar los nombres para que sean más fáciles de leer
# ------------------------------------------------------------

datos_plot$combustible <- recode(
  datos_plot$combustible,
  "fuelload_litter_Mgha"   = "Hojarasca",
  "fuelload_1h_Mgha"       = "1-h",
  "fuelload_10h_Mgha"      = "10-h",
  "fuelload_100h_Mgha"     = "100-h",
  "fuelload_1000s_Mgha"    = "1000-h (s)",
  "fuelload_1000r_Mgha"    = "1000-h (r)",
  "fuelload_fwd_Mgha"      = "FWD",
  "fuelload_1000h_Mgha"    = "1000-h",
  "fuelload_surface_Mgha"  = "Superficial",
  "fuelload_total_Mgha"    = "Total"
)

# ------------------------------------------------------------
# 4. Crear la grilla de diagramas de cajas
# ------------------------------------------------------------

ggplot(
  datos_plot,
  aes(
    x = tipo,
    y = carga,
    fill = tipo
  )
) +
  
  geom_boxplot(
    width = 0.65,
    alpha = 0.75,
    na.rm = TRUE
  ) +
  
  facet_wrap(
    ~ combustible,
    scales = "free_y",
    ncol = 3
  ) +
  
  labs(
    x = "Condición del sitio",
    y = expression("Carga de combustible (Mg ha"^{-1}*")"),
    title = "Carga de combustibles según condición del sitio"
  ) +
  
  theme_bw() +
  
  theme(
    legend.position = "none",
    
    # Títulos de cada panel
    strip.text = element_text(
      size = 14,
      face = "bold"
    ),
    
    # Texto de los ejes
    axis.title = element_text(
      size = 16,
      face = "bold"
    ),
    
    axis.text = element_text(
      size = 14
    ),
    
    # Título general
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = 0.5
    ),
    
    # Separación entre paneles
    panel.spacing = unit(1, "lines")
  )


transect_fuel_loads <- transect_fuel_loads %>%
  mutate(
    tiempo_desde_quema = 2026 - year
  )



#################################################################################
#                           DIVERSIDAD
#################################################################################
diversidad= read.csv(file='datos/diversidad.csv')

diversidad$tipo <- ifelse(grepl("UB", diversidad$plot), "no quemado", "quemado")

rangos <- diversidad %>%
  group_by(tipo, nombre_cientifico) %>%
  summarise(abundancia = sum(abund), .groups = "drop") %>%
  arrange(tipo, desc(abundancia)) %>%
  group_by(tipo) %>%
  mutate(
    rango = row_number(),
    log_abundancia = log(abundancia),
    proporcion = abundancia / sum(abundancia)
  ) %>%
  ungroup()

#write.csv(rangos, "rangos.csv", row.names = FALSE)


ggplot(rangos, aes(rango, log_abundancia, color = tipo)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic()

library(ggplot2)
library(dplyr)
library(patchwork)

# Grafico No Quemado (Salmón)
g_no_quemado <- rangos %>%
  filter(tipo == "no quemado") %>%
  mutate(especie_lbl = reorder(paste0(nombre_cientifico, " (R", rango, ")"), log_abundancia)) %>%
  ggplot(aes(x = especie_lbl, y = log_abundancia, fill = log_abundancia)) +
  geom_col(color = "black", linewidth = 0.2, width = 0.8) +
  coord_flip() +
  scale_fill_gradient(low = "#ffcccc", high = "#f8766d", name = "No quemado") +
  labs(title = "no quemado", x = "Especie (Nombre científico)", y = expression(log(Abundancia))) +
  theme_classic(base_size = 10) +
  theme(
    axis.text.y = element_text(face = "italic", size = 7.5),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Gráfico Quemado (Azul)
g_quemado <- rangos %>%
  filter(tipo == "quemado") %>%
  mutate(especie_lbl = reorder(paste0(nombre_cientifico, " (R", rango, ")"), log_abundancia)) %>%
  ggplot(aes(x = especie_lbl, y = log_abundancia, fill = log_abundancia)) +
  geom_col(color = "black", linewidth = 0.2, width = 0.8) +
  coord_flip() +
  scale_fill_gradient(low = "#cceeff", high = "#00bfc4", name = "Quemado") +
  labs(title = "quemado", x = NULL, y = expression(log(Abundancia))) +
  theme_classic(base_size = 10) +
  theme(
    axis.text.y = element_text(face = "italic", size = 7.5),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Unir ambos paneles
g_no_quemado + g_quemado

library(patchwork)

# Gráfica 1 (izquierda): líneas + puntos + tendencia
g_lineas <- ggplot(rangos, aes(rango, log_abundancia, color = tipo)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic()

# Combinar: izquierda + derecha
g_lineas + (g_no_quemado | g_quemado)



mod1 <- lm(log_abundancia~ rango * tipo, data=rangos)
summary(mod1)

anova(mod1, test="F")


library(ggplot2)
library(dplyr)

# 1. Crear el dataframe con los datos de Números de Hill
datos_hill <- data.frame(
  q = factor(rep(c("q = 0\n(Riqueza)", "q = 1\n(Equitatividad)", "q = 2\n(Dominancia)"), each = 2)),
  tipo = factor(rep(c("no quemado", "quemado"), 3), levels = c("no quemado", "quemado")),
  valor = c(14, 18, 7.92, 9.41, 6.10, 6.40)
)

# 2. Generar la gráfica de barras agrupadas
ggplot(datos_hill, aes(x = q, y = valor, fill = tipo)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black", linewidth = 0.3) +
  geom_text(aes(label = round(valor, 1)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("no quemado" = "#f8766d", "quemado" = "#00bfc4")) +
  scale_y_continuous(limits = c(0, 21)) +
  labs(
    x = "Orden de Diversidad (q)",
    y = "Valor obtenido de las series",
    fill = "Tratamiento"
  ) +
  theme_classic(base_size = 11) +
  theme(
    axis.text = element_text(color = "black"),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )


##########################HILL NUMBERS
matriz_tratamientos <- diversidad %>%
  group_by(tipo, nombre_cientifico) %>%
  summarise(abundancia_total = sum(abund, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = nombre_cientifico, 
    values_from = abundancia_total, 
    values_fill = 0
  ) %>%
  column_to_rownames("tipo")

####diversidad alfa
###riqueza (q0)
hill_taxa(comm = matriz_tratamientos, q = 0)
###divesidad basada en la abndancia de todas las especies en la comunidad (equitatividad)
hill_taxa(comm = matriz_tratamientos, q = 1)
#####diversidad basada en la abundancia de las especies más dominantes en la comunidad (dominancia)
hill_taxa(comm = matriz_tratamientos, q = 2)



#################################################################################
#                           ESTRUCTURA DE LA POBLACIÓN
#################################################################################
vegetacion= read.csv(file='datos/vegetacion.csv')


vegetacion$altura_m <- as.numeric(gsub(",", ".", vegetacion$altura_m))
vegetacion$DAP <- as.numeric(gsub(",", ".", vegetacion$DAP))

vegetacion %>%
  group_by(tipo) %>%
  summarise(p_shapiro = shapiro.test(altura_m)$p.value)

vegetacion %>%
  group_by(tipo) %>%
  summarise(p_shapiro = shapiro.test(DAP)$p.value)

wilcox.test(altura_m ~ tipo, data = vegetacion)
vegetacion %>%group_by(tipo) %>%summarise(Mediana = median(altura_m),IQR= IQR(altura_m))

wilcox.test(DAP ~ tipo, data = vegetacion)
vegetacion %>%group_by(tipo) %>%summarise(Mediana = median(DAP),IQR= IQR(DAP))



# 1. Gráfico de cajas para la Altura
ggplot(vegetacion, aes(x = tipo, y = altura_m, fill = tipo)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  theme_bw() +
  labs(x = "Tratamiento", y = "Altura (m)", title = "Comparación de Altura por Tipo")

# 2. Gráfico de cajas para el DAP
ggplot(vegetacion, aes(x = tipo, y = DAP, fill = tipo)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  theme_bw() +
  labs(x = "Tratamiento", y = "DAP (cm)", title = "Comparación de DAP por Tipo")

#################################################################################
#                            INTENSIDAD
#################################################################################
intensidad = read.csv(file='datos/intensidad.csv')

burn <- transect_fuel_loads %>% filter(tipo == "quemado")

burn <- burn %>%  left_join(intensidad %>% select(plot_id, intensidad), by = "plot_id")

burn$intensidad <- as.numeric(gsub(",", ".", burn$intensidad))

#PLOT

# 1. Definir el vector de mapeo (Nombre_original = "Nombre_nuevo")
etiquetas_combustibles <- c(
  "fuelload_1h_Mgha"     = "1-h\n(0-0.6 cm)",
  "fuelload_10h_Mgha"    = "10-h\n(0.6-2.5 cm)",
  "fuelload_100h_Mgha"   = "100-h\n(2.5-8 cm)",
  "fuelload_fwd_Mgha"    = "Total\nfine woody debris",
  "fuelload_1000s_Mgha"  = "1000-h\nsound (> 8 cm)",
  "fuelload_1000r_Mgha"  = "1000-h\nrotten (> 8 cm)",
  "fuelload_1000h_Mgha"  = "Total\ncoarse woody debris",
  "fuelload_litter_Mgha" = "Litter",
  "fuelload_surface_Mgha"= "Surface", # Ajustar si aplica
  "fuelload_total_Mgha"  = "Total"
)

# 2. Preparar datos en formato largo, renombrar y convertir a factor ordenado
burn_long <- burn %>%
  select(-fuelload_duff_Mgha) %>% 
  pivot_longer(
    cols = starts_with("fuelload_"), 
    names_to = "tipo_combustible", 
    values_to = "carga"
  ) %>% 
  # Reemplazar nombres y definir el orden exacto de los paneles
  mutate(
    tipo_combustible = recode(tipo_combustible, !!!etiquetas_combustibles),
    tipo_combustible = factor(tipo_combustible, levels = unname(etiquetas_combustibles))
  )

# Vector con las etiquetas donde sí se dibujará la línea punteada
vars_con_tendencia <- c(
  "100-h\n(2.5-8 cm)", 
  "Total\nfine woody debris", 
 # "Superficial",
  "Total"# Cambiar según corresponda si graficas la superficie total
)

# 3. Generar la gráfica
Sperman <- ggplot(burn_long, aes(x = intensidad, y = carga)) +
  geom_point(color = "gray30", alpha = 0.7, size = 1.5) +
  geom_smooth(
    data = filter(burn_long, tipo_combustible %in% vars_con_tendencia),
    method = "lm",
    se = TRUE,
    linetype = "dashed",
    color = "#0072B2",
    fill = "gray70"
  ) +
  facet_wrap(~ tipo_combustible, scales = "free_y") +
  theme_classic() +
  theme(
    strip.background = element_blank(), # Fondo limpio para las etiquetas superiores
    strip.text = element_text(face = "bold", size = 9) # Texto en negrita para mejor lectura
  ) +
  labs(
    x = "Fire intensity",
    y = "Fuel load (Mg/ha)"
  )

ggsave(
  filename = file.path("Tables and figures/Figure_4.png"),
  plot = Sperman,
  width = 13,
  height = 10,
  dpi = 300,
  units = "in"
)



#Correlación de sperman
corr_litter <- cor.test(x = burn$intensidad, y = burn$fuelload_litter_Mgha, method = 'spearman')
corr_1h <- cor.test(x = burn$intensidad, y = burn$fuelload_1h_Mgha, method = 'spearman')
corr_10h <- cor.test(x = burn$intensidad, y = burn$fuelload_10h_Mgha, method = 'spearman')
corr_100h <- cor.test(x = burn$intensidad, y = burn$fuelload_100h_Mgha, method = 'spearman')
corr_1000s <- cor.test(x = burn$intensidad, y = burn$fuelload_1000s_Mgha, method = 'spearman')
corr_1000r <- cor.test(x = burn$intensidad, y = burn$fuelload_1000r_Mgha, method = 'spearman')
corr_fwd <- cor.test(x = burn$intensidad, y = burn$fuelload_fwd_Mgha, method = 'spearman')
corr_1000h <- cor.test(x = burn$intensidad, y = burn$fuelload_1000h_Mgha, method = 'spearman')
corr_surface <- cor.test(x = burn$intensidad, y = burn$fuelload_surface_Mgha, method = 'spearman')
corr_total <- cor.test(x = burn$intensidad, y = burn$fuelload_total_Mgha, method = 'spearman')

corr_litter
corr_1h
corr_10h
corr_100h
corr_fwd
corr_1000s
corr_1000r
corr_1000h
corr_total
corr_surface
#################################################################################
#                           DATOS GENERALES LA MALINTZI
#################################################################################
general= read.csv(file='datos/general_malinche.csv')



general$fecha_inicio <- parse_date_time(
  general$fecha_inicio,
  orders = c("ymd", "dmy")
)
general$fecha_fin <- parse_date_time(
  general$fecha_fin,
  orders = c("ymd", "dmy", "dmY", "Ymd")
)
general$area <- as.numeric(sub(",", ".", general$area, fixed = TRUE))
general$year <- year(general$fecha_inicio)
general$mes <- month(general$fecha_inicio)

# ------------------------------------------------------------------------------
# 0. Cargar librerías necesarias
# ------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(scales)
library(stringr)

# Tema general con estética para publicación científica
tema_cientifico <- theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "sans", color = "black"),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(color = "black", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

# ------------------------------------------------------------------------------
# 1. UNIFICACIÓN Y LIMPIEZA DEL DATAFRAME
# ------------------------------------------------------------------------------
general_clean <- general %>%
  mutate(
    # Quitar espacios en blanco extra al inicio o final
    tip_veg = trimws(tip_veg),
    causa = trimws(causa),
    tipo_incen = trimws(tipo_incen),
    MUNICIPIO.C.80 = trimws(MUNICIPIO.C.80),
    
    # Estandarizar tipo de vegetación unificando duplicados y corrigiendo caracteres
    tip_veg_clean = case_when(
      grepl("Pino-Encino|BPQ", tip_veg, ignore.case = TRUE) ~ "Bosque de Pino-Encino",
      grepl("Encino-Pino|BQP", tip_veg, ignore.case = TRUE) ~ "Bosque de Encino-Pino",
      grepl("Pino", tip_veg, ignore.case = TRUE) & !grepl("Encino", tip_veg, ignore.case = TRUE) ~ "Bosque de Pino",
      grepl("Encino", tip_veg, ignore.case = TRUE) & !grepl("Pino", tip_veg, ignore.case = TRUE) ~ "Bosque de Encino",
      grepl("Oyamel", tip_veg, ignore.case = TRUE) ~ "Bosque de Oyamel",
      grepl("Pradera de Alta Monta", tip_veg, ignore.case = TRUE) ~ "Pradera de Alta Montaña",
      grepl("Pastizal", tip_veg, ignore.case = TRUE) ~ "Pastizal Natural",
      TRUE ~ tip_veg
    )
  )

# ------------------------------------------------------------------------------
# 2. GENERACIÓN DE GRÁFICAS
# ------------------------------------------------------------------------------

# 1. Distribución del área afectada (Escala Logarítmica)
g1 <- ggplot(general_clean, aes(x = area)) +
  geom_histogram(bins = 30, fill = "grey40", color = "black", linewidth = 0.3) +
  scale_x_log10(labels = label_comma()) +
  labs(
    title = "Distribución de la Superficie Afectada",
    x = "Área afectada (ha, escala log10)",
    y = "Frecuencia (N° de incendios)"
  ) +
  tema_cientifico

print(g1)

# 2. Ocurrencia Anual por Tipo de Incendio
g2 <- general_clean %>%
  count(year, tipo_incen) %>%
  ggplot(aes(x = factor(year), y = n, fill = tipo_incen)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", linewidth = 0.3) +
  scale_fill_grey(start = 0.2, end = 0.8, name = "Tipo de Incendio") +
  labs(
    title = "Ocurrencia Anual de Incendios Forestales",
    x = "Año",
    y = "Número de incendios"
  ) +
  tema_cientifico

print(g2)

# 3. Patrón estacional mensual (Frecuencia por Mes)
meses_labels <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                  "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

g3 <- general_clean %>%
  count(mes) %>%
  ggplot(aes(x = factor(mes, levels = 1:12, labels = meses_labels), y = n)) +
  geom_bar(stat = "identity", fill = "grey30", color = "black", linewidth = 0.3) +
  labs(
    title = "Distribución Mensual de Incendios",
    x = "Mes",
    y = "Número total de incendios"
  ) +
  tema_cientifico

print(g3)

# 4. Principales causas de incendios
g4 <- general_clean %>%
  count(causa) %>%
  mutate(causa = reorder(causa, n)) %>%
  ggplot(aes(x = n, y = causa)) +
  geom_col(fill = "grey40", color = "black", linewidth = 0.3) +
  labs(
    title = "Principales Causas de Incendios Forestales",
    x = "Número de incendios",
    y = "Causa registrada"
  ) +
  tema_cientifico

print(g4)

# 5. Área afectada según Tipo de Vegetación (Boxplot en escala Logarítmica)
g5 <- ggplot(general_clean, aes(x = reorder(tip_veg_clean, area, FUN = median), y = area)) +
  geom_boxplot(fill = "grey85", color = "black", outlier.size = 1.2, outlier.alpha = 0.6) +
  scale_y_log10(labels = label_comma()) +
  coord_flip() +
  labs(
    title = "Superficie Afectada por Tipo de Vegetación",
    x = "Tipo de vegetación",
    y = "Área afectada (ha, escala log10)"
  ) +
  tema_cientifico

print(g5)

# 6. Top 10 Municipios con mayor número de incendios
g6 <- general_clean %>%
  count(MUNICIPIO.C.80) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(MUNICIPIO.C.80, n), y = n)) +
  geom_col(fill = "grey35", color = "black", linewidth = 0.3) +
  coord_flip() +
  labs(
    title = "Top 10 Municipios con Mayor Frecuencia de Incendios",
    x = "Municipio",
    y = "Número de incendios"
  ) +
  tema_cientifico

print(g6)



# 1. Limpieza rigurosa y creación de 'tip_veg_clean'
general_clean <- general %>%
  mutate(
    # Normalizar espacios en blanco extra
    tip_veg_str = str_squish(tip_veg),
    
    # Estandarización de categorías
    tip_veg_clean = case_when(
      str_detect(tip_veg_str, regex("Pino-Encino|BPQ", ignore_case = TRUE)) ~ "Bosque de Pino-Encino",
      str_detect(tip_veg_str, regex("Encino-Pino|BQP", ignore_case = TRUE)) ~ "Bosque de Encino-Pino",
      str_detect(tip_veg_str, regex("Pino", ignore_case = TRUE)) & !str_detect(tip_veg_str, regex("Encino", ignore_case = TRUE)) ~ "Bosque de Pino",
      str_detect(tip_veg_str, regex("Encino", ignore_case = TRUE)) & !str_detect(tip_veg_str, regex("Pino", ignore_case = TRUE)) ~ "Bosque de Encino",
      str_detect(tip_veg_str, regex("Pradera|Monta", ignore_case = TRUE)) ~ "Pradera de Alta Montaña",
      str_detect(tip_veg_str, regex("Oyamel", ignore_case = TRUE)) ~ "Bosque de Oyamel",
      str_detect(tip_veg_str, regex("Pastizal", ignore_case = TRUE)) ~ "Pastizal Natural",
      TRUE ~ tip_veg_str
    )
  )

# 2. Gráfica de Área Total Afectada (usando tip_veg_clean)
g5_total <- general_clean %>%
  group_by(tip_veg_clean) %>%
  summarise(area_total = sum(area, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(tip_veg_clean, area_total), y = area_total)) +
  geom_col(fill = "grey40", color = "black", linewidth = 0.3) +
  scale_y_continuous(labels = label_comma()) +
  coord_flip() +
  labs(
    title = "Área Total Afectada por Tipo de Vegetación",
    x = "Tipo de vegetación",
    y = "Superficie total afectada (ha)"
  ) +
  tema_cientifico

print(g5_total)



#####################################MAPA


# 1. Crear el dataframe con el número de incendios por municipio
incendios_por_municipio <- general %>%
  # Limpiar espacios en blanco al inicio y final del nombre del municipio
  mutate(MUNICIPIO.C.80 = str_squish(MUNICIPIO.C.80)) %>%
  # Contar la cantidad de incendios por municipio
  count(MUNICIPIO.C.80, name = "num_incendios") %>%
  # Ordenar de mayor a menor número de incendios
  arrange(desc(num_incendios))

# 2. Visualizar las primeras filas del nuevo dataframe
head(incendios_por_municipio)



###############################################################################33
#                                                   MAPA
#################################################################################

library(sf)
library(dplyr)
library(ggplot2)
library(stringr)

sf_use_s2(FALSE)

# Tema cartográfico para publicación científica
tema_mapa <- theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "sans", color = "black"),
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"),
    axis.title = element_blank(),
    axis.text = element_text(size = 8, color = "black"),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.background = element_rect(fill = "white")
  )

# ------------------------------------------------------------------------------
# 2. Cargar capas vectoriales (ANP Malinche y Municipios)
# ------------------------------------------------------------------------------
# Polígono del Parque Nacional La Malinche
malinche <- st_read("inf_espacial/shp_anp_20260924/shp_anp_20260924/shp_anp_20260924.shp") %>%
  filter(NOMBRE == "La Montaña Malinche o Matlalcuéyatl") %>%
  st_make_valid()

# Capa vectorial de municipios de México
municipios_mex <- st_read("inf_espacial/municipio/mun21cw.shp") %>%
  st_make_valid()

# Homogeneizar el Sistema de Referencia de Coordenadas (CRS) al de Malinche
municipios_mex <- st_transform(municipios_mex, st_crs(malinche))

# ------------------------------------------------------------------------------
# 3. Filtrar municipios dentro/intersectando La Malinche
# ------------------------------------------------------------------------------
# Filtrado por intersección espacial geométrica
municipios_malinche_sf <- municipios_mex[st_intersects(municipios_mex, malinche, sparse = FALSE), ]

# ------------------------------------------------------------------------------
# 4. Unir la frecuencia de incendios del dataframe 'general'
# ------------------------------------------------------------------------------
# Conteo de incendios por municipio desde tu dataframe
incendios_mun <- general %>%
  mutate(MUNICIPIO = str_squish(MUNICIPIO.C.80)) %>%
  count(MUNICIPIO, name = "num_incendios")

# Unir datos con el objeto espacial (Asegúrate de ajustar NOM_MUN por NOM_ENT si aplica)
mapa_datos <- municipios_malinche_sf %>%
  mutate(NOM_MUN_CLEAN = str_squish(NOM_MUN)) %>% 
  left_join(incendios_mun, by = c("NOM_MUN_CLEAN" = "MUNICIPIO")) %>%
  mutate(num_incendios = ifelse(is.na(num_incendios), 0, num_incendios))

# ------------------------------------------------------------------------------
# 5. Generación del Mapa
# ------------------------------------------------------------------------------
g_mapa <- ggplot() +
  # 1. Capa de municipios con gradiente según el número de incendios
  geom_sf(data = mapa_datos, aes(fill = num_incendios), color = "grey30", linewidth = 0.3) +
  
  # 2. Capa del límite del Parque Nacional La Malinche (Línea punteada en rojo/negro)
  geom_sf(data = malinche, fill = NA, color = "black", linetype = "dashed", linewidth = 0.8) +
  
  # 3. Escala de color (Gris oscuro / Negro para valores más altos)
  scale_fill_gradient(
    low = "#f7f7f7", 
    high = "#252525", 
    name = "N° de Incendios"
  ) +
  
  labs(
    title = "Frecuencia de Incendios por Municipio",
    subtitle = "Municipios dentro del Parque Nacional La Malinche"
  ) +
  tema_mapa

print(g_mapa)




# ------------------------------------------------------------------------------
# 1. Cargar librerías y configurar parámetros espaciales
# ------------------------------------------------------------------------------
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggspatial)

sf_use_s2(FALSE)

# Definir un CRS común explícito (WGS 84 / EPSG:4326)
crs_destino <- st_crs(4326)

# Tema cartográfico para publicación científica
tema_mapa <- theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "sans", color = "black"),
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"),
    axis.title = element_blank(),
    axis.text = element_text(size = 8, color = "black"),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.background = element_rect(fill = "white")
  )

# ------------------------------------------------------------------------------
# 2. Cargar capas vectoriales y homogeneizar CRS a EPSG:4326
# ------------------------------------------------------------------------------
# Polígono del Parque Nacional La Malinche
malinche <- st_read("inf_espacial/shp_anp_20260924/shp_anp_20260924/shp_anp_20260924.shp") %>%
  filter(NOMBRE == "La Montaña Malinche o Matlalcuéyatl") %>%
  st_make_valid() %>%
  st_transform(crs_destino)

# Capa vectorial de municipios de México
municipios_mex <- st_read("inf_espacial/municipio/mun21cw.shp") %>%
  st_make_valid() %>%
  st_transform(crs_destino)

# Función para leer, validar y transformar puntos GeoPackage
cargar_puntos <- function(ruta) {
  puntos <- st_read(ruta) %>% st_make_valid()
  # Si el CRS original es NA, se asigna 4326 por defecto antes de transformar
  if (is.na(st_crs(puntos))) {
    st_crs(puntos) <- crs_destino
  } else {
    puntos <- st_transform(puntos, crs_destino)
  }
  return(puntos)
}

# Cargar y unir puntos desde inf_espacial
puntos_3oct <- cargar_puntos("inf_espacial/3oct.gpkg")
puntos_4oct <- cargar_puntos("inf_espacial/4oct.gpkg")
puntos_6oct <- cargar_puntos("inf_espacial/6oct.gpkg")

puntos_todos <- bind_rows(puntos_3oct, puntos_4oct, puntos_6oct) %>%
  filter(!st_is_empty(.)) %>% 
  mutate(Anio = str_extract(name, "^\\d{4}"))

# ------------------------------------------------------------------------------
# 3. Filtrar municipios dentro/intersectando La Malinche
# ------------------------------------------------------------------------------
municipios_malinche_sf <- municipios_mex[st_intersects(municipios_mex, malinche, sparse = FALSE), ]

# ------------------------------------------------------------------------------
# 4. Unir la frecuencia de incendios del dataframe 'general'
# ------------------------------------------------------------------------------
incendios_mun <- general %>%
  mutate(MUNICIPIO = str_squish(MUNICIPIO.C.80)) %>%
  count(MUNICIPIO, name = "num_incendios")

mapa_datos <- municipios_malinche_sf %>%
  mutate(NOM_MUN_CLEAN = str_squish(NOM_MUN)) %>% 
  left_join(incendios_mun, by = c("NOM_MUN_CLEAN" = "MUNICIPIO")) %>%
  mutate(num_incendios = ifelse(is.na(num_incendios), 0, num_incendios))

# Obtener límites espaciales del área de estudio (para evitar deformación del encuadre)
bbox_malinche <- st_bbox(municipios_malinche_sf)

# ------------------------------------------------------------------------------
# 5. Generación del Mapa
# ------------------------------------------------------------------------------
g_mapa <- ggplot() +
  # 1. Capa de municipios (Polígonos)
  geom_sf(data = mapa_datos, aes(fill = num_incendios), color = "grey30", linewidth = 0.3) +
  
  # 2. Capa del límite del Parque Nacional La Malinche
  geom_sf(data = malinche, fill = NA, color = "black", linetype = "dashed", linewidth = 0.8) +
  
  # 3. Capa de puntos por año
  geom_sf(data = puntos_todos, aes(color = Anio), size = 2.8, alpha = 0.9) +
  
  # 4. Etiquetas de los puntos
  geom_sf_text(
    data = puntos_todos, 
    aes(label = name), 
    size = 0, 
    nudge_y = 0.003, # Ajuste adecuado para coordenadas geográficas (grados)
    check_overlap = TRUE,
    fontface = "bold"
  ) +
  
  # 5. Escala de color de fondo (Incendios)
  scale_fill_gradient(
    low = "#f7f7f7", 
    high = "#252525", 
    name = "N° de Incendios"
  ) +
  
  # 6. Escala discreta de colores para los años
  scale_color_brewer(
    palette = "Set1", 
    name = "Año de registro"
  ) +
  
  # 7. Restringir la vista espacial exactamente a los municipios seleccionados
  coord_sf(
    xlim = c(bbox_malinche["xmin"], bbox_malinche["xmax"]),
    ylim = c(bbox_malinche["ymin"], bbox_malinche["ymax"]),
    expand = FALSE
  ) +
  
  # 8. Flecha hacia el Norte (Esquina superior izquierda 'tl')
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering(fill = c("black", "white"))
  ) +
  
  # 9. Regla de escala espacial (Esquina inferior derecha 'br')
  annotation_scale(
    location = "br",
    width_hint = 0.3,
    style = "ticks",
    text_color = "black"
  ) +
  
  labs(
    title = "Frecuencia de Incendios por Municipio",
    subtitle = "Municipios dentro del Parque Nacional La Malinche"
  ) +
  tema_mapa

print(g_mapa)