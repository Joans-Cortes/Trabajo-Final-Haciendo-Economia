# ------------------------------------------------------------------
# Gráficas de la encuesta al personal
# Proyecto final - Grupo B2 (Estrategias): cafeterías de Chapinero
#
# Entrada : Output/Datos_Limpios/encuesta_limpia.csv  (salida de limpieza_encuesta.R)
# Salida  : Output/Graficas/*.png
#
# Ejecutar desde la carpeta raíz del repositorio, DESPUÉS de limpieza_encuesta.R.
# Los números de los títulos (n, conteos, correlaciones) se calculan con los datos.
# Los títulos de g04, g07 y g09 incluyen una conclusión escrita a mano: hay que
# releerlos si cambian las respuestas.
# ------------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

dir.create("Output/Graficas", recursive = TRUE, showWarnings = FALSE)

datos <- read_csv("Output/Datos_Limpios/encuesta_limpia.csv", show_col_types = FALSE) %>%
  mutate(local = str_to_title(str_squish(p2_local)),
         # nombre corto para etiquetar puntos (quita palabras genéricas)
         corto = str_squish(str_remove_all(local, regex("\\b(café|cafe|coffee|premium|restaurante|roasters)\\b", ignore_case = TRUE))))
N <- nrow(datos)

# Estilo común ---------------------------------------------------------
azul <- "#2979d6"; gris <- "#c4c3b6"; texto <- "#222222"; sub <- "#666666"

tema <- theme_minimal(base_size = 12) +
  theme(plot.title.position = "plot",
        plot.title    = element_text(size = 15, colour = texto),
        plot.subtitle = element_text(size = 11, colour = sub, margin = margin(b = 10)),
        plot.caption  = element_text(size = 9, colour = sub, hjust = 0),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title = element_text(size = 11, colour = texto),
        axis.text  = element_text(colour = texto))
theme_set(tema)

coma  <- function(x, d = 2) gsub("\\.", ",", formatC(x, format = "f", digits = d))
pct   <- function(n, N) paste0(n, " (", round(100 * n / N), " %)")
nota  <- sprintf("Encuesta al personal de cafeterías de especialidad, Chapinero (n = %d)", N)

guardar <- function(g, archivo, ancho = 8.5, alto = 4.8) {
  ggsave(file.path("Output/Graficas", archivo), g, width = ancho, height = alto,
         dpi = 200, bg = "white")
}

# Barras horizontales ordenadas, con la más alta resaltada
barras_h <- function(df, titulo, subtitulo) {
  df <- df %>% arrange(n) %>% mutate(etiqueta = factor(etiqueta, levels = etiqueta),
                                     destaca = n == max(n))
  ggplot(df, aes(n / N_ref, etiqueta, fill = destaca)) +
    geom_col(width = 0.68) +
    geom_text(aes(label = pct(n, N_ref)), hjust = -0.1, size = 3.8, colour = texto) +
    scale_fill_manual(values = c(`TRUE` = azul, `FALSE` = gris), guide = "none") +
    scale_x_continuous(limits = c(0, 1.18), breaks = NULL, expand = c(0, 0)) +
    labs(title = titulo, subtitle = subtitulo, x = NULL, y = NULL, caption = nota) +
    theme(panel.grid.major.x = element_blank())
}

# ====================================================================
# 1. Métodos de preparación (P11) – respuesta múltiple
# ====================================================================
N_ref <- N
met <- tibble(
  etiqueta = c("Espresso y bebidas con leche", "Métodos filtrados (V60, Chemex)",
               "Bebidas de autor o de temporada", "Cold brew", "Espresso frío y sodas"),
  n = c(sum(datos$m_espresso), sum(datos$m_filtrado), sum(datos$m_autor), sum(datos$m_coldbrew),
        sum(str_detect(datos$p11_metodos, "Bebidas fr"), na.rm = TRUE)))
g1 <- barras_h(met,
  sprintf("El espresso está en las %d cafeterías; los filtrados, en %d", N, sum(datos$m_filtrado)),
  "Cafeterías que ofrecen cada método (respuesta múltiple)")
guardar(g1, "g01_metodos_preparacion.png", alto = 4)

# ====================================================================
# 2. Permanencia (P25) y gasto (P26)
# ====================================================================
niv_perm  <- c("Menos de\n15 min", "15 a 30\nmin", "30 a 60\nmin", "1 a 2\nhoras", "Más de\n2 horas")
niv_gasto <- c("Menos de\n$10 mil", "$10–19\nmil", "$20–29\nmil", "$30–44\nmil", "$45 mil\no más")

cat_df <- bind_rows(
  datos %>% count(nivel = p25_nivel) %>% mutate(panel = "Tiempo de permanencia", etiqueta = niv_perm[nivel]),
  datos %>% count(nivel = p26_nivel) %>% mutate(panel = "Gasto promedio por visita", etiqueta = niv_gasto[nivel])) %>%
  group_by(panel) %>% mutate(destaca = n == max(n)) %>% ungroup() %>%
  mutate(etiqueta = factor(etiqueta, levels = c(niv_perm, niv_gasto)),
         panel = factor(panel, levels = c("Tiempo de permanencia", "Gasto promedio por visita")))

modo_perm  <- niv_perm[as.integer(names(which.max(table(datos$p25_nivel))))]
modo_gasto <- niv_gasto[as.integer(names(which.max(table(datos$p26_nivel))))]
g2 <- ggplot(cat_df, aes(etiqueta, n, fill = destaca)) +
  geom_col(width = 0.8) +
  geom_text(aes(label = n), vjust = -0.4, size = 4, colour = texto) +
  facet_wrap(~panel, scales = "free_x") +
  scale_fill_manual(values = c(`TRUE` = azul, `FALSE` = gris), guide = "none") +
  scale_y_continuous(breaks = NULL, expand = expansion(mult = c(0, 0.15))) +
  labs(title = sprintf("Lo más frecuente: estancias de %s y gasto de %s por persona",
                       str_replace_all(modo_perm, "\n", " "), str_replace_all(modo_gasto, "\n", " ")),
       subtitle = "Cafeterías por categoría", x = NULL, y = NULL, caption = nota) +
  theme(panel.grid.major.x = element_blank(), strip.text = element_text(size = 12, hjust = 0, colour = texto),
        axis.text.x = element_text(size = 9.5))
guardar(g2, "g02_permanencia_y_gasto.png", alto = 4.4)

# ====================================================================
# 3. Franjas de mayor afluencia (P27) – respuesta múltiple
# ====================================================================
fr <- tibble(
  etiqueta = c("Fines de semana en la tarde", "Entre semana en la tarde", "Entre semana al mediodía",
               "Fines de semana en la mañana", "Entre semana en la mañana"),
  n = c(sum(datos$f_fin_tarde), sum(datos$f_sem_tarde), sum(datos$f_sem_medio),
        sum(datos$f_fin_manana), sum(datos$f_sem_manana)))
top <- fr %>% slice_max(n, n = 1, with_ties = FALSE)
g3 <- barras_h(fr,
  sprintf("%s es el pico de afluencia en %d de %d cafeterías", top$etiqueta, top$n, N),
  "Cafeterías que señalan cada franja como de mayor afluencia (respuesta múltiple)")
guardar(g3, "g03_franjas_afluencia.png", alto = 4)

# ====================================================================
# 4. Interacción con el personal vs. clientes habituales (P8–P10 vs P23)
# ====================================================================
r4 <- cor(datos$indice_personal, datos$p23_habituales)
etiquetar <- datos %>% filter(p23_habituales == max(p23_habituales))
g4 <- ggplot(datos, aes(indice_personal, p23_habituales)) +
  geom_point(size = 5.5, colour = azul, fill = azul, alpha = 0.4, shape = 21, stroke = 0.8) +
  geom_text(data = etiquetar, aes(label = corto), vjust = -1.4, size = 3.6, colour = texto, check_overlap = TRUE) +
  scale_y_continuous(breaks = 0:10, limits = c(min(datos$p23_habituales) - 0.5, 10.8)) +
  scale_x_continuous(labels = function(x) coma(x, 1), expand = expansion(mult = 0.1)) +
  labs(title = "Más interacción con el cliente no se asocia con más clientes habituales",
       subtitle = sprintf("Una cafetería por punto (n = %d) · correlación r = %s", N, coma(r4)),
       x = "Índice de interacción con el personal (1 a 5)", y = "Clientes habituales de cada 10",
       caption = "Los puntos más oscuros son varias cafeterías con los mismos valores.") +
  theme(panel.grid.major.y = element_line(colour = "#e3e3e3"))
guardar(g4, "g04_interaccion_vs_habituales.png", alto = 5.2)

# ====================================================================
# 5. Cada pregunta del índice de personal (P8, P9, P10) – distribución
#    Muestra la variación que el promedio esconde.
# ====================================================================
dims <- c(p8_explica = "Explica el origen o la preparación (P8)",
          p9_personaliza = "Recomienda o personaliza (P9)",
          p10_recuerda = "Recuerda el pedido habitual (P10)")
lik <- datos %>%
  select(all_of(names(dims))) %>%
  pivot_longer(everything(), names_to = "pregunta", values_to = "valor") %>%
  count(pregunta, valor) %>%
  complete(pregunta, valor = 1:5, fill = list(n = 0)) %>%
  mutate(etiqueta = factor(dims[pregunta], levels = rev(dims)),
         valor = factor(valor, levels = 5:1))
medias <- datos %>% summarise(across(all_of(names(dims)), mean)) %>%
  pivot_longer(everything(), names_to = "pregunta", values_to = "media") %>%
  mutate(etiqueta = factor(dims[pregunta], levels = rev(dims)))
menor <- medias %>% slice_min(media, n = 1, with_ties = FALSE)
g5 <- ggplot(lik, aes(n, etiqueta, fill = valor)) +
  geom_col(position = "fill", width = 0.65, colour = "white") +
  geom_text(aes(label = ifelse(n > 0, n, "")), position = position_fill(vjust = 0.5), size = 3.8, colour = "white") +
  geom_text(data = medias, aes(x = 1.02, y = etiqueta, label = paste0("prom. ", coma(media))),
            inherit.aes = FALSE, hjust = 0, size = 3.8, colour = texto) +
  scale_fill_manual(values = c(`5` = "#0b4a8b", `4` = "#2979d6", `3` = "#8bb6e8", `2` = "#d6d4c7", `1` = "#a9a79a"),
                    name = "Respuesta\n(1 a 5)", drop = FALSE) +
  scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 1.2)) +
  labs(title = sprintf("La pregunta con menor puntaje es «%s» (promedio %s)", str_remove(as.character(menor$etiqueta), " \\(P\\d+\\)"), coma(menor$media)),
       subtitle = "Número de cafeterías por respuesta en cada pregunta del índice de interacción",
       x = NULL, y = NULL, caption = nota) +
  theme(panel.grid.major.x = element_blank())
guardar(g5, "g05_distribucion_indice_personal.png", alto = 3.8)

# ====================================================================
# 6. Elementos de la experiencia presentes (P11–P18)
#    "No sé" = NA y no cuenta como ausencia (mismo criterio del índice).
# ====================================================================
items <- tribble(
  ~etiqueta,                              ~col,
  "Métodos filtrados",                    "m_filtrado",
  "Cold brew",                            "m_coldbrew",
  "Bebidas de autor o de temporada",      "m_autor",
  "Café de origen o lotes especiales",    "p12_origen_si",
  "Tuesta en el local o en el sitio",     "p13_tostion_propia",
  "Vende café en grano",                  "p14_grano_si",
  "Comida preparada (no solo repostería)","comida",
  "Hace actividades (talleres, catas)",   "p16_nivel_ge1",
  "Programa de clientes frecuentes",      "p17_fidelizacion_si",
  "Usa redes sociales",                   "p18_redes_si")
d6 <- datos %>% mutate(comida = as.numeric(p15_nivel == 2), p16_nivel_ge1 = as.numeric(p16_nivel >= 1))
it6 <- items %>%
  mutate(n  = sapply(col, function(c) sum(d6[[c]], na.rm = TRUE)),
         nv = sapply(col, function(c) sum(!is.na(d6[[c]]))))
resalta <- it6 %>% mutate(destaca = n == max(n))
it6 <- it6 %>% arrange(n / nv) %>% mutate(etiqueta = factor(etiqueta, levels = etiqueta))
g6 <- ggplot(it6, aes(n / nv, etiqueta)) +
  geom_col(width = 0.68, fill = gris) +
  geom_col(data = filter(it6, n / nv == max(n / nv)), width = 0.68, fill = azul) +
  geom_text(aes(label = sprintf("%d de %d (%d %%)", n, nv, round(100 * n / nv))), hjust = -0.08, size = 3.7, colour = texto) +
  scale_x_continuous(limits = c(0, 1.3), breaks = NULL, expand = c(0, 0)) +
  labs(title = {t6 <- it6 %>% slice_max(n / nv, n = 1, with_ties = FALSE); sprintf("El elemento más común: %s (%d de %d cafeterías)", str_to_lower(as.character(t6$etiqueta)), t6$n, t6$nv)},
       subtitle = "Cafeterías que cuentan con cada elemento de la experiencia de consumo",
       x = NULL, y = NULL,
       caption = str_wrap(paste0(nota, ". \"No sé\" no cuenta como ausencia; por eso algunos denominadores son menores que n."), 100)) +
  theme(panel.grid.major.x = element_blank())
guardar(g6, "g06_elementos_experiencia.png", alto = 5)

# ====================================================================
# 7. Experiencia de consumo vs. clientes habituales (P11–P18 vs P23)
# ====================================================================
r7 <- cor(datos$indice_experiencia, datos$p23_habituales)
g7 <- ggplot(datos, aes(indice_experiencia, p23_habituales)) +
  geom_point(size = 5.5, colour = azul, fill = azul, alpha = 0.4, shape = 21, stroke = 0.8) +
  scale_y_continuous(breaks = 0:10, limits = c(min(datos$p23_habituales) - 0.5, 10.5)) +
  scale_x_continuous(breaks = 0:10) +
  labs(title = "Más elementos de experiencia no se asocian claramente con más habituales",
       subtitle = sprintf("Una cafetería por punto (n = %d) · correlación r = %s", N, coma(r7)),
       x = "Índice de experiencia (elementos presentes, 0 a 10)", y = "Clientes habituales de cada 10",
       caption = "Los puntos más oscuros son varias cafeterías con los mismos valores.") +
  theme(panel.grid.major.y = element_line(colour = "#e3e3e3"))
guardar(g7, "g07_experiencia_vs_habituales.png", alto = 5.2)

# ====================================================================
# 8. Gasto por visita según personal y experiencia (P26 vs índices)
# ====================================================================
etq_gasto <- c("Menos de\n$10 mil", "$10–19\nmil", "$20–29\nmil", "$30–44\nmil", "$45 mil\no más")
d8 <- datos %>%
  select(local, p26_nivel, `Índice de interacción (1 a 5)` = indice_personal,
         `Índice de experiencia (0 a 10)` = indice_experiencia) %>%
  pivot_longer(-c(local, p26_nivel), names_to = "indice", values_to = "valor") %>%
  mutate(gasto = factor(etq_gasto[p26_nivel], levels = etq_gasto[sort(unique(p26_nivel))]),
         indice = factor(indice, levels = c("Índice de interacción (1 a 5)", "Índice de experiencia (0 a 10)")))
n_gasto <- datos %>% count(p26_nivel) %>% mutate(gasto = etq_gasto[p26_nivel])
r8a <- cor(datos$indice_personal, datos$p26_nivel, method = "spearman")
r8b <- cor(datos$indice_experiencia, datos$p26_nivel, method = "spearman")
g8 <- ggplot(d8, aes(gasto, valor)) +
  geom_boxplot(width = 0.5, colour = "#8c8b80", fill = "#ecebe3", outlier.shape = NA) +
  geom_jitter(width = 0.12, height = 0, size = 2.6, colour = azul, alpha = 0.7) +
  facet_wrap(~indice, scales = "free_y") +
  labs(title = "Gasto por visita según el índice de interacción y el de experiencia",
       subtitle = sprintf("Cada punto es una cafetería · correlación de Spearman con el gasto: interacción %s; experiencia %s",
                          coma(r8a), coma(r8b)),
       x = "Gasto promedio por visita", y = NULL,
       caption = paste0(nota, ". Con pocos locales por categoría de gasto, el patrón es descriptivo, no concluyente.")) +
  theme(panel.grid.major.x = element_blank(), strip.text = element_text(size = 11.5, hjust = 0, colour = texto),
        axis.text.x = element_text(size = 9.5))
guardar(g8, "g08_gasto_vs_indices.png", ancho = 10, alto = 4.8)

# ====================================================================
# 9. Motivos de visita (P19–P22) – promedio de lo respondido
#    OJO: ninguna encuesta suma 10; se lee como importancia percibida (0 a 10),
#    no como proporción de clientes. Ver sección 8.1 del informe.
# ====================================================================
mot <- tibble(
  etiqueta = c("Reunirse o conversar", "Tomar un café de calidad", "Comprar para llevar", "Trabajar o estudiar"),
  media = c(mean(datos$p20_reunirse), mean(datos$p21_cafe_calidad), mean(datos$p22_llevar), mean(datos$p19_trabajar))) %>%
  arrange(media) %>% mutate(etiqueta = factor(etiqueta, levels = etiqueta), destaca = media == max(media))
g9 <- ggplot(mot, aes(media, etiqueta, fill = destaca)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = coma(media)), hjust = -0.15, size = 4, colour = texto) +
  scale_fill_manual(values = c(`TRUE` = azul, `FALSE` = gris), guide = "none") +
  scale_x_continuous(limits = c(0, 10.8), breaks = NULL, expand = c(0, 0)) +
  labs(title = "Reunirse y tomar un café de calidad son los motivos que el personal percibe como más importantes",
       subtitle = "Promedio de lo respondido (0 a 10) en cada motivo de visita",
       x = NULL, y = NULL,
       caption = str_wrap(sprintf("%s. Importancia percibida, no proporción de clientes: ninguna de las %d encuestas suma 10.", nota, N), 100)) +
  theme(panel.grid.major.x = element_blank(), plot.title = element_text(size = 13))
guardar(g9, "g09_motivos_de_visita.png", alto = 4)

cat("Gráficas guardadas en Output/Graficas:\n")
print(list.files("Output/Graficas"))
cat("\nCorrelaciones: interaccion-habituales r =", round(r4, 3),
    "| experiencia-habituales r =", round(r7, 3),
    "| Spearman gasto: interaccion", round(r8a, 3), ", experiencia", round(r8b, 3), "\n")
