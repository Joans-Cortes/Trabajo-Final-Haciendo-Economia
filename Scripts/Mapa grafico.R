library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(maptiles)
library(tidyterra)
library(ggrepel)

# 1. Ubicación base (funciona en RStudio, Rscript o consola)
carpeta_actual <- tryCatch({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(p)) dirname(normalizePath(p, mustWork = FALSE)) else getwd()
  } else {
    getwd()
  }
}, error = function(e) getwd())

raiz <- if (tolower(basename(carpeta_actual)) == "scripts") {
  dirname(carpeta_actual)
} else {
  carpeta_actual
}

# 2. Buscar "Base de datos cafes.csv" en rutas comunes (incluyendo Documentos/Documents)
nombre_csv_base <- "Base de datos cafes.csv"
carpeta_usuario <- Sys.getenv("USERPROFILE")
if (!nzchar(carpeta_usuario)) carpeta_usuario <- Sys.getenv("HOME")

rutas_posibles <- c(
  file.path(carpeta_actual, nombre_csv_base),
  file.path(getwd(), nombre_csv_base),
  file.path(raiz, "RawData", nombre_csv_base),
  file.path(raiz, nombre_csv_base),
  file.path(carpeta_usuario, "Documents", nombre_csv_base),
  file.path(carpeta_usuario, "Documentos", nombre_csv_base),
  file.path(carpeta_usuario, "OneDrive", "Documents", nombre_csv_base),
  file.path(carpeta_usuario, "OneDrive", "Documentos", nombre_csv_base)
)

existentes <- rutas_posibles[file.exists(rutas_posibles) & !dir.exists(rutas_posibles)]
ruta_base <- if (length(existentes) > 0) existentes[1] else NA_character_

if (is.na(ruta_base)) {
  raices_busqueda <- unique(c(carpeta_actual, raiz, getwd()))
  raices_busqueda <- raices_busqueda[dir.exists(raices_busqueda)]
  
  hallados <- unlist(lapply(raices_busqueda, function(r) {
    archivos <- list.files(
      path = r,
      pattern = "^Base de datos cafes\\.csv$",
      recursive = TRUE,
      full.names = TRUE
    )
    archivos[!dir.exists(archivos)]
  }))
  
  if (length(hallados) > 0) {
    ruta_base <- hallados[1]
  } else if (interactive()) {
    elegido <- tryCatch(file.choose(), error = function(e) "")
    if (nzchar(elegido) && file.exists(elegido) && !dir.exists(elegido)) {
      ruta_base <- elegido
    }
  }
}

if (is.null(ruta_base) || length(ruta_base) == 0 || is.na(ruta_base) || !nzchar(ruta_base) ||
    !file.exists(ruta_base) || dir.exists(ruta_base)) {
  stop("No se encontró 'Base de datos cafes.csv'. Déjalo en Documentos o junto al script.", call. = FALSE)
}

ruta_base <- normalizePath(ruta_base, mustWork = TRUE)
message("Usando CSV: ", ruta_base)

# Carpetas de salida: Output para CSV y Output/Graficas para la imagen
carpeta_salida <- if (dir.exists(file.path(raiz, "Output"))) {
  file.path(raiz, "Output")
} else {
  dirname(ruta_base)
}

carpeta_graficas <- file.path(carpeta_salida, "Graficas")
dir.create(carpeta_graficas, recursive = TRUE, showWarnings = FALSE)

# 3. Leer base original (detectando , o ;) y filtrar los 18 cafés
primera_linea <- readLines(ruta_base, n = 1, warn = FALSE)
sep_detectado <- if (length(gregexpr(";", primera_linea)[[1]]) > length(gregexpr(",", primera_linea)[[1]])) ";" else ","

df_base <- if (sep_detectado == ";") {
  read_delim(ruta_base, delim = ";", show_col_types = FALSE)
} else {
  read_csv(ruta_base, show_col_types = FALSE)
}

names(df_base) <- trimws(names(df_base))
if (!"longitud" %in% names(df_base)) {
  cand_lon <- intersect(c("LONGITUD", "Longitud", "lon", "LON", "Lon", "x", "X"), names(df_base))
  if (length(cand_lon) > 0) names(df_base)[names(df_base) == cand_lon[1]] <- "longitud"
}
if (!"latitud" %in% names(df_base)) {
  cand_lat <- intersect(c("LATITUD", "Latitud", "lat", "LAT", "Lat", "y", "Y"), names(df_base))
  if (length(cand_lat) > 0) names(df_base)[names(df_base) == cand_lat[1]] <- "latitud"
}

limpiar_coord <- function(x) {
  x_chr <- gsub(",", ".", trimws(as.character(x)), fixed = TRUE)
  suppressWarnings(as.numeric(x_chr))
}

seleccion <- c(
  "CHAP_224" = "La Mata",
  "CHAP_078" = "Azahar",
  "CHAP_270" = "18 El Chico",
  "CHAP_301" = "Kafo",
  "CHAP_265" = "Autóctona",
  "CHAP_024" = "Cultor Contenedor",
  "CHAP_248" = "Kaldivia",
  "CHAP_254" = "Nómada",
  "CHAP_299" = "Juan Barista",
  "CHAP_264" = "Colo",
  "CHAP_074" = "Amor Perfecto",
  "CHAP_298" = "Aria",
  "CHAP_300" = "Lina",
  "CHAP_302" = "Cofibinz",
  "CHAP_178" = "Libertario",
  "CHAP_303" = "Mistral Nogal",
  "CHAP_257" = "San Alberto",
  "CHAP_304" = "Rico"
)

df_18 <- df_base %>%
  mutate(ID_CAFE = trimws(as.character(ID_CAFE))) %>%
  filter(ID_CAFE %in% names(seleccion)) %>%
  mutate(
    etiqueta = recode(ID_CAFE, !!!seleccion),
    longitud = limpiar_coord(longitud),
    latitud  = limpiar_coord(latitud)
  )

# 4. Agregar Bana y guardar CSV de 19 locales
bana <- tibble(
  ID_CAFE = "CHAP_BANA_MANUAL",
  nombre = "Bana",
  longitud = -74.06052802883643,
  latitud = 4.653779155254168,
  origen = "Coordenada proporcionada",
  etiqueta = "Bana"
)

df_19 <- bind_rows(df_18, bana)

filas_na <- df_19 %>% filter(is.na(longitud) | is.na(latitud))
if (nrow(filas_na) > 0) {
  print(filas_na %>% select(any_of(c("ID_CAFE", "nombre", "etiqueta", "longitud", "latitud"))))
  stop("Hay filas con NA en longitud o latitud. Revisa la tabla impresa arriba.", call. = FALSE)
}

ruta_csv_19 <- file.path(carpeta_salida, "cafes_19_chapinero.csv")
write_csv(df_19, ruta_csv_19)

# 5. Proyección espacial y polígono de área para descargar teselas
cafes_sf <- st_as_sf(df_19, coords = c("longitud", "latitud"), crs = 4326) %>%
  st_transform(3857)

bb <- st_bbox(cafes_sf)
xmin_0 <- as.numeric(bb["xmin"])
xmax_0 <- as.numeric(bb["xmax"])
ymin_0 <- as.numeric(bb["ymin"])
ymax_0 <- as.numeric(bb["ymax"])

dx <- (xmax_0 - xmin_0) * 0.16
dy <- (ymax_0 - ymin_0) * 0.16

xlim_r <- c(xmin_0 - dx, xmax_0 + dx)
ylim_r <- c(ymin_0 - dy, ymax_0 + dy)

pol_coords <- matrix(
  c(
    xlim_r[1], ylim_r[1],
    xlim_r[2], ylim_r[1],
    xlim_r[2], ylim_r[2],
    xlim_r[1], ylim_r[2],
    xlim_r[1], ylim_r[1]
  ),
  ncol = 2,
  byrow = TRUE
)

area_tiles <- st_sfc(st_polygon(list(pol_coords)), crs = 3857)
fondo_mapa <- get_tiles(area_tiles, provider = "Esri.WorldStreetMap", zoom = 15, crop = TRUE)

# 6. Graficar y guardar JPG dentro de Graficas
p <- ggplot() +
  geom_spatraster_rgb(data = fondo_mapa) +
  geom_sf(
    data = cafes_sf,
    color = "white",
    fill = "#6F3E1A",
    shape = 21,
    size = 3.6,
    stroke = 1.1
  ) +
  geom_label_repel(
    data = cafes_sf,
    aes(label = etiqueta, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.9,
    fontface = "bold",
    color = "#24160E",
    fill = scales::alpha("white", 0.88),
    label.size = 0.15,
    max.overlaps = 30,
    seed = 42
  ) +
  coord_sf(xlim = xlim_r, ylim = ylim_r, expand = FALSE) +
  labs(
    title = "Preguntamos al personal de 19 cafeterías de especialidad en Chapinero",
    subtitle = "Ubicación de los 19 locales encuestados en Chapinero, Bogotá",
    caption = "Estimaciones del personal de 19 cafeterías de Chapinero; las diferencias son entre estos locales."
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "#555555", hjust = 0, margin = margin(b = 8)),
    plot.caption = element_text(size = 8, color = "#666666", hjust = 0, margin = margin(t = 8)),
    plot.margin = margin(t = 12, r = 15, b = 12, l = 15)
  )

ruta_imagen <- file.path(carpeta_graficas, "g01_mapa_de_los_cafes_fondo.jpg")
ggsave(ruta_imagen, plot = p, width = 11, height = 7.5, dpi = 300)

message("CSV guardado en: ", ruta_csv_19)
message("Mapa guardado en: ", ruta_imagen)