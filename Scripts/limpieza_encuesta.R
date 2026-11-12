# ------------------------------------------------------------------
# Limpieza de las respuestas de la encuesta al personal
# Proyecto final - Grupo B2 (Estrategias): cafeterías de Chapinero
#
# Entrada : RawData/Respuestas_con_ID_CAFE.xlsx (hojas "Respuestas" y "Equivalencias")
#           RawData/base_datos_cafes_ampliada.csv (opcional, para las coordenadas)
# Salida  : Output/Datos_Limpios/encuesta_limpia.csv
#           Output/Datos_Limpios/revisiones_pendientes.csv
#
# Ejecutar desde la carpeta raíz del repositorio.
# ------------------------------------------------------------------

library(readxl)
library(dplyr)
library(stringr)
library(readr)

dir.create("Output/Datos_Limpios", recursive = TRUE, showWarnings = FALSE)

# 1. Lectura y nombres cortos ---------------------------------------
ruta <- "RawData/Respuestas_con_ID_CAFE.xlsx"
raw <- read_excel(ruta, sheet = "Respuestas")

# Columnas: ID_CAFE, ID, hora de inicio, hora de fin y las 27 preguntas del Forms
nombres <- c("id_cafe", "id_respuesta", "hora_inicio", "hora_fin",
             "p1_consentimiento", "p2_local", "p3_atiende", "p4_antiguedad",
             "p5_personas", "p6_baristas", "p7_capacitacion", "p8_explica",
             "p9_personaliza", "p10_recuerda", "p11_metodos", "p12_origen",
             "p13_tostion", "p14_grano", "p15_alimentos", "p16_actividades",
             "p17_fidelizacion", "p18_redes", "p19_trabajar", "p20_reunirse",
             "p21_cafe_calidad", "p22_llevar", "p23_habituales", "p24_frecuencia",
             "p25_permanencia", "p26_gasto", "p27_franjas")
raw <- raw[, seq_along(nombres)]   # ignora columnas vacías al final
names(raw) <- nombres

# Si Forms/Excel guardó los números como texto ("3"), se pasan a número
raw <- raw %>% mutate(across(c(p8_explica:p10_recuerda, p19_trabajar:p23_habituales), as.numeric))

# Estado de cada ID (hoja Equivalencias)
equiv <- read_excel(ruta, sheet = "Equivalencias") %>%
  select(id_cafe = ID_CAFE, estado_id = Estado) %>%
  distinct(id_cafe, .keep_all = TRUE)

# 2. Excluir el piloto ------------------------------------------------
# El Excel ya no trae correo ni nombre, así que no hay datos personales que quitar.
n_inicial <- nrow(raw)
datos <- raw %>%
  filter(id_cafe != "Piloto") %>%
  left_join(equiv, by = "id_cafe")
cat("Respuestas leídas:", n_inicial, "| sin el piloto:", nrow(datos), "\n")

# 3. Funciones auxiliares -------------------------------------------
minus <- function(x) str_to_lower(str_squish(as.character(x)))

# Sí/No -> 1/0; "No sé" -> NA (no cuenta como ausencia)
si_no <- function(x) {
  x <- minus(x)
  case_when(
    str_detect(x, "^no s[eé]") ~ NA_real_,
    str_detect(x, "^s[ií]")    ~ 1,
    str_detect(x, "^no")       ~ 0,
    TRUE                       ~ NA_real_
  )
}

# 4. Filtros: consentimiento y quien atiende ------------------------
datos <- datos %>%
  mutate(consiente = si_no(p1_consentimiento),
         atiende   = si_no(p3_atiende))

excluidas <- datos %>% filter(consiente != 1 | atiende != 1 | is.na(consiente) | is.na(atiende))
datos <- datos %>% filter(consiente == 1, atiende == 1)
cat("Excluidas por consentimiento o por no atender:", nrow(excluidas), "\n")

# 5. Recodificación -------------------------------------------------
datos <- datos %>%
  mutate(
    # P4 antigüedad (1 a 3)
    p4_nivel = case_when(
      str_detect(minus(p4_antiguedad), "menos de 3")  ~ 1,
      str_detect(minus(p4_antiguedad), "3 a 12")      ~ 2,
      str_detect(minus(p4_antiguedad), "m[aá]s de 1") ~ 3,
      TRUE ~ NA_real_),

    # P5 personas por turno: número libre, se extrae y se marca
    p5_num        = as.numeric(str_extract(p5_personas, "\\d+")),
    p5_censurado  = str_detect(minus(p5_personas), "m[aá]s de"),
    p5_atipico    = !is.na(p5_num) & p5_num > 15,   # se revisa, no se borra

    p6_barista = si_no(p6_baristas),

    # P7 capacitación (0 a 3); no entra al índice de personal
    p7_nivel = case_when(
      str_detect(minus(p7_capacitacion), "^nunca")           ~ 0,
      str_detect(minus(p7_capacitacion), "solo al entrar")   ~ 1,
      str_detect(minus(p7_capacitacion), "algunas veces")    ~ 2,
      str_detect(minus(p7_capacitacion), "cada mes")         ~ 3,
      TRUE ~ NA_real_),

    # P11 métodos de preparación (selección múltiple)
    m_espresso = as.numeric(str_detect(p11_metodos, "Espresso")),
    m_filtrado = as.numeric(str_detect(p11_metodos, "Filtrados")),
    m_coldbrew = as.numeric(str_detect(p11_metodos, "Cold brew")),
    m_autor    = as.numeric(str_detect(p11_metodos, "Bebidas de autor")),
    n_metodos  = m_espresso + m_filtrado + m_coldbrew + m_autor,

    p12_origen_si = si_no(p12_origen),

    # P13 tostión: propia si es "Local propio" o "En el sitio de venta".
    # "Ninguno" o un proveedor externo (texto libre) no cuenta como propia.
    p13_tostion_propia = case_when(
      str_detect(minus(p13_tostion), "local propio|en el sitio de venta") ~ 1,
      TRUE ~ 0),
    p13_proveedor_externo = !str_detect(minus(p13_tostion),
                              "local propio|en el sitio de venta|ninguno"),

    p14_grano_si = si_no(p14_grano),

    # P15 alimentos (0 a 2) y P16 actividades (0 a 3)
    p15_nivel = case_when(
      str_detect(minus(p15_alimentos), "^no$")          ~ 0,
      str_detect(minus(p15_alimentos), "solo reposter") ~ 1,
      str_detect(minus(p15_alimentos), "comida")        ~ 2,
      TRUE ~ NA_real_),
    p16_nivel = case_when(
      str_detect(minus(p16_actividades), "^nunca")  ~ 0,
      str_detect(minus(p16_actividades), "pocas")   ~ 1,
      str_detect(minus(p16_actividades), "cada mes") ~ 2,
      str_detect(minus(p16_actividades), "cada semana") ~ 3,
      TRUE ~ NA_real_),
    actividades_frecuentes = as.numeric(p16_nivel >= 2),

    p17_fidelizacion_si = si_no(p17_fidelizacion),
    p18_redes_si        = si_no(p18_redes),

    # P24 frecuencia de los habituales (0 a 5); "No sé" queda NA
    p24_nivel = case_when(
      str_detect(minus(p24_frecuencia), "no hay clientes") ~ 0,
      str_detect(minus(p24_frecuencia), "una vez al mes")  ~ 1,
      str_detect(minus(p24_frecuencia), "cada dos semanas") ~ 2,
      str_detect(minus(p24_frecuencia), "una vez a la semana") ~ 3,
      str_detect(minus(p24_frecuencia), "varias veces") ~ 4,
      str_detect(minus(p24_frecuencia), "todos los d") ~ 5,
      TRUE ~ NA_real_),

    # P25 permanencia (1 a 5)
    p25_nivel = case_when(
      str_detect(minus(p25_permanencia), "menos de 15") ~ 1,
      str_detect(minus(p25_permanencia), "15 a 30")     ~ 2,
      str_detect(minus(p25_permanencia), "30 a 60")     ~ 3,
      str_detect(minus(p25_permanencia), "1 a 2")       ~ 4,
      str_detect(minus(p25_permanencia), "m[aá]s de 2") ~ 5,
      TRUE ~ NA_real_),

    # P26 gasto (1 a 5)
    p26_nivel = case_when(
      str_detect(p26_gasto, "Menos de")      ~ 1,
      str_detect(p26_gasto, "10\\.000 a")    ~ 2,
      str_detect(p26_gasto, "20\\.000 a")    ~ 3,
      str_detect(p26_gasto, "30\\.000 a")    ~ 4,
      str_detect(p26_gasto, "45\\.000 o m")  ~ 5,
      TRUE ~ NA_real_),

    # P27 franjas de afluencia (selección múltiple)
    f_sem_manana = as.numeric(str_detect(p27_franjas, "Entre semana en la ma")),
    f_sem_medio  = as.numeric(str_detect(p27_franjas, "Entre semana al medio")),
    f_sem_tarde  = as.numeric(str_detect(p27_franjas, "Entre semana en la tarde")),
    f_fin_manana = as.numeric(str_detect(p27_franjas, "Fines de semana en la ma")),
    f_fin_tarde  = as.numeric(str_detect(p27_franjas, "Fines de semana en la tarde"))
  )

# 6. Controles ------------------------------------------------------
# P19-P22 (motivos de visita): la pregunta decía "de cada 10 clientes, ¿cuántos
# vienen principalmente por cada motivo?", así que las cuatro respuestas debían
# sumar 10. Microsoft Forms no validaba la suma y el personal las respondió como
# una escala de 0 a 10 por motivo: ninguna encuesta suma 10 (totales de 24 a 40).
# No se modifica ningún dato original. Se hace lo siguiente:
#   - motivos_total y ok_motivos_suma10 marcan el problema (queda en revisiones).
#   - motivo_pct_* es un AJUSTE de control: cada respuesta se divide entre la suma
#     de su establecimiento y se expresa en % (suma 100). Supone que el personal
#     quiso indicar el peso relativo de cada motivo; eso no se puede verificar.
#   - Resultado principal: promedio de lo respondido (importancia percibida 0-10),
#     NO proporción de clientes. El ajuste solo sirve como chequeo del orden de los
#     motivos (ver Output/Datos_Limpios/resumen_motivos.csv e informe, sección 8.1).
# Proporciones válidas solo se obtendrían volviendo a preguntar a cada local.
datos <- datos %>%
  mutate(
    motivos_total = p19_trabajar + p20_reunirse + p21_cafe_calidad + p22_llevar,
    ok_motivos_suma10 = motivos_total == 10,
    motivo_pct_trabajar = 100 * p19_trabajar / motivos_total,
    motivo_pct_reunirse = 100 * p20_reunirse / motivos_total,
    motivo_pct_cafe     = 100 * p21_cafe_calidad / motivos_total,
    motivo_pct_llevar   = 100 * p22_llevar / motivos_total,
    ok_p23_rango = p23_habituales >= 0 & p23_habituales <= 10,
    id_pendiente = id_cafe == "REVISAR"
  )

# 7. Índices --------------------------------------------------------
# Personal: promedio de P8 a P10 (P7 tiene otra escala y no se promedia)
# Experiencia: elementos presentes de P11 a P18. "No sé" = NA, no cuenta como ausente.
# Se excluye el espresso porque todos los locales lo ofrecen.
datos <- datos %>%
  mutate(
    indice_personal = rowMeans(cbind(p8_explica, p9_personaliza, p10_recuerda), na.rm = TRUE),
    items_presentes = rowSums(cbind(m_filtrado, m_coldbrew, m_autor, p12_origen_si,
                                    p13_tostion_propia, p14_grano_si,
                                    as.numeric(p15_nivel >= 1),
                                    as.numeric(p16_nivel >= 1),
                                    p17_fidelizacion_si, p18_redes_si), na.rm = TRUE),
    items_validos = rowSums(!is.na(cbind(m_filtrado, m_coldbrew, m_autor, p12_origen_si,
                                         p13_tostion_propia, p14_grano_si,
                                         p15_nivel, p16_nivel,
                                         p17_fidelizacion_si, p18_redes_si))),
    indice_experiencia = items_presentes,
    prop_experiencia   = items_presentes / items_validos,

    # Motivo más frecuente de cada local (si hay empate, se marca)
    motivo_max = pmax(p19_trabajar, p20_reunirse, p21_cafe_calidad, p22_llevar),
    motivo_dominante = case_when(
      rowSums(cbind(p19_trabajar, p20_reunirse, p21_cafe_calidad, p22_llevar) == motivo_max) > 1 ~ "empate",
      p19_trabajar == motivo_max     ~ "trabajar_estudiar",
      p20_reunirse == motivo_max     ~ "reunirse",
      p21_cafe_calidad == motivo_max ~ "cafe_calidad",
      p22_llevar == motivo_max       ~ "llevar",
      TRUE ~ NA_character_)
  )

# 8. Un solo registro por ID_CAFE -----------------------------------
duplicados <- datos %>% count(id_cafe) %>% filter(n > 1, id_cafe != "REVISAR")
datos <- datos %>% arrange(hora_inicio)
if (nrow(duplicados) > 0) {
  cat("Hay ID_CAFE repetidos; se conserva el primer registro de cada uno:\n")
  print(duplicados)
  datos <- datos %>% filter(id_cafe == "REVISAR" | !duplicated(id_cafe))
}

# 9. Coordenadas desde la base (los cafés agregados no las tienen todavía)
if (file.exists("RawData/base_datos_cafes_ampliada.csv")) {
  base <- read_csv("RawData/base_datos_cafes_ampliada.csv", show_col_types = FALSE) %>%
    select(id_cafe = ID_CAFE, nombre_base = nombre, longitud, latitud)
  datos <- left_join(datos, base, by = "id_cafe")
} else {
  # Sin la base de cafés no hay coordenadas: se crean vacías para que el resto del script corra
  datos <- datos %>% mutate(nombre_base = NA_character_, longitud = NA_real_, latitud = NA_real_)
}

# 10. Revisiones pendientes y salida --------------------------------
revisiones <- datos %>%
  transmute(id_respuesta, id_cafe, p2_local, estado_id,
            motivos_no_suman_10 = !ok_motivos_suma10,
            p23_fuera_de_rango = !ok_p23_rango,
            p5_atipico, p5_censurado, id_pendiente,
            sin_coordenadas = is.na(latitud)) %>%
  filter(motivos_no_suman_10 | p23_fuera_de_rango | p5_atipico | p5_censurado |
           id_pendiente | sin_coordenadas | is.na(estado_id) | estado_id != "Confirmado")

# 11. Tipos de variable (ver hoja "Tipos_R" del Excel) -------------
datos <- datos %>%
  mutate(
    across(c(id_respuesta, p5_num, p8_explica:p10_recuerda, p19_trabajar:p23_habituales,
             p4_nivel, p7_nivel, p15_nivel, p16_nivel, p24_nivel, p25_nivel, p26_nivel,
             n_metodos, motivos_total, items_presentes, items_validos, indice_experiencia),
           as.integer),
    across(c(consiente, atiende, p6_barista, m_espresso:m_autor, p12_origen_si,
             p13_tostion_propia, p14_grano_si, p17_fidelizacion_si, p18_redes_si,
             actividades_frecuentes, f_sem_manana:f_fin_tarde),
           as.integer),
    across(c(id_cafe, estado_id, motivo_dominante), as.character)
  )
glimpse(datos)

write_csv(datos, "Output/Datos_Limpios/encuesta_limpia.csv")
write_csv(revisiones, "Output/Datos_Limpios/revisiones_pendientes.csv")

# Resumen de motivos de visita (P19-P22): promedio respondido y promedio ajustado
resumen_motivos <- tibble(
  motivo = c("Trabajar o estudiar", "Reunirse o conversar", "Tomar un café de calidad", "Comprar para llevar"),
  promedio_respondido_0a10 = c(mean(datos$p19_trabajar), mean(datos$p20_reunirse),
                               mean(datos$p21_cafe_calidad), mean(datos$p22_llevar)),
  promedio_ajustado_pct = c(mean(datos$motivo_pct_trabajar), mean(datos$motivo_pct_reunirse),
                            mean(datos$motivo_pct_cafe), mean(datos$motivo_pct_llevar))
) %>%
  mutate(posicion = rank(-promedio_respondido_0a10, ties.method = "min"),
         across(c(promedio_respondido_0a10, promedio_ajustado_pct), ~ round(.x, 2))) %>%
  arrange(posicion)
write_csv(resumen_motivos, "Output/Datos_Limpios/resumen_motivos.csv")

cat("\nRegistros en la base limpia:", nrow(datos), "\n")
cat("Motivos que no suman 10:", sum(!datos$ok_motivos_suma10), "de", nrow(datos), "\n")
cat("Resumen de motivos (P19-P22) guardado en Output/Datos_Limpios/resumen_motivos.csv\n")
cat("Revisiones pendientes:", nrow(revisiones), "\n")
