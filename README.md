# Estrategias de las cafeterías de especialidad en Chapinero, Bogotá

Proyecto final – Grupo B2 (Estrategias)

**Curso:** Haciendo Economía
**Universidad:** Universidad del Rosario
**Profesor:** Paul Rodríguez Lesmes
**Semestre:** 2026-2

## Integrantes

- Joan Santiago Cortés
- Juan Esteban Plazas
- Angelica Maria Diaz
- Maria Paula Monroy

## Pregunta de investigación

¿Cómo se relacionan el ambiente, la interacción con el personal y la experiencia de consumo de las cafeterías de especialidad de Chapinero con la frecuencia de visita y el gasto por visita de sus clientes, según lo que reporta el personal que las atiende?

## Descripción

El proyecto estudia las estrategias de las cafeterías de especialidad de Chapinero (Bogotá), agrupadas en tres factores: ambiente, interacción con el personal y experiencia de consumo. Se relacionan con la frecuencia de visita y el gasto por visita de los clientes.

El enfoque es descriptivo y cuantitativo. La información primaria se recoge con una encuesta al **personal que atiende** en cada local (barista, mesero o caja), no a los dueños ni a los clientes. Por eso la frecuencia, el gasto y el motivo de visita son **estimaciones del personal**, no mediciones directas de los clientes, y la unidad de análisis es el **local**.

## Estructura del repositorio

```
README.md
Documentacion/
RawData/
Output/
    Datos_Limpios/
    Figuras/
    Mapeo/
Scripts/
```

| Carpeta | Contenido |
|---|---|
| `Documentacion/` | Documentos del proyecto (ver detalle abajo). |
| `RawData/` | Datos originales sin modificar: `base_datos_cafes.csv`, el shapefile `cafes_chapinero` (con sus archivos `.shx`, `.dbf`, `.prj` y `.cpg`) y `Respuestas_de_la_encuesta.xlsx`. |
| `Output/Datos_Limpios/` | Datos procesados, generados a partir de `RawData/` con los scripts. |
| `Output/Figuras/` | Gráficas de los datos. |
| `Output/Mapeo/` | mapas, documentos de los mapas sacados del script `Cafeteria.py` , incluido `mapa_chapinero.png`. |
| `Scripts/` | Código del proyecto. Hoy incluye `Cafeteria.py`. |

Las carpetas vacías contienen un archivo `.gitkeep` para que Git las conserve.

## Documentación

| Archivo | Descripción |
|---|---|
| `Marco_Teorico.docx` | Marco teórico del proyecto. |
| `Revision_Literatura.docx` | Resumen de una página de la revisión de literatura: qué se conoce, vacíos y ajuste de la pregunta. |
| `Revision_Literatura_PRISMA_final.xlsx` | Documentos sistematizados, registro de búsquedas y flujo PRISMA. |
| `Plan_de_Analisis.docx` | Asignación de cafés por integrante, protocolo de sustitución y análisis estadístico previsto. |

## Revisión de literatura

La búsqueda siguió el flujo PRISMA e incluye literatura académica y literatura gris (documentos del Distrito y notas de prensa). El registro de búsquedas, el flujo PRISMA y el detalle de cada documento están en `Revision_Literatura_PRISMA_final.xlsx`.

**Fecha de la búsqueda:** 16 al 21 de octubre de 2026
**Fuentes consultadas:** Google Scholar, SciELO, Dialnet, bogota.gov.co (Secretaría de Desarrollo Económico) y prensa (La República, Portafolio y El Tiempo)

## Datos

### Inventario espacial de cafés

`Scripts/Cafeteria.py` descarga de OpenStreetMap todos los objetos con la etiqueta `amenity=cafe` que tienen nombre en Chapinero, convierte polígonos en puntos centrales y les asigna un identificador único `ID_CAFE` (CHAP_001, CHAP_002, …). Produce:

- `base_datos_cafes.csv`: 294 registros con `ID_CAFE`, `nombre`, `longitud` y `latitud`.
- `cafes_chapinero.shp`: la misma información como shapefile.
- `mapa_chapinero.png`: mapa descriptivo de los 294 puntos.

Este es un **inventario de cafés**, no una lista de cafeterías de especialidad. La selección de los locales a visitar se hizo aparte, con el criterio de café de especialidad del marco teórico (café o experiencia cafetera con atributos distintivos, como sabor y aroma, origen, procesamiento, sostenibilidad e historia, que el establecimiento comunica mediante la preparación, el servicio y el entorno).

### Selección de locales y trabajo de campo

Se visitarán **20 locales**, 5 por integrante, más una lista de reserva por sector. Si un local no participa tras dos intentos, se reemplaza por uno de la reserva con ubicación, flujo de clientes y perfil de negocio similares. La lista por integrante, las fechas y el protocolo están en `Plan_de_Analisis.docx`.

### Encuesta al personal

Se aplica de forma presencial con un formulario de Microsoft Forms: <https://forms.cloud.microsoft/r/mkMKm7Pn3j>. Tiene un consentimiento informado, 27 preguntas y no pide datos personales de quien responde.

**Piloto:** se probó el formulario con una respuesta (28 de octubre de 2026). Sirve para validar el instrumento y la exportación, y no entra al análisis.

**Periodo de recolección:**

### Variables y para qué sirven

| Preguntas | Variable | Uso en el análisis |
|---|---|---|
| P1 | Consentimiento | Solo se analizan respuestas con "Sí". |
| P2 | Nombre del local | Llave para unir cada respuesta con `ID_CAFE`. |
| P3, P4 | Filtro y antigüedad de quien responde | Verificar que responde quien atiende y describir al informante. |
| P5 a P10 | Personal: personas por turno, baristas, capacitación, explicación del café, personalización, memoria del pedido | Índice de interacción con el personal. |
| P11 a P18 | Oferta: métodos de preparación, café de origen, tostión, café en grano, alimentos, actividades, programa de clientes frecuentes, redes sociales | Índice de experiencia de consumo y verificación de atributos de especialidad. |
| P19 a P22 | Motivo de visita (de cada 10 clientes) | Variable de contexto: composición de motivos por local. |
| P23 a P25 | Clientes habituales, frecuencia de los habituales y permanencia | Resultado: frecuencia de visita (estimada). |
| P26 | Gasto promedio por visita | Resultado: gasto (estimado). |
| P27 | Franjas de mayor afluencia | Descripción de la afluencia; no entra a las asociaciones. |
| Pendiente | Ambiente físico | Ficha de observación del encuestador, todavía por crear. |

## Plan de análisis

El análisis es descriptivo, con asociaciones exploratorias, porque hay unos 20 locales y una persona encuestada por local:

1. **Limpieza y control:** que los motivos (P19 a P22) sumen 10, que P23 esté entre 0 y 10 y que haya un solo registro por local.
2. **Descriptivo:** frecuencias y porcentajes, medianas y rangos, y gráficos de barras.
3. **Índices simples** de interacción con el personal y de experiencia de consumo, por local.
4. **Asociaciones exploratorias** entre los índices y los resultados (correlación de Spearman, comparación por grupos), sin interpretarlas como causalidad.
5. **Mapas** descriptivos de los locales visitados.

## Cómo reproducir los resultados

**Lenguaje:** Python
**Paquetes:** `geopandas`, `matplotlib`, `osmnx`, `pandas`
**Fecha de descarga de OpenStreetMap:**

Orden de ejecución:
1. `Scripts/Cafeteria.py`: genera el CSV, el shapefile y el mapa. El script guarda los archivos en la carpeta desde la que se ejecuta, así que hay que moverlos a `RawData/` y `Output/Figuras/`.
2. Scripts de limpieza y análisis de la encuesta: pendientes.
3. Archivo de las encuestas con variables y el piloto en `RawData/` Como: Respuestas de la encuesta

## Estado del proyecto

- [x] Pregunta de investigación
- [x] Revisión de literatura y marco teórico
- [x] Inventario espacial de cafés (script, CSV, shapefile y mapa)
- [x] Diseño de la encuesta, plan de análisis y piloto
- [ ] Ficha de observación del ambiente
- [ ] Trabajo de campo
- [ ] Análisis de datos
- [ ] Resultados finales

## Contacto

**Correo de contacto:** joans.cortes@urosario.edu.co
