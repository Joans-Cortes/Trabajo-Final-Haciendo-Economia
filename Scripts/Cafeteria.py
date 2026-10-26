import geopandas as gpd
import matplotlib.pyplot as plt
import osmnx as ox
import pandas as pd

print("1. Descargando datos espaciales de Chapinero...")
place_query = "Chapinero, Bogotá, Colombia"
tags = {"amenity": "cafe"}
gdf = ox.features_from_place(place_query, tags)

# Filtrar solo elementos con nombre registrado
gdf = gdf[gdf["name"].notna()].copy()

print("2. Procesando la base de datos y creando Identificador Único...")
# CORRECCIÓN: Convertir cualquier polígono o edificio en su punto central (centroide)
gdf["geometry"] = gdf.geometry.centroid

gdf = gdf.reset_index(drop=True)
gdf["ID_CAFE"] = [f"CHAP_{str(i+1).zfill(3)}" for i in range(len(gdf))]

# Proyección estándar WGS84
gdf = gdf.to_crs(epsg=4326)
gdf["longitud"] = gdf.geometry.x
gdf["latitud"] = gdf.geometry.y

# Seleccionar columnas clave para la entrega
gdf_clean = gdf[["ID_CAFE", "name", "longitud", "latitud", "geometry"]].copy()
gdf_clean.columns = ["ID_CAFE", "nombre", "longitud", "latitud", "geometry"]

print("3. Exportando archivos para la Entrega 3a...")
# Shapefile
gdf_clean.to_file("cafes_chapinero.shp", encoding="utf-8")

# Base de datos CSV
df_base = pd.DataFrame(gdf_clean.drop(columns="geometry"))
df_base.to_csv("base_datos_cafes.csv", index=False, encoding="utf-8-sig")

print("4. Generando mapa descriptivo...")
fig, ax = plt.subplots(figsize=(8, 10))
gdf_clean.plot(ax=ax, color="darkred", markersize=25, alpha=0.7, label="Cafeterías")
plt.title("Mapa de Ubicación: Cafeterías en Chapinero", fontsize=12, fontweight="bold")
plt.xlabel("Longitud")
plt.ylabel("Latitud")
plt.grid(True, linestyle="--", alpha=0.5)
plt.legend()
plt.tight_layout()
plt.savefig("mapa_chapinero.png", dpi=300)

print("\n¡PROCESO COMPLETADO EXITOSAMENTE!")