"""
09_explorar_E07_34_35.py
========================

Las series 19984 y 19985 ("Agricultura, ganadería silvicultura,
caza y pesca") están en E07.34.* y E07.35.*. Estas ramas pueden
tener desagregación sectorial × moneda — exactamente lo que
necesitamos para σ^C observado.

Uso:
    python3 cierre_v6/09_explorar_E07_34_35.py
"""

from bccr import SW
import pandas as pd
from pathlib import Path

OUT = Path("cierre_v6/outputs")
OUT.mkdir(parents=True, exist_ok=True)
cat = SW.indicadores

print("========================================================")
print("RAMA E07.34.* (completa)")
print("========================================================")
r34 = cat[cat['cuenta'].fillna("").str.startswith("E07.34")].copy()
print(f"{len(r34)} series")
cols = [c for c in ['codigo','nombre','cuenta','periodo','Unidad'] if c in r34.columns]
print(r34[cols].to_string())
r34.to_csv(OUT / "bccr_py_E07_34_completa.csv")

print("\n========================================================")
print("RAMA E07.35.* (completa)")
print("========================================================")
r35 = cat[cat['cuenta'].fillna("").str.startswith("E07.35")].copy()
print(f"{len(r35)} series")
print(r35[cols].to_string())
r35.to_csv(OUT / "bccr_py_E07_35_completa.csv")

print("\n========================================================")
print("RAMA E07.36.* (completa)")
print("========================================================")
r36 = cat[cat['cuenta'].fillna("").str.startswith("E07.36")].copy()
print(f"{len(r36)} series")
print(r36[cols].to_string())
r36.to_csv(OUT / "bccr_py_E07_36_completa.csv")

print("\n========================================================")
print("RAMA E07.37.* (completa)")
print("========================================================")
r37 = cat[cat['cuenta'].fillna("").str.startswith("E07.37")].copy()
print(f"{len(r37)} series")
print(r37[cols].to_string())
r37.to_csv(OUT / "bccr_py_E07_37_completa.csv")

# Buscar por nombre cualquier serie con actividades CIIU
print("\n========================================================")
print("OTRAS SERIES con nombres de sectores")
print("========================================================")
sectores = ['Construcción', 'Agricultura', 'Manufactura', 'Comercio', 'Transporte',
            'Turismo', 'Industria', 'Inmobiliaria', 'Servicios', 'Información',
            'Profesional']
patron = '|'.join(sectores)
m = cat['nombre'].fillna("").str.contains(patron, case=False, regex=True)
sec = cat[m].copy()
print(f"{len(sec)} series con nombre sectorial")
# Filtrar por las que están en USD o en colones
usd = sec[sec['cuenta'].fillna("").str.endswith(".M.USD")]
crc = sec[sec['cuenta'].fillna("").str.endswith(".M.CRC")]
print(f"  - En USD: {len(usd)}")
print(f"  - En CRC: {len(crc)}")
print(f"\nSeries sectoriales en USD:")
print(usd[cols].head(30).to_string())
print(f"\nSeries sectoriales en CRC (muestra):")
print(crc[cols].head(30).to_string())

# Buscar específicamente cartera o saldo crédito que diga "moneda extranjera"
print("\n========================================================")
print("Búsquedas refinadas — cartera / saldo / colocaciones / activos")
print("========================================================")
for q in [{"todos": "cartera moneda"},
          {"todos": "saldo moneda"},
          {"todos": "colocaciones moneda"},
          {"todos": "activos moneda extranjera"},
          {"todos": "crédito moneda nacional"},
          {"todos": "crédito m/e"}]:
    print(f"\n[{q}]")
    try:
        r = SW.buscar(**q)
        if isinstance(r, pd.DataFrame) and not r.empty:
            print(f"  {len(r)} filas")
            ccols = [c for c in ['codigo','DESCRIPCION','periodo','Unidad'] if c in r.columns]
            print(r[ccols].head(15).to_string())
    except Exception as e:
        print(f"  ERROR: {e}")

print("\n[09] Listo.")
