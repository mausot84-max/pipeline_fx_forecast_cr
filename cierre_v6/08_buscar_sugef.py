"""
08_buscar_sugef.py
==================

Busca en el catálogo de Romero series de crédito de la SUGEF
con desagregación por actividad económica y moneda. El cuadro
SUGEF típicamente distingue entre deudores generadores vs no
generadores y permite cruzar actividad × moneda.

Uso:
    python3 cierre_v6/08_buscar_sugef.py
"""

from bccr import SW
import pandas as pd
from pathlib import Path

OUT = Path("cierre_v6/outputs")
OUT.mkdir(parents=True, exist_ok=True)

def busca(label, **kwargs):
    print(f"\n========================================================")
    print(f"BÚSQUEDA — {label}")
    print(f"========================================================")
    try:
        r = SW.buscar(**kwargs)
        if isinstance(r, pd.DataFrame) and not r.empty:
            print(f"OK: {len(r)} filas")
            cols = [c for c in ['codigo','DESCRIPCION','periodo','Unidad','cuenta'] if c in r.columns]
            print(r[cols].head(40).to_string())
            fname = "bccr_py_sugef_" + label.lower().replace(" ", "_") + ".csv"
            r.to_csv(OUT / fname)
        else:
            print("  (vacío)")
    except Exception as e:
        print(f"  ERROR: {e}")

# --- búsquedas SUGEF y generadores ---
busca("SUGEF", frase="SUGEF")
busca("no_generadores", frase="no generadores")
busca("generadores", frase="generadores")
busca("generadores_divisas", todos="generadores divisas")
busca("deudores_actividad", todos="deudores actividad")
busca("deudores_moneda", todos="deudores moneda")
busca("cartera_moneda_actividad", todos="cartera moneda actividad")
busca("cartera_dolares", todos="cartera dólares")
busca("cartera_colones", todos="cartera colones")
busca("credito_actividad_moneda", todos="crédito actividad moneda")
busca("credito_moneda_extranjera_actividad", todos="crédito extranjera actividad")

# Buscar por prefijo en cuenta del catálogo (E07.* es Crédito; otros prefijos pueden ser SUGEF)
print("\n========================================================")
print("EXPLORACIÓN — prefijos de cuenta E07 (crédito)")
print("========================================================")
cat = SW.indicadores
e07 = cat[cat['cuenta'].fillna("").str.startswith("E07")].copy()
print(f"Series con prefijo E07.*: {len(e07)} filas")
prefijos = e07['cuenta'].str[:14].value_counts().head(30)
print("\nTop 30 sub-prefijos (primeros 14 chars):")
print(prefijos.to_string())
e07.to_csv(OUT / "bccr_py_sugef_e07_todas.csv")

# Buscar exactamente E07.07.07.* (la rama de crédito por actividad económica)
print("\n========================================================")
print("RAMA E07.07.07.* (Crédito al sector privado por actividad)")
print("========================================================")
rama = cat[cat['cuenta'].fillna("").str.startswith("E07.07.07")].copy()
print(f"Series en rama E07.07.07.*: {len(rama)}")
cols = [c for c in ['codigo','nombre','periodo','Unidad','cuenta'] if c in rama.columns]
print(rama[cols].to_string())
rama.to_csv(OUT / "bccr_py_sugef_rama_E07_07_07.csv")

# Buscar otros prefijos potenciales: D=Sistema bancario, etc.
print("\n========================================================")
print("EXPLORACIÓN — qué prefijos de cuenta existen")
print("========================================================")
cuentas = cat['cuenta'].fillna("").str[:3].value_counts().head(20)
print("Top 20 prefijos de 3 chars:")
print(cuentas.to_string())

print("\n[08] Listo. Si aparece algo de SUGEF o generadores, lo proceso.")
