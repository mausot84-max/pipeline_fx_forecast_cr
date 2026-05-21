"""
07_describir_codes.py
=====================

Recupera descripciones de los códigos identificados en los pasos
05 y 06 directamente del catálogo en memoria de SW.indicadores,
con manejo robusto de tipos.

Uso:
    python3 cierre_v6/07_describir_codes.py
"""

from bccr import SW
import pandas as pd
from pathlib import Path

OUT = Path("cierre_v6/outputs")
OUT.mkdir(parents=True, exist_ok=True)

cat = SW.indicadores
print(f"Catálogo cargado: {len(cat)} indicadores")
print(f"Columnas: {list(cat.columns)}")
print(f"Tipo de índice: {cat.index.dtype}")
print()

def describir(codigos, label):
    """Filtra el catálogo por una lista de códigos y devuelve DataFrame."""
    # Convertir códigos a tipo del índice
    if cat.index.dtype == 'object':
        codigos = [str(c) for c in codigos]
    else:
        codigos = [int(c) for c in codigos]
    presentes = [c for c in codigos if c in cat.index]
    ausentes = [c for c in codigos if c not in cat.index]
    if not presentes:
        print(f"  ({label}) ningún código presente en el catálogo")
        return pd.DataFrame()
    df = cat.loc[presentes].copy()
    df['codigo'] = presentes
    print(f"\n=== {label} ===")
    print(f"Presentes: {len(presentes)}/{len(codigos)}")
    if ausentes:
        print(f"Ausentes: {ausentes[:20]}{'...' if len(ausentes)>20 else ''}")
    print(df[['codigo', 'DESCRIPCION', 'Unidad', 'periodo']].to_string())
    df.to_csv(OUT / f"bccr_py_desc_{label}.csv")
    return df

# 1. Subcuentas de 1495 (crédito por actividad)
codes_credito = [1495, 1415, 1416, 1417, 1418, 1419, 1420, 1421, 1422,
                 1423, 1424, 1425, 1426, 1427, 1428, 1429, 1430,
                 1496, 1497, 1498, 1499, 1500, 1501, 1502, 1503,
                 1504, 1505, 1506, 1507, 1508, 1509, 1510]
describir(codes_credito, "credito_actividad_1495")

# 2. Subcuentas de 1415 (sistema bancario nacional)
codes_sbn = [1415, 1416, 1417, 1418, 1419, 1420, 1421, 1422, 1423,
             1424, 1425, 1426, 1427, 1428, 1429, 1430]
describir(codes_sbn, "sbn_1415")

# 3. Subcuentas de 3650 (exportaciones FOB mensual)
codes_exp = [3650, 33, 37849, 41318, 41320, 41322,
             37969, 37970, 37971, 37972, 37973, 37974, 37975, 37976, 37977,
             37978, 37979, 37980, 37981, 37982, 37983, 37984, 37985, 37986,
             37987, 37988, 37989, 37990, 37991, 2103]
describir(codes_exp, "exports_3650")

# 4. Subcuentas de 1494 (crédito interno total) — el "+" indica
#    series adicionales (23xxx, 89xxx) que pueden ser por moneda.
codes_int = [1494, 1304, 1413, 1467, 3641, 1307, 1308, 1309, 1310,
             20848, 23684, 23685, 23686, 23687, 23688, 23689, 23690,
             23691, 23692, 23693, 23694, 23695, 23696,
             89724, 89725, 89726, 89727, 89728, 89729, 89730,
             89731, 89732, 89733, 89734, 89735, 89736, 89737, 89738,
             89739, 89740, 89741]
describir(codes_int, "credito_interno_1494")

# 5. Búsquedas de patrones específicos que faltaron
print("\n========================================================")
print("BÚSQUEDAS COMPLEMENTARIAS")
print("========================================================")

def busca(label, **kwargs):
    try:
        r = SW.buscar(**kwargs)
        if isinstance(r, pd.DataFrame) and not r.empty:
            print(f"\n[{label}] {len(r)} filas")
            print(r.head(15).to_string())
            r.to_csv(OUT / f"bccr_py_busca2_{label.lower().replace(' ', '_')}.csv")
    except Exception as e:
        print(f"  [{label}] error: {e}")

busca("Crédito_colones", todos="crédito colones")
busca("Crédito_dolares", todos="crédito dólares")
busca("Crédito_extranjera", todos="crédito moneda extranjera")
busca("Saldos_actividad", todos="saldo actividad económica")
busca("Bienes_actividad", todos="exportaciones bienes actividad")
busca("Exportaciones_regimen", todos="exportaciones régimen")
busca("Exportaciones_definitivo", frase="régimen definitivo")
busca("Exportaciones_especial", frase="régimen especial")
busca("Cuadro_82", frase="cuadro 82")

print("\n[07] Listo. Pegame especialmente:")
print("  bccr_py_desc_credito_actividad_1495.csv")
print("  bccr_py_desc_exports_3650.csv")
print("  bccr_py_desc_credito_interno_1494.csv")
