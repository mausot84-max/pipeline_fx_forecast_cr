"""
06_explore_subcuentas.py
========================

Explora la jerarquía del catálogo del SDDE en los puntos clave que
identificó la primera ronda de búsqueda:

  - 1495 = Crédito al sector privado por actividad económica
  - 1494 = Crédito interno total
  - 1415 = Sistema bancario nacional
  - 3650 = Exportaciones FOB (mensual, USD)
  - 3462 = Exportaciones FOB (balanza de pagos desglosada)
  - 805  = Exportaciones de bienes FOB real

Y busca patrones más específicos de actividad económica.

Uso:
    python3 cierre_v6/06_explore_subcuentas.py
"""

from bccr import SW
import pandas as pd
from pathlib import Path

OUT = Path("cierre_v6/outputs")
OUT.mkdir(parents=True, exist_ok=True)

def explora_subcuentas(codigo, label, max_level=4):
    print(f"\n========================================================")
    print(f"SUBCUENTAS de {codigo} — {label}")
    print(f"========================================================")
    try:
        # arbol=True imprime jerarquía visual; capturamos también la lista
        sub = SW.subcuentas(codigo, maxlevel=max_level, arbol=True)
        if sub:
            # Para cada código encontrado, mostrar su descripción
            info = []
            for c in sub:
                try:
                    desc = SW.indicadores.loc[int(c), 'DESCRIPCION']
                    periodo = SW.indicadores.loc[int(c), 'periodo']
                    unidad = SW.indicadores.loc[int(c), 'Unidad']
                except Exception:
                    desc = periodo = unidad = ""
                info.append({"codigo": c, "DESCRIPCION": desc,
                             "Unidad": unidad, "periodo": periodo})
            df = pd.DataFrame(info)
            df.to_csv(OUT / f"bccr_py_subcuentas_{codigo}.csv", index=False)
            print(f"\nGuardado: bccr_py_subcuentas_{codigo}.csv")
            # imprimir descripción de cada uno
            print(df.to_string())
    except Exception as e:
        print(f"  ERROR: {e}")

def busca_y_guarda(label, **kwargs):
    print(f"\n========================================================")
    print(f"BÚSQUEDA — {label}")
    print(f"========================================================")
    try:
        res = SW.buscar(**kwargs)
        if isinstance(res, pd.DataFrame) and not res.empty:
            print(f"OK: {len(res)} filas")
            print(res.head(30).to_string())
            fname = "bccr_py_busca_" + label.lower().replace(" ", "_").replace("/", "_") + ".csv"
            res.to_csv(OUT / fname)
            print(f"Guardado: {fname}")
        else:
            print("  (vacío)")
    except Exception as e:
        print(f"  ERROR: {e}")

# ---- subcuentas del crédito ----
explora_subcuentas(1495, "Crédito al sector privado por actividad económica", max_level=5)
explora_subcuentas(1494, "Crédito interno total", max_level=4)
explora_subcuentas(1415, "Sistema bancario nacional", max_level=4)

# ---- subcuentas de exportaciones ----
explora_subcuentas(3650, "Exportaciones FOB (mensual USD)", max_level=4)
explora_subcuentas(3462, "Exportaciones FOB (balanza desglosada)", max_level=4)
explora_subcuentas(805, "Exportaciones de bienes FOB real", max_level=4)

# ---- búsquedas refinadas ----
busca_y_guarda("Exportaciones agropecuarias", frase="exportaciones agropecuarias")
busca_y_guarda("Exportaciones manufactura", frase="exportaciones manufactura")
busca_y_guarda("Exportaciones banano", frase="exportaciones banano")
busca_y_guarda("Exportaciones tradicionales", frase="exportaciones tradicionales")
busca_y_guarda("Exportaciones no tradicionales", frase="exportaciones no tradicionales")
busca_y_guarda("Exportaciones zona franca", frase="zona franca")
busca_y_guarda("Crédito agropecuario", frase="crédito agropecuario")
busca_y_guarda("Crédito comercio", frase="crédito comercio")
busca_y_guarda("Crédito construcción", frase="crédito construcción")
busca_y_guarda("Cartera por actividad", todos="cartera actividad")
busca_y_guarda("Saldo crédito moneda nacional", todos="saldo crédito moneda")
busca_y_guarda("Saldo crédito moneda extranjera", todos="saldo crédito extranjera")

print("\n[06] Listo. Mandame:")
print("  cierre_v6/outputs/bccr_py_subcuentas_*.csv")
print("  cierre_v6/outputs/bccr_py_busca_*.csv")
