"""
05_discover_via_bccr_py.py
==========================

Consulta el catálogo interno del paquete bccr de Randall Romero para
identificar los códigos SDDE de las series que necesitamos:

  - Exportaciones FOB por actividad económica (cuadro 82 BCCR).
  - Crédito por actividad económica × moneda (cuadro 144 BCCR).
  - Cualquier serie complementaria de exportaciones o crédito útil
    para la pinza empírica.

Uso (desde la raíz del proyecto pipeline_fx_forecast_cr):

    pip install bccr --upgrade
    python cierre_v6/05_discover_via_bccr_py.py

Salidas en cierre_v6/outputs/:

    bccr_py_exportaciones_actividad.csv
    bccr_py_credito_actividad.csv
    bccr_py_credito_moneda.csv
    bccr_py_busqueda_libre.csv  (consultas complementarias)
"""

import os
import sys
import pandas as pd
from pathlib import Path

# ---- credenciales (opcional, si las del paquete fallan) -----------
def cargar_credenciales_renviron():
    """Lee BCCR_EMAIL y BCCR_TOKEN desde .Renviron si existe."""
    env = {}
    renv = Path(".Renviron")
    if renv.exists():
        for line in renv.read_text().splitlines():
            if "=" in line and not line.startswith("#"):
                k, v = line.split("=", 1)
                env[k.strip()] = v.strip()
    return env.get("BCCR_EMAIL"), env.get("BCCR_TOKEN")

# ---- instanciar cliente -------------------------------------------
try:
    from bccr import SW
except ImportError:
    print("ERROR: el paquete bccr no está instalado.")
    print("Corré: pip install bccr --upgrade")
    sys.exit(1)

email, token = cargar_credenciales_renviron()
cliente = SW  # default: usa las credenciales del paquete

# Si SW falla por credenciales (los del paquete caducan a veces),
# intentar instanciar uno con las del usuario.
def crear_cliente_personalizado(email, token):
    from bccr import ServicioWeb
    print(f"[05] Usando credenciales propias del .Renviron: {email}")
    return ServicioWeb(nombre="Mauricio", correo=email, token=token,
                       indicadores=None)

# ---- ejecutar búsquedas -------------------------------------------
OUT = Path("cierre_v6/outputs")
OUT.mkdir(parents=True, exist_ok=True)

def buscar_seguro(cliente_o_funcion, **kwargs):
    """Intenta búsqueda con cliente principal; si falla por
    credenciales, intenta con cliente personalizado."""
    try:
        return cliente_o_funcion.buscar(**kwargs)
    except Exception as e:
        msg = str(e)
        print(f"[05] Búsqueda falló: {msg[:80]}")
        if email and token:
            try:
                custom = crear_cliente_personalizado(email, token)
                return custom.buscar(**kwargs)
            except Exception as e2:
                print(f"[05] También falló con credenciales propias: {e2}")
        return pd.DataFrame()

print("\n========================================================")
print("BÚSQUEDA 1 — Exportaciones FOB por actividad")
print("========================================================")
res1 = buscar_seguro(cliente, todos="exportaciones FOB")
if isinstance(res1, pd.DataFrame) and not res1.empty:
    res1.to_csv(OUT / "bccr_py_exportaciones_fob.csv", index=True)
    print(f"OK: {len(res1)} filas -> bccr_py_exportaciones_fob.csv")
    print(res1.head(20).to_string())

print("\n========================================================")
print("BÚSQUEDA 2 — Exportaciones por actividad (sin 'FOB')")
print("========================================================")
res2 = buscar_seguro(cliente, todos="exportaciones actividad")
if isinstance(res2, pd.DataFrame) and not res2.empty:
    res2.to_csv(OUT / "bccr_py_exportaciones_actividad.csv", index=True)
    print(f"OK: {len(res2)} filas -> bccr_py_exportaciones_actividad.csv")
    print(res2.head(20).to_string())

print("\n========================================================")
print("BÚSQUEDA 3 — Crédito por actividad económica")
print("========================================================")
res3 = buscar_seguro(cliente, todos="crédito actividad")
if isinstance(res3, pd.DataFrame) and not res3.empty:
    res3.to_csv(OUT / "bccr_py_credito_actividad.csv", index=True)
    print(f"OK: {len(res3)} filas -> bccr_py_credito_actividad.csv")
    print(res3.head(40).to_string())

print("\n========================================================")
print("BÚSQUEDA 4 — Crédito por moneda")
print("========================================================")
res4 = buscar_seguro(cliente, todos="crédito moneda")
if isinstance(res4, pd.DataFrame) and not res4.empty:
    res4.to_csv(OUT / "bccr_py_credito_moneda.csv", index=True)
    print(f"OK: {len(res4)} filas -> bccr_py_credito_moneda.csv")
    print(res4.head(20).to_string())

print("\n========================================================")
print("BÚSQUEDA 5 — Cartera de crédito")
print("========================================================")
res5 = buscar_seguro(cliente, todos="cartera crédito actividad")
if isinstance(res5, pd.DataFrame) and not res5.empty:
    res5.to_csv(OUT / "bccr_py_cartera_credito.csv", index=True)
    print(f"OK: {len(res5)} filas -> bccr_py_cartera_credito.csv")
    print(res5.head(40).to_string())

print("\n========================================================")
print("BÚSQUEDA 6 — Saldo crédito sector privado por actividad")
print("========================================================")
res6 = buscar_seguro(cliente, todos="saldo crédito privado")
if isinstance(res6, pd.DataFrame) and not res6.empty:
    res6.to_csv(OUT / "bccr_py_saldo_credito.csv", index=True)
    print(f"OK: {len(res6)} filas -> bccr_py_saldo_credito.csv")
    print(res6.head(40).to_string())

# Subcuentas: si el código 33 (exports total) tiene subcuentas,
# extraerlas. Análogo para 4814 (crédito en colones).
print("\n========================================================")
print("SUBCUENTAS — código 33 (Exportaciones FOB total)")
print("========================================================")
try:
    sub33 = cliente.subcuentas(33, maxlevel=3, arbol=False)
    print(f"Subcuentas de 33: {sub33}")
    pd.DataFrame({"codigo": sub33}).to_csv(OUT / "bccr_py_subcuentas_33.csv", index=False)
except Exception as e:
    print(f"  no se pudieron obtener subcuentas de 33: {e}")

print("\n========================================================")
print("SUBCUENTAS — código 4814 (Crédito sector privado colones)")
print("========================================================")
try:
    sub4814 = cliente.subcuentas(4814, maxlevel=3, arbol=False)
    print(f"Subcuentas de 4814: {sub4814}")
    pd.DataFrame({"codigo": sub4814}).to_csv(OUT / "bccr_py_subcuentas_4814.csv", index=False)
except Exception as e:
    print(f"  no se pudieron obtener subcuentas de 4814: {e}")

print("\n========================================================")
print("SUBCUENTAS — código 4815 (Crédito sector privado dólares, inferido)")
print("========================================================")
try:
    sub4815 = cliente.subcuentas(4815, maxlevel=3, arbol=False)
    print(f"Subcuentas de 4815: {sub4815}")
    pd.DataFrame({"codigo": sub4815}).to_csv(OUT / "bccr_py_subcuentas_4815.csv", index=False)
except Exception as e:
    print(f"  no se pudieron obtener subcuentas de 4815: {e}")

print("\n[05] Listo. Mandame los CSVs de cierre_v6/outputs/ con prefijo bccr_py_*")
print("    Con esos códigos armamos codes_seleccionados.csv extendido")
print("    y corremos download + compute.")
