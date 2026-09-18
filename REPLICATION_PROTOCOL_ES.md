# Protocolo de reproducción computacional independiente

## 1. Objetivo

Este protocolo permite que dos investigadores reproduzcan de forma independiente los análisis del repositorio privado `zebrafish-ucs-exploratory-archetypes`, utilizando la misma base de datos y el mismo código del manuscrito.

Este procedimiento constituye una **reproducción computacional**: se vuelven a ejecutar los análisis sobre los datos históricos originales. No constituye una repetición experimental del UCS ni una nueva obtención de datos.

Cada replicador debe trabajar en su propio computador o, como mínimo, en una copia independiente del repositorio. No deben compartir entre sí archivos generados durante el análisis.

## 2. Seguridad y acceso al repositorio

### Regla obligatoria

El propietario **no debe compartir** su contraseña de GitHub, códigos de autenticación de dos factores, token personal, clave SSH ni códigos de recuperación.

Cada replicador debe usar su propia cuenta de GitHub. El acceso al repositorio privado se concede mediante una invitación como colaborador.

### Acciones del propietario del repositorio

Repetir estos pasos para cada colega:

1. Abrir `https://github.com/MisfoldERMind/zebrafish-ucs-exploratory-archetypes`.
2. Hacer clic en **Settings**.
3. En el menú lateral, abrir **Collaborators**.
4. Hacer clic en **Add people**.
5. Buscar el nombre de usuario o correo asociado a la cuenta GitHub del colega.
6. Seleccionar la cuenta correcta y confirmar la invitación.
7. Pedir al colega que acepte la invitación recibida por correo electrónico o dentro de GitHub.

GitHub documenta este procedimiento en [Inviting collaborators to a personal repository](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/repository-access-and-collaboration/inviting-collaborators-to-a-personal-repository).

## 3. Registro previo de cada reproducción

Antes de descargar los archivos, cada replicador debe registrar:

- nombre o código del replicador;
- fecha de inicio;
- sistema operativo;
- identificador completo del commit suministrado por el autor;
- versión de R;
- versión de RStudio;
- versión de IBM SPSS Statistics;
- cualquier desviación respecto de este protocolo.

Para que ambas reproducciones utilicen exactamente los mismos archivos, el autor debe suministrar el mismo identificador de commit a los dos colegas. Un archivo descargado a partir de un commit concreto conserva el mismo contenido aunque posteriormente cambie la rama `main`.

## 4. Descarga de una copia independiente

### Opción recomendada: archivo ZIP del commit de referencia

1. Iniciar sesión en GitHub con la cuenta personal del replicador.
2. Aceptar la invitación al repositorio, si todavía está pendiente.
3. Abrir el enlace al commit proporcionado por el autor.
4. En la vista de archivos de ese commit, hacer clic en **Code**.
5. Hacer clic en **Download ZIP**.
6. Guardar el archivo en el computador.
7. Extraerlo en una ruta local corta y simple, por ejemplo:

   `C:/Replication/zebrafish-ucs-exploratory-archetypes`

8. No trabajar directamente dentro del archivo ZIP.
9. No guardar la copia de trabajo dentro de una carpeta sincronizada por OneDrive, Dropbox o Google Drive durante la ejecución.

GitHub también permite descargar una instantánea desde **Code > Download ZIP**. Para reproducibilidad, debe preferirse el commit indicado por el autor y no una copia cambiante de `main`. Véase [Downloading source code archives](https://docs.github.com/en/repositories/working-with-files/using-files/downloading-source-code-archives).

### Alternativa: GitHub Desktop

El replicador puede usar **Code > Open with GitHub Desktop** y seguir las instrucciones de clonación. Esta alternativa conserva el historial del repositorio, pero no es necesaria para ejecutar los análisis. Véase [Cloning a repository](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository).

## 5. Comprobación inicial de los archivos

Abrir la carpeta extraída y confirmar que contiene, como mínimo:

- `README.md`;
- `01_Data/Raw/Resultados revisados AP.xlsx`;
- `02_R/00_Setup/000_setup_project.R`;
- `02_R/00_Setup/001_load_lapcom_data.R`;
- las carpetas de los Objetivos 1–4;
- `02_R/README.md`;
- `07_Environment/README.md`.

No cambiar el nombre de la base de datos, de las carpetas ni de los scripts. No editar manualmente `Resultados revisados AP.xlsx`.

## 6. Programas necesarios

Instalar:

1. R 4.6.1;
2. RStudio Desktop;
3. IBM SPSS Statistics 27.0.1, utilizado únicamente para Two-Step clustering;
4. Microsoft Excel o un programa compatible, únicamente para revisar el archivo exportado por SPSS.

El entorno de referencia es Microsoft Windows 10. Las versiones registradas de los paquetes se encuentran en `07_Environment/README.md`.

### Paquetes de R

Abrir RStudio y ejecutar una sola vez en la consola:

```r
install.packages(
  c(
    "cluster", "corrplot", "cowplot", "dplyr", "ggplot2",
    "ggrepel", "igraph", "irr", "mclust", "openxlsx",
    "patchwork", "permute", "readxl", "rstatix", "scales",
    "tibble", "tidyr", "vegan"
  ),
  dependencies = TRUE
)
```

Si un paquete ya está instalado, R puede conservarlo o actualizarlo. No modificar los scripts para resolver un error de paquete: registrar primero el mensaje completo y la versión instalada.

## 7. Apertura correcta del proyecto en RStudio

1. Abrir RStudio.
2. Ir a **File > New Project > Existing Directory**.
3. Hacer clic en **Browse**.
4. Seleccionar la carpeta raíz extraída, es decir, la carpeta que contiene `README.md`, `01_Data` y `02_R`.
5. Hacer clic en **Create Project**.
6. En la consola, ejecutar `getwd()`.
7. Confirmar que la ruta mostrada corresponde a la raíz del repositorio.

Los scripts utilizan rutas relativas al repositorio. Por ello, no solicitarán al usuario seleccionar manualmente la planilla de datos. La base debe permanecer en:

`01_Data/Raw/Resultados revisados AP.xlsx`

## 8. Inicialización y auditoría de la base

### Script 000

1. En el panel **Files** de RStudio, abrir `02_R`.
2. Abrir `00_Setup`.
3. Hacer clic en `000_setup_project.R`.
4. Con el script abierto, hacer clic en **Source**.
5. Esperar a que termine la ejecución.

La consola debe informar:

- `LAPCOM UCS REPOSITORY INITIALISED`;
- la raíz identificada;
- la verificación de la base principal;
- el siguiente script: `001_load_lapcom_data.R`.

### Script 001

1. Abrir `001_load_lapcom_data.R`.
2. Hacer clic en **Source**.
3. Confirmar que no aparece ningún mensaje iniciado por `Error`.

La auditoría esperada es:

- 178 individuos;
- 89 Control;
- 89 Stress/UCS;
- cuatro cohortes en `Author`;
- ausencia de valores faltantes o no finitos en las cuatro variables conductuales.

Este script crea `01_Data/Processed/LAPCOM_raw.rds` y `01_Data/Processed/control.rds`.

## 9. Regla general de ejecución

Para cada script:

1. abrirlo desde el panel **Files**;
2. leer el encabezado, donde se indican las dependencias;
3. hacer clic en **Source**;
4. esperar a que regrese el indicador `>` en la consola;
5. confirmar que no hubo `Error`;
6. registrar cualquier advertencia (`Warning`) sin modificar el código;
7. pasar al script siguiente solamente cuando el anterior haya terminado.

No ejecutar scripts simultáneamente y no saltar scripts. Los objetos intermedios se crean en `01_Data/Processed` y `03_Results/Objects`.

## 10. Orden del análisis en R

El índice autoritativo y actualizado es `02_R/README.md`. El orden resumido es el siguiente.

### Objetivo 1 — Arquitectura de referencia derivada de Control

Abrir `02_R/01_Objective1_ControlArchitecture` y ejecutar, en este orden:

1. `005_Figure3_PCA.R`;
2. `006.0_Optimisation_k_Louvain.R`;
3. `006.1_ModularityNullTest_Louvain.R`;
4. `006_Figure4_Louvain.R`;
5. `007_FigureS_ExploratorySegmentSummaries.R`;
6. `008_TableS_ExploratorySegmentSummaries.R`;
7. `009_Table3_ContinuousBehaviouralReference.R`;
8. `010_ClusterStability.R`;
9. `010_LouvainExploratorySubsamplingStability.R`;
10. `010.1_Figure1_Objective1_MainPanel_CORRECTED.R`;
11. `010.2_SupplementaryFigureS1_Objective1.R`.

Control esperado: plateau de equivalencia exacta en `k = 44–46`, `k = 45` para la inferencia de estabilidad y `k = 46` para la proyección posterior de perfiles fijos.

### Objetivo 2 — Redistribución poblacional después de UCS

Abrir `02_R/02_Objective2_PopulationReorganization` y ejecutar todos los scripts definitivos desde `011` hasta `020` en orden numérico. Después ejecutar:

`021_Figure2_Main_PCA_Mahalanobis_CORREGIDO_v4.R`

Los scripts `019` y `020` son dependencias obligatorias de `021`.

Controles esperados del análisis definitivo:

- desplazamiento del centroide 4D: `D4D = 1.264`;
- PERMANOVA parcial: `F(1,173) = 8.10`, `R² = 0.0423`, `p = 0.0016`.

### Objetivo 3 — Organización de novo dentro de la población UCS

Abrir `02_R/03_Objective3_StressPhenotypes` y ejecutar los scripts desde `020` hasta `027` en orden numérico. Después ejecutar el script suplementario `028`.

Controles esperados:

- grafo UCS representativo con `k = 48`;
- dos comunidades de 44 y 45 individuos.

### Objetivo 4 — Definiciones operativas y comparación de métodos

Abrir `02_R/04_Objective4_OperationalDefinitionsOfBehaviouralResilience`.

1. Ejecutar los scripts `027`–`032` en orden numérico.
2. Detener la ejecución de R después del script `032`.
3. Realizar el análisis Two-Step en SPSS según la sección siguiente.
4. Volver a RStudio.
5. Ejecutar los scripts `033`–`035` en orden numérico.
6. Ejecutar los scripts `037`–`039` en orden numérico.

## 11. Etapa Two-Step en IBM SPSS Statistics 27.0.1

### Preparar el archivo generado por R

Después de ejecutar el script `032`, confirmar que existe:

`02_R/04_Objective4_OperationalDefinitionsOfBehaviouralResilience/SPSS_TwoStep/Input/Objective4_TwoStep_Input.xlsx`

Este archivo debe contener 89 peces Stress. Los únicos predictores continuos son:

- `distance_rz`;
- `crossing_rz`;
- `top_entry_rz`;
- `top_time_rz`.

`Author`, `ID` y `Traceability_ID` son identificadores y no deben utilizarse como predictores.

### Abrir los datos

1. Abrir IBM SPSS Statistics 27.0.1.
2. Ir a **File > Open > Data**.
3. Cambiar el tipo de archivo a Excel.
4. Seleccionar `Objective4_TwoStep_Input.xlsx`.
5. Seleccionar la hoja `TwoStep_predictors`.
6. Marcar la opción para leer los nombres de variables desde la primera fila.
7. Confirmar que aparecen 89 filas y las siete columnas esperadas.

### Ejecutar la sintaxis

1. Ir a **File > Open > Syntax**.
2. Abrir:

   `SPSS_TwoStep/Syntax/Objective4_TwoStep_Analysis_CORREGIDO.sps`

3. En la ventana de sintaxis, presionar `Ctrl+A`.
4. Ir a **Run > Selection** o pulsar el botón triangular de ejecución.
5. Esperar a que SPSS genere el visor de resultados.
6. Confirmar una solución automática de dos clústeres, con tamaños de 21 y 68 individuos.
7. Confirmar que la variable `TwoStep_Cluster` fue añadida al final del conjunto de datos activo.

### Exportar la clasificación para R

1. Conservar únicamente estas columnas en el archivo de clasificación:

   - `Author`;
   - `ID`;
   - `Traceability_ID`;
   - `TwoStep_Cluster`.

2. Exportarlas a un libro Excel llamado exactamente:

   `Objective4_TwoStep_Classification_CORREGIDO.xlsx`

3. Nombrar la hoja exactamente `Classification`.
4. Guardar el archivo en:

   `SPSS_TwoStep/Exported_Classifications/`

5. Antes de sustituir el archivo de referencia incluido en la copia descargada, guardarlo fuera de la carpeta de trabajo con el nombre `Objective4_TwoStep_Classification_REFERENCE.xlsx`.
6. Colocar el archivo recién generado con el nombre y ruta exactos esperados por el script `033`.

Al ejecutar `033_Import_TwoStepClassification_CORREGIDO.R`, R debe confirmar 89 peces, dos clústeres y tamaños 21/68. R no vuelve a calcular Two-Step: importa la pertenencia generada en SPSS, audita la trazabilidad y orienta las clases mediante el Behavioural Index heredado.

## 12. Finalización y registro del entorno

Al terminar todos los scripts, ejecutar en la consola de R:

```r
capture.output(
  sessionInfo(),
  file = "03_Results/replication_sessionInfo.txt"
)
```

Cada replicador debe entregar al autor:

1. `03_Results/replication_sessionInfo.txt`;
2. una copia de `03_Results/Tables`;
3. una copia de `03_Results/Figures`;
4. el archivo SPSS generado o su exportación de clasificación;
5. un registro de advertencias y errores;
6. una tabla de concordancia con los controles esperados de este protocolo;
7. confirmación de que no editó la base ni los scripts.

## 13. Criterio de reproducción satisfactoria

La reproducción se considera satisfactoria cuando:

- los dos replicadores parten del mismo commit y de copias independientes;
- los scripts terminan sin errores;
- se verifican 178 peces, distribuidos en 89 Control y 89 Stress;
- se reproducen los controles numéricos principales;
- SPSS reproduce dos clústeres de 21 y 68 individuos;
- las tablas, figuras y clasificaciones concuerdan con los resultados de referencia, salvo diferencias de renderización o redondeo debidamente documentadas;
- cualquier desviación se informa antes de modificar código o datos.

## 14. Limitación actual del entorno

El repositorio documenta R 4.6.1, SPSS 27.0.1 y las versiones principales de los paquetes. Sin embargo, todavía no contiene un `renv.lock` definitivo y faltan en el registro las versiones exactas de `readxl`, `corrplot`, `cowplot` y `scales`. Por tanto, los archivos `sessionInfo()` de ambos replicadores deben conservarse. Antes de la publicación o del archivo permanente, el entorno definitivo debe congelarse a partir del computador que generó los resultados finales.
