# Reportes de Trayectoria Educativa

Este archivo corresponde al registro de la **7ma versión de los reportes**, en él se encontrarán con la estructura que poseen el repositorio alojado en github y el paso a paso para poder generar los documentos, archivos y proceso de debug

## El proyecto

Este proyecto está organizado en 5 carpetas principales (códigos, imágenes génericas, imágenes temporales, inputs y outputs)

1.  Códigos:

    1.  Acá se encuentran los códigos de los backend (script de R) en el que se ejecuta gran parte del procesamiento de las bases y análisis de datos.

    2.  También se encuentran los frontend (Rmkd), acá se encuentra todo el formato del pdf que se genera.

    3.  **Actor clave**: Según corresponda cada actor clave (rbd, sostenedor, seremi, deprov, nacional) tiene una carpeta donde se encuentra su propio backend y front end.

    4.  **(NEW)** CSV to Rds: Para poder hacer más ágil el proceso de sincronización, se decidió trabajar con las bases en formato Rds, para permitir que se pueda subir a las 5 máquinas virtuales a la vez la misma información.

2.  Imágenes Génericas: Esta carpeta tiene los logos ministeriales junto también con algunas fuentes que se requieren.

3.  Inputs: En esta carpeta se cargan los 2 archivos fijos (A) Nombre_grados y (B) Tabla variables.*Nota:es posible agregar un tercer archivo fijo (C) que sea la carta al ministro.*

4.  Imágenes Temporales: Esta carpeta es donde se guardan todas las imágenes generadas del Loop del `backend` para poder ser insertadas en cada pdf una vez que se corre el código.

5.  Outputs: Lugar donde se almacenan los pdf, .tex y xlsx del tratamiento de datos. *Nota*: Esta carpeta también tiene una estructura interna según la máquina virtual que se esté utilizando y el actor clave que estemos analizando.

## Paso a Paso 

1. Se debe ejecutar el código `csv_to_rda` para cambiar las bases y asi usar menos espacio en el repositorio (*Al ser un repo libre el espacio es de 2 gb* (creo))
