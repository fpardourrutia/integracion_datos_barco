# integracion_datos_barco
Scripts para integrar la base de datos BARCO Lab

Procedimiento general:
1. Leer exceles con todas las columnas como caracteres para que no se introduzcan
NA's por tipo de datos incompatibles.
2. Homologar nombres de columnas en todos los exceles para que no se dupliquen
columnas equivalentes al unirlos.
3. Unir Exceles con rbind.fill
4. Verificar valores de cada columna para eliminar nombres aproximadamente
duplicados y otros defectos. Enfocarse en particular a homologar llaves naturales
y tipos de datos dentro de una misma columna.
5. Cambiar tipos de datos de todas las columnas al más apropiado.
6. Crear ID's de las tablas a partir de los valores de las columnas de llave
natural (por ejemplo, para proyecto, el nombre del proyecto).
7. Crear cada tabla mediante agrupaciones por su correspondiente ID.
8. Eliminar nombres aproximadamente duplicados y otros defectos que no se hayan
corregido con anterioridad.

