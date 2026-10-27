# Presentación para el congreso de musicología

## Archivos y uso

`presentacion-congreso.qmd` es la fuente editable. Abre el archivo en RStudio y pulsa Render. `presentacion-congreso.html` es la versión compilada. Las imágenes originales están en `imagenes/`; conserva esa carpeta junto al QMD. El HTML incorpora los recursos y puede compartirse como un solo archivo.

```sh
quarto render presentacion-congreso.qmd --to revealjs
quarto preview presentacion-congreso.qmd
```

No requiere ejecutar análisis en R ni Python. Las cifras proceden del PDF, sin recalcular los modelos originales. Pulsa S en la presentación para ver las notas del expositor.

## Estructura actual

La exposición contiene 31 diapositivas, con un tiempo orientativo de 20 minutos. Las referencias y dos gráficos de respaldo quedan fuera de ese tiempo. Conviene ensayar el guion y adaptar el ritmo oral.

El marco conceptual desarrolla música urbana como categoría controversial e hipergénero, la distinción chilena respecto del rap, la noción relacional de género y el SNA. La pregunta central aborda la coherencia y estabilización a través de las colaboraciones.

Los resultados incluyen conclusiones específicas de descriptivos, comunidades y ERGM. El método de comunidades explica Leiden y modularidad ponderada. El bloque ERGM desarrolla la construcción de modelos, los coeficientes, las odds condicionales y sus límites.

Cada diapositiva muestra como máximo un gráfico, centrado y ampliado. Las ocho imágenes originales mantienen sus rótulos en inglés. Las citas siguen la afirmación con referencias entre paréntesis. Las referencias se transcribieron del PDF proporcionado, sin comprobación de las obras originales.

## Identificación

La segunda diapositiva incluye los datos proporcionados por el expositor: Sebastian Matías Muñoz Tapia; ANID, Universidad Alberto Hurtado, CMUS; Fondecyt de Iniciación N.º 11260379; sebastianmunozt@gmail.com.

## Publicación

Sube el QMD y la carpeta de imágenes para conservar la fuente editable. Para publicar mediante GitHub Pages, copia el HTML renderizado como `docs/index.html` en tu repositorio y configura Pages para servir esa carpeta desde la rama elegida. Actualiza esa copia después de cada compilación.

La revisión incorpora género como proceso de categorización de músicas, músicos y públicos, y dos diapositivas comparativas de comunidades entre períodos: estructura y generaciones, apertura internacional y articulaciones locales. No se asume continuidad de membresía entre comunidades numeradas de particiones independientes.
