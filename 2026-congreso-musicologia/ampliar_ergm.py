from pathlib import Path
import re
p=Path('@2026-congreso-musicologia/presentacion-congreso.qmd')
s=p.read_text(encoding='utf-8')
a=s.index('## Los modelos respaldan la diferenciación relacional')
b=s.index('## Género y participación localizada',a)
s=s[:a]+'''## ERGM: qué queremos explicar

- Preguntamos **qué patrones se asocian con que dos artistas colaboren**, considerando atributos y estructura de la red.
- Para cada pareja elegible registramos **1 = al menos una colaboración** y **0 = ninguna** en el período.
- Estimamos una red por período. La frecuencia de colaboración queda en el análisis descriptivo.
- El ERGM contrasta la red observada con una distribución de redes posibles (Koskinen y Snijders, 2012).

::: {.notes}
**0:45 · acumulado pendiente.**

Después de describir la red y sus comunidades, preguntamos qué patrones siguen asociados con la existencia de un vínculo cuando se consideran conjuntamente. Utilizamos modelos exponenciales de grafos aleatorios, o ERGM. La unidad que se modela es la red completa, de modo que podemos incorporar dependencias entre vínculos mediante estadísticas estructurales (Koskinen y Snijders, 2012).

Aquí simplificamos cada pareja: uno si colaboró al menos una vez y cero si no. No estimamos cuántas canciones compartirá ni predecimos el futuro de su carrera. Ajustamos el modelo por separado en cada período, con el mismo conjunto de términos para facilitar una comparación cautelosa.
:::

## ERGM: construcción y selección del modelo

| Paso | Qué incorporamos |
|:---|:---|
| **M1** | Nivel general de vínculos y restricciones del diseño |
| **M2** | Diferencia entre años de inicio y vínculos Chile–extranjero |
| **M3** | Similitud de género y de tipo de artista |
| **M4** | Presencia de artistas con un único colaborador |

Seleccionamos **M4** por su mejor AIC y BIC en ambos períodos y evaluamos su ajuste mediante redes simuladas (disertación, 2026, pp. 43–50).

::: {.notes}
**0:45 · acumulado pendiente.**

Construimos los modelos por etapas para evaluar qué aporta cada bloque. M1 fija el nivel general de conectividad y respeta restricciones de la muestra. M2 añade distancia entre años de inicio y el contraste nacional. M3 incorpora semejanza de género y distingue solistas de agrupaciones. M4 añade una estadística que cuenta artistas con exactamente un colaborador.

M4 mejora AIC y BIC respecto de los anteriores en ambos períodos. Después simulamos redes bajo el modelo y comparamos su grado, distancias y vecinos compartidos con los datos observados. También revisamos las cadenas de estimación MCMC. La selección combina ajuste, estabilidad y una especificación comparable, sin asumir que el modelo representa toda la vida musical (disertación, 2026, secciones 5.3.2–5.3.4).
:::

## ERGM: lectura de los coeficientes {.grafico}

![](imagenes/coeficientes-ergm.png){height=560 fig-alt="Coeficientes ERGM M4 e intervalos de confianza del 95 por ciento para ambos períodos"}

::: {.compacto}
Los puntos son estimaciones y las líneas sus intervalos del 95 %. El cero marca ausencia de asociación para el término estimado. Verde: formación. Naranja: boom (disertación, 2026, figura 8).
:::

::: {.notes}
**1:00 · acumulado pendiente.**

Primero ubicamos el cero. Para los atributos de las parejas, un coeficiente negativo indica menor propensión condicional a colaborar al aumentar la variable o pertenecer al contraste indicado, considerando los demás términos. Un coeficiente positivo indica la asociación contraria. Los coeficientes no son porcentajes ni probabilidades directas.

La diferencia entre años de inicio es negativa: trayectorias más distantes en el tiempo se asocian con menos vínculos. El contraste Chile–extranjero también es negativo respecto de Chile–Chile. Mismo género y mismo tipo de artista muestran asociaciones positivas.

El término de grado uno requiere otra lectura: es una estadística de la red, no un atributo de la pareja. Su signo positivo favorece configuraciones con más artistas de un solo colaborador. No implica que tener un vínculo cause la formación de otro. Los intervalos ayudan a apreciar la incertidumbre, y la comparación entre colores es descriptiva, no una prueba formal de cambio entre períodos (disertación, 2026, pp. 46 y 51–52).
:::

## ERGM: resultados y alcance

- **Proximidad generacional:** colaborar se asocia con trayectorias iniciadas en años cercanos.
- **Internacionalización selectiva:** los vínculos Chile–extranjero siguen siendo menos probables que los Chile–Chile, dentro del modelo.
- **Periferia persistente:** M4 representa mejor la abundancia de artistas con un solo colaborador.
- **Límite:** las simulaciones subestiman los vecinos compartidos. La cohesión local requiere otras evidencias (disertación, 2026, pp. 50–52).

::: {.notes}
**1:00 · acumulado pendiente.**

Los resultados complementan la descripción y las comunidades. La asociación con proximidad de trayectorias respalda la relevancia de la dimensión generacional. La mayor cantidad de extranjeros no equivale a mezcla transnacional uniforme: el contraste Chile–extranjero permanece negativo. El término de grado uno permite representar mejor una red que conserva numerosos participantes periféricos.

La comparación internacional está limitada al espacio elegible por diseño. Los artistas externos entran por sus vínculos con el núcleo chileno, y los vínculos extranjero–extranjero están restringidos. La especificación también contempla que las redes finales no tienen aislados.

El ajuste reproduce razonablemente patrones amplios de grado y distancias, pero subestima parejas con varios vecinos compartidos. Las alternativas para capturar esa estructura no ofrecen una solución estable y comparable en ambos períodos. Por eso interpretamos la cohesión junto a la transitividad ponderada y las comunidades. Estos resultados son asociaciones, no causas demostradas (disertación, 2026, secciones 5.3.4–5.3.6).
:::

'''+s[b:]
# Reallocate two minutes from introductory/context slides to ERGM.
parts=re.split(r'(?m)(?=^## )',s)
for i,part in enumerate(parts):
    if any(part.startswith('## '+title+'\n') for title in ['El problema de una categoría compartida','Categorías y mundos musicales','Dos períodos de observación','Circuitos distintos dentro del boom']):
        part=part.replace('**1:30 · acumulado','**1:00 · acumulado',1)
    parts[i]=part
s=''.join(parts)
total=0
def timing(m):
    global total
    minutes,seconds=int(m[1]),int(m[2]); total+=minutes*60+seconds
    return f'**{minutes}:{seconds:02d} · acumulado {total//60}:{total%60:02d}.**'
s=re.sub(r'\*\*(\d+):(\d{2}) · acumulado [^*]+\*\*',timing,s)
assert total==1200,total
s=s.replace('Exposición: 19 diapositivas','Exposición: 22 diapositivas')
# Add the methodological source already listed in the supplied dissertation.
s+='''

## Referencia metodológica del ERGM {.appendix}

::: {.referencias}
Koskinen, J. y Snijders, T. A. B. (2012). Simulation, estimation, and goodness of fit. En D. Lusher, J. Koskinen y G. Robins (eds.), *Exponential Random Graph Models for Social Networks: Theory, Methods, and Applications*. Cambridge University Press.
:::

::: {.notes}
Referencia tomada de la bibliografía del documento base. Fuera de los 20 minutos.
:::
'''
p.write_text(s,encoding='utf-8')
p=Path('@2026-congreso-musicologia/LEEME.md')
t=p.read_text(encoding='utf-8').replace('portada y 18 diapositivas de contenido','portada y 21 diapositivas de contenido').replace('Las dos diapositivas de referencias y las dos de gráficos de respaldo','Las tres diapositivas de referencias y las dos de gráficos de respaldo')
t+='\nEl ERGM se explica en cuatro diapositivas: pregunta y variable dependiente, secuencia M1–M4 y selección, lectura del gráfico y resultados con límites. El bloque dispone de 3 minutos y 30 segundos dentro de la exposición de 20 minutos.\n'
p.write_text(t,encoding='utf-8')
print('ERGM ampliado a cuatro diapositivas; tiempo total: 20:00.')
