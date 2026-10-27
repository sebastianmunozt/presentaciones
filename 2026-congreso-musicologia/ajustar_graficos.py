from pathlib import Path
import re
root=Path('@2026-congreso-musicologia')
p=root/'presentacion-congreso.qmd'
s=p.read_text(encoding='utf-8-sig')
parts=re.split(r'(?m)(?=^## )',s)
result=[]
for part in parts:
    if part.startswith('## La red crece y se integra'):
        notes=part[part.index('::: {.notes}'):]
        first='''## Red de formación y consolidación nacional

![](imagenes/red-formacion.png){height=490 fig-alt="Red de formación y consolidación nacional, 2014 a febrero de 2022"}

::: {.compacto}
La red reúne **213 artistas y 410 vínculos**. El componente mayor contiene al **89,7 %** de los artistas (disertación, 2026, tabla 2A).
:::

::: {.notes}
**0:30 · acumulado pendiente.**

La red de formación muestra una estructura de menor tamaño, con 213 artistas y 410 parejas colaboradoras. Verde identifica artistas chilenos y naranja artistas extranjeros. El tamaño de cada nodo representa su número de colaboradores y el grosor de las aristas la intensidad ordinal. Presentamos esta imagen como referencia para observar el cambio de escala en el siguiente período. Las posiciones espaciales responden al algoritmo de disposición y no son distancias geográficas.
:::

'''
        second='''## Red del boom y expansión internacional

![](imagenes/red-boom.png){height=490 fig-alt="Red del boom y expansión internacional, marzo de 2022 a diciembre de 2024"}

::: {.compacto}
La red llega a **564 artistas y 1.579 vínculos**. El componente mayor alcanza el **96,6 %** (disertación, 2026, tabla 2A).
:::

'''+notes.replace('**1:15 · acumulado 8:30.**','**0:45 · acumulado pendiente.**')
        result.extend([first,second])
    elif part.startswith('## Las comunidades conservan diferencias generacionales'):
        notes=part[part.index('::: {.notes}'):]
        first='''## Comunidades durante la formación

![](imagenes/comunidades-formacion.png){height=490 fig-alt="Siete comunidades Leiden durante la formación"}

::: {.compacto}
La partición identifica **siete comunidades**. La diferenciación relacional dialoga con las divisiones generacionales de la escena (Muñoz-Tapia y Pinochet-Cobos, 2025).
:::

::: {.notes}
**0:40 · acumulado pendiente.**

La partición del primer período identifica siete comunidades. Los colores distinguen los agrupamientos, el tamaño de nodo representa grado y las formas distinguen grupos y solistas, según la leyenda del gráfico. La partición utiliza el componente mayor y pesos ordinales de colaboración. Su selección combina modularidad, estabilidad e interpretación. Las diferencias entre rap histórico y agrupamientos urbanos más recientes requieren una lectura histórica y contextual (Muñoz-Tapia y Pinochet-Cobos, 2025).
:::

'''
        second='''## Comunidades durante el boom

![](imagenes/comunidades-boom.png){height=490 fig-alt="Ocho comunidades Leiden durante el boom"}

::: {.compacto}
La partición identifica **ocho comunidades** dentro de una red mayor. Los números y colores son específicos de cada período (disertación, 2026, sección 5.2).
:::

'''+notes.replace('**1:30 · acumulado 12:15.**','**0:50 · acumulado pendiente.**')
        result.extend([first,second])
    elif part.startswith('## Aumentan las colaboraciones recurrentes'):
        notes=part[part.index('::: {.notes}'):]
        result.append('''## Aumentan las colaboraciones recurrentes

![](imagenes/recurrencia.png){height=490 fig-alt="Distribución de parejas por número de colaboraciones en ambos períodos"}

::: {.compacto}
Las parejas con **dos o más pistas** pasan del **17,6 % al 25,1 %**. Predominan los vínculos de una sola pista (disertación, 2026, figura 5).
:::

'''+notes)
    elif part.startswith('## Los modelos respaldan la diferenciación relacional'):
        notes=part[part.index('::: {.notes}'):]
        result.append('''## Los modelos respaldan la diferenciación relacional

![](imagenes/coeficientes-ergm.png){height=490 fig-alt="Coeficientes ERGM M4 e intervalos de confianza del 95 por ciento"}

::: {.compacto}
El modelo identifica proximidad generacional, internacionalización selectiva y participación con un único colaborador. Los coeficientes expresan **asociaciones** (disertación, 2026, figura 8).
:::

'''+notes)
    else: result.append(part)
s=''.join(result)
replacements={
'- Muñoz-Tapia, Arias Salvado y Hammou (2025) entienden música urbana como una categoría que reúne trayectorias heterogéneas.':'- Comprendemos música urbana como una categoría que reúne trayectorias heterogéneas (Muñoz-Tapia, Arias Salvado y Hammou, 2025).',
'- Muñoz-Tapia y Pinochet-Cobos (2025) sitúan su formación en disputas generacionales del caso chileno.':'- Su formación en Chile está atravesada por disputas generacionales (Muñoz-Tapia y Pinochet-Cobos, 2025).',
'- Brackett (2016) y Drott (2013) comprenden el **género** como una configuración histórica de convenciones y relaciones.':'- Comprendemos el **género** como una configuración histórica de convenciones y relaciones (Brackett, 2016; Drott, 2013).',
'- Becker (2008) y Crossley (2020) sitúan la **cooperación** en la base de los mundos artísticos y musicales.':'- La **cooperación** sostiene los mundos artísticos y musicales (Becker, 2008; Crossley, 2020).',
'- White (2008) y Crossley (2025a) articulan **identidades categoriales y vínculos** para explicar formaciones sociales.':'- Las formaciones sociales articulan **identidades categoriales y vínculos** (White, 2008; Crossley, 2025a).',
'- El resultado dialoga con **Hesmondhalgh (2025)**: mayor circulación musical puede coexistir con desigualdades persistentes.':'- Una mayor circulación musical puede coexistir con desigualdades persistentes (Hesmondhalgh, 2025).',
'- Born (2011) permite situar estos resultados en la relación entre música e identidades sociales.':'- La música participa en la materialización de identidades sociales (Born, 2011).',
'- Las comunidades extienden al plano relacional la historia generacional de **Muñoz-Tapia y Pinochet-Cobos (2025)**.':'- Las comunidades extienden al plano relacional la historia generacional de la música urbana chilena (Muñoz-Tapia y Pinochet-Cobos, 2025).',
'- Los resultados dialogan con **White (2008) y Crossley (2025a)**: categorías y vínculos participan en la organización de un mundo musical.':'- Categorías y vínculos participan en la organización de un mundo musical (White, 2008; Crossley, 2025a).',
'Muñoz-Tapia, Arias Salvado y Hammou (2025) muestran que urban music, hurban y música urbana mantienen afinidades, pero no designan formaciones equivalentes.':'Urban music, hurban y música urbana mantienen afinidades, pero no designan formaciones equivalentes (Muñoz-Tapia, Arias Salvado y Hammou, 2025).',
'Para Chile, Muñoz-Tapia y Pinochet-Cobos (2025) reconstruyen una conformación controversial, marcada por diferencias generacionales y por las relaciones entre el rap y nuevas expresiones urbanas.':'En Chile, esta conformación controversial está marcada por diferencias generacionales y por las relaciones entre el rap y nuevas expresiones urbanas (Muñoz-Tapia y Pinochet-Cobos, 2025).',
'Brackett (2016) y Drott (2013) permiten comprender los géneros como configuraciones históricas que articulan sonidos, instituciones, tecnologías e identidades.':'Comprendemos los géneros como configuraciones históricas que articulan sonidos, instituciones, tecnologías e identidades (Brackett, 2016; Drott, 2013).',
'Becker (2008) y Crossley (2020) dirigen la atención a la cooperación que sostiene los mundos artísticos y musicales.':'La cooperación sostiene los mundos artísticos y musicales (Becker, 2008; Crossley, 2020).',
'White (2008), con la noción de catnets, vincula las pertenencias categoriales con las relaciones concretas. Crossley (2025a) retoma el problema de la formación de grupos y las identidades.':'La noción de catnets vincula las pertenencias categoriales con las relaciones concretas (White, 2008). Esta articulación permite abordar la formación de grupos y las identidades (Crossley, 2025a).',
'Hesmondhalgh (2025) ofrece un marco para pensar esta ambivalencia: la ampliación de la circulación musical puede coexistir con desigualdades.':'La ampliación de la circulación musical puede coexistir con desigualdades (Hesmondhalgh, 2025).',
'Siguiendo la preocupación de Born (2011) por música e identidades, esto abre una pregunta sobre las condiciones de participación que requeriría investigación adicional.':'La relación entre música e identidades abre preguntas sobre las condiciones de participación que requieren investigación adicional (Born, 2011).',
'El análisis extiende la historia de Muñoz-Tapia y Pinochet-Cobos (2025) y dialoga con White (2008) y Crossley (2025a).':'El análisis extiende al plano relacional la historia generacional de esta categoría (Muñoz-Tapia y Pinochet-Cobos, 2025). Categorías y vínculos se articulan en la organización de un mundo musical (White, 2008; Crossley, 2025a).'
}
for a,b in replacements.items(): s=s.replace(a,b)
# Recalculate cumulative timing; separating graphs does not lengthen the talk.
total=0
def timing(m):
    global total
    minutes,seconds=int(m[1]),int(m[2]); total+=minutes*60+seconds
    return f'**{minutes}:{seconds:02d} · acumulado {total//60}:{total%60:02d}.**'
s=re.sub(r'\*\*(\d+):(\d{2}) · acumulado [^*]+\*\*',timing,s)
assert total==1200,total
s=s.replace('Exposición: 17 diapositivas','Exposición: 19 diapositivas')
for slide in re.split(r'(?m)^## ',s):
    assert len(re.findall(r'!\[.*?\]\(',slide))<=1
p.write_text(s,encoding='utf-8')
readme=root/'LEEME.md'
t=readme.read_text(encoding='utf-8').replace('portada y 16 diapositivas de contenido','portada y 18 diapositivas de contenido').replace('Las citas autor-año están integradas en las afirmaciones y el guion oral.','Las citas siguen el formato afirmación seguida de referencia entre paréntesis. Cada diapositiva contiene como máximo un gráfico, ampliado para facilitar su lectura.')
readme.write_text(t,encoding='utf-8')
print('19 diapositivas de exposición, 20 minutos, máximo un gráfico por diapositiva.')
