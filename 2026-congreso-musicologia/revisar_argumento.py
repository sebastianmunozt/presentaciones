from pathlib import Path
import re
p=Path('@2026-congreso-musicologia/presentacion-congreso.qmd')
s=p.read_text(encoding='utf-8')
def slide(title,body,notes):
    return f'## {title}\n\n{body.strip()}\n\n::: {{.notes}}\n**0:40 · acumulado pendiente.**\n\n{notes.strip()}\n:::\n\n'
def replace(title,new):
    global s
    pat=r'(?ms)^## '+re.escape(title)+r'\n.*?(?=^## |\Z)'
    s,n=re.subn(pat,lambda m:new,s,count=1)
    assert n==1,title
front=slide('Autoría y proyecto', '''**[Nombre completo por confirmar]**

[Afiliación institucional por confirmar]

**ANID · Fondecyt de Iniciación**

[Nombre o número del proyecto por confirmar]

[Correo por confirmar]''','Presentar autoría, afiliación y financiamiento. Los campos entre corchetes están pendientes de los datos del expositor. ANID se identifica como organismo de financiamiento, sin asumir que sea la institución de afiliación laboral.')
front+=slide('Música urbana: una categoría controversial', '''- La categoría reúne reggaetón, trap, dembow, mambo chileno, RKT y rap, con fronteras variables (Muñoz-Tapia, Arias Salvado y Hammou, 2025).
- En Chile, el rap histórico vinculado al hip hop y al boom bap suele distinguirse de lo reconocido como **urbano** (Muñoz-Tapia y Pinochet-Cobos, 2025).
- Su delimitación implica disputas de pertenencia, generación y legitimidad.''','El problema de partida es la controversia: quién entra en la categoría y cómo se nombran las diferencias. La delimitación analítica amplia incorpora rap, pero esto no implica que todos los artistas se identifiquen como urbanos. En el caso chileno, las trayectorias históricas del rap se distinguen con frecuencia de las nuevas formaciones urbanas. Se trata de fronteras históricas y discutidas, no de una separación absoluta ni de ausencia de intercambios (Muñoz-Tapia y Pinochet-Cobos, 2025).')
front+=slide('Música urbana como hipergénero', '''- Un **hipergénero** agrupa géneros y prácticas heterogéneas bajo una clasificación amplia (Buch, 2013, 2018).
- Su coherencia puede descansar en circuitos, públicos e infraestructuras compartidas, aun con diferencias sonoras.
- Música urbana articula variantes locales y transnacionales como un **hipergénero glocal** (Muñoz-Tapia, Arias Salvado y Hammou, 2025).''','El hipergénero permite comprender la escala de la clasificación. Rap, trap y reggaetón conservan convenciones e historias propias, mientras una categoría más amplia los conecta de manera variable. Glocal señala que las conexiones transnacionales se traducen y reorganizan en contextos locales. Por eso música urbana no significa exactamente lo mismo en Chile que urban music en Estados Unidos. La controversia sobre el rap es parte de este proceso de articulación, no una anomalía que debamos borrar (Buch, 2013, 2018; Muñoz-Tapia, Arias Salvado y Hammou, 2025).')
replace('El problema de una categoría compartida',front)
replace('Preguntas de investigación',slide('Pregunta central: coherencia y estabilización', '''**¿Cómo adquiere coherencia y se estabiliza una formación musical tan heterogénea?**

Estudiamos esa estabilización a través de las **colaboraciones entre artistas**.

- ¿Cómo se expande e integra la red?
- ¿Qué fronteras generacionales e internacionales persisten?
- ¿Cómo se distribuye la participación?''','La pregunta central vincula heterogeneidad y estabilización. Buscamos entender cómo una formación musical controversial adquiere continuidad mediante relaciones observables. La estabilización se examina a través de la integración de la red, la recurrencia de colaboraciones y la organización de agrupamientos diferenciados. Es una interpretación de conjunto: ninguna medida aislada prueba la estabilización de una categoría. Tampoco afirmamos que las colaboraciones sean su único mecanismo.'))
replace('Categorías y mundos musicales',slide('Género: convenciones, relaciones e historia', '''- Comprendemos el género como una **configuración histórica de convenciones y relaciones** (Brackett, 2016; Drott, 2013).
- Las convenciones orientan sonidos, actuaciones y expectativas. La circulación y las instituciones transforman sus significados.
- El género articula elementos musicales y sociales: tecnologías, imágenes, públicos e identidades (Brackett, 2016; Born, 2011).''','Comprender el género históricamente implica preguntar cómo se reconocen y cambian sus convenciones. Los sonidos importan, pero se articulan con públicos, modos de presentación e instituciones. La idea de ensamblaje subraya que esos elementos heterogéneos pueden reorganizarse y que su relación no es necesaria ni fija. Las fronteras de género dependen de prácticas de clasificación y reconocimiento, y también participan en la producción de identidades (Brackett, 2016; Drott, 2013; Born, 2011).')+slide('Colaboraciones y estabilización de un mundo musical', '''- La producción musical descansa en **cooperación y convenciones** (Becker, 2008).
- Los mundos musicales adquieren continuidad mediante relaciones y prácticas recurrentes (Crossley, 2020).
- Las categorías orientan afinidades y las relaciones pueden reforzar o modificar pertenencias (White, 2008; Crossley, 2025a).

**Observamos una dimensión relacional de la estabilización.**''','Las colaboraciones acreditadas permiten seguir una capa de cooperación pública y acumulativa. Su repetición y distribución muestran continuidades y fronteras dentro del mundo musical. Las pertenencias categoriales pueden orientar con quién resulta legítimo o deseable colaborar, mientras los vínculos reiterados pueden reforzar esas pertenencias. El análisis observa patrones compatibles con esa articulación sin probar la dirección causal entre clasificación y colaboración (White, 2008; Crossley, 2025a).')+slide('SNA: análisis de redes sociales', '''- El **análisis de redes sociales**, o **SNA** por sus siglas en inglés, estudia actores, vínculos y sus configuraciones.
- Permite analizar posiciones centrales y periféricas, integración, agrupamientos y patrones de asociación.
- En el estudio de mundos musicales, complementa la historia con evidencia sobre **quién coopera con quién y cómo se organiza esa cooperación** (Crossley, 2020, 2025a).

Aquí combinamos **descriptivos, comunidades y ERGM**.''','SNA significa Social Network Analysis, en español análisis de redes sociales. Su objeto son relaciones entre actores y las estructuras que forman. Aquí no se refiere específicamente a redes sociales digitales como Instagram. En la perspectiva de mundos musicales, permite examinar las relaciones que sostienen producción y circulación, en diálogo con la interpretación histórica (Crossley, 2020). Los descriptivos resumen el conjunto, las comunidades identifican agrupamientos y los ERGM evalúan asociaciones condicionales entre atributos, estructura y existencia de vínculos.'))
# Add section-specific synthesis.
insert=s.index('## Comunidades durante la formación')
s=s[:insert]+slide('Conclusiones de los descriptivos', '''- **Integración:** más artistas forman parte de una estructura conectada.
- **Continuidad:** aumenta la proporción de colaboraciones recurrentes.
- **Desigualdad:** la participación típica sigue siendo reducida, con mediana de dos colaboradores.

La estabilización relacional combina **integración y recurrencia**, conservando diferencias de participación (disertación, 2026, pp. 23–30).''','Los descriptivos muestran crecimiento con mayor integración y más recurrencia. Esta combinación aporta indicios de continuidad del espacio colaborativo, mientras la mediana estable y la periferia impiden equiparar consolidación con participación homogénea. El crecimiento corresponde al campo visible en Spotify y las ventanas tienen distinta duración. Estas cautelas delimitan lo que podemos afirmar sobre estabilización.')+slide('Comunidades: qué hace el método Leiden', '''- Una comunidad reúne artistas con **más peso de colaboración interno del esperado según su conectividad**.
- Leiden busca una partición que optimice la **modularidad ponderada** del componente mayor.
- Utilizamos pesos ordinales: **una, dos y tres o más pistas**.
- Comparamos resoluciones y ejecuciones, y contrastamos con Louvain: ajuste, estabilidad, tamaños e interpretación (disertación, 2026, pp. 30–34).''','La técnica distribuye artistas en grupos y evalúa si el peso de sus conexiones internas supera un patrón de referencia que considera su conectividad ponderada. Esa comparación se resume en la modularidad. El parámetro de resolución cambia el nivel de detalle de la partición, por lo que no fijamos el número de grupos de antemano. Repetimos ejecuciones y comparamos alternativas para evitar depender de una sola solución. Leiden favorece comunidades internamente conectadas y Louvain aporta un contraste. Una comunidad matemática no equivale automáticamente a un género ni demuestra una identidad reconocida por los propios artistas.')+s[insert:]
# Place the communities conclusion after examples and before the international interpretation.
insert=s.index('## La internacionalización es selectiva')
s=s[:insert]+slide('Conclusiones de las comunidades', '''- La integración global conserva **agrupamientos diferenciados**.
- Persisten trayectorias de rap histórico junto a circuitos urbanos más recientes.
- La presencia extranjera y la intensidad colaborativa varían entre comunidades.

El hipergénero articula diferencias **sin suprimir sus fronteras internas** (Muñoz-Tapia y Pinochet-Cobos, 2025; Muñoz-Tapia, Arias Salvado y Hammou, 2025).''','Las comunidades muestran una estabilización diferenciada: la categoría conecta circuitos que no se vuelven equivalentes. La separación entre rap histórico y nuevas trayectorias urbanas adquiere una expresión relacional, aunque no es absoluta. La interpretación se apoya en la historia y las trayectorias, y no únicamente en colores del algoritmo. Los números de comunidad son específicos de cada período y no identifican una continuidad directa entre grupos.')+s[insert:]
# Elaborate coefficient interpretation with a defensible numeric example from source tables.
insert=s.index('## ERGM: lectura de los coeficientes')
s=s[:insert]+slide('ERGM: cómo interpretar un coeficiente', '''- El coeficiente actúa sobre las **log-odds condicionales** de un vínculo: depende de la red y de los demás términos.
- Para un atributo de pareja, **exp(β)** expresa un multiplicador de odds, con el resto constante.
- Ejemplo del boom: **β = −1,00** para Chile–extranjero implica **exp(−1) ≈ 0,37** veces las odds de Chile–Chile.
- Esto equivale a un 63 % menos de **odds**, no de probabilidad (disertación, 2026, tabla 7B).''','Las odds son la razón entre la probabilidad de que exista el vínculo y la de que no exista. El coeficiente se interpreta condicionalmente: mantenemos los otros atributos y la configuración relevante del resto de la red. En un ERGM, cada estadística aporta su coeficiente multiplicado por el cambio que produciría agregar ese vínculo. Para el indicador Chile–extranjero, ese cambio es uno y podemos exponenciar directamente su coeficiente. El resultado de 0,37 expresa odds relativas, no una probabilidad de colaboración del 37 por ciento. La probabilidad requiere sumar todas las contribuciones del modelo y aplicar la transformación logística. El término de grado uno exige calcular cómo cambian los grados de ambos extremos y no admite esa lectura simple como atributo de pareja.')+s[insert:]
s=s.replace('## ERGM: resultados y alcance','## Conclusiones del ERGM')
# Strengthen general conclusions around stabilization.
s=s.replace('## Conclusiones: aporte a la musicología','## Conclusiones: estabilización de un hipergénero')
s=s.replace('- Categorías y vínculos participan en la organización de un mundo musical (White, 2008; Crossley, 2025a).','- El hipergénero adquiere continuidad mediante colaboraciones, mientras conserva diferencias y controversias internas (White, 2008; Crossley, 2025a).')
# Add source references already present in the supplied text.
s+='''

## Referencias sobre hipergénero {.appendix}

::: {.referencias}
Buch, E. (2013). Le duo de la musique savante et la musique populaire: Genres, hypergenres et sens commun. En E. Pedler y J. Cheyronnaud (eds.), *Théories ordinaires*. Éditions de l’EHESS.

Buch, E. (2018). La musique classique est-elle un genre? Quelques remarques sur les pratiques de catégorisation à l’ère numérique. En S. Dorin (ed.), *Déchiffrer les publics de la musique classique: Perspectives comparatives, historiques et sociologiques*. Éditions des Archives Contemporaines.
:::

::: {.notes}
Referencias transcritas de la bibliografía del documento base. Fuera de los 20 minutos.
:::
'''
# Allocate 20 minutes in 5-second increments according to section needs.
parts=re.split(r'(?m)(?=^## )',s)
main=[i for i,v in enumerate(parts) if '{.appendix' not in v.split('\n',1)[0]]
weights=[]
for i in main:
    title=parts[i].split('\n',1)[0]
    w=40
    if i==0:w=20
    elif 'Autoría' in title:w=15
    elif 'cómo interpretar un coeficiente' in title:w=65
    elif 'qué hace el método' in title:w=60
    elif 'Género:' in title or 'SNA:' in title:w=50
    elif '.grafico}' in title:w=35
    elif 'Conclusiones' in title:w=30
    weights.append(w)
units=[max(3,round(w/sum(weights)*240)) for w in weights]
while sum(units)<240:
    j=max(range(len(units)),key=lambda k:weights[k]/sum(weights)*240-units[k]);units[j]+=1
while sum(units)>240:
    j=max(range(len(units)),key=lambda k:units[k]-weights[k]/sum(weights)*240);units[j]-=1
total=0
for i,u in zip(main,units):
    sec=u*5;total+=sec
    timing=f'**{sec//60}:{sec%60:02d} · acumulado {total//60}:{total%60:02d}.**'
    parts[i]=re.sub(r'\*\*\d+:\d{2} · acumulado [^*]+\*\*',timing,parts[i],count=1)
s=''.join(parts)
# Update metadata comments without tying to stale prior counts.
s=re.sub(r'<!-- Completar author.*?-->',f'<!-- Exposición: {len(main)} diapositivas, 20 minutos. Referencias y respaldo fuera del tiempo.\nIdentificación pendiente de los datos del expositor en la segunda diapositiva.\nCitas y referencias basadas en el PDF proporcionado. -->',s,flags=re.S)
assert total==1200
for part in parts: assert len(re.findall(r'!\[.*?\]\(',part))<=1
p.write_text(s,encoding='utf-8')
print(f'{len(main)} diapositivas de exposición; 20:00; identificación pendiente.')
