from pathlib import Path
import re
p=Path('@2026-congreso-musicologia/presentacion-congreso.qmd')
s=p.read_text(encoding='utf-8')
def slide(title,body,notes,duration):
 return f'## {title}\n\n{body}\n\n::: {{.notes}}\n**{duration} · acumulado pendiente.**\n\n{notes}\n:::\n\n'
def replace(title,new):
 global s
 s,n=re.subn(r'(?ms)^## '+re.escape(title)+r'\n.*?(?=^## |\Z)',lambda m:new,s,count=1)
 assert n==1
replace('Género: convenciones, relaciones e historia',slide('Géneros como procesos de categorización', '''- Los géneros se producen al **poner nombre a músicas, músicos y públicos**, agrupándolos y diferenciándolos históricamente (Brackett, 2016).
- Esas clasificaciones generan expectativas sobre sonidos, prácticas y pertenencias. Sus fronteras se negocian y disputan (Brackett, 2016; Drott, 2013).
- La categorización se articula con **redes e identidades**: orienta afinidades y las relaciones pueden reforzar o transformar sus significados (Born, 2011; White, 2008; Crossley, 2025a).''', '''Entendemos los géneros como procesos de categorización. Poner nombre a músicas, músicos y públicos permite reconocer afinidades, establecer diferencias y organizar expectativas. Es un proceso histórico en el que participan artistas, públicos, medios, industria y plataformas (Brackett, 2016). Las convenciones son parte de ese proceso: cambian con la circulación y la disputa sobre qué pertenece a cada género (Drott, 2013).

Las categorías también ofrecen posiciones de identificación. Nombrar a alguien como rapero o artista urbano puede orientar expectativas sobre sus públicos y colaboradores. Las redes son los canales donde esas identificaciones circulan y se ponen en práctica, y las relaciones pueden a su vez modificar las fronteras categoriales (Born, 2011; White, 2008; Crossley, 2025a).''','0:55'))
replace('Circuitos distintos dentro del boom',slide('Comunidades: comparación entre períodos', '''::: {.compacto}
| Dimensión | P1: formación | P2: boom |
|:---|:---|:---|
| Partición Leiden | 7 comunidades, 191 artistas | 8 comunidades, 545 artistas |
| Rap | C2 y C6: predominan inicios en los años 1990 | C5: rap histórico; C8: cohorte dominante 2005–2009 |
| Urbano reciente | Predominan inicios entre 2015 y 2019 | C1, C2, C3 y C6: predominan inicios entre 2020 y 2024 |
| Lectura | Diferenciación generacional visible | Mayor escala, con diferencias internas persistentes |

La comparación es de **perfiles y configuraciones**, no de grupos idénticos seguidos en el tiempo (disertación, 2026, pp. 33–41).
:::''', '''La comparación se realiza dentro del componente mayor de cada red: 191 y 545 artistas. El primer período identifica siete comunidades y el segundo ocho. Las comunidades del segundo período tienen entre 18 y 129 artistas, frente a entre siete y 46 en el primero.

En P1, C2 y C6 reúnen perfiles de rap histórico, mientras gran parte de las otras comunidades tiene una cohorte dominante de inicio entre 2015 y 2019. En P2, el rap sigue visible en C5 y C8, con diferencias generacionales internas, y varias comunidades urbanas tienen predominio de inicios entre 2020 y 2024. Esto muestra persistencia y reorganización de perfiles, no una correspondencia directa entre sus miembros. Las cohortes indican inicio profesional, no edad biológica (disertación, 2026, secciones 5.2.1–5.2.4).''','0:35')+slide('Comunidades: apertura internacional y articulaciones locales', '''::: {.compacto}
| Dimensión | P1: formación | P2: boom |
|:---|:---|:---|
| Articulación internacional | C5: Pablo Chill-E y Julianno Sosa, conexiones transnacionales tempranas | C1: 62 % extranjeros; C6: 90 %, con fuerte conexión a circuitos argentinos |
| Colaboración recurrente | C4: destaca la intensidad en reggaetón y mambo | C2: mayor intensidad media ordinal, con 117 artistas y 21 % extranjeros |
| Mujeres y grupos mixtos | C2: 27 %, con conexiones alrededor de Ana Tijoux | C4: 29 %, con AKRIILA, Shirel, SOULFIA y otras |

La expansión combina **núcleos internacionales y circuitos locales intensos** (disertación, 2026, pp. 37–40).
:::''', '''En el primer período ya aparecen conexiones internacionales, especialmente en la comunidad de Pablo Chill-E y Julianno Sosa. En el segundo, la apertura se organiza en al menos dos perfiles: C1 combina figuras chilenas con colaboradores de múltiples escenas, mientras C6 presenta el mayor porcentaje extranjero y conexiones particularmente visibles con circuitos argentinos.

Al mismo tiempo, C2 del boom tiene la mayor intensidad media ordinal y una composición más nacional. La internacionalización no reemplaza la recurrencia local. También varía la presencia de mujeres y agrupaciones mixtas: destaca C2 en P1, alrededor de Ana Tijoux, y C4 en P2. Esta comparación no demuestra que una comunidad haya migrado a otra. Compara perfiles situados en particiones independientes (disertación, 2026, tablas 5A–5B e interpretación).''','0:40'))
s=s.replace('- La integración global conserva **agrupamientos diferenciados**.','- La red pasa de siete a ocho comunidades en un componente mayor mucho más amplio.').replace('- Persisten trayectorias de rap histórico junto a circuitos urbanos más recientes.','- Persiste la diferenciación entre rap y urbano, con nuevas cohortes y configuraciones.').replace('- La presencia extranjera y la intensidad colaborativa varían entre comunidades.','- La apertura internacional convive con circuitos nacionales de colaboración intensa.')
# Reallocate 30 seconds to the new comparison without shortening the genre discussion.
parts=re.split(r'(?m)(?=^## )',s)
for i,part in enumerate(parts):
 if any(part.startswith('## '+t+'\n') for t in ['SNA: análisis de redes sociales','La internacionalización es selectiva','Alcance de los hallazgos']):
  def shorten(m):
   sec=int(m[1])*60+int(m[2])-10
   return f'**{sec//60}:{sec%60:02d} · acumulado pendiente.**'
  part=re.sub(r'\*\*(\d+):(\d{2}) · acumulado [^*]+\*\*',shorten,part,count=1)
 parts[i]=part
s=''.join(parts)
total=0
def timing(m):
 global total
 sec=int(m[1])*60+int(m[2]);total+=sec
 return f'**{sec//60}:{sec%60:02d} · acumulado {total//60}:{total%60:02d}.**'
s=re.sub(r'\*\*(\d+):(\d{2}) · acumulado [^*]+\*\*',timing,s)
assert total==1200,total
s=s.replace('Exposición: 30 diapositivas','Exposición: 31 diapositivas')
p.write_text(s,encoding='utf-8')
p=Path('@2026-congreso-musicologia/LEEME.md')
s=p.read_text(encoding='utf-8').replace('30 diapositivas','31 diapositivas')
s+='\nLa revisión incorpora género como proceso de categorización de músicas, músicos y públicos, y dos diapositivas comparativas de comunidades entre períodos: estructura y generaciones, apertura internacional y articulaciones locales. No se asume continuidad de membresía entre comunidades numeradas de particiones independientes.\n'
p.write_text(s,encoding='utf-8')
print('Marco conceptual y comparación de comunidades actualizados; 20:00.')
