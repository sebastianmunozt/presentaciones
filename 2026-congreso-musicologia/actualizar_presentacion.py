from pathlib import Path
import re
root=Path('@2026-congreso-musicologia')
p=root/'presentacion-congreso.qmd'
s=p.read_text(encoding='utf-8-sig')
# Move documentary provenance into speaker notes and remove detached citation blocks.
def source_to_notes(m):
    source=m.group(1).strip()
    return '\n::: {.notes}\nProcedencia de esta diapositiva: '+source+'\n:::\n'
s=re.sub(r'\n::: \{\.fuente\}\n(.*?)\n:::\n',source_to_notes,s,flags=re.S)
s=re.sub(r'<!-- Imagen opcional:.*?-->', '', s, flags=re.S)
s=s.replace('.reveal .fuente { font-size: 0.56em; color: #52645d; margin-top: 1.4em; }','.reveal .columns { gap: 1.1em; }\n.reveal .compacto { font-size: 0.76em; }\n.reveal .grafico img { margin: 0; object-fit: contain; }')
s=s.replace('- Música urbana reúne rap, trap, reggaetón, mambo y otras trayectorias.\n- La categoría organiza reconocimiento, pertenencia y circulación.', '- Muñoz-Tapia, Arias Salvado y Hammou (2025) entienden música urbana como una categoría que reúne trayectorias heterogéneas.\n- Muñoz-Tapia y Pinochet-Cobos (2025) sitúan su formación en disputas generacionales del caso chileno.')
s=s.replace('- **Género:** convenciones y clasificaciones que cambian históricamente.\n- **Mundos musicales:** cooperación y relaciones que sostienen la actividad artística.\n- **Categorías y redes:** la identificación compartida y los vínculos se articulan entre sí.', '- Brackett (2016) y Drott (2013) comprenden el **género** como una configuración histórica de convenciones y relaciones.\n- Becker (2008) y Crossley (2020) sitúan la **cooperación** en la base de los mundos artísticos y musicales.\n- White (2008) y Crossley (2025a) articulan **identidades categoriales y vínculos** para explicar formaciones sociales.')
# Inline author-year references in the spoken script as well.
for a,b in [('Muñoz-Tapia, Arias Salvado y Hammou muestran','Muñoz-Tapia, Arias Salvado y Hammou (2025) muestran'),('Muñoz-Tapia y Pinochet-Cobos reconstruyen','Muñoz-Tapia y Pinochet-Cobos (2025) reconstruyen'),('Brackett y Drott permiten','Brackett (2016) y Drott (2013) permiten'),('Becker y Crossley dirigen','Becker (2008) y Crossley (2020) dirigen'),('White, con la noción','White (2008), con la noción'),('Crossley retoma','Crossley (2025a) retoma'),('diálogo con Muñoz-Tapia y Pinochet-Cobos.','diálogo con Muñoz-Tapia y Pinochet-Cobos (2025).'),('Hesmondhalgh ofrece','Hesmondhalgh (2025) ofrece'),('preocupación de Born por','preocupación de Born (2011) por')]: s=s.replace(a,b)
# Replace slide bodies while retaining all speaker notes.
def body(title,new):
    global s
    pat=r'(## '+re.escape(title)+r'\n).*?(?=\n::: \{\.notes\})'
    s,n=re.subn(pat,lambda m:m.group(1)+'\n'+new.strip()+'\n',s,count=1,flags=re.S)
    assert n==1,title
body('La red crece y se integra','''::: {.columns}
::: {.column width="50%"}
![](imagenes/red-formacion.png){height=420 fig-alt="Red de formación y consolidación nacional"}
:::
::: {.column width="50%"}
![](imagenes/red-boom.png){height=420 fig-alt="Red del boom y expansión internacional"}
:::
:::

::: {.compacto}
La comparación registra **213 a 564 artistas**, **410 a 1.579 vínculos** y un componente mayor que pasa del **89,7 % al 96,6 %** (disertación, 2026, tabla 2A).
:::''')
body('Aumentan las colaboraciones recurrentes','''::: {.columns}
::: {.column width="67%"}
![](imagenes/recurrencia.png){height=440 fig-alt="Distribución de parejas por número de colaboraciones en ambos períodos"}
:::
::: {.column width="33%"}
::: {.compacto}
En la figura 5 de la disertación (2026), las parejas con **dos o más pistas** pasan del **17,6 % al 25,1 %**.

Predominan las colaboraciones de una sola pista.

Gris: una pista. Azul: dos. Verde: tres o más.
:::
:::
:::''')
body('Las comunidades conservan diferencias generacionales','''::: {.columns}
::: {.column width="50%"}
![](imagenes/comunidades-formacion.png){height=410 fig-alt="Siete comunidades Leiden durante la formación"}
:::
::: {.column width="50%"}
![](imagenes/comunidades-boom.png){height=410 fig-alt="Ocho comunidades Leiden durante el boom"}
:::
:::

::: {.compacto}
Las particiones dialogan con la diferenciación generacional descrita por **Muñoz-Tapia y Pinochet-Cobos (2025)**. Los colores identifican comunidades dentro de cada período, no grupos equivalentes entre períodos.
:::''')
body('La internacionalización es selectiva','''- En la disertación (2026, pp. 28 y 51), la presencia extranjera sube del **21,2 % al 38,9 %**, pero el modelo conserva una asociación negativa para vínculos Chile–extranjero.
- La expansión internacional se concentra en comunidades específicas.
- El resultado dialoga con **Hesmondhalgh (2025)**: mayor circulación musical puede coexistir con desigualdades persistentes.''')
body('Los modelos respaldan la diferenciación relacional','''::: {.columns}
::: {.column width="70%"}
![](imagenes/coeficientes-ergm.png){height=440 fig-alt="Coeficientes del ERGM M4 e intervalos de confianza del 95 por ciento"}
:::
::: {.column width="30%"}
::: {.compacto}
El modelo M4 de la disertación (2026) identifica:

- Proximidad entre años de inicio.
- Internacionalización selectiva.
- Participación con un único colaborador.

**Asociaciones, sin interpretación causal.**
:::
:::
:::''')
s=s.replace('- Ana Tijoux articula conexiones relevantes en el segmento de rap histórico.', '- Ana Tijoux articula conexiones relevantes en el segmento de rap histórico.\n- Born (2011) permite situar estos resultados en la relación entre música e identidades sociales.')
# Turn the final takeaway into an explicit two-slide conclusions section.
start=s.index('## La música urbana se consolida mediante relaciones diferenciadas')
end=s.index('## Referencias de consulta I',start)
s=s[:start]+'''## Conclusiones: respuestas a las preguntas

1. **Estructura:** la red crece y reúne a más artistas en un componente común, con una periferia amplia.
2. **Internacionalización:** aumenta la presencia extranjera, concentrada en circuitos específicos.
3. **Participación:** crece la recurrencia, pero la mediana permanece en dos colaboradores.

::: {.notes}
**0:45 · acumulado 19:30.**

Los resultados de la disertación (2026, pp. 52–56) responden a las tres preguntas. La red se expande y se integra. La internacionalización modifica su composición, aunque se concentra en sectores particulares. La participación permanece desigual: conviven artistas con muchos vínculos y una periferia de pocos colaboradores. Además, algunas asociaciones se repiten con mayor frecuencia. Estas dimensiones permiten describir la consolidación como una transformación de las relaciones, manteniendo las cautelas sobre duración de los períodos y cobertura de plataforma.
:::

## Conclusiones: aporte a la musicología

- Las comunidades extienden al plano relacional la historia generacional de **Muñoz-Tapia y Pinochet-Cobos (2025)**.
- Los resultados dialogan con **White (2008) y Crossley (2025a)**: categorías y vínculos participan en la organización de un mundo musical.

**¿Cómo se relacionan estas fronteras con las pertenencias que reconocen los propios artistas?**

::: {.notes}
**0:30 · acumulado 20:00.**

El aporte consiste en conectar clasificación musical y cooperación. El análisis extiende la historia de Muñoz-Tapia y Pinochet-Cobos (2025) y dialoga con White (2008) y Crossley (2025a). Queda abierta la comparación entre los agrupamientos observados y las pertenencias reconocidas por los artistas, mediante entrevistas e investigación histórica.
:::

'''+s[end:]
# Keep two additional source figures available for questions without expanding speaking time.
s+='''

## Respaldo: composición de la red {.appendix}

![](imagenes/composicion.png){height=495 fig-alt="Composición por país, tipo de artista, género y sello en cada período"}

::: {.notes}
Fuera de los 20 minutos. Figura 3 de la disertación (2026), p. 27. Gráfico original, con sus categorías y leyendas en inglés.
:::

## Respaldo: generaciones de ingreso {.appendix}

![](imagenes/generaciones.png){height=495 fig-alt="Distribución de artistas por cohorte de inicio de trayectoria"}

::: {.notes}
Fuera de los 20 minutos. Figura 4 de la disertación (2026), p. 28. Generación corresponde al año de inicio profesional, no al año de nacimiento.
:::
'''
s=s.replace('Exposición: 16 diapositivas, 20 minutos.\nLas dos últimas diapositivas contienen referencias de consulta.', 'Exposición: 17 diapositivas, 20 minutos.\nSe añaden dos diapositivas de referencias y dos de gráficos de respaldo.')
# Merge adjacent notes divs so Reveal has a single notes block per slide.
s=re.sub(r'\n:::\s*\n::: \{\.notes\}\n', '\n\n', s)
p.write_text(s,encoding='utf-8')
print('QMD actualizado: citas narrativas, conclusiones y ocho figuras originales.')
