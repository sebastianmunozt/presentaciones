from pathlib import Path
p=Path('@2026-congreso-musicologia/presentacion-congreso.qmd')
s=p.read_text(encoding='utf-8')
parts=s.split('\n## ')
for i,part in enumerate(parts):
    if 'Procedencia de esta diapositiva:' in part and '::: {.notes}\nProcedencia de esta diapositiva:' not in part:
        part=part.replace('\nProcedencia de esta diapositiva:', '\n:::\n\n::: {.notes}\nProcedencia de esta diapositiva:',1)
    if 'Material de consulta, fuera' in part and '::: {.notes}\nMaterial de consulta' not in part:
        part=part.replace('\nMaterial de consulta, fuera','\n:::\n\n::: {.notes}\nMaterial de consulta, fuera',1)
    parts[i]=part
s='\n## '.join(parts)
p.write_text(s,encoding='utf-8')
p=Path('@2026-congreso-musicologia/LEEME.md')
s=p.read_text(encoding='utf-8-sig').replace('portada y 15 diapositivas de contenido','portada y 16 diapositivas de contenido').replace('Las dos diapositivas de referencias quedan fuera de ese tiempo.', 'Las dos diapositivas de referencias y las dos de gráficos de respaldo quedan fuera de ese tiempo. La sección de conclusiones contiene dos diapositivas.').replace('Hay comentarios en el código con las figuras sugeridas del texto original.', 'La carpeta ya contiene ocho gráficos originales extraídos del PDF. Seis aparecen durante la exposición y dos quedan como respaldo. Se conservan sus rótulos en inglés.').replace('Las citas autor-año y las referencias están escritas directamente', 'Las citas autor-año están integradas en las afirmaciones y el guion oral. Las referencias están escritas directamente')
p.write_text(s,encoding='utf-8')
print('Estructura de notas y documentación actualizadas')
