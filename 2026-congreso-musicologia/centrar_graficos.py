from pathlib import Path
import re
p=Path('@2026-congreso-musicologia/presentacion-congreso.qmd')
s=p.read_text(encoding='utf-8')
s=s.replace('.reveal .grafico img { margin: 0; object-fit: contain; }', '''.reveal section.grafico { text-align: center; }
.reveal section.grafico h2 { font-size: 1.16em; margin-bottom: 0.2em; }
.reveal section.grafico > p { margin: 0; }
.reveal section.grafico img {
  display: block;
  margin: 0 auto;
  width: auto;
  height: 560px;
  max-height: 560px;
  max-width: 100%;
  object-fit: contain;
}
.reveal section.grafico .compacto {
  font-size: 0.65em;
  line-height: 1.2;
  margin-top: 0.2em;
}
.reveal section.grafico .compacto p { margin: 0; }''')
parts=re.split(r'(?m)(?=^## )',s)
for i,part in enumerate(parts):
    if re.search(r'!\[.*?\]\(imagenes/',part):
        line,rest=part.split('\n',1)
        if '{.appendix}' in line:
            line=line.replace('{.appendix}','{.appendix .grafico}')
        else: line+=' {.grafico}'
        rest=re.sub(r'height=\d+', 'height=560', rest)
        parts[i]=line+'\n'+rest
s=''.join(parts)
p.write_text(s,encoding='utf-8')
print('Ocho gráficos centrados y ampliados a 560 px de alto.')
