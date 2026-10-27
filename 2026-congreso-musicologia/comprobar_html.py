from pathlib import Path
import re
h=Path('@2026-congreso-musicologia/presentacion-congreso.html').read_text(encoding='utf-8')
notes=re.findall(r'<aside class="notes".*?</aside>',h,re.S)
visible=re.sub(r'<aside class="notes".*?</aside>','',h,flags=re.S)
print('Bloques de notas:',len(notes))
print('Procedencia visible:', 'Procedencia de esta diapositiva:' in visible)
print('Imagenes incorporadas:',len(re.findall(r'<img[^>]+src="data:image',h)))
assert 'Procedencia de esta diapositiva:' not in visible
assert len(re.findall(r'<img[^>]+src="data:image',h))==8
