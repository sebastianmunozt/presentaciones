from pathlib import Path
from pypdf import PdfReader
root=Path('@2026-congreso-musicologia')
out=root/'imagenes'
out.mkdir(exist_ok=True)
r=PdfReader(root/'14340176_SOCY60000.pdf')
for page,name in [(25,'red-formacion'),(26,'red-boom'),(27,'composicion'),(28,'generaciones'),(29,'recurrencia'),(35,'comunidades-formacion'),(36,'comunidades-boom'),(46,'coeficientes-ergm')]:
    im=list(r.pages[page-1].images)[0]
    im.image.save(out/(name+'.png'))
print('8 gráficos originales extraídos')
