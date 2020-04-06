a1 = Posicio {columna = 'a', fila = 1}
b1 = Posicio {columna = 'b', fila = 1}
c1 = Posicio {columna = 'c', fila = 1}
d1 = Posicio {columna = 'd', fila = 1}
e1 = Posicio {columna = 'e', fila = 1}
f1 = Posicio {columna = 'f', fila = 1}
g1 = Posicio {columna = 'g', fila = 1}
h1 = Posicio {columna = 'h', fila = 1}
a2 = Posicio {columna = 'a', fila = 2}
b2 = Posicio {columna = 'b', fila = 2}
c2 = Posicio {columna = 'c', fila = 2}
d2 = Posicio {columna = 'd', fila = 2}
e2 = Posicio {columna = 'e', fila = 2}
f2 = Posicio {columna = 'f', fila = 2}
g2 = Posicio {columna = 'g', fila = 2}
h2 = Posicio {columna = 'h', fila = 2}
a7 = Posicio {columna = 'a', fila = 7}
b7 = Posicio {columna = 'b', fila = 7}
c7 = Posicio {columna = 'c', fila = 7}
d7 = Posicio {columna = 'd', fila = 7}
e7 = Posicio {columna = 'e', fila = 7}
f7 = Posicio {columna = 'f', fila = 7}
g7 = Posicio {columna = 'g', fila = 7}
h7 = Posicio {columna = 'h', fila = 7}
a8 = Posicio {columna = 'a', fila = 8}
b8 = Posicio {columna = 'b', fila = 8}
c8 = Posicio {columna = 'c', fila = 8}
d8 = Posicio {columna = 'd', fila = 8}
e8 = Posicio {columna = 'e', fila = 8}
f8 = Posicio {columna = 'f', fila = 8}
g8 = Posicio {columna = 'g', fila = 8}
h8 = Posicio {columna = 'h', fila = 8}


c4 = Posicio {columna = 'c', fila = 4}
e4 = Posicio {columna = 'e', fila = 4}
c6 = Posicio {columna = 'c', fila = 6}
f6 = Posicio {columna = 'f', fila = 6}
e5 = Posicio {columna = 'e', fila = 5}

ta1b = Peca {idp = 'T', blanca = True, posicio = a1}
cb1b = Peca {idp = 'C', blanca = True, posicio = b1}
ac1b = Peca {idp = 'A', blanca = True, posicio = c1}
dd1b = Peca {idp = 'D', blanca = True, posicio = f7} --
re1b = Peca {idp = 'R', blanca = True, posicio = e1}
af1b = Peca {idp = 'A', blanca = True, posicio = c4} --
cg1b = Peca {idp = 'C', blanca = True, posicio = g1}
th1b = Peca {idp = 'T', blanca = True, posicio = h1}
pa2b = Peca {idp = 'P', blanca = True, posicio = a2}
pb2b = Peca {idp = 'P', blanca = True, posicio = b2}
pc2b = Peca {idp = 'P', blanca = True, posicio = c2}
pd2b = Peca {idp = 'P', blanca = True, posicio = d2}
pe2b = Peca {idp = 'P', blanca = True, posicio = e4}--
pf2b = Peca {idp = 'P', blanca = True, posicio = f2}
pg2b = Peca {idp = 'P', blanca = True, posicio = g2}
ph2b = Peca {idp = 'P', blanca = True, posicio = h2}


ta8n = Peca {idp = 'T', blanca = False, posicio = a8}
cb8n = Peca {idp = 'C', blanca = False, posicio = c6}--
ac8n = Peca {idp = 'A', blanca = False, posicio = c8}
dd8n = Peca {idp = 'D', blanca = False, posicio = d8}
re8n = Peca {idp = 'R', blanca = False, posicio = e8}
af8n = Peca {idp = 'A', blanca = False, posicio = f8}
cg8n = Peca {idp = 'C', blanca = False, posicio = f6}--
th8n = Peca {idp = 'T', blanca = False, posicio = h8}
pa7n = Peca {idp = 'P', blanca = False, posicio = a7}
pb7n = Peca {idp = 'P', blanca = False, posicio = b7}
pc7n = Peca {idp = 'P', blanca = False, posicio = c7}
pd7n = Peca {idp = 'P', blanca = False, posicio = d7}
pe7n = Peca {idp = 'P', blanca = False, posicio = e5}--
pf7n = Peca {idp = 'P', blanca = False, posicio = f7}
pg7n = Peca {idp = 'P', blanca = False, posicio = g7}
ph7n = Peca {idp = 'P', blanca = False, posicio = h7}



taul = Tauler [ta8n,cb8n,ac8n,dd8n,re8n,af8n,cg8n,th8n,pa7n,pb7n,pc7n,pd7n,pe7n,{-pf7n,-}pg7n,ph7n,pa2b,pb2b,pc2b,pd2b,pe2b,pf2b,pg2b,ph2b,ta1b,cb1b,ac1b,dd1b,re1b,af1b,cg1b,th1b]