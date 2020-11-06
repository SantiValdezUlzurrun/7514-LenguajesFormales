## Iota
```py
Def iota = funrec o [id, ~<>]
Def funrec = < o [1, ~1] -> 2; funrec o [-o[1, ~1], apndl]
```

## Factorial
```py
Def ! = eq0 -> ~1;*o[id, ! o sub1]
Def eq0 = eq o [id, ~0]
Def sub1 = - o [id, ~1]
```

## 1 a) Maximo dos numeros
```py
Def mayor = > -> 1; 2
```

### 1 b) Maximo de secuencia
```py
Def max = /mayor
```

### 1 c) Primer atomo de secuencia
```py
Def auxev = null o 1 -> 2; apndl
Def ev = (/auxev) o apndr o [id, ~<>]
Def primatom = 1 o ev o @ (atom -> id; ~<>)
```

### 1 d) minimax (minimo entre maximos por fila)
```py
Def menor = < -> 1; 2
Def min = /menor
Def minimax = min o @ max
```

### 2 a) Pertenecia de un elemento a una secuencia
```py
Def pert = null o 2 -> ~F; (/or) o (@ eq) o distl
```

### 2 b) Si una secuencia tiene longitud uno
```py
Def tienelong1 = eq o [length, ~1]
```

### 2 c) Es cantidad par de atomos en una secuencia
```py
Def auxiota = (> o [1, 2] -> 3; auxiota o [+ o [1, ~1], 2, apndr o [3, 1]])
Def iota = auxiota o [~1, id, ~<>]
Def espar = eq o [* o [length o iota o % o [id, ~2], ~2], id]
Def unoatomos = @ (atom -> ~1; ~0)
Def cantatom = (/+) o unoatomos
Def escatompar = espar o cantatom
```

### Función AñadirTacho: Devuelve una secuencia idéntica a la ingresada con una secuencia vacía como último caracter.
```py
Def addTacho = apndr o [id, ~<>]
```

### Función EliminarRepetidos: Devuelve la secuencia ingresada sin los elementos repetidos.
```py
Def er = /(pert -> 2; apndl) o addTacho
```

### Función EliminarVacío: Toma como entrada una secuencia, devuelve la misma secuencia sin subsecuencias vacías dentro.
```py
Def ev = /(null o 1 -> 2; apndl) o addTacho
```

### 3 a) Union entre dos secencias
```py
Def union = er o /apndl o apndr
```

### 3 b) Interseccion entre dos secuencias.
```py
Def fins = (@ (pert -> 1; ~<>)) o distr
Def int = er o ev o fins
```

### 3 c) Diferencia entre dos secuencias.
```py
Def fdif = (@ (pert -> ~<>; 1)) o distr
Def diff = er o ev o fdif
```

### 3 d) Diferencia simétrica entre dos secuencias.
```py
Def diffS = union o [diff, diff o reverse]
```

## 14 a) Devuelve una secuencia con los invictos de los partidos.
```py
Def invictos = diff o trans
```
