a) capicua ← {∧/(⍵ = ⌽⍵)}
b) f← {(×/((⍵<⍺)×⍵)~0)÷(×/(((⍳(⍴⍵))×(⍵<⍺))~0))}
c) traza ← {+/+/ ⍵ × (⍵ = ⍉⍵)}
d) srl ← {1 ↓ (∊ ⍵ 0)}
e) ∊
f) minMax ← {⌊/(⌈/⍵)}
g) paresMenorQueElMaximo ← {(⍵ × (⍵ ∊ (2×(⍳(⌈/⍵)))) ∧ (⍵ < (⌈/⍵))) ~0}
h) parEnPosicionYMenorQueElMaximo ←{(⍵ × (((⍴⍵) ⍴ 0 1) ∧ ⍵ < (⌈/⍵))) ~0}
i) ceros  ← { 1 ↓ ((2× (⍴⍵)) ⍴ 0 1) \⍵}
k) igualesEnMismaPos ← {(⍵ × ⍵ = ⍺) ~0}

