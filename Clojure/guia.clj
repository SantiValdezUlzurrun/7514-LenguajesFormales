(ns guia
  (:gen-class))

;3
(defn sig-mul-diez [n]
  (obtenerSiguienteMultiplo n))

(defn obtenerSiguienteMultiplo [n]
  (if (= (rem n 10) 0)
    n
  (obtenerSiguienteMultiplo (+ n 1))))

;4
(defn obtenerByteNde [n, data]
  (let [cantPos (- 32 (* n 8))]
    (bit-shift-left (bit-shift-right data cantPos)  cantPos))) 

;5

(defn ctoi [n]
  (- (int n) (int \0)))

(defn capicua? [n]
  (= (map ctoi (reverse (str n))) 
     (map ctoi (seq (str n)))))

;6

(defn aprox-pi [n]
  (* 4 (sumaEuler n)))

(defn sumaEuler [n]
 (reduce (fn [a b] (+ a b) ) (map * (unoMenosUno n) (map inversa (iotaImpar n)))))

(defn iotaImpar [n]
  (map (fn [a] (+ (* a 2) 1)) (take n (range))))

(defn inversa [a]
  (/ 1 a))

(defn unoMenosUno [n]
  (map (fn [a] (if (= (rem a 2) 0) 1 -1)) (take n (range))))


;7

(defn invertir [n]
  (apply str (map ctoi (reverse (str n)))))


;12
(defn uno-para [s] 
  (str "Uno para " s ", uno para mí"))
    
(defn repartir
  ([] (uno-para "vos"))
  ([& more] (map uno-para more)))

;13
(defn pares [l1 l2]
  (concat (posPar l1) (posPar l2)))

(defn posPar [lista]
  (map (partial nth lista) (iotaPar (count lista))))

(defn iotaPar [n]
  (filter (fn [x] (< x n))
    (take n (map (fn [a] (* a 2)) (take n (range))))))


; 21 iterativo

(declare fila)

(defn triangular [matriz]
  (map fila matriz (range (count matriz))))

(defn fila [fila n]
  (into (nthnext fila n) (repeat n 0)))


; 22 iterativo 

(defn diagonal [matriz]
  (map nth matriz (range (count matriz))))

; 21 recursivo

(declare eliminarPosiciones)
(declare generarCeros)

(defn triangularRec [matriz]
 (map into 
      (eliminarPosiciones (list (first matriz)) (map rest (rest matriz)))  
      (generarCeros '() '() (count matriz))))

(defn eliminarPosiciones [matriz resto]
  (if (empty? resto) 
    (reverse matriz)
  (let [nuevaMatriz (into matriz (list (first resto)))]
    (eliminarPosiciones nuevaMatriz (map rest (rest resto))))))

(defn generarCeros [matriz ceros n]
  (if (= (count ceros) n) 
    (reverse matriz)
  (let [nuevaMatriz (into matriz (list ceros))]
    (generarCeros nuevaMatriz (into '(0) ceros) n))))

; 22 recursiva 

(declare obtenerDiagonal)

(defn diagonalRec [matriz]
  (obtenerDiagonal '() matriz 0))

(defn obtenerDiagonal [diag matriz n]
  (if (= n (count matriz)) 
    (reverse diag)
  (let [nuevaDiag (cons (nth (nth matriz n) n) diag)]
    (obtenerDiagonal nuevaDiag matriz (+ n 1)))))



; 30
(defn slice [cadena nro]
  (apply map str (partition nro 1 cadena)))


(defn sublist [lista inicial longitud]
  (apply list (subvec (vec lista) inicial longitud)))
