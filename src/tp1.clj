(ns exercise1)

(defn CrearUnHecho
  "Recibe clave y valor y devuelve un nuevo hecho"
  [nuevoHecho hecho]
  (let [clave (keyword (get hecho 0)) valor (get hecho 1)]
	(assoc nuevoHecho clave valor)   
   	)
)

(defn AgruparEnLista
  [lista1 lista2]
	(conj lista1 lista2)    
)



(defn AgregarHechoAMapaDeHechos
  [mapaDeHechos hecho]
"pre: Recibe el mapa de hechos y el hecho a agregar
  pos: Devuelve el mapa de hechos con el hecho agregado"
  (def listaVacia [])
  (let [clave (first(keys hecho))]
  	(if(contains? mapaDeHechos clave)
     	(let [valoresMapa (get mapaDeHechos clave) valoresHecho (get hecho clave)]
          	(def listaValores (AgruparEnLista valoresMapa valoresHecho))
          	(assoc mapaDeHechos clave listaValores))
         	 
     	(assoc mapaDeHechos clave (conj listaVacia (get hecho clave)))
  	)
  )
)

; No funciona y no se porque mierda es (no lo puedo probar)

(defn generarMapaRelacional
  [mapaRelacional, parametros, numeroParametro, noMeDeja]
  (if(> (count parametros) 0)
	(let [mapa-aux (generarMapaRelacional(rest parametros (map inc numeroParametro)))]
    	(assoc mapaRelacional (first parametros) (first numeroParametro))  
    	(merge mapaRelacional mapa-aux)
	)
	{}
  )
)

; (defn CrearRegla
; [relga hechosDeLaRegla]
; "Regla esta compuesto por el nombre de la regla (clave) y sus parametros
;   Hechos de la regla es una lista de hechos y los parametros que le pasa
;   Devuelve la regla con el siguiente formato: {:clave [[hecho [0]][hecho1 [1 0]]]}"
 
;   (let[claveRegla (get regla 0) parametrosRegla (get regla 1)]
; 	(def mapaRelacionalParametros (generarMapaRelacional {} parametrosRegla 0))           	 
             	 
             	 
;   )

; )

(defn AgregarReglaAMapaDeReglas
  [mapaDeReglas regla]
 
  (let [clave (first(keys regla))]
   	(if (contains? mapaDeReglas clave)
    	nil
    	(merge mapaDeReglas regla)
   	)
  )
)






(defn perteneceALista
  [lista elemento]  
 
  (if (= (count lista) 0)
  	false
  	(if(= (first lista) elemento)
    	true
    	(perteneceALista (rest lista) elemento)
  	)
  )
)


(defn ExisteHecho
  [mapaDeHechos hecho]
  "Recibe el mapa de hechos y el hecho a ser validado
  Devuelve true o false, si existe o no respectivamente"
   
   (let [clave (keyword(get hecho 0))]
    	(if (contains? mapaDeHechos clave)
        	(let [valoresDeClave (get mapaDeHechos clave) valorDelHecho (get hecho 1)]
           	(perteneceALista valoresDeClave valorDelHecho)
        	)
        	false
    	)
   )
)

(defn armarConsultaDeHechos
  [hechosDeLaRegla parametrosRegla]  
  ; "Devuelve una lista de hechos de regla [["Padre" ["santiago" "pepe"]] ["varon" ["santiago"]]] "
 
)


(defn validarHechosDeRegla
  [consultaDeHechos mapaDeHechos]  
 
  (if (> (count consultaDeHechos) 0)
	(if (ExisteHecho mapaDeHechos (first consultaDeHechos))
  	(validarHechosDeRegla (rest consultaDeHechos) mapaDeHechos)
  	false    
	) 	 
	true    
  )

)


(defn validarRegla
  [ConsultaDeRegla mapaDeReglas mapaDeHechos]  
  "Devuelve true si se cumple la regla y false si no existe, no se cumple"
  (let[clave (get ConsultaDeRegla 0) parametrosRegla (get ConsultaDeRegla 1)]
	(if (contains? mapaDeReglas clave)
  	(let[hechosDeLaRegla (get mapaDeReglas clave)
       	consultaDeHechos (armarConsultaDeHechos hechosDeLaRegla parametrosRegla)]
    	(validarHechosDeRegla consultaDeHechos mapaDeHechos)
  	)
  	false
	)
  )
)


; Pruebas del ejercicio


(def mapa1 (CrearUnHecho {} ["Padre" ["juan" "maria"]]))


(def mapaHechos (AgregarHechoAMapaDeHechos {} mapa1))


(def mapa2 (CrearUnHecho {} ["Padre" ["pablo" "maria"]]))

(def mapa3 (CrearUnHecho {} ["Padre" ["fede" "johy"]]))

(def mapa4 (AgregarHechoAMapaDeHechos mapaHechos mapa2))

(AgregarHechoAMapaDeHechos mapa4 mapa3)

(ExisteHecho mapa4 ["Padre" ["pablo" "maria"]])

; (let[numeroInicial 0](generarMapaRelacional {} ["pablo" "maria"] numeroInicial))

(validarHechosDeRegla [["Padre" ["pablo" "maria"]] ["Padre" ["juan" "maria"]]] mapa4)



