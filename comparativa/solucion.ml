(*
========================================================
FUNCIÓN: transicion
NATURALEZA: Pura
ESTRATEGIA: Pattern Matching (match ... with)
IMPACTO: No destructiva
========================================================
*)

type color =
  | Rojo
  | Amarillo
  | Verde

let transicion color_actual cambiar_a =
  match (color_actual, cambiar_a) with
  | (Rojo, Verde) -> (Rojo, "cambiar-a-verde")
  | (Verde, Amarillo) -> (Verde, "cambiar-a-amarillo")
  | (Amarillo, Rojo) -> (Amarillo, "cambiar-a-rojo")
  | _ -> (color_actual, "accion-por-defecto")

(*
========================================================
FUNCIÓN: timer
NATURALEZA: Pura
ESTRATEGIA: Cálculo matemático + condicionales
IMPACTO: No destructiva
========================================================
*)

let timer tiempo_unix =
  let instante = tiempo_unix mod 216 in
  if instante < 90 then
    Rojo
  else if instante < 96 then
    Amarillo
  else
    Verde

(*
========================================================
CASOS DE PRUEBA
========================================================

transicion Rojo Verde;;
Resultado:
(Rojo, "cambiar-a-verde")

transicion Verde Amarillo;;
Resultado:
(Verde, "cambiar-a-amarillo")

transicion Amarillo Rojo;;
Resultado:
(Amarillo, "cambiar-a-rojo")

timer 0;;
Resultado:
Rojo

timer 90;;
Resultado:
Amarillo

timer 96;;
Resultado:
Verde

timer 216;;
Resultado:
Rojo
========================================================
*)
