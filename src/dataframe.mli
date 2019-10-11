type _ t
val conv : ('a -> 'b) -> ('b -> 'a) -> 'b t -> 'a t
val t1 : 'a array -> 'a t
val t2 : 'a array -> 'b array -> ('a * 'b) t
val t3 : 'a array -> 'b array -> 'c array -> ('a * 'b * 'c) t
val t4 :
  'a array -> 'b array -> 'c array -> 'd array -> ('a * 'b * 'c * 'd) t
val t5 :
  'a array ->
  'b array ->
  'c array -> 'd array -> 'e array -> ('a * 'b * 'c * 'd * 'e) t
val t6 :
  'a array ->
  'b array ->
  'c array ->
  'd array -> 'e array -> 'f array -> ('a * 'b * 'c * 'd * 'e * 'f) t
val t7 :
  'a array ->
  'b array ->
  'c array ->
  'd array ->
  'e array -> 'f array -> 'g array -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) t
val t8 :
  'a array ->
  'b array ->
  'c array ->
  'd array ->
  'e array ->
  'f array ->
  'g array -> 'h array -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) t
val t9 :
  'a array ->
  'b array ->
  'c array ->
  'd array ->
  'e array ->
  'f array ->
  'g array ->
  'h array -> 'i array -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) t
val t10 :
  'a array ->
  'b array ->
  'c array ->
  'd array ->
  'e array ->
  'f array ->
  'g array ->
  'h array ->
  'i array ->
  'j array -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j) t
val length : _ t -> int
val row : 'a t -> int -> 'a
val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
