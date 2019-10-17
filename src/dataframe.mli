type _ t

val merge_tups : 'a t -> 'b t -> ('a * 'b) t
val conv : ('a -> 'b) -> ('b -> 'a) -> 'b t -> 'a t
val length : _ t -> int
val row : 'a t -> int -> 'a
val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

module type C = sig
  type 'a c
  val t1 : 'a c -> 'a t
  val t2 : 'a c -> 'b c -> ('a * 'b) t
  val t3 : 'a c -> 'b c -> 'c c -> ('a * 'b * 'c) t
  val t4 : 'a c -> 'b c -> 'c c -> 'd c -> ('a * 'b * 'c * 'd) t
  val t5 : 'a c -> 'b c -> 'c c -> 'd c -> 'e c -> ('a * 'b * 'c * 'd * 'e) t
  val t6 : 'a c -> 'b c -> 'c c -> 'd c -> 'e c -> 'f c -> ('a * 'b * 'c * 'd * 'e * 'f) t
  val t7 : 'a c -> 'b c -> 'c c -> 'd c -> 'e c -> 'f c -> 'g c -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) t
  val t8 : 'a c -> 'b c -> 'c c -> 'd c -> 'e c -> 'f c -> 'g c -> 'h c -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) t
  val t9 : 'a c -> 'b c -> 'c c -> 'd c -> 'e c -> 'f c -> 'g c -> 'h c -> 'i c -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) t
  val t10 :'a c -> 'b c -> 'c c -> 'd c -> 'e c -> 'f c -> 'g c -> 'h c -> 'i c -> 'j c -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j) t
end

module A : C with type 'a c := 'a Array.t
module L : C with type 'a c := 'a List.t

