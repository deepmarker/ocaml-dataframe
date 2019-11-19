type _ t

val merge_tups : 'a t -> 'b t -> ('a * 'b) t
val conv : ('a -> 'b) -> ('b -> 'a) -> 'b t -> 'a t

val length : _ t -> int
val width : _ t -> int
val dim : _ t -> int * int

val row : 'a t -> int -> 'a
val map : ('a -> 'b) -> 'a t -> 'b list
val rev_map : ('a -> 'b) -> 'a t -> 'b list
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b list
val rev_mapi : (int -> 'a -> 'b) -> 'a t -> 'b list
val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val rev_iter : ('a -> unit) -> 'a t -> unit
val rev_iteri : (int -> 'a -> unit) -> 'a t -> unit
val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val to_list : 'a t -> 'a list
val to_seq : 'a t -> 'a Seq.t

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

val c1 : ('a -> 'b) -> ('b -> 'a) -> 'a list -> 'a t
val c2 : ('a -> 'b * 'c) -> ('b * 'c -> 'a) -> 'a list -> 'a t
val c3 : ('a -> 'b * 'c * 'd) -> ('b * 'c * 'd -> 'a) -> 'a list -> 'a t
val c4 : ('a -> 'b * 'c * 'd * 'e) -> ('b * 'c * 'd * 'e -> 'a) -> 'a list -> 'a t
val c5 : ('a -> 'b * 'c * 'd * 'e * 'f) -> ('b * 'c * 'd * 'e * 'f -> 'a) -> 'a list -> 'a t
val c6 : ('a -> 'b * 'c * 'd * 'e * 'f * 'g) -> ('b * 'c * 'd * 'e * 'f * 'g -> 'a) -> 'a list -> 'a t
val c7 : ('a -> 'b * 'c * 'd * 'e * 'f * 'g * 'h) -> ('b * 'c * 'd * 'e * 'f * 'g * 'h -> 'a) -> 'a list -> 'a t
val c8 : ('a -> 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) -> ('b * 'c * 'd * 'e * 'f * 'g * 'h * 'i -> 'a) -> 'a list -> 'a t
val c9 : ('a -> 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j) -> ('b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j -> 'a) -> 'a list -> 'a t
val c10 : ('a -> 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k) -> ('b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k -> 'a) -> 'a list -> 'a t
