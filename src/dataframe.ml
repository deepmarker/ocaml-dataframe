type _ t =
  | Col : 'a array -> 'a t
  | Cols : 'a t * 'b t -> ('a * 'b) t
  | Conv : ('a -> 'b) * ('b -> 'a) * 'b t -> 'a t

let merge_tups a b = Cols (a, b)
let conv ffrom fto v = Conv (ffrom, fto, v)

let rec length : type a. a t -> int = function
  | Col a -> Array.length a
  | Cols (a, _) -> length a
  | Conv (_, _, a) -> length a

let rec row : type a. a t -> int -> a = fun t n ->
  match t with
  | Col a -> Array.get a n
  | Cols (a, b) -> row a n, row b n
  | Conv (_, t, p) -> t (row p n)

let iter : type a. (a -> 'b) -> a t -> unit = fun f t ->
  let n = length t in
  for i = 0 to n - 1 do
    f (row t i)
  done

let iteri : type a. (int -> a -> 'b) -> a t -> unit = fun f t ->
  let n = length t in
  for i = 0 to n - 1 do
    f i (row t i)
  done

let fold : type a. ('b -> a -> 'b) -> 'b -> a t -> 'b = fun f a t ->
  let n = length t in
  let acc = ref a in
  for i = 0 to n - 1 do
    acc := f !acc (row t i)
  done ;
  !acc

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

module A = struct
  let t1 a = Col a
  let t2 a b = Cols (Col a, Col b)
  let t3 a b c = conv (fun (a, b, c) -> a, (b, c)) (fun (a, (b, c)) -> a, b, c) (Cols (Col a, t2 b c))
  let t4 a b c d = conv (fun (a, b, c, d) -> a, (b, c, d)) (fun (a, (b, c, d)) -> a, b, c, d) (Cols (Col a, t3 b c d))
  let t5 a b c d e = conv (fun (a, b, c, d, e) -> a, (b, c, d, e)) (fun (a, (b, c, d, e)) -> a, b, c, d, e) (Cols (Col a, t4 b c d e))
  let t6 a b c d e f = conv  (fun (a, b, c, d, e, f) -> a, (b, c, d, e, f)) (fun (a, (b, c, d, e, f)) -> a, b, c, d, e, f) (Cols (Col a, t5 b c d e f))
  let t7 a b c d e f g = conv (fun (a, b, c, d, e, f, g) -> a, (b, c, d, e, f, g)) (fun (a, (b, c, d, e, f, g)) -> a, b, c, d, e, f, g) (Cols (Col a, t6 b c d e f g))
  let t8 a b c d e f g h = conv (fun (a, b, c, d, e, f, g, h) -> a, (b, c, d, e, f, g, h)) (fun (a, (b, c, d, e, f, g, h)) -> a, b, c, d, e, f, g, h) (Cols (Col a, t7 b c d e f g h))
  let t9 a b c d e f g h i = conv (fun (a, b, c, d, e, f, g, h, i) -> a, (b, c, d, e, f, g, h, i)) (fun (a, (b, c, d, e, f, g, h, i)) -> a, b, c, d, e, f, g, h, i) (Cols (Col a, t8 b c d e f g h i))
  let t10 a b c d e f g h i j = conv (fun (a, b, c, d, e, f, g, h, i, j) -> a, (b, c, d, e, f, g, h, i, j)) (fun (a, (b, c, d, e, f, g, h, i, j)) -> a, b, c, d, e, f, g, h, i, j) (Cols (Col a, t9 b c d e f g h i j))
end

module L = struct
  open Array
  open A
  let t1 a = t1 (of_list a)
  let t2 a b = t2 (of_list a) (of_list b)
  let t3 a b c = t3 (of_list a) (of_list b) (of_list c)
  let t4 a b c d = t4 (of_list a) (of_list b) (of_list c) (of_list d)
  let t5 a b c d e = t5 (of_list a) (of_list b) (of_list c) (of_list d) (of_list e)
  let t6 a b c d e f = t6 (of_list a) (of_list b) (of_list c) (of_list d) (of_list e) (of_list f)
  let t7 a b c d e f g = t7 (of_list a) (of_list b) (of_list c) (of_list d) (of_list e) (of_list f) (of_list g)
  let t8 a b c d e f g h = t8 (of_list a) (of_list b) (of_list c) (of_list d) (of_list e) (of_list f) (of_list g) (of_list h)
  let t9 a b c d e f g h i = t9 (of_list a) (of_list b) (of_list c) (of_list d) (of_list e) (of_list f) (of_list g) (of_list h) (of_list i)
  let t10 a b c d e f g h i j = t10 (of_list a) (of_list b) (of_list c) (of_list d) (of_list e) (of_list f) (of_list g) (of_list h) (of_list i) (of_list j)
end
