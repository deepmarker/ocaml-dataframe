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
  for i = 0 to n - 1 do f (row t i) done

let iteri : type a. (int -> a -> 'b) -> a t -> unit = fun f t ->
  let n = length t in
  for i = 0 to n - 1 do f i (row t i) done

let rev_iter : type a. (a -> 'b) -> a t -> unit = fun f t ->
  let n = length t in
  for i = n - 1 downto 0 do f (row t i) done

let rev_iteri : type a. (int -> a -> 'b) -> a t -> unit = fun f t ->
  let n = length t in
  for i = n - 1 downto 0 do f i (row t i) done

let fold_left : type a. ('b -> a -> 'b) -> 'b -> a t -> 'b = fun f a t ->
  let n = length t in
  let acc = ref a in
  for i = 0 to n - 1 do acc := f !acc (row t i) done ;
  !acc

let fold_right : type a. (a -> 'b -> 'b) -> a t -> 'b -> 'b = fun f t a ->
  let n = length t in
  let acc = ref a in
  for i = n - 1 downto 0 do acc := f (row t i) !acc done ;
  !acc

let rev_map : type a. (a -> 'b) -> a t -> 'b list = fun f t ->
  fold_left (fun a v -> f v :: a) [] t

let map : type a. (a -> 'b) -> a t -> 'b list = fun f t ->
  fold_right (fun v a -> f v :: a) t []

let rev_mapi : type a. (int -> a -> 'b) -> a t -> 'b list = fun f t ->
  let _, r = fold_left (fun (i, a) v -> succ i, f i v :: a) (0, []) t in
  r

let mapi : type a. (int -> a -> 'b) -> a t -> 'b list = fun f t ->
  let _, r = fold_right (fun v (i, a) -> pred i, f i v :: a) t (pred (length t), []) in
  r

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

let c1 ffrom fto a = conv ffrom fto (L.t1 (List.map ffrom a))
let c2 ffrom fto a =
  let a, b = List.fold_right (fun elt (a, b) ->
      let v1, v2 = ffrom elt in
      v1 :: a, v2 :: b) a ([], []) in
  conv ffrom fto (L.t2 a b)
let c3 ffrom fto a =
  let a, b, c = List.fold_right (fun elt (a, b, c) ->
      let v1, v2, v3 = ffrom elt in
      v1 :: a, v2 :: b, v3 :: c) a ([], [], []) in
  conv ffrom fto (L.t3 a b c)
let c4 ffrom fto a =
  let a, b, c, d = List.fold_right (fun elt (a, b, c, d) ->
      let v1, v2, v3, v4 = ffrom elt in
      v1 :: a, v2 :: b, v3 :: c, v4 :: d) a ([], [], [], []) in
  conv ffrom fto (L.t4 a b c d)
let c5 ffrom fto a =
  let a, b, c, d, e = List.fold_right (fun elt (a, b, c, d, e) ->
      let v1, v2, v3, v4, v5 = ffrom elt in
      v1 :: a, v2 :: b, v3 :: c, v4 :: d, v5 :: e) a ([], [], [], [], []) in
  conv ffrom fto (L.t5 a b c d e)
let c6 ffrom fto a =
  let a, b, c, d, e, f = List.fold_right (fun elt (a, b, c, d, e, f) ->
      let v1, v2, v3, v4, v5, v6 = ffrom elt in
      v1 :: a, v2 :: b, v3 :: c, v4 :: d, v5 :: e, v6 :: f) a ([], [], [], [], [], []) in
  conv ffrom fto (L.t6 a b c d e f)
let c7 ffrom fto a =
  let a, b, c, d, e, f, g = List.fold_right (fun elt (a, b, c, d, e, f, g) ->
      let v1, v2, v3, v4, v5, v6, v7 = ffrom elt in
      v1 :: a, v2 :: b, v3 :: c, v4 :: d, v5 :: e, v6 :: f, v7 :: g) a ([], [], [], [], [], [], []) in
  conv ffrom fto (L.t7 a b c d e f g)
let c8 ffrom fto a =
  let a, b, c, d, e, f, g, h = List.fold_right (fun elt (a, b, c, d, e, f, g, h) ->
      let v1, v2, v3, v4, v5, v6, v7, v8 = ffrom elt in
      v1 :: a, v2 :: b, v3 :: c, v4 :: d, v5 :: e, v6 :: f, v7 :: g, v8 :: h) a ([], [], [], [], [], [], [], []) in
  conv ffrom fto (L.t8 a b c d e f g h)
let c9 ffrom fto a =
  let a, b, c, d, e, f, g, h, i = List.fold_right (fun elt (a, b, c, d, e, f, g, h, i) ->
      let v1, v2, v3, v4, v5, v6, v7, v8, v9 = ffrom elt in
      v1 :: a, v2 :: b, v3 :: c, v4 :: d, v5 :: e, v6 :: f, v7 :: g, v8 :: h, v9 :: i) a ([], [], [], [], [], [], [], [], []) in
  conv ffrom fto (L.t9 a b c d e f g h i)
let c10 ffrom fto a =
  let a, b, c, d, e, f, g, h, i, j = List.fold_right (fun elt (a, b, c, d, e, f, g, h, i, j) ->
      let v1, v2, v3, v4, v5, v6, v7, v8, v9, v10 = ffrom elt in
      v1 :: a, v2 :: b, v3 :: c, v4 :: d, v5 :: e, v6 :: f, v7 :: g, v8 :: h, v9 :: i, v10 :: j) a ([], [], [], [], [], [], [], [], [], []) in
  conv ffrom fto (L.t10 a b c d e f g h i j)
