module Char = struct
  let is_figure =
    let c0 = int_of_char '0' in
    let c9 = int_of_char '9' in
    fun c ->
    let c = int_of_char c in
    c0 <= c && c <= c9

  let int_of_figure =
    let c0 = int_of_char '0' in
    fun c ->
    assert (is_figure c);
    int_of_char c - c0
end

module Float = struct
  let of_int = float_of_int

  let round x = floor (x +. 0.5)

  let round_to_int x = int_of_float (x +. 0.5)
end

module List = struct
  include List

  let init f n =
    let rec aux k =
      if k >= n then []
      else (f k)::(aux (k+1))
    in
    aux 0
end

module String = struct
  include String

  (** Substring of all characters matching a predicate. *)
  let sub_all s b p =
    let rec aux i =
      if b+i < String.length s && p s.[b+i] then aux (i+1)
      else i
    in
    let n = aux 0 in
    sub s b n
end
