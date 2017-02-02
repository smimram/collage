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

module List = struct
  include List

  let init f n =
    let rec aux k =
      if k >= n then []
      else (f k)::(aux (k+1))
    in
    aux 0
end
