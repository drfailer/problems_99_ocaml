open Printf

(******************************************************************************)
(*                                   types                                    *)
(******************************************************************************)

type 'a rle = One of 'a | Many of int * 'a
type 'a node = NodeOne of 'a | NodeMany of 'a node list

(******************************************************************************)
(*                              helper function                               *)
(******************************************************************************)

let rec foldl acc func lst =
  match lst with [] -> acc | x :: xs -> foldl (func acc x) func xs

(* let map func = *)
(*   let rec map_aux acc lst = *)
(*     match lst with [] -> acc | x :: xs -> map_aux (acc @ [ func x ]) xs *)
(*   in *)
(*   map_aux [] *)

(******************************************************************************)
(*                             to string function                             *)
(******************************************************************************)

let str_opt opt = match opt with Some x -> x | None -> "None"

let str_opt2 opt =
  match opt with Some (x, y) -> sprintf "(%s, %s)" x y | None -> "None"

let str_lst =
  let rec str_lst_aux acc lst =
    match lst with
    | [] -> sprintf "%s ]" acc
    | x :: xs -> str_lst_aux (sprintf "%s %s" acc x) xs
  in
  str_lst_aux "["

let str_lst2 =
  let rec str_lst_aux acc lst =
    match lst with
    | [] -> sprintf "%s ]" acc
    | x :: xs -> str_lst_aux (sprintf "%s %s" acc (str_lst x)) xs
  in
  str_lst_aux "["

let str_lst_tuple =
  let rec str_lst_aux acc lst =
    match lst with
    | [] -> sprintf "%s ]" acc
    | (a, b) :: xs -> str_lst_aux (sprintf "%s (%d, %s)" acc a b) xs
  in
  str_lst_aux "["

let str_lst_rle =
  let rec str_lst_aux acc lst =
    match lst with
    | [] -> sprintf "%s ]" acc
    | Many (a, b) :: xs -> str_lst_aux (sprintf "%s (%d, %s)" acc a b) xs
    | One a :: xs -> str_lst_aux (sprintf "%s %s" acc a) xs
  in
  str_lst_aux "["

let str_bool b = if b then "true" else "false"

(******************************************************************************)
(*                                    Last                                    *)
(******************************************************************************)

let rec last lst =
  match lst with [] -> None | [ x ] -> Some x | _ :: xs -> last xs

(******************************************************************************)
(*                                  Last two                                  *)
(******************************************************************************)

let rec last_two lst =
  match lst with
  | [ _ ] | [] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: xs -> last_two xs

(******************************************************************************)
(*                                     at                                     *)
(******************************************************************************)

let rec at lst i =
  match lst with
  | x :: xs -> if i == 1 then Some x else at xs (i - 1)
  | _ -> None

(******************************************************************************)
(*                                   length                                   *)
(******************************************************************************)

let length =
  let rec length_aux acc lst =
    match lst with [] -> acc | _ :: xs -> length_aux (acc + 1) xs
  in
  length_aux 0

(******************************************************************************)
(*                                  reverse                                   *)
(******************************************************************************)

let rev =
  let rec rev_aux acc lst =
    match lst with [] -> acc | x :: xs -> rev_aux (x :: acc) xs
  in
  rev_aux []

(******************************************************************************)
(*                                is palindrom                                *)
(******************************************************************************)

let is_palindrom lst =
  let rec string_eq lst1 lst2 =
    match (lst1, lst2) with
    | [], [] -> true
    | x :: xs, y :: ys -> x == y && string_eq xs ys
    | _ -> false
  in
  string_eq lst (rev lst)

(******************************************************************************)
(*                                  flatten                                   *)
(******************************************************************************)

let rec flatten lst =
  match lst with
  | [] -> []
  | NodeOne x :: xs -> x :: flatten xs
  | NodeMany x :: xs -> flatten x @ flatten xs

(******************************************************************************)
(*                                  compress                                  *)
(******************************************************************************)

let rec compress lst =
  match lst with
  | x :: y :: tail ->
      if x == y then compress (y :: tail) else x :: compress (y :: tail)
  | _ -> lst

let compress_term =
  let rec compress_aux acc lst =
    match lst with
    | [] -> acc
    | x :: [] -> acc @ [ x ]
    | x :: y :: tail ->
        if x == y then compress_aux acc (y :: tail)
        else compress_aux (acc @ [ x ]) (y :: tail)
  in
  compress_aux []

(******************************************************************************)
(*                                    pack                                    *)
(******************************************************************************)

let pack lst =
  let rec pack_aux acc sublist current lst =
    match lst with
    | [] -> ( match sublist with [] -> acc | _ -> acc @ [ sublist ])
    | x :: xs ->
        if x == current then pack_aux acc (x :: sublist) x xs
        else pack_aux (acc @ [ sublist ]) [ x ] x xs
  in
  match lst with [] -> [] | x :: xs -> pack_aux [] [ x ] x xs

(******************************************************************************)
(*                                   encode                                   *)
(******************************************************************************)

let encode lst =
  let rec encode_aux acc count current lst =
    match lst with
    | [] -> if count > 0 then acc @ [ (count, current) ] else acc
    | x :: xs ->
        if x == current then encode_aux acc (count + 1) x xs
        else encode_aux (acc @ [ (count, current) ]) 1 x xs
  in
  match lst with [] -> [] | x :: xs -> encode_aux [] 1 x xs

(******************************************************************************)
(*                              encode modified                               *)
(******************************************************************************)

(*
NOTE: this is the direct solution (problem 13), but we can also use the
previously written functions. However, this solution would be less efficient
since it introduces unecessary operations.
*)
let encode_mod lst =
  let rec encode_aux acc count current lst =
    match lst with
    | [] ->
        if count > 0 then
          acc @ [ (if count == 1 then One current else Many (count, current)) ]
        else acc
    | x :: xs ->
        if x == current then encode_aux acc (count + 1) x xs
        else
          encode_aux
            (acc
            @ [ (if count == 1 then One current else Many (count, current)) ])
            1 x xs
  in
  match lst with [] -> [] | x :: xs -> encode_aux [] 1 x xs

(******************************************************************************)
(*                                   decode                                   *)
(******************************************************************************)

let rec decode lst =
  let rec repeat acc n x =
    if n == 0 then acc else repeat (x :: acc) (n - 1) x
  in
  match lst with
  | [] -> []
  | One a :: xs -> a :: decode xs
  | Many (c, l) :: xs -> repeat [] c l @ decode xs

(******************************************************************************)
(*                                 replicate                                  *)
(******************************************************************************)

let replicate lst n =
  let rec repeat acc n x =
    if n == 0 then acc else repeat (x :: acc) (n - 1) x
  in
  foldl [] (fun acc x -> repeat [] n x @ acc) lst

(******************************************************************************)
(*                                    drop                                    *)
(******************************************************************************)

let drop =
  let rec drop_aux acc lst n =
    match lst with
    | [] -> acc
    | x :: xs -> if n == 1 then acc @ xs else drop_aux (acc @ [ x ]) xs (n - 1)
  in
  drop_aux []

(******************************************************************************)
(*                                   split                                    *)
(******************************************************************************)

let split =
  let rec split_aux acc lst n =
    match lst with
    | [] -> (acc, [])
    | x :: xs ->
        if n <= 1 then (acc @ [ x ], xs) else split_aux (acc @ [ x ]) xs (n - 1)
  in
  split_aux []

(******************************************************************************)
(*                                   slice                                    *)
(******************************************************************************)

let slice lst a b =
  match split lst a with
  | _, [] -> []
  | _, t ->
      let s, _ = split t (b - 1) in
      s

(******************************************************************************)
(*                                    MAIN                                    *)
(******************************************************************************)

let () =
  printf "last ['a' ; 'b' ; 'c' ; 'd'] => %s\n"
    (str_opt (last [ "a"; "b"; "c"; "d" ]));
  printf "last [] => %s\n" (str_opt (last []));
  printf "last_two [] => %s\n" (str_opt2 (last_two []));
  printf "last_two ['a'] => %s\n" (str_opt2 (last_two [ "a" ]));
  printf "last_two ['a'; 'b'; 'c'; 'd'] => %s\n"
    (str_opt2 (last_two [ "a"; "b"; "c"; "d" ]));
  printf "at ['a'; 'b'; 'c'; 'd'; 'e] => %s\n"
    (str_opt (at [ "a"; "b"; "c"; "d"; "e" ] 3));
  printf "at ['a'] => %s\n" (str_opt (at [ "a" ] 3));
  printf "length [] => %d\n" (length []);
  printf "length ['a'; 'b'; 'c'] => %d\n" (length [ "a"; "b"; "c" ]);
  printf "length ['a'; 'b'; 'c'] => %d\n" (length [ "a"; "b"; "c" ]);
  printf "rev ['a'; 'b'; 'c'] => %s\n" (str_lst (rev [ "a"; "b"; "c" ]));
  printf "is_palindrom ['x'; 'a'; 'm'; 'a'; 'x'] = %s\n"
    (str_bool (is_palindrom [ "x"; "a"; "m"; "a"; "x" ]));
  printf "is_palindrom ['a'; 'b'] = %s\n" (str_bool (is_palindrom [ "a"; "b" ]));
  printf
    "flatten [One 'a'; Many [One 'b'; Many [One 'c' ;One 'd']; One 'e']] = %s\n"
    (str_lst
       (flatten
          [
            NodeOne "a";
            NodeMany
              [
                NodeOne "b"; NodeMany [ NodeOne "c"; NodeOne "d" ]; NodeOne "e";
              ];
          ]));
  printf
    "compress ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; \
     'e'; 'e'] = %s\n"
    (str_lst
       (compress
          [
            "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e";
          ]));
  printf
    "compress ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; \
     'e'; 'e'] = %s\n"
    (str_lst
       (compress_term
          [
            "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e";
          ]));
  printf
    "pack ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'd'; 'e'; 'e'; \
     'e'; 'e'] = %s\n"
    (str_lst2
       (pack
          [
            "a";
            "a";
            "a";
            "a";
            "b";
            "c";
            "c";
            "a";
            "a";
            "d";
            "d";
            "e";
            "e";
            "e";
            "e";
          ]));
  printf
    "encode ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; \
     'e'] = %s\n"
    (str_lst_tuple
       (encode
          [
            "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e";
          ]));
  printf
    "encode_mod ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; \
     'e'; 'e'] = %s\n"
    (str_lst_rle
       (encode_mod
          [
            "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e";
          ]));
  printf
    "decode [Many (4, 'a'); One 'b'; Many (2, 'c'); Many (2, 'a'); One 'd'; \
     Many (4, 'e')] = %s\n"
    (str_lst
       (decode
          [
            Many (4, "a");
            One "b";
            Many (2, "c");
            Many (2, "a");
            One "d";
            Many (4, "e");
          ]));
  printf "replicate ['a' ; 'b' ; 'c'] 3 = %s\n"
    (str_lst (replicate [ "a"; "b"; "c" ] 3));
  printf "drop ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'] 3 = %s\n"
    (str_lst (drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3));
  let lst1, lst2 =
    split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  in
  printf
    "split ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'] 3 = (%s, %s)\n"
    (str_lst lst1) (str_lst lst2);
  printf "slice ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'] 2 6 = %s\n"
    (str_lst (slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6));
  ()
