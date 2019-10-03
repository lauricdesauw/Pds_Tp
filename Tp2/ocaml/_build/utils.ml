let tmp = ref 0
let lab = ref 0
let glob = ref 0

(* generate a new unique local identifier (starting with %) *)
let newtmp: unit -> string = function () ->
  tmp := succ !tmp;
  "%tmp" ^ (string_of_int !tmp)

(* generate a new unique label starting with str *)
let newlab str =
  lab := succ !lab;
  str ^ (string_of_int !lab)

(* generate a new unique global identifier (starting with @str) *)
let newglob str =
  glob := succ !glob;
  "@" ^ str ^ (string_of_int !glob)

(* transform escaped newlines ('\' 'n') into newline form suitable for LLVM
 * and append the NUL character (end of string)
 * return a pair: the new string, and its size (according to LLVM)
 *)
let string_transform str =
  let re = Str.regexp_string "\\n"
  (* replace all \n by \0A and append an \00 at the end.
   * return a pair: the new string and the number of matches
   *)
  in let rec aux str pos matches =
    try
      let _ = Str.search_forward re str pos in (* can raise Not_found *)
      let str' = Str.replace_first re "\\\\0A" str
      in aux str' (1 + Str.match_beginning ()) (succ matches)
    with Not_found ->
      (str ^ "\\00", matches)
  in let r = aux str 0 0
  in (fst r, 1 + (String.length str) - (snd r))
  (*         + 1 for \00             - 1 by \n because each ('\' '\n') is transformed into one char *)
