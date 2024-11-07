open Syntax
open Format

let fmt_type =
  let fmt_effect fmt = function
      Effect.EDiv -> fprintf fmt "div"
    | Effect.EConsole -> fprintf fmt "console"
  in
  let pp_comma fmt () = fprintf fmt ", " in
  let fmt_effects fmt e =
    if e = ESet.empty then
      fprintf fmt ""
    else
      fprintf fmt "<%a>" (pp_print_seq ~pp_sep:pp_comma fmt_effect)
        (ESet.to_seq e)
  in
  let rec aux fmt =
  function 
    TCon s -> fprintf fmt "%s" s
  | TApp (s, t) -> fprintf fmt "%s <%a>" s aux t
  | TFun (l, r, e) ->
     fprintf fmt "(%a) -> %a %a" (pp_print_list ~pp_sep:pp_comma aux) l
       fmt_effects e aux r
  | TVar r ->
     match !r with
       TVLink t -> aux fmt t
     | TVUnbd a ->
        fprintf fmt "%d" a
  in aux
