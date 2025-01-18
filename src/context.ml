open Syntax

(* on n'utilise pas le mot clé mutable, car les expressions avec `with` ne
copient pas le champ par référence *)

type ctx =
  { var : (pure_type * bool) SMap.t
  ; ret_type : pure_type
  ; rec_fun : string
  ; rec_has_console : bool option ref
  ; cons : (int * (int list * pure_type list * pure_type)) SMap.t
  ; tctx : int SMap.t }

let merge_ctx c1 c2 =
  SMap.merge (fun _ v1 v2 -> match v1, v2 with
        None, None -> None
      | _, Some v -> Some v
      | Some v, _ -> Some v) c1 c2

let tvar = ref 0

let new_tvar () =
  incr tvar; TVar (ref (TVUnbd !tvar))

module IMap = Map.Make(Int)

let inst_cons (tv, arg, res) =
  let map =
    List.map (fun x -> x, new_tvar ()) tv |> List.to_seq |> IMap.of_seq
  in
  let rec subst = function
    | TApp (s, l) -> TApp (s, List.map subst l)
    | TFun (arg, res, eff) -> TFun (List.map subst arg, subst res, eff)
    | TVar r -> match !r with
      | TVUnbd i -> IMap.find i map
      | TVLink t -> subst t
  in
  List.map subst arg, subst res
