open Syntax

type ctx =
  { var : (pure_type * bool) SMap.t ; ret_type : pure_type
  ; rec_fun : string ; mutable rec_has_console : bool option }

let merge_ctx c1 c2 =
  SMap.merge (fun _ v1 v2 ->
      match v1, v2 with
        None, None -> None
      | _, Some v -> Some v
      | Some v, _ -> Some v) c1 c2
