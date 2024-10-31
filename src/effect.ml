open Syntax
open Syntax.Effect
open Format
open Error

exception EffectUnification

let valid_effects_list = ["div", EDiv; "console", EConsole]

let valid_effects =
  valid_effects_list |> List.to_seq |> SMap.of_seq

let (++) (eff, constr) (eff', constr') =
  ESet.union eff eff', if constr = None then constr' else constr

let add_effect (eff, constr) e =
  ESet.add e eff, constr

let no_effect = ESet.empty, None

let unify_effset ((eff, constr) as e) ((eff', constr') as e') =
  if ESet.mem EDiv eff && not ESet.(mem EDiv eff') || ESet.mem EDiv eff' && not ESet.(mem EDiv eff) then
    raise EffectUnification
  else
    let has_console (eff, constr) =
      ESet.mem EConsole eff || match constr with
        None -> false
      | Some r -> match !r with
          None -> false
        | Some b -> b
    in
    let unify_console (eff, constr) (eff', constr') =
      if has_console (eff, constr) && not ESet.(mem EConsole eff') then
        begin match constr' with
          None -> raise EffectUnification
        | Some r ->
            match !r with
              Some false -> raise EffectUnification
            | _ -> r := Some true
        end;
      if not ESet.(mem EConsole eff') then
        match constr with
          None -> ()
        | Some r ->
            match !r with
              Some true -> raise EffectUnification
            | _ -> r := Some false
    in
    unify_console e e';
    unify_console e' e

let check_valid_effect {string; loc} =
  if not SMap.(mem string valid_effects) then
    error_str loc @@ sprintf "Unknown effect: %s" string
