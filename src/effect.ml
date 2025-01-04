open Syntax
open Syntax.Effect
open Context
open Format
open Error

let valid_effects_list = ["div", EDiv; "console", EConsole]

let valid_effects = valid_effects_list |> List.to_seq |> SMap.of_seq

let (++) e e' =
  match e, e' with
  | NoRec s, NoRec s' -> NoRec (ESet.union s s')
  | NoRec s, HasRec s' | HasRec s, NoRec s' | HasRec s, HasRec s' ->
      HasRec (ESet.union s s')

let unify_effset env e e' = match e, e' with
  | NoRec s, NoRec s' -> s = s'
  | NoRec s, HasRec s' | HasRec s', NoRec s ->
      let sconsole = ESet.mem EConsole s in
      (match !(env.rec_has_console) with
         None -> env.rec_has_console := Some sconsole; true
       | Some b -> b = sconsole) && ESet.mem EDiv s = ESet.mem EDiv s'
  | HasRec s, HasRec s' ->
      if ESet.mem EConsole s <> ESet.mem EConsole s' then
        (match !(env.rec_has_console) with
          None -> env.rec_has_console := Some true; true
        | Some b -> b) && ESet.mem EDiv s = ESet.mem EDiv s'
      else ESet.mem EDiv s = ESet.mem EDiv s'

let get_set ctx = function
    NoRec s -> s
  | HasRec s when !(ctx.rec_has_console) = Some true -> ESet.add EConsole s
  | HasRec s -> s

let erase_effect {string; strloc} =
  match SMap.find_opt string valid_effects with
  | None -> error_str strloc @@ sprintf "Unknown effect: %s" string
  | Some e -> e

let erase_effects l =
  NoRec (List.map erase_effect l |> ESet.of_list)
