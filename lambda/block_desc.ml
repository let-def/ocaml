(* Descriptors definition and construction *)

open Introspect

type approx = Introspect.Desc.approx

type t = Introspect.Desc.t

let dump = Desc.dump

let compare = Desc.compare

let format ppf t =
  dump (Format.pp_print_string ppf) t

let index t =
  if Sys.introspection_enabled then
    Desc.index ~bits:Config.reserved_header_bits t
  else
    0

type entry = Desc.entry

external compiler_descriptors : unit -> entry list ref
  = "caml_compiler_block_descs"
let library = compiler_descriptors ()

let register t =
  if Sys.introspection_enabled then
    library := Desc.entry t :: !library; t

let make_array approx =
  register (Desc.Array approx)

let make_tuple tag name fields =
  register (Desc.Tuple {name; tag; fields})

let make_record tag name fields =
  register (Desc.Record {name; tag; fields})

let register_polymorphic_variant name =
  ignore (register (Desc.Polymorphic_variant_constant name) : Desc.t)

let make_polymorphic_variant name =
  register_polymorphic_variant name;
  register Desc.Polymorphic_variant

let empty = Desc.unknown

let simple_ref = Desc.simple_ref

(* Manage collections of descriptors *)

let pending_descriptors () = List.map snd !library

type library = entry list

let empty_library = []

let iter_library f lib = List.iter (fun (h, d) -> f h d) lib

(* The hash is a function of the descriptor: comparing descriptors is
   enough *)
let compare_entry (_, d1) (_, d2) = compare d1 d2

let emit () =
  library :=
    List.sort_uniq compare_entry (Desc.entry simple_ref :: !library);
  !library

let reset () =
  library := []

let[@tail_mod_cons] rec merge_uniq x xs = function
  | [] -> x :: xs
  | y :: ys ->
      let c = compare_entry x y in
      if c = 0 then
        merge_uniq x xs ys
      else if c < 0 then
        x :: merge_uniq y ys xs
      else
        y :: merge_uniq x xs ys

let[@tail_mod_cons] rec merge_all = function
  | [] | [_] as list -> list
  | [] :: rest -> merge_all rest
  | (x :: xs) :: ys :: rest ->
      let xy = merge_uniq x xs ys in
      xy :: merge_all rest

let rec merge_loop = function
  | [] -> []
  | [x] -> x
  | list -> merge_loop (merge_all list)

let link libraries = merge_loop libraries

(* Collisions: distinct descriptors with the same index that the runtime
   cannot tell apart using the tag and size of a block. *)

type shape =
  | Any_size of int list (* tags, any size *)
  | Sized of int * int   (* tag, size *)
  | No_block

let shape : t -> shape = function
  | Desc.Array _ -> Any_size [0; Obj.double_array_tag]
  | Desc.Tuple {tag; fields; _} | Desc.Record {tag; fields; _} ->
      Sized (tag, Array.length fields)
  | Desc.Polymorphic_variant -> Sized (0, 2)
  | Desc.Polymorphic_variant_constant _ | Desc.Unknown -> No_block

let may_overlap d1 d2 =
  match shape d1, shape d2 with
  | No_block, _ | _, No_block -> false
  | Any_size tags1, Any_size tags2 ->
      List.exists (fun t -> List.mem t tags2) tags1
  | Any_size tags, Sized (tag, _) | Sized (tag, _), Any_size tags ->
      List.mem tag tags
  | Sized (t1, s1), Sized (t2, s2) -> t1 = t2 && s1 = s2

let collisions (lib : library) =
  let by_index = Hashtbl.create 17 in
  List.iter (fun (h, d) ->
      let i = Desc.index_of_hash ~bits:Config.reserved_header_bits h in
      if i <> 0 then Hashtbl.add by_index i d)
    lib;
  let result = ref [] in
  Hashtbl.iter (fun i d ->
      List.iter (fun d' ->
          if compare d d' < 0 && may_overlap d d' then
            result := (i, d, d') :: !result)
        (Hashtbl.find_all by_index i))
    by_index;
  List.sort Stdlib.compare !result

let report_collisions ppf lib =
  if Sys.introspection_enabled then
    match collisions lib with
    | [] -> ()
    | l ->
        Format.fprintf ppf
          "@[<v>Block descriptors: %d collision(s), \
           the corresponding values may be printed without names:"
          (List.length l);
        List.iter (fun (i, d, d') ->
            Format.fprintf ppf "@,  %d: @[%a@ <-> %a@]" i format d format d')
          l;
        Format.fprintf ppf "@]@."
