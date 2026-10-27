let enabled = Sys.introspection_enabled

module Desc = struct
  type approx =
    | Any
    | Char
    | Int
    | Constants of string array
    | Polymorphic_variants

  type view =
    | Unknown
    | Array of approx
    | Tuple of { name: string; tag: int; fields: (string * approx) array }
    | Record of { name: string; tag: int; fields: (string * approx) array }
    | Polymorphic_variant
    | Polymorphic_variant_constant of string

  type t = view

  let unknown = Unknown

  let simple_ref = Record {name = ""; tag = 0; fields = [|"contents", Any|]}

  let compare a b =
    match a, b with
    | Unknown, Unknown -> 0
    | Unknown, _ -> -1
    | _, Unknown -> +1
    | a, b -> compare a b

  let equal a b = compare a b = 0

  let view x = x

  let dump_array o f = function
    | [||] -> o "[||]"
    | members ->
      o "[|";
      f o members.(0);
      for i = 1 to Array.length members - 1 do
        o ";";
        f o members.(i)
      done;
      o "|]"

  let dump_escaped o s =
    o (Printf.sprintf "%S" s)

  let dump_approx o = function
    | Any -> o "Any"
    | Char -> o "Char"
    | Int -> o "Int"
    | Polymorphic_variants -> o "Polymorphic_variants"
    | Constants strings ->
        o "Constants ";
        dump_array o dump_escaped strings

  let dump_field o (f, apx) =
    o "(";
    dump_escaped o f;
    o ", ";
    dump_approx o apx;
    o ")"

  let dump o = function
    | Unknown ->
        o "Unknown"
    | Array apx ->
        o "Array ";
        dump_approx o apx
    | Tuple {name; tag; fields} ->
        o "Tuple {name = ";
        dump_escaped o name;
        o "; tag = ";
        o (string_of_int tag);
        o "; fields = ";
        dump_array o dump_field fields;
        o "}"
    | Record {name; tag; fields} ->
        o "Record {name = ";
        dump_escaped o name;
        o "; tag = ";
        o (string_of_int tag);
        o "; fields = ";
        dump_array o dump_field fields;
        o "}"
    | Polymorphic_variant ->
        o "Polymorphic_variant"
    | Polymorphic_variant_constant name ->
        o "Polymorphic_variant_constant ";
        dump_escaped o name

  let to_string t =
    let buf = Buffer.create 63 in
    dump (Buffer.add_string buf) t;
    Buffer.contents buf

  let hash_variant s =
    let accu = ref 0 in
    for i = 0 to String.length s - 1 do
      accu := 223 * !accu + Char.code s.[i]
    done;
    (* reduce to 31 bits *)
    accu := !accu land (1 lsl 31 - 1);
    (* make it signed for 64 bits architectures *)
    if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

  (* The whole descriptor is hashed, through its canonical textual
     representation.  [Hashtbl.hash] hashes all the bytes of a string and
     returns the same 30-bit non-negative integer on all platforms (native,
     bytecode on 32-bit and 64-bit hosts, js_of_ocaml), which matters because
     hashes computed by the compiler are compared with headers read at
     runtime. *)
  let hash_bits = 30

  let hash = function
    | Unknown -> 0
    | d ->
        (* 0 is reserved for [Unknown] *)
        let h = Hashtbl.hash (to_string d) in
        if h = 0 then 1 else h

  (* The index of a descriptor in a configuration with [bits] reserved header
     bits.  0 is reserved for "no descriptor". *)
  let index_of_hash ~bits h =
    if bits <= 0 || h = 0 then 0
    else
      let i = h land ((1 lsl (min bits hash_bits)) - 1) in
      if i = 0 then 1 else i

  let index ~bits d = index_of_hash ~bits (hash d)

  type entry = int * t

  let entry d = (hash d, d)

  external read_self_descriptors : unit -> entry list =
    "caml_read_bdsc_section"

  external compiler_descriptors : unit -> entry list ref =
    "caml_compiler_block_descs"
end

module Index = struct
  let reserved_bits = Obj.reserved_bits ()

  type t = {
    lock : Mutex.t;
    descriptors : (int, Desc.t list) Hashtbl.t;
    known_variants: (string, unit) Hashtbl.t;
    variants : (int, string list) Hashtbl.t;
  }

  let raw_make () : t = {
    lock = Mutex.create ();
    descriptors = Hashtbl.create 17;
    known_variants = Hashtbl.create 17;
    variants = Hashtbl.create 17;
  }

  (* Must be called with [t.lock] held.
     [h] is the hash computed by the compiler, it is never recomputed: this
     way the runtime agrees with the code that allocated the blocks. *)
  let register_unlocked (t : t) ((h, desc) : Desc.entry) =
    match desc with
    | Desc.Polymorphic_variant_constant name ->
        if not (Hashtbl.mem t.known_variants name) then begin
          Hashtbl.add t.known_variants name ();
          let i = Desc.hash_variant name in
          match Hashtbl.find t.variants i with
          | exception Not_found -> Hashtbl.add t.variants i [name]
          | names -> Hashtbl.replace t.variants i (name :: names)
        end
    | tag ->
        let i = Desc.index_of_hash ~bits:reserved_bits h in
        begin match Hashtbl.find t.descriptors i with
        | exception Not_found -> Hashtbl.add t.descriptors i [tag]
        | tags ->
            if not (List.exists (Desc.equal tag) tags) then
              Hashtbl.replace t.descriptors i (tag :: tags)
        end

  let register_list_unlocked t tags =
    if enabled then List.iter (register_unlocked t) tags

  let register t desc =
    if enabled then Mutex.protect t.lock (fun () -> register_unlocked t desc)

  let register_list t tags =
    if enabled then
      Mutex.protect t.lock (fun () -> register_list_unlocked t tags)

  let make () =
    let result = raw_make () in
    register_list result [Desc.entry Desc.unknown; Desc.entry Desc.simple_ref];
    result

  let lookup (t : t) i =
    Mutex.protect t.lock (fun () ->
        try Hashtbl.find t.descriptors i
        with Not_found -> [])

  let lookup_by_reserved_bits t o =
    lookup t (Obj.get_reserved o)

  let lookup_variant (t : t) i =
    Mutex.protect t.lock (fun () ->
        try Hashtbl.find t.variants i
        with Not_found -> [])

  (* State of the index of the current process, protected by [self_lock] *)

  let self_lock = Mutex.create ()

  let self_index_state = ref None

  (* Last value of [!(Desc.compiler_descriptors ())] that was registered.
     New descriptors are consed in front of the list, so we only register the
     prefix that differs from the previous value. *)
  let last_compiler_descriptors = ref []

  let dynamic_libraries = ref []

  (* Last value of [!dynamic_libraries] that was registered. *)
  let last_dynamic_libraries = ref []

  let register_new_prefix index ~last current register =
    if current != !last then begin
      let rec aux l =
        if l != !last then
          match l with
          | [] -> ()
          | x :: xs -> register index x; aux xs
      in
      aux current;
      last := current
    end

  let self_index () =
    Mutex.protect self_lock (fun () ->
        let index = match !self_index_state with
          | Some index -> index
          | None ->
              let index = make () in
              register_list index (Desc.read_self_descriptors ());
              self_index_state := Some index;
              index
        in
        register_new_prefix index
          ~last:last_compiler_descriptors
          !(Desc.compiler_descriptors ())
          register;
        register_new_prefix index
          ~last:last_dynamic_libraries
          !dynamic_libraries
          register_list;
        index
      )

  let add_dynamic_library = function
    | [] -> ()
    | library ->
        Mutex.protect self_lock (fun () ->
            dynamic_libraries := library :: !dynamic_libraries)

  let dynamic_libraries () =
    Mutex.protect self_lock (fun () -> !dynamic_libraries)
end

module Dyn = struct
  type t = Desc.approx * Obj.t
  let get_approx (approx, _ : t) = approx
  let get_obj (_, obj : t) = obj

  let lift ?(approx=Desc.Any) (obj : Obj.t) : t = (approx, obj)
  let lift_any ?approx obj = lift ?approx (Obj.repr obj)

  type 'a fields = {count: int; get: int -> 'a}
  let field_count f = f.count
  let field_get f i =
    if (i < 0 || i >= f.count) then
      invalid_arg "Introspect.Desc.field_get: index out of bounds";
    f.get i

  type view =
    | String of string
    (* [String "foo"] = "foo" *)
    | Float of float
    (* [Float 12.12] = 12.12 *)
    | Char of char
    (* [Char 'c'] = 'x' *)
    | Int_or_constant of int * string list
    (* [Int_or_constant (1, ["`Bla"])] = 1 or `Bla *)
    | Constant of string list
    (* [Constant ["`Bla"]] = `Bla *)
    | Array of t fields
    (* [Array f] = [|f0, f1, f2, ...|] *)
    | Tuple of { name: string; fields: (string * t) fields }
    (* [Tuple f] = (f0, f1, f2, ...) *)
    | Record of { name: string; fields: (string * t) fields }
    (* [Record f] = { fst f0 : snd f0; fst f1 : snd f1; ... } *)
    | Polymorphic_variant of string * t
    (* [Extension ("Foo", uid, args)] = Foo args
       given exception Foo (of args)
          or type t += Foo (of args) *)
    | Extension of string * int * t fields
    | Closure | Abstract | Custom | Unknown
    | Lazy_unforced
    | Lazy_forcing
    | Lazy_forward of t

  let block_size obj =
    if Obj.tag obj = Obj.double_array_tag
    then Array.Floatarray.length (Obj.obj obj)
    else Obj.size obj

  let fields_of_block f obj =
    let get =
      if Obj.tag obj = Obj.double_array_tag
      then fun i -> f i (Obj.repr (Obj.double_field obj i))
      else fun i -> f i (Obj.field obj i)
    in
    {count = block_size obj; get}

  (* Can [v] be a value approximated by [approx]? *)
  let approx_compatible (approx : Desc.approx) v =
    match approx with
    | Desc.Any -> true
    | Desc.Int -> Obj.is_int v
    | Desc.Char ->
        Obj.is_int v && (let c : int = Obj.obj v in c >= 0 && c < 256)
    | Desc.Constants names ->
        Obj.is_block v ||
        (let c : int = Obj.obj v in c >= 0 && c < Array.length names)
    | Desc.Polymorphic_variants ->
        Obj.is_int v || (Obj.tag v = 0 && Obj.size v = 2)

  (* Check the fields of [obj] against a descriptor. Only used to
     disambiguate between candidates, so a bounded number of array elements
     is checked. *)
  let fields_compatible obj (desc : Desc.t) =
    let check_fields n approx =
      let rec loop i = i >= n || (approx i (Obj.field obj i) && loop (i + 1)) in
      Obj.tag obj = Obj.double_array_tag (* fields are floats *) || loop 0
    in
    match desc with
    | Desc.Array approx ->
        check_fields (min 32 (Obj.size obj))
          (fun _ v -> approx_compatible approx v)
    | Desc.Tuple {fields; _} | Desc.Record {fields; _} ->
        check_fields (Array.length fields)
          (fun i v -> approx_compatible (snd fields.(i)) v)
    | Desc.Polymorphic_variant
    | Desc.Polymorphic_variant_constant _ | Desc.Unknown -> true

  let find_tag t obj =
    let otag = Obj.tag obj in
    if otag > Obj.last_non_constant_constructor_tag &&
       otag <> Obj.double_array_tag then None
    else
      let osize = block_size obj in
      let select = function
        | Desc.Array _ ->
            otag = 0 || otag = Obj.double_array_tag
        | Desc.Polymorphic_variant ->
            otag = 0 && osize = 2 && Obj.is_int (Obj.field obj 0)
        | Desc.Tuple t ->
            otag = t.tag && osize = Array.length t.fields
        | Desc.Record t ->
            otag = t.tag && osize = Array.length t.fields
        | Desc.Polymorphic_variant_constant _ -> false
        | Desc.Unknown -> false
      in
      let unique = function
        | [] -> None
        | d :: ds -> if List.for_all (Desc.equal d) ds then Some d else None
      in
      (* Different descriptors can share the same index (hash collisions).
         If several of them are compatible with the tag and size of [obj],
         try to discriminate using the contents of the fields.  If this is
         still ambiguous, fall back to raw printing rather than printing
         with wrong names. *)
      match List.filter select (Index.lookup_by_reserved_bits t obj) with
      | [] -> None
      | [d] -> Some d
      | candidates ->
          match unique candidates with
          | Some _ as result -> result
          | None -> unique (List.filter (fields_compatible obj) candidates)

  let no_approx (_ : int) (obj : Obj.t) = (Desc.Any, obj)

  let no_name (_ : int) (obj : Obj.t) = ("", (Desc.Any, obj))

  let as_extension_tag obj =
    if Obj.tag obj = Obj.object_tag && Obj.size obj = 2 then
      let name = Obj.field obj 0 in
      let uid = Obj.field obj 1 in
      if Obj.is_int uid && Obj.tag name = Obj.string_tag
      then Some ((Obj.obj name : string), (Obj.obj uid : int))
      else None
    else None

  let view_raw (obj : Obj.t) =
    if Obj.is_int obj then
      Int_or_constant (Obj.obj obj, [])
    else
      let tag = Obj.tag obj in
      if tag = 0 then (
        let size = Obj.size obj in
        if size >= 2 then
          match as_extension_tag (Obj.field obj 0) with
          | None ->
             Tuple { name = ""; fields = fields_of_block no_name obj }
          | Some (path, uid) ->
             let get i = no_approx 0 (Obj.field obj (i + 1)) in
             Extension (path, uid, {count = size - 1; get})
        else
          Tuple { name = ""; fields = fields_of_block no_name obj }
      ) else if tag <= Obj.last_non_constant_constructor_tag then
        Tuple { name = "Tag#" ^ string_of_int tag;
                fields = fields_of_block no_name obj }
      else if tag = Obj.string_tag then
        String (Obj.obj obj)
      else if tag = Obj.double_tag then
        Float (Obj.obj obj)
      else if tag = Obj.double_array_tag then
        Array (fields_of_block no_approx obj)
      else if tag = Obj.closure_tag || tag = Obj.infix_tag then
        Closure
      else if tag = Obj.abstract_tag then
        Abstract
      else if tag = Obj.custom_tag then
        Custom
      else if tag = Obj.lazy_tag then
        Lazy_unforced
      else if tag = Obj.forcing_tag then
        Lazy_forcing
      else if tag = Obj.forward_tag then
        Lazy_forward (lift (Obj.field obj 0))
      else
        match as_extension_tag obj with
        | Some (path, uid) ->
           let get _ = assert false in
           Extension (path, uid, {count = 0; get})
        | None -> Unknown

  let view ?(index=Index.self_index ()) (approx, obj) =
    if Obj.is_int obj then
      let i = (Obj.obj obj : int) in
      match approx with
      | Desc.Any ->
          Int_or_constant (i, Index.lookup_variant index i)
      | Desc.Int ->
          Int_or_constant (i, [])
      | Desc.Char ->
          (try Char (Char.chr i) with _ -> Int_or_constant (i, []))
      | Desc.Constants names ->
          if i >= 0 && i < Array.length names
          then Constant [names.(i)]
          else Int_or_constant (i, [])
      | Desc.Polymorphic_variants ->
          begin match Index.lookup_variant index i with
          | [] -> Int_or_constant (i, [])
          | names -> Constant (List.map ((^)"`") names)
          end
    else
      match find_tag index obj with
      | Some (Desc.Array approx ) ->
          Array (fields_of_block (fun _ obj -> approx, obj) obj)
      | Some (Desc.Record {name; fields}) ->
          let get_field i obj =
            let fname, fapprox = fields.(i) in
            (fname, (fapprox, obj))
          in
          Record { name; fields = fields_of_block get_field obj }
      | Some (Desc.Tuple {name; fields}) ->
          let get_field i obj =
            let name, approx = fields.(i) in
            (name, (approx, obj))
          in
          Tuple {name; fields = fields_of_block get_field obj}
      | Some (Desc.Polymorphic_variant) ->
          let name = (Obj.obj (Obj.field obj 0) : int) in
          let payload = Obj.field obj 1 in
          begin match Index.lookup_variant index name with
          | [] -> Polymorphic_variant (string_of_int name, lift payload)
          | name :: _ -> Polymorphic_variant (name, lift payload)
          end
      | Some (Desc.Unknown)
      | Some (Desc.Polymorphic_variant_constant _)
      | None -> view_raw obj

  let view_obj ?index ?approx obj = view ?index (lift ?approx obj)
  let view_any ?index ?approx obj = view ?index (lift_any ?approx obj)

  module H = Hashtbl.Make(struct type t = Obj.t let equal = (==) let hash = Hashtbl.hash end)
  open Format

  (* Print the fields, stopping with "..." once the step budget is exhausted
     (each printed field consumes steps). *)
  let pp_fields steps f sep ppf fields =
    let n = field_count fields in
    let rec loop i =
      if i < n then begin
        if i > 0 then fprintf ppf "%s@ " sep;
        if i > 0 && !steps <= 0 then
          pp_print_string ppf "..."
        else begin
          fprintf ppf "@[%a@]" f (field_get fields i);
          loop (i + 1)
        end
      end
    in
    loop 0

  let pp_tuple_field f ppf = function
    | ("", v) -> fprintf ppf "@[%a@]" f v
    | (k, v)  -> fprintf ppf "~%s:@[%a@]" k f v

  let pp_record_field f ppf (k, v) =
    fprintf ppf "@[%s = %a@]" k f v

  (* Iterative walk of the spine: cons cells stay in the cycle table until the
     whole list is printed. *)
  let pp_list_elements table index steps f ppf fields =
    let rec loop fields visited =
      begin match field_get fields 0 with
      | "", car -> fprintf ppf "@[%a@]" f car;
      | lbl, car -> fprintf ppf "~%s:@[%a@]" lbl f car;
      end;
      let _lbl, cdr = field_get fields 1 in
      match view ~index cdr with
      | Constant ["[]"]
      | Int_or_constant (0, _) -> visited
      | Tuple {name = "::"; fields} when field_count fields = 2 ->
          fprintf ppf ";@ ";
          if !steps <= 0 then begin
            pp_print_string ppf "...";
            visited
          end else begin
            let raw = get_obj cdr in
            if H.mem table raw then begin
              fprintf ppf "<cycle>";
              visited
            end else begin
              H.add table raw ();
              loop fields (raw :: visited)
            end
          end
      | _ -> fprintf ppf "<malformed list>"; visited
    in
    List.iter (H.remove table) (loop fields [])

  let lpar ppf cond =
    if cond then fprintf ppf "(@[<hv>"

  let rpar ppf cond =
    if cond then fprintf ppf "@])"

  (* Float printing from typing/oprint.ml *)

  let valid_float_lexeme s =
    let l = String.length s in
    let rec loop i =
      if i >= l then s ^ "." else
      match s.[i] with
      | '0' .. '9' | '-' -> loop (i+1)
      | _ -> s
    in
    loop 0

  let float_repres f =
    match classify_float f with
      FP_nan -> "nan"
    | FP_infinite ->
        if f < 0.0 then "neg_infinity" else "infinity"
    | _ ->
        let float_val =
          let s1 = Printf.sprintf "%.12g" f in
          if f = float_of_string s1 then s1 else
          let s2 = Printf.sprintf "%.15g" f in
          if f = float_of_string s2 then s2 else
          Printf.sprintf "%.18g" f
        in
        valid_float_lexeme float_val

  let pp_dynval table index steps self fragile ppf : view -> _ = function
    | String s ->
        fprintf ppf "%S" s
    | Float f ->
        pp_print_string ppf (float_repres f)
    | Char c ->
        fprintf ppf "%C" c
    | Int_or_constant (i, keys) ->
        let protect = fragile && not (List.is_empty keys) in
        fprintf ppf "%a%d%a%a"
          lpar protect
          i
          (fun ppf -> List.iter (fprintf ppf " or `%s")) keys
          rpar protect
    | Constant [] ->
        fprintf ppf "<invalid constant>"
    | Constant [name] ->
        fprintf ppf "%s" name
    | Constant (name :: names) ->
        fprintf ppf "%a%s%a%a"
          lpar fragile
          name
          (fun ppf -> List.iter (fprintf ppf " or %s")) names
          rpar fragile
    | Array arr ->
        fprintf ppf "[|@[<hv>%a@]|]" (pp_fields steps (self false) ";") arr
    | Tuple { name ="::"; fields } when field_count fields = 2 ->
        fprintf ppf "[@[<hv>%a@]]"
          (pp_list_elements table index steps (self false)) fields
    | Tuple { name = ""; fields } ->
        fprintf ppf "(@[<hv>%a@])"
          (pp_fields steps (pp_tuple_field (self false)) ",") fields
    | Tuple { name; fields }
      when field_count fields = 1 && not fragile && fst (field_get fields 0) = "" ->
        fprintf ppf "%s @[<hv>%a@]" name (self true) (snd (field_get fields 0))
    | Tuple { name; fields } ->
        lpar ppf fragile;
        if name <> "" then fprintf ppf "%s " name;
        fprintf ppf "(@[<hv>%a@])"
          (pp_fields steps (pp_tuple_field (self false)) ",") fields;
        rpar ppf fragile
    | Record {name; fields} ->
        let protect = fragile && name <> "" in
        lpar ppf protect;
        if name <> "" then fprintf ppf "%s " name;
        fprintf ppf "{@[<hv>%a@]}"
          (pp_fields steps (pp_record_field (self false)) ";") fields;
        rpar ppf protect
    | Extension (name, uid, fields) when field_count fields = 0 ->
        fprintf ppf "%s/%d" name uid
    | Extension (name, uid, fields) ->
        fprintf ppf "%s/%d (@[<hv>%a@])"
          name uid (pp_fields steps (self false) ",") fields
    | Polymorphic_variant (name, payload) ->
        fprintf ppf "%a`%s @[<hv>%a@]%a"
          lpar fragile
          name (self true) payload
          rpar fragile
    | Closure  -> fprintf ppf "<closure>"
    | Abstract -> fprintf ppf "<abstract>"
    | Custom   -> fprintf ppf "<custom>"
    | Unknown  -> fprintf ppf "<unknown>"
    | Lazy_unforced  -> fprintf ppf "<lazy>"
    | Lazy_forcing   -> fprintf ppf "<lazy (forcing)>"
    | Lazy_forward d ->
        fprintf ppf "%alazy @[<hv>%a@]%a"
          lpar fragile
          (self true) d
          rpar fragile

  let get_index = function
    | Some index -> index
    | None -> Index.self_index ()

  let pp_raw table ?index ?(depth=20) ?(steps=ref max_int) fragile ppf obj =
    let index = get_index index in
    let rec aux depth fragile ppf obj =
      if depth <= 0 || !steps <= 0 then
        Format.pp_print_string ppf "..."
      else (
        decr steps;
        let raw = get_obj obj in
        let protect = Obj.is_block raw && Obj.tag raw < Obj.no_scan_tag in
        if protect && H.mem table raw then
          Format.pp_print_string ppf "<cycle>"
        else (
          if protect then H.add table raw ();
          pp_dynval table index steps (aux (depth - 1)) fragile ppf
            (view ~index obj);
          if protect then H.remove table raw
        )
      )
    in
    aux (depth - 1) fragile ppf obj

  let pp ?index ?depth ?steps ppf obj =
    pp_raw (H.create 7) ?index ?depth ?steps false ppf obj

  let pp_view ?index ?depth ?(steps=ref max_int) ppf view =
    let index = get_index index in
    let table = H.create 7 in
    pp_dynval table index steps (pp_raw table ~index ?depth ~steps) false ppf
      view
end

module Print = struct
  let format_any ?index ?depth ?steps ppf any =
    Dyn.pp ?index ?depth ?steps ppf (Dyn.lift (Obj.repr any))

  let print_any ?index ?depth ?steps obj =
    Format.fprintf Format.std_formatter "@[%a@]%!"
      (format_any ?index ?depth ?steps) obj

  let prerr_any ?index ?depth ?steps obj =
    Format.fprintf Format.err_formatter "@[%a@]%!"
      (format_any ?index ?depth ?steps) obj

  let print_any_endline ?index ?depth ?steps obj =
    Format.fprintf Format.std_formatter "@[%a@]\n%!"
      (format_any ?index ?depth ?steps) obj

  let prerr_any_endline ?index ?depth ?steps obj =
    Format.fprintf Format.err_formatter "@[%a@]\n%!"
      (format_any ?index ?depth ?steps) obj
end

module P_list = struct
  type any =
    | []
    | (::) : 'a * any -> any
end

module P = struct
  (* Generic printer when in a hurry *)
  let fprint oc xs =
    let ppf = Format.formatter_of_out_channel oc in
    let rec aux = function
      | P_list.[] -> ()
      | P_list.(x :: xs) as self ->
          let x = match Dyn.view_any self with
            | Dyn.Tuple {fields; _} | Dyn.Record {fields; _} ->
                snd (Dyn.field_get fields 0)
            | _ -> Dyn.lift_any x
          in
          begin match Dyn.view x with
          | Dyn.String s -> Format.pp_print_string ppf s
          | _ -> Dyn.pp ppf x
          end;
          aux xs
    in
    aux xs;
    Format.pp_print_flush ppf ()

  let print xs = fprint stdout xs

  let eprint xs = fprint stderr xs

  let fprintln oc xs =
    fprint oc xs;
    output_char oc '\n'

  let println xs =
    fprintln stdout xs

  let eprintln xs =
    fprintln stderr xs;
    flush stderr
end

module Json = struct
  open Dyn

  let hex = "0123456789abcdef"

  (* ---------- Generic json writer ---------- *)

  type root = View of view | Value of Dyn.t

  let write_root ~output_substring ~output_char
        ?index ?(depth = 20) ?(steps = ref max_int) root =
    let index = Dyn.get_index index in
    let output_string s = output_substring s 0 (String.length s) in
    let table = H.create 7 in

    (* -- primitives -- *)

    let escaped_byte c =
      let c = Char.code c in
      output_string "\\u00";
      output_char hex.[c lsr 4];
      output_char hex.[c land 15]
    in

    (* Valid UTF-8 passes through; invalid bytes become \u00XX (Latin-1),
       so the output is always valid JSON. *)
    let json_string s =
      output_char '"';
      let len = String.length s in
      let start = ref 0 and i = ref 0 in
      let flush () =
        if !i > !start then output_substring s !start (!i - !start)
      in
      let escape str =
        flush (); output_string str; incr i; start := !i
      in
      while !i < len do
        (* Fast path: skip plain ASCII bytes. *)
        while
          !i < len &&
            (let c = String.unsafe_get s !i in
             c >= ' ' && c < '\128' && c <> '"' && c <> '\\')
        do incr i done;
        (* Slow path: at most one special byte / UTF-8 sequence, then
           back to the fast path. Safe runs stay pending in [start..i)
           and are flushed as one slice. *)
        if !i < len then
          match String.unsafe_get s !i with
          | '"' -> escape "\\\""
          | '\\' -> escape "\\\\"
          | '\n' -> escape "\\n"
          | '\r' -> escape "\\r"
          | '\t' -> escape "\\t"
          | '\b' -> escape "\\b"
          | '\012' -> escape "\\f"
          | '\000' .. '\031' as c ->
             flush (); escaped_byte c; incr i; start := !i
          | '\032' .. '\127' -> incr i (* unreachable: fast path *)
          | c ->
             let d = String.get_utf_8_uchar s !i in
             if Uchar.utf_decode_is_valid d then
               i := !i + Uchar.utf_decode_length d
             else begin
                 flush (); escaped_byte c; incr i; start := !i
               end
      done;
      flush ();
      output_char '"'
    in

    (* Shortest of %.12g / %.15g / %.17g that round-trips; %g output is
       valid JSON. JSON has no nan/infinity, so those become strings. *)
    let json_float f =
      match classify_float f with
      | FP_nan -> output_string "\"nan\""
      | FP_infinite ->
         output_string (if f < 0.0 then "\"-infinity\"" else "\"infinity\"")
      | _ ->
         let s1 = Printf.sprintf "%.12g" f in
         if f = float_of_string s1 then output_string s1 else
           let s2 = Printf.sprintf "%.15g" f in
           if f = float_of_string s2 then output_string s2 else
             output_string (Printf.sprintf "%.17g" f)
    in

    (* -- values -- *)

    let rec value depth obj =
      if depth <= 0 || !steps <= 0 then output_string "\"...\""
      else begin
          decr steps;
          let raw = get_obj obj in
          let protect = Obj.is_block raw && Obj.tag raw < Obj.no_scan_tag in
          if not protect then dynval (depth - 1) (view ~index obj)
          else if H.mem table raw then output_string "\"<cycle>\""
          else begin
              H.add table raw ();
              dynval (depth - 1) (view ~index obj);
              H.remove table raw
            end
        end

    (* {"Name": ...} *)
    and open_constr name =
      output_char '{'; json_string name; output_char ':'

    and dynval depth (v : view) =
      match v with
      | String s -> json_string s
      | Float f -> json_float f
      | Char c -> json_string (String.make 1 c)
      | Int_or_constant (i, []) -> output_string (string_of_int i)
      | Int_or_constant (_, keys) ->
         (* matches a polymorphic-variant hash: print as a constant *)
         json_string (String.concat "|" keys)
      | Constant [] -> output_string "\"<invalid constant>\""
      | Constant ["[]"] -> output_string "[]"
      | Constant ["()"] -> output_string "null"
      | Constant ["true"] -> output_string "true"
      | Constant ["false"] -> output_string "false"
      | Constant names -> json_string (String.concat "|" names)
      | Array arr ->
         elements depth (field_count arr) (field_get arr)
      | Tuple { name = "::"; fields } when field_count fields = 2 ->
         list depth fields
      | Tuple { name = ""; fields } -> tuple depth fields
      | Tuple { name; fields } ->
         open_constr name; payload depth fields; output_char '}'
      | Record { name = ""; fields } -> record depth fields
      | Record { name; fields } ->
         open_constr name; record depth fields; output_char '}'
      | Extension (name, _uid, fields) when field_count fields = 0 ->
         json_string name
      | Extension (name, _uid, fields) ->
         open_constr name;
         let n = field_count fields in
         if n = 1 then value depth (field_get fields 0)
         else elements depth n (field_get fields);
         output_char '}'
      | Polymorphic_variant (name, p) ->
         open_constr name; value depth p; output_char '}'
      | Lazy_forward d -> value depth d
      | Lazy_unforced -> output_string "\"<lazy>\""
      | Lazy_forcing -> output_string "\"<lazy (forcing)>\""
      | Closure -> output_string "\"<closure>\""
      | Abstract -> output_string "\"<abstract>\""
      | Custom -> output_string "\"<custom>\""
      | Unknown -> output_string "\"<unknown>\""

    (* JSON array of [n] elements, ending with "..." once the step budget is
       exhausted. *)
    and elements depth n get =
      output_char '[';
      let rec loop i =
        if i < n then begin
          if i > 0 then output_char ',';
          if i > 0 && !steps <= 0 then output_string "\"...\""
          else begin
            value depth (get i);
            loop (i + 1)
          end
        end
      in
      loop 0;
      output_char ']'

    (* Lone unlabelled argument is inlined:
       [Some 1] -> {"Some":1}, [Foo (1, 2)] -> {"Foo":[1,2]}. *)
    and payload depth fields =
      if field_count fields = 1 && fst (field_get fields 0) = "" then
        value depth (snd (field_get fields 0))
      else tuple depth fields

    (* Unlabelled -> array; any labels -> object keyed by label or index. *)
    and tuple depth fields =
      let n = field_count fields in
      let labelled = ref false in
      for i = 0 to n - 1 do
        if fst (field_get fields i) <> "" then labelled := true
      done;
      if not !labelled then
        elements depth n (fun i -> snd (field_get fields i))
      else begin
          output_char '{';
          for i = 0 to n - 1 do
            if i > 0 then output_char ',';
            let k, v = field_get fields i in
            json_string (if k = "" then string_of_int i else k);
            output_char ':';
            value depth v
          done;
          output_char '}'
        end

    and record depth fields =
      output_char '{';
      for i = 0 to field_count fields - 1 do
        if i > 0 then output_char ',';
        let k, v = field_get fields i in
        json_string k;
        output_char ':';
        value depth v
      done;
      output_char '}'

    (* Tail-recursive spine walk; cons cells stay in the cycle table until
       the whole list is written. *)
    and list depth fields =
      output_char '[';
      let rec loop fields visited =
        value depth (snd (field_get fields 0));
        let cdr = snd (field_get fields 1) in
        match view ~index cdr with
        | Constant ["[]"] | Int_or_constant (0, _) -> visited
        | Tuple { name = "::"; fields } when field_count fields = 2 ->
           output_char ',';
           if !steps <= 0 then begin output_string "\"...\""; visited end
           else begin
               let raw = get_obj cdr in
               if H.mem table raw then begin
                   output_string "\"<cycle>\""; visited
                 end else begin
                   H.add table raw ();
                   loop fields (raw :: visited)
                 end
             end
        | _ ->
           output_char ',';
           output_string "\"<malformed list>\"";
           visited
      in
      List.iter (H.remove table) (loop fields []);
      output_char ']'
    in
    match root with
    | View v -> dynval (depth - 1) v
    | Value v -> value depth v

  let write ~output_substring ~output_char ?index ?depth ?steps root =
    write_root ~output_substring ~output_char ?index ?depth ?steps (View root)

  (* ---------- Instances ---------- *)

  let view_to_buffer ?index ?depth ?steps b root =
    write
      ~output_substring:(Buffer.add_substring b)
      ~output_char:(Buffer.add_char b)
      ?index ?depth ?steps root

  let view_to_string ?index ?depth ?steps root =
    let b = Buffer.create 256 in
    view_to_buffer ?index ?depth ?steps b root;
    Buffer.contents b

  let view_to_channel ?index ?depth ?steps oc root =
    write
      ~output_substring:(output_substring oc)
      ~output_char:(output_char oc)
      ?index ?depth ?steps root

  (* Start from the value itself (rather than its view) so that the root is
     part of cycle detection. *)
  let to_buffer ?index ?depth ?steps b root =
    write_root
      ~output_substring:(Buffer.add_substring b)
      ~output_char:(Buffer.add_char b)
      ?index ?depth ?steps (Value (Dyn.lift_any root))

  let to_string ?index ?depth ?steps root =
    let b = Buffer.create 256 in
    to_buffer ?index ?depth ?steps b root;
    Buffer.contents b

  let to_channel ?index ?depth ?steps oc root =
    write_root
      ~output_substring:(output_substring oc)
      ~output_char:(output_char oc)
      ?index ?depth ?steps (Value (Dyn.lift_any root))
end
