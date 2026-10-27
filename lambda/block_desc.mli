(* Descriptors definition and construction *)

type approx = Introspect.Desc.approx

type t = Introspect.Desc.t

val dump : (string -> unit) -> t -> unit
val compare : t -> t -> int
val format : Format.formatter -> t -> unit

(* Print a descriptor in intermediate representations: nothing unless
   [-dblock-desc] is set, in which case the descriptor is printed after
   [sep] (a space for [format_dumped]). *)
val format_dumped : Format.formatter -> t -> unit
val format_dumped_sep : string -> Format.formatter -> t -> unit

(* Index of a descriptor in the reserved header bits of the target
   configuration (0 if introspection is disabled). *)
val index : t -> int

val empty : t
val simple_ref : t
val make_array : approx -> t
val make_tuple : int -> string -> (string * approx) array -> t
val make_record : int -> string -> (string * approx) array -> t
val register_polymorphic_variant : string -> unit
val make_polymorphic_variant : string -> t

(* Manage collections of descriptors *)

val pending_descriptors : unit -> t list

(* A library is a sorted list of descriptors, each paired with its hash
   ([Introspect.Desc.hash]). The runtime uses the stored hash and never
   recomputes it. *)
type library = private Introspect.Desc.entry list
val empty_library : library
val iter_library : (int -> t -> unit) -> library -> unit

val emit : unit -> library
val reset : unit -> unit

val link : library list -> library

(* Report descriptors that share an index in the target configuration and
   cannot be distinguished at runtime by the tag and size of blocks. *)
val report_collisions : Format.formatter -> library -> unit
