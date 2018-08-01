open Format
open Capnptk.Declarative

(* This can be done carefully *)
type context = {
  fmt : formatter;
  get_node : int64 -> (Schema.Node.t c);
}

(* 
 * We want to generate in a predictable fashion the name that we will transform to if a keyword is matched.
 * This means that we need to define transformations that occur in the following situations:
   * The simplest mapping is to add an underscore.
   * This means that object -> object_. But what if we define an object_?
 * *)


let open_top fmt = 
  fprintf fmt "@[<v>"; fmt

let comment comment fmt = 
  fprintf fmt "@,@,(* @[";
  pp_print_text fmt comment;
  fprintf fmt "@ *) @]@,";
  fmt

let statement_open name fmt =
  fprintf fmt "open %s@ " name; fmt


let import_from_head name fmt =
  fprintf fmt "@ type t = Self.%s.t@ let  t = Self.%s.t" name name; fmt

let open_head fmt = 
  fprintf fmt "@ @[@[<v 2>module Self = struct"; fmt

let open_body fmt = 
  fprintf fmt "@ @[<v 0>"; fmt

let close_body fmt = 
  fprintf fmt "@ @]"; fmt

let open_module name fmt =
  fprintf fmt "@ @[@[<hov 2>module %s = struct" name; fmt

let open_body_module name fmt =
  fprintf fmt "@ @[<v>@[<v 2>module %s = struct" name; fmt

let structure_type dwords pwords fmt =
  fprintf fmt "@ @[type t@ let t : t sg@ =@ sg %d %d@]" dwords pwords; fmt

let interface_type id fmt =
  fprintf fmt "@ @[type t@ let t : t ig@ =@ ig 0x%LxL@]" id; fmt

let enum_type enumerants fmt =
  fprintf fmt "@ @[<hov>type t =";
  enumerants |> Array.iter (fun x ->
    fprintf fmt "@ @[| %s@]" x 
  );

  fprintf fmt "@ let t =@ let f =@ function" ;
  enumerants |> Array.iteri (fun i x ->
    fprintf fmt "@ | %d -> %s" i x;
  );
  fprintf fmt "@ | n ->@ raise@ (Capnptk.OrdinalError n)";
  fprintf fmt "@ in@ let g =@ function";
  enumerants |> Array.iteri (fun i x ->
    fprintf fmt "@ | %s -> %d" x i;
  );
  fprintf fmt "@ in@ Enum(f, g)@]" ;

  fmt

let close_module fmt =
  fprintf fmt "@]@ end@]"; fmt

let close_types = close_module

let close_top fmt = 
  fprintf fmt "@]";
  fmt
