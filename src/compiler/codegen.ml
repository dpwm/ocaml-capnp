open Format
open Capnptk.Serialization.Declarative

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

let ctklibs_open = statement_open "Capnptk.Serialization.Declarative" 

let interface_method iface a b name id fmt = 
  fprintf fmt "@ @[let %s = defmethod %s %s %s %d %S @]" name iface a b id name; fmt


let import_from_head name fmt =
  fprintf fmt "@ include (Self.%s : Type with type t = Self.%s.t)" name name; fmt

let open_head fmt = 
  fprintf fmt "@ @[@[<v 2>module Self = struct"; fmt

let open_body fmt = 
  fprintf fmt "@ @[<v 0>"; fmt

let close_body fmt = 
  fprintf fmt "@ @]"; fmt

let open_module name fmt =
  fprintf fmt "@ @[@[<v 2>module %s = struct" name; fmt

let open_body_module name fmt =
  fprintf fmt "@ @ @[<v>@[<v 2>module %s = struct" name; fmt

let structure_type dwords pwords fmt =
  fprintf fmt "@ @[type t'@ type t = t' sgu let t : t g@ =@ sg %d %d@]" dwords pwords; fmt

let interface_type nmeth id fmt =
  fprintf fmt "@ @[type t'@ type t = t' igu let t : t g@ =@ ig %d 0x%LxL@]" nmeth id; fmt

let let_statement name expr fmt = 
  fprintf fmt "@ let %s = %s" name expr; fmt

let union_block fmt d xs = 
  fprintf fmt "@,@ @[<v 2>type union =";
  xs |> Array.iter (function 
    | (constructor, Some typ, _, _) -> 
        fprintf fmt "@ | %s of %s" constructor typ
    | (constructor, None, _, _) -> 
        fprintf fmt "@ | %s" constructor
  );
  fprintf fmt "@]";
  fprintf fmt "@ @[<v 2>let union =";
  fprintf fmt "@ let union_tag = field t UInt16 %ldl in" d;
  fprintf fmt "@ @[<v 2>let f c =";
  fprintf fmt "@ @[<v 2>match c => union_tag with";

  xs |> Array.iter (function
    | (constructor, Some _, tag, Some accessor) ->
        fprintf fmt "@ | %d -> %s (c => (%s))" tag constructor accessor
    | (constructor, None, tag, None) ->
        fprintf fmt "@ | %d -> %s" tag constructor
    | (_, _, _, _) -> failwith "inconsistent"
  );

  fprintf fmt "@ | n -> raise (Capnptk.Serialization.OrdinalError n)";
  fprintf fmt "@]";
  fprintf fmt "@]@ in";
  fprintf fmt "@ @[<v 2>let g b = function";

  xs |> Array.iter (function
    | (constructor, Some _, tag, Some accessor) ->
        fprintf fmt "@ | %s v -> b |> set (%s) v |> set union_tag %d" constructor accessor tag 
    | (constructor, None, tag, None) ->
        fprintf fmt "@ | %s -> b |> set union_tag %d;" constructor tag 
    | (_, _, _, _) -> failwith "inconsistent"
  );

  fprintf fmt "@]@ in ug f g";
  fprintf fmt "@]"



let enum_type enumerants fmt =
  fprintf fmt "@ @[<hov>type t =";
  enumerants |> Array.iter (fun x ->
    fprintf fmt "@ @[| %s@]" x 
  );

  fprintf fmt "@ let t =@ let f =@ function" ;
  enumerants |> Array.iteri (fun i x ->
    fprintf fmt "@ | %d -> %s" i x;
  );
  fprintf fmt "@ | n ->@ raise@ (Capnptk.Serialization.OrdinalError n)";
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
