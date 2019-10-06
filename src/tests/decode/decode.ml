module ByteStream = Capnptk.Fstream.ByteStream
module Pos = Capnptk.Fstream.Pos

module Test_schema = struct
  open Capnptk.Declarative

  module Self = struct
    module Test = struct
      type t' type t = t' sgu let t : t g = sg 1 1
    end
    module TestContainer = struct
      type t' type t = t' sgu let t : t g = sg 1 1
    end
  end


  module Test = struct
    include (Self.Test : Type with type t = Self.Test.t)
    let id = field t UInt64 0l
    let name = field t Text 0l
  end

  module TestContainer = struct
    include (Self.TestContainer : Type with type t = Self.TestContainer.t)
    let contents = field t (List Self.Test.t) 0l
    let list8 = field t (List UInt8) 0l
    let list16 = field t (List UInt16) 1l
    let list32 = field t (List Int32) 2l
    let list64 = field t (List Int32) 3l
  end

end

let decode () =
  let ic = open_in "test.bin" in
  let v = Capnptk.Utils.of_input_channel ic in
  close_in ic;
  v |> Capnptk.Utils.decode Test_schema.Test.t

let test_read_pos () =
  let open Capnptk.Declarative in
  let v = decode () in
  Alcotest.(check int) "seg offset matches" 0 (ByteStream.pos v.stream).seg;
  Alcotest.(check int) "pos offset matches" 8 (ByteStream.pos v.stream).off


let test_read_int64 () =
  let open Capnptk.Declarative in
  let v = decode () in
  let id = v |> get Test_schema.Test.id in
  Alcotest.(check int64) "id matches" 0xa814a386358a5db0L id

let test_read_string () =
  let open Capnptk.Declarative in
  let v = decode () in
  let name = v |> get Test_schema.Test.name in
  Alcotest.(check string) "name matches" "Test" name


let decode_container () =
  let ic = open_in "test_container.bin" in
  let v = Capnptk.Utils.of_input_channel ic in
  close_in ic;
  v |> Capnptk.Utils.decode Test_schema.TestContainer.t

let test_container_pos () =
  let open Capnptk.Declarative in
  let v = decode_container () in
  Alcotest.(check int) "seg offset matches" 0 (ByteStream.pos v.stream).seg;
  Alcotest.(check int) "pos offset matches" 8 (ByteStream.pos v.stream).off

let test_decode_list () =
  let open Capnptk.Declarative in
  let v = decode_container () in
  let xs = v |> get Test_schema.TestContainer.contents in
  Alcotest.(check int) "set offset matches" 3 (Array.length xs);
  xs |> Array.iteri (fun n v -> 
      let id = Int64.(add (of_int n) 0xa814a386358a5db0L) in
      Alcotest.(check int64) "id matches" id (v |> get Test_schema.Test.id);
      let name = match n with
        | 1 -> "Test2"
        | 2 -> "Test3"
        | _ -> "Test"
      in
      Alcotest.(check string) "name matches" name (v |> get Test_schema.Test.name)
    );

    let xs = v |> get Test_schema.TestContainer.list8 in
    Alcotest.(check (array int)) "uint8 list matches" [|0;1;2;3;4|] xs;
    let xs = v |> get Test_schema.TestContainer.list16 in
    Alcotest.(check (array int)) "uint16 list matches" [|0;1;2;3;4|] xs


let () =
  let open Alcotest in
  run "Serialization" [
    "decode", [
      test_case "test_read_pos" `Quick test_read_pos;
      test_case "test_read_int64" `Quick test_read_int64;
      test_case "test_read_string" `Quick test_read_string;
      test_case "test_container_pos" `Quick test_container_pos;
      test_case "test_decode_list" `Quick test_decode_list;
    ]
  ]
