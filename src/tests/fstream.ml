module ByteStream = Capnptk.Fstream.ByteStream

let write_three_i64 () =
  let xs = [0xf5e228fcfadaeeb5L; 0x32c9d31b379b1c1aL; 0xfac56c5e2ceaed39L] in
  let b = ByteStream.create 24 in
  List.iter (fun x -> ByteStream.write_i64 b x) xs;
  ByteStream.setpos b 0;
  let ys = List.map (fun _ -> ByteStream.read_i64 b) xs in
  Alcotest.(check (list int64)) "same lists" xs ys

let write_four_i64 () =
  let xs = [0L;1L;2L] in
  let b = ByteStream.create 24 in
  List.iter (fun x -> ByteStream.write_i64 b x) xs;
  Alcotest.(check_raises) "overflow" End_of_file begin fun () -> 
    ByteStream.write_i64 b 3L
  end

let write_three_i32 () =
  let xs = [0x53749e33l; 0xafcbb23cl; 0xc2b0da3bl] in
  let b = ByteStream.create 12 in
  List.iter (fun x -> ByteStream.write_i32 b x) xs;
  ByteStream.setpos b 0;
  let ys = List.map (fun _ -> ByteStream.read_i32 b) xs in
  Alcotest.(check (list int32)) "same lists" xs ys

let write_four_i32 () =
  let xs = [0l;1l;2l] in
  let b = ByteStream.create 12 in
  List.iter (fun x -> ByteStream.write_i32 b x) xs;
  Alcotest.(check_raises) "overflow" End_of_file begin fun () -> 
    ByteStream.write_i32 b 3l
  end


let write_three_u16 () =
  let xs = [0x8d43; 0xa7ba; 0xdd02] in
  let b = ByteStream.create 6 in
  List.iter (fun x -> ByteStream.write_u16 b x) xs;
  ByteStream.setpos b 0;
  let ys = List.map (fun _ -> ByteStream.read_u16 b) xs in
  Alcotest.(check (list int)) "same lists" xs ys

let write_four_u16 () =
  let xs = [0;1;2] in
  let b = ByteStream.create 6 in
  List.iter (fun x -> ByteStream.write_u16 b x) xs;
  Alcotest.(check_raises) "overflow" End_of_file begin fun () -> 
    ByteStream.write_u16 b 3
  end

let write_three_u8 () =
  let xs = [0x8d; 0xa7; 0xdd] in
  let b = ByteStream.create 3 in
  List.iter (fun x -> ByteStream.write_u8 b x) xs;
  ByteStream.setpos b 0;
  let ys = List.map (fun _ -> ByteStream.read_u8 b) xs in
  Alcotest.(check (list int)) "same lists" xs ys

let write_four_u8 () =
  let xs = [0;1;2] in
  let b = ByteStream.create 3 in
  List.iter (fun x -> ByteStream.write_u8 b x) xs;
  Alcotest.(check_raises) "overflow" End_of_file begin fun () -> 
    ByteStream.write_u8 b 3
  end

let () =
  let open Alcotest in
  run "FStream" [
    "ByteStream", [
      test_case "write_three_i64" `Quick write_three_i64;
      test_case "write_four_i64" `Quick write_four_i64;
      test_case "write_three_i32" `Quick write_three_i32;
      test_case "write_four_i32" `Quick write_four_i32;
      test_case "write_three_u16" `Quick write_three_u16;
      test_case "write_four_u16" `Quick write_four_u16;
      test_case "write_three_u8" `Quick write_three_u8;
      test_case "write_four_u8" `Quick write_four_u8;
    ]
  ]
