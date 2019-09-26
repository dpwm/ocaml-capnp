module ByteStream = Capnptk.Fstream.ByteStream
module Pos = Capnptk.Fstream.Pos

let write_read_n t fw fr n m xs () =
  let b = ByteStream.create n in
  List.iter (fun x -> fw b x) xs;
  Alcotest.(check int) "bytestream has correct length" (ByteStream.length b) m;
  ByteStream.setpos b Pos.empty;
  let ys = List.map (fun _ -> fr b) xs in
  Alcotest.(check (list t)) "bytestream reads what has been written" xs ys


let wr_i64 = ByteStream.(write_read_n Alcotest.int64 write_i64 read_i64 (3 * 8) 24 [
    0xf5e228fcfadaeeb5L; 0x32c9d31b379b1c1aL; 0xfac56c5e2ceaed39L])

let wr_i64_2 = ByteStream.(write_read_n Alcotest.int64 write_i64 read_i64 2 24 [
    0xf5e228fcfadaeeb5L; 0x32c9d31b379b1c1aL; 0xfac56c5e2ceaed39L;])

let wr_i64_3 = ByteStream.(write_read_n Alcotest.int64 write_i64 read_i64 16 2400 (List.init 100 (fun _ -> [
    0xf5e228fcfadaeeb5L; 0x32c9d31b379b1c1aL; 0xfac56c5e2ceaed39L]) |> List.concat))

let wr_i32 = ByteStream.(write_read_n Alcotest.int32 write_i32 read_i32 (3 * 4) 12 [
    0x53749e33l; 0xafcbb23cl; 0xc2b0da3bl])

let wr_i32_2 = ByteStream.(write_read_n Alcotest.int32 write_i32 read_i32 2 12 [
    0x53749e33l; 0xafcbb23cl; 0xc2b0da3bl])

let wr_u16 = ByteStream.(write_read_n Alcotest.int write_u16 read_u16 1 6 [0x8d43; 0xa7ba; 0xdd02])

let wr_u8 = ByteStream.(write_read_n Alcotest.int write_u8 read_u8 1 3 [0x8d; 0xa7; 0xdd])

let () =
  let open Alcotest in
  run "FStream" [
    "ByteStream", [
      test_case "wr_i64" `Quick wr_i64;
      test_case "wr_i64_2" `Quick wr_i64_2;
      test_case "wr_i64_3" `Quick wr_i64_3;
      test_case "wr_i32" `Quick wr_i32;
      test_case "wr_i32_2" `Quick wr_i32_2;
      test_case "wr_u16" `Quick wr_u16;
      test_case "wr_u8" `Quick wr_u8;
    ]
  ]
