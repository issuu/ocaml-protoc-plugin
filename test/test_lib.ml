open StdLabels
open Ocaml_protoc_plugin

module type T = sig
  type t [@@deriving show, eq]
  val to_proto' : Writer.t -> t -> Writer.t
  val to_proto : t -> Writer.t
  val from_proto : Reader.t -> t Result.t
  val name' : unit -> string
  val merge: t -> t -> t
end

let hexlify data =
  let acc = ref [] in
  String.iter ~f:(fun ch -> (acc := Char.code ch :: !acc)) data;
  List.rev !acc
  |> List.map ~f:(Printf.sprintf "%02x")
  |> String.concat ~sep:"-"
  |> Printf.printf "Buffer: '%s'\n"

let dump_protoc ?(protoc_args=[]) name data =
  let protobuf_file, type_name =
    match String.split_on_char ~sep:'.' name with
    | protobuf_name :: type_name ->
      Printf.sprintf "%s.proto" (String.uncapitalize_ascii protobuf_name),
      String.concat ~sep:"." type_name
      | _ -> failwith "Illegal type name"
  in
  let filename = Filename.temp_file name ".bin" in
  let cout = open_out filename in
  output_string cout data;
  close_out cout;
  Printf.printf "%!";
  let res = Sys.command
      (Printf.sprintf
         "protoc %s --decode=%s %s < %s"
         (String.concat ~sep:" " protoc_args)
         type_name
         protobuf_file
         filename)
  in
  Sys.remove filename;
  match res with
  | 0 -> ()
  | n -> Printf.printf "'protoc' exited with status code: %d\n" n

let test_merge (type t) (module M : T with type t = t) (t: t) =
  let iterations = [1;2;3;4] in
  let writer = Writer.init () in
  let _ =
    List.fold_left ~init:(writer, t) ~f:(fun (writer, expect) i ->
      let writer = M.to_proto' writer t in
      let contents = Writer.contents writer |> Reader.create in
      let () =
        match M.from_proto contents with
        | Error err ->  Printf.printf "Error decoding after %d iterations: %s\n" i (Result.show_error err)
        | Ok observed when M.equal expect observed -> ()
        | Ok observed ->
          Printf.printf "Wrong value after %d iterations\nExpect: %s\nObserved:%s\n" i ([%show: M.t] expect) ([%show: M.t] observed)
      in
      (writer, M.merge expect t)
    ) iterations
  in
  ()



(** Create a common function for testing. *)
let test_encode (type t) ?dump ?(protoc=true) ?protoc_args (module M : T with type t = t) ?(validate : t option) ?(expect : t option) (t : t) =
  let expect = Option.value ~default:t expect in
  let () = match validate with
    | Some v when v <> expect -> Printf.printf "Validate match failed\n"
    | _ -> ()
  in
  let data = M.to_proto expect |> Writer.contents in

  let () =
    match dump with
    | Some _ -> hexlify data
    | None -> ()
  in
  let () = match protoc with
    | true -> dump_protoc ?protoc_args (M.name' ()) data
    | false -> ()
  in
  let in_data_unordered =
    let writer = Writer.init () in
    Writer.write_field writer (1 lsl 29 - 1) (Field.varint_unboxed 5);
    let _ = M.to_proto' writer expect in
    Reader.create (Writer.contents writer)
  in
  let in_data = Reader.create data in
  match (M.from_proto in_data, M.from_proto in_data_unordered) with
  | Ok observed, Ok observed_unordered -> begin
      match M.equal expect observed, M.equal expect observed_unordered with
      | true, true ->
        test_merge (module M) expect
      | false, _ ->
        Printf.printf "\nExpect: %s\nObserved:%s\n" ([%show: M.t] expect) ([%show: M.t] observed)
      | _, false ->
        Printf.printf "\nExpect(unordered):%s\nObserved:%s\n" ([%show: M.t] expect) ([%show: M.t] observed_unordered)
    end
  | Error err, _ ->
    Printf.printf "\nDecode failed: %s \n" (Result.show_error err)
  | _, Error err ->
    Printf.printf "\nDecode unordered failed: %s \n" (Result.show_error err)
