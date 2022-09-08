open StdLabels

module type T = sig
  type t [@@deriving show, eq]
  val to_proto : t -> Ocaml_protoc_plugin.Writer.t
  val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
  val name' : unit -> string
end

let hexlify data =
  let acc = ref [] in
  String.iter ~f:(fun ch -> (acc := Char.code ch :: !acc)) data;
  List.rev !acc
  |> List.map ~f:(Printf.sprintf "%02x")
  |> String.concat ~sep:"-"
  |> Caml.Printf.printf "Buffer: '%s'\n"

let dump_protoc name data =
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
         "protoc --decode=%s %s < %s"
         type_name
         protobuf_file
         filename)
  in
  Sys.remove filename;
  match res with
  | 0 -> ()
  | n -> Printf.printf "'protoc' exited with status code: %d\n" n


(** Create a common function for testing. *)
let test_encode (type t) ?dump ?(protoc=true) (module M : T with type t = t) ?(validate : t option) (expect : t) =
  let () = match validate with
    | Some v when v <> expect -> Printf.printf "Validate match failed\n"
    | _ -> ()
  in
  let data = M.to_proto expect |> Ocaml_protoc_plugin.Writer.contents in
  let () =
    match dump with
    | Some _ -> hexlify data
    | None -> ()
  in
  let () = match protoc with
    | true -> dump_protoc (M.name' ()) data
    | false -> ()
  in
  (* Decode the message *)
  let in_data = Ocaml_protoc_plugin.Reader.create data in
  match M.from_proto in_data with
  | Ok observed when M.equal expect observed -> ()
  | Ok observed ->
    Printf.printf "\nExpect  :%s\nObserved:%s\n" ([%show: M.t] expect) ([%show: M.t] observed)
  | Error err ->
    Printf.printf "\nDecode failed: %s \n" (Ocaml_protoc_plugin.Result.show_error err)
