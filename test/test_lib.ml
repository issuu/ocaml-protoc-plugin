open Base

module type T = sig
  type t [@@deriving show, eq]
  val to_proto : t -> Protobuf.Writer.t
  val from_proto : Protobuf.Reader.t -> (t, Protobuf.Deserialize.error) Result.t
  val name : unit -> string
end

let hexlify data =
  String.to_list data
  |> List.map ~f:Char.to_int
  |> List.map ~f:(Printf.sprintf "%02x")
  |> String.concat ~sep:"-"
  |> Caml.Printf.printf "Buffer: '%s'\n"

let dump_protoc name data =
  let protobuf_file, type_name =
    match String.split ~on:'.' name with
    | protobuf_name :: type_name ->
      Printf.sprintf "%s.proto" (String.uncapitalize protobuf_name),
      String.concat ~sep:"." type_name
      | _ -> failwith "Illegal type name"
  in
  let filename = Caml.Filename.temp_file name ".bin" in
  let cout = Caml.open_out filename in
  Caml.output_string cout data;
  Caml.close_out cout;
  Caml.Printf.printf "%!";
  let res = Caml.Sys.command
      (Printf.sprintf
         "protoc --decode=%s %s < %s"
         type_name
         protobuf_file
         filename)
  in
  Caml.Sys.remove filename;
  match res with
  | 0 -> ()
  | n -> Caml.Printf.printf "'protoc' exited with status code: %d\n" n


(** Create a common function for testing. *)
let test_encode (type t) ?dump ?(protoc=true) (module M : T with type t = t) (expect : t) =
  let data = M.to_proto expect |> Protobuf.Writer.contents in
  let () =
    match dump with
    | Some _ -> hexlify data
    | None -> ()
  in
  let () = match protoc with
    | true -> dump_protoc (M.name ()) data
    | false -> ()
  in
  (* Decode the message *)
  let in_data = Protobuf.Reader.create data in
  match M.from_proto in_data with
  | Ok observed when M.equal expect observed -> ()
  | Ok observed ->
    Caml.Printf.printf "\nExpect  :%s\nObserved:%s\n" ([%show: M.t] expect) ([%show: M.t] observed)
  | Error err ->
    Caml.Printf.printf "\nDecode failed: %s \n" (Protobuf.Deserialize.show_error err)
