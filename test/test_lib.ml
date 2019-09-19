open Base

module type T = sig
  type t [@@deriving show, eq]
  val to_proto : t -> Protobuf.Writer.t
  val from_proto : Protobuf.Reader.t -> (t, Protobuf.Deserialize.error) Result.t
  val name : string
end

let hexlify data =
  String.to_list data
  |> List.map ~f:Char.to_int
  |> List.map ~f:(Printf.sprintf "%02x")
  |> String.concat ~sep:"-"
  |> Stdlib.Printf.printf "Buffer: '%s'\n"

(** Create a common function for testing. *)
let test_encode ?dump protobuf_file (type t) (module M : T with type t = t) (expect : t) =
  let filename = Stdlib.Filename.temp_file M.name ".bin" in
  let cout = Stdlib.open_out filename in
  let data = M.to_proto expect |> Protobuf.Writer.contents in
  let () =
    match dump with
    | Some _ -> hexlify data
    | None -> ()
  in
  Stdlib.output_string cout data;
  Stdlib.close_out cout;
  Stdlib.Printf.printf "%!";
  (* flush *)
  let _:int = Stdlib.Sys.command
      (Printf.sprintf
         "protoc --decode=%s %s < %s | tr \"\\n\" \"; \" | sed -E 's/ +/ /g'"
         M.name
         protobuf_file
         filename)
  in
  Stdlib.Sys.remove filename;
  (* Decode the message *)
  let in_data = Protobuf.Reader.create data in
  match M.from_proto in_data with
  | Ok observed when M.equal expect observed -> ()
  | Ok observed ->
    Stdlib.Printf.printf "\nExpect  :%s\nObserved:%s\n" ([%show: M.t] expect) ([%show: M.t] observed)
  | Error err ->
    Stdlib.Printf.printf "\nDecode failed: %s\n" (Protobuf.Deserialize.show_error err)
